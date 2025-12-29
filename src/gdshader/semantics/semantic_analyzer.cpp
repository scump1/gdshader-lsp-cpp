
#include "gdshader/semantics/semantic_analyzer.hpp"

namespace gdshader_lsp {

AnalysisResult SemanticAnalyzer::analyze(const ProgramNode* ast) 
{
    symbols = SymbolTable(); // Clear
    diagnostics.clear();
    typeRegistry = TypeRegistry();

    currentShaderType = ShaderType::Spatial; // Reset default
    currentProcessorFunction = ShaderStage::Global;

    registerGlobalFunctions();
    
    if (ast) {
        visitProgram(ast);
    }
    
    return { std::move(symbols), std::move(typeRegistry), diagnostics };
}

// -------------------------------------------------------------------------
// TOP LEVEL
// -------------------------------------------------------------------------

void SemanticAnalyzer::visitShaderType(const ShaderTypeNode* node) 
{
    if (node->shaderType == "canvas_item") currentShaderType = ShaderType::CanvasItem;
    else if (node->shaderType == "particles") currentShaderType = ShaderType::Particles;
    else if (node->shaderType == "sky") currentShaderType = ShaderType::Sky;
    else if (node->shaderType == "fog") currentShaderType = ShaderType::Fog;
    else if (node->shaderType == "spatial") currentShaderType = ShaderType::Spatial;
    else {
        reportError(node, "Unknown shader type, must be on of: canvas_item, particles, sky, fog, or spatial.");
        currentShaderType = ShaderType::Spatial;
    }
}

void SemanticAnalyzer::visitUniform(const UniformNode* node) 
{
    TypePtr type = typeRegistry.getType(node->type);
    Symbol s{node->name, type, {type}, SymbolType::Uniform, node->line, node->column, node->hint};

    if (!symbols.add(s)) reportError(node, "Redefinition of uniform '" + s.name + "'");
    if (node->defaultValue) {
        visit(node->defaultValue.get());
        TypePtr valType = resolveType(node->defaultValue.get());
        if (*valType != *type && valType->kind != TypeKind::UNKNOWN) {
            reportError(node, "Type mismatch: Cannot initialize uniform '" + node->type + 
                "' with value of type '" + valType->toString() + "'");
        }
    }

    if (!node->hint.empty()) {
        
        std::string hintName = node->hint;
        size_t parenPos = hintName.find('(');
        if (parenPos != std::string::npos) {
            hintName = hintName.substr(0, parenPos);
        }

        // Validate based on the Godot Docs table
        bool valid = true;
        if (hintName == "source_color") {
            // Valid for: vec3, vec4, sampler2D
            valid = (type->name == "vec3" || type->name == "vec4" || type->name == "sampler2D");
        } 
        else if (hintName == "hint_range") {
            // Valid for: int, float
            valid = (type->name == "int" || type->name == "float");
        }
        else if (hintName == "hint_normal" || 
                 hintName == "hint_default_white" || 
                 hintName == "hint_default_black" ||
                 hintName == "hint_default_transparent" ||
                 hintName == "hint_screen_texture" || 
                 hintName == "hint_depth_texture") {
            // Valid for: sampler2D
            valid = (type->name == "sampler2D");
        }

        if (!valid) {
            reportError(node, "Hint '" + hintName + "' is not valid for uniform type '" + type->name + "'");
        }
    }

}

void SemanticAnalyzer::visitVarying(const VaryingNode* node) 
{
    TypePtr type = typeRegistry.getType(node->type);
    Symbol s{node->name, type, {type}, SymbolType::Varying, node->line, node->column, ""};
    if (!symbols.add(s)) reportError(node, "Redefinition of varying '" + s.name + "'");
}

void SemanticAnalyzer::visitConst(const ConstNode* node) 
{
    TypePtr type = typeRegistry.getType(node->type);
    Symbol s{node->name, type, {type}, SymbolType::Const, node->line, node->column, ""};
    if (!symbols.add(s)) reportError(node, "Redefinition of const '" + s.name + "'");
    if (node->value) {
        visit(node->value.get());
        TypePtr valType = resolveType(node->value.get());
        if (*valType != *type && valType->kind != TypeKind::UNKNOWN) {
            reportError(node, "Type mismatch in const declaration.");
        }
    }
}

void SemanticAnalyzer::visitStruct(const StructNode* node) 
{
    std::vector<std::pair<std::string, TypePtr>> members;

    for(const auto& m : node->members) {
        // getType now handles "float[5]" automatically!
        TypePtr memberType = typeRegistry.getType(m.type);

        if (memberType->kind == TypeKind::UNKNOWN) {
            reportError(node, "Unknown type '" + m.type + "' in struct member");
        }
        
        // Prevent recursive definition
        if (memberType->name == node->name) {
            reportError(node, "Struct '" + node->name + "' cannot contain itself.");
        }

        members.push_back({m.name, memberType});
    }

    typeRegistry.registerStruct(node->name, members);

    // Register the Struct itself as a Symbol so it can be used later
    TypePtr structType = typeRegistry.getType(node->name);
    Symbol s{node->name, structType, {structType}, SymbolType::Struct, node->line, node->column, ""};
    
    if (!symbols.add(s)) reportError(node, "Redefinition of struct '" + s.name + "'");
}

void SemanticAnalyzer::visitFunction(const FunctionNode* node) 
{
    TypePtr returnType = typeRegistry.getType(node->returnType);
    std::vector<TypePtr> paramTypes;
    for (const auto& arg : node->arguments) {
        paramTypes.push_back(typeRegistry.getType(arg.type));
    }

    // 2. Create Symbol with signature
    Symbol funcSym{
        node->name, 
        returnType, 
        paramTypes,        // <--- NEW
        SymbolType::Function, 
        node->line, 
        node->column, 
        ""
    };

    if (!symbols.add(funcSym)) {
        reportError(node, "Redefinition of function '" + node->name + "' with same signature");
    }
    symbols.pushScope(node->line);

    ShaderStage previousStage = currentProcessorFunction;
    bool previousReturnFlag = currentFunctionHasReturn;
    TypePtr previousExpected = currentExpectedReturnType;

    if (node->name == "vertex") currentProcessorFunction = ShaderStage::Vertex;
    else if (node->name == "fragment") currentProcessorFunction = ShaderStage::Fragment;
    else if (node->name == "light") currentProcessorFunction = ShaderStage::Light;
    else currentProcessorFunction = ShaderStage::Global;

    currentExpectedReturnType = returnType;
    currentFunctionHasReturn = false;

    // Load built-ins if this is a processor function (vertex, fragment, etc.)
    loadBuiltinsForFunction(node->name);

    if (isProcessorFunction(node->name)) {
        if (node->returnType != "void") {
            reportError(node, "Processor function '" + node->name + "' must return 'void'.");
        }
        if (!node->arguments.empty()) {
            reportError(node, "Processor function '" + node->name + "' must not have arguments.");
        }
    }
 
    for (size_t i = 0; i < node->arguments.size(); i++) {
        const auto& arg = node->arguments[i];
        TypePtr t = paramTypes[i];
        Symbol argSym{arg.name, t, {t}, SymbolType::Variable, node->line, node->column, ""};
        symbols.add(argSym);
    }

    if (node->body) {
        for (const auto& stmt : node->body->statements) {
            visit(stmt.get());
        }
    }

    // Return Check
    if (node->returnType != "void" && !currentFunctionHasReturn) {
        reportError(node, "Function '" + node->name + "' must return a value");
    }
    
    // Restore State
    currentProcessorFunction = previousStage;
    currentFunctionHasReturn = previousReturnFlag;
    currentExpectedReturnType = previousExpected;

    int endLine = node->line; 
    if (node->body) {
        endLine = !node->body->statements.empty() ? node->body->statements.back()->line : node->body->endLine;
    }
    symbols.popScope(endLine);

    if (returnType->name != "void" && !allPathsReturn(node->body.get())) {
        reportError(node, "Not all code paths return a value in function '" + node->name + "'");
    }

}


void SemanticAnalyzer::visit(const ASTNode* node) 
{
    if (!node) return;

    if (auto p = dynamic_cast<const ProgramNode*>(node)) visitProgram(p);
    else if (auto s = dynamic_cast<const ShaderTypeNode*>(node)) visitShaderType(s);
    else if (auto u = dynamic_cast<const UniformNode*>(node)) visitUniform(u);
    else if (auto v = dynamic_cast<const VaryingNode*>(node)) visitVarying(v);
    else if (auto c = dynamic_cast<const ConstNode*>(node)) visitConst(c);
    else if (auto st = dynamic_cast<const StructNode*>(node)) visitStruct(st);
    else if (auto f = dynamic_cast<const FunctionNode*>(node)) visitFunction(f);
    
    else if (auto b = dynamic_cast<const BlockNode*>(node)) visitBlock(b);
    else if (auto vd = dynamic_cast<const VariableDeclNode*>(node)) visitVarDecl(vd);
    else if (auto i = dynamic_cast<const IfNode*>(node)) visitIf(i);
    else if (auto fr = dynamic_cast<const ForNode*>(node)) visitFor(fr);
    else if (auto w = dynamic_cast<const WhileNode*>(node)) visitWhile(w);
    else if (auto ret = dynamic_cast<const ReturnNode*>(node)) visitReturn(ret);
    else if (auto es = dynamic_cast<const ExpressionStatementNode*>(node)) visitExpressionStatement(es);
    else if (auto dw = dynamic_cast<const DoWhileNode*>(node)) visitDoWhile(dw);
    else if (auto sw = dynamic_cast<const SwitchNode*>(node)) visitSwitch(sw);
    else if (auto d = dynamic_cast<const DiscardNode*>(node)) visitDiscard(d);

    else if (auto expr = dynamic_cast<const ExpressionNode*>(node)) visitExpression(expr);
}

void SemanticAnalyzer::visitProgram(const ProgramNode* node) 
{
    // First pass: Find shader_type (it affects everything else)
    for (const auto& child : node->nodes) {
        if (auto s = dynamic_cast<const ShaderTypeNode*>(child.get())) {
            visitShaderType(s);
            break; 
        }
    }

    // Second pass: Visit everything
    for (const auto& child : node->nodes) {
        // Skip ShaderType as we already handled it
        if (dynamic_cast<const ShaderTypeNode*>(child.get())) continue;
        visit(child.get());
    }
}

// -------------------------------------------------------------------------
// STATEMENTS
// -------------------------------------------------------------------------

void SemanticAnalyzer::visitBlock(const BlockNode* node) 
{
    symbols.pushScope(node->line);
    for (const auto& stmt : node->statements) {
        visit(stmt.get());
    }
    symbols.popScope(node->endLine);
}

void SemanticAnalyzer::visitVarDecl(const VariableDeclNode* node) 
{
    TypePtr type = typeRegistry.getType(node->type);

    if (type->kind == TypeKind::UNKNOWN) reportError(node, "Unknown type '" + node->type + "'");

    Symbol s{node->name, type, {type}, SymbolType::Variable, node->line, node->column, ""};
    if (!symbols.add(s)) reportError(node, "Redefinition of variable '" + s.name + "'");
    
    if (node->initializer) {
        visit(node->initializer.get());
        TypePtr initType = resolveType(node->initializer.get());
        
        // Smart Comparison using TypePtr
        if (initType->kind != TypeKind::UNKNOWN && *initType != *type) {
            reportError(node, "Type mismatch: Cannot initialize '" + type->toString() + 
                "' with '" + initType->toString() + "'");
        }
    }
}

void SemanticAnalyzer::visitIf(const IfNode* node) 
{
    if (node->condition) visit(node->condition.get());
    TypePtr condT = resolveType(node->condition.get());

    if (condT->name != "bool" && condT->kind != TypeKind::UNKNOWN) reportError(node, "If condition must be bool");

    if (node->thenBranch) visit(node->thenBranch.get());
    if (node->elseBranch) visit(node->elseBranch.get());
}

void SemanticAnalyzer::visitFor(const ForNode* node) 
{
    symbols.pushScope(node->line); // 'for' creates a scope for init variable
    if (node->init) visit(node->init.get());
    if (node->condition) visit(node->condition.get());
    if (node->increment) visit(node->increment.get());
    if (node->body) visit(node->body.get());
    symbols.popScope(node->endLine);
}

void SemanticAnalyzer::visitWhile(const WhileNode* node) 
{
    if (node->condition) visit(node->condition.get());
    if (node->body) visit(node->body.get());
}

void SemanticAnalyzer::visitReturn(const ReturnNode* node) 
{
    TypePtr actualType = typeRegistry.getType("void");
    currentFunctionHasReturn = true;
    
    if (node->value) {
        visit(node->value.get());
        actualType = resolveType(node->value.get());
    }

    if (!currentExpectedReturnType) return; // Should not happen

    if (currentExpectedReturnType->name == "void") {
        if (actualType->name != "void") reportError(node, "Void function cannot return a value");
    } else {
        if (actualType->name == "void") {
            reportError(node, "Function must return a value of type '" + currentExpectedReturnType->toString() + "'");
        } else if (actualType->kind != TypeKind::UNKNOWN && *actualType != *currentExpectedReturnType) {
            reportError(node, "Type mismatch: Expected return type '" + currentExpectedReturnType->toString() + 
                "' but found '" + actualType->toString() + "'");
        }
    }
}

void SemanticAnalyzer::visitExpressionStatement(const ExpressionStatementNode* node) {
    if (node->expr) visit(node->expr.get());
}

void SemanticAnalyzer::visitDoWhile(const DoWhileNode* node) 
{
    if (node->body) visit(node->body.get());
    
    if (node->condition) {
        visit(node->condition.get());
    }
}

void SemanticAnalyzer::visitSwitch(const SwitchNode* node) 
{
    TypePtr exprType = typeRegistry.getUnknownType();
    
    if (node->expression) {
        visit(node->expression.get());
        exprType = resolveType(node->expression.get());
        
        // GLSL/Godot Switch only allows integers
        if (exprType->name != "int" && exprType->name != "uint" && exprType->name != "unknown") {
            reportError(node->expression.get(), "Switch expression must be 'int' or 'uint', found '" + exprType->name + "'");
        }
    }

    for(const auto& c : node->cases) {
        if (!c->isDefault && c->value) {
            visit(c->value.get());
            TypePtr cType = resolveType(c->value.get());
            if (*cType != *exprType && cType->kind != TypeKind::UNKNOWN) {
                reportError(c->value.get(), "Case Type mismatch");
            }
        }
        for (const auto& stmt : c->statements) visit(stmt.get());
    }
}

void gdshader_lsp::SemanticAnalyzer::visitDiscard(const DiscardNode *node)
{
    if (currentProcessorFunction == ShaderStage::Vertex) {
        reportError(node, "'discard' cannot be used in the vertex processor.");
    }
}

// -------------------------------------------------------------------------
// EXPRESSIONS
// -------------------------------------------------------------------------

void SemanticAnalyzer::visitExpression(const ExpressionNode* node) 
{
    if (auto id = dynamic_cast<const IdentifierNode*>(node)) visitIdentifier(id);
    else if (auto bin = dynamic_cast<const BinaryOpNode*>(node)) visitBinaryOp(bin);
    else if (auto un = dynamic_cast<const UnaryOpNode*>(node)) visitUnaryOp(un);
    else if (auto call = dynamic_cast<const FunctionCallNode*>(node)) visitFunctionCall(call);
    else if (auto mem = dynamic_cast<const MemberAccessNode*>(node)) visitMemberAccess(mem);
    else if (auto tern = dynamic_cast<const TernaryNode*>(node)) {
        visit(tern->condition.get());
        visit(tern->trueExpr.get());
        visit(tern->falseExpr.get());
    }
    // LiteralNode needs no checking
}

void SemanticAnalyzer::visitIdentifier(const IdentifierNode* node) 
{
    const Symbol* s = symbols.lookup(node->name);
    if (!s) {
        reportError(node, "Undefined identifier '" + node->name + "'");
    }
}

void SemanticAnalyzer::visitBinaryOp(const BinaryOpNode* node) 
{
    // Check for assignment to const/read-only
    if (node->op == TokenType::TOKEN_EQUAL) {
        visitAssignment(node);
        return;
    }
    
    if (node->left) visit(node->left.get());
    if (node->right) visit(node->right.get());

    TypePtr l = resolveType(node->left.get());
    TypePtr r = resolveType(node->right.get());

    // If children are already "unknown", an error was likely already reported 
    // (e.g., "Undefined identifier"). Stop here to prevent error cascading.
    if (l->kind == TypeKind::UNKNOWN || r->kind == TypeKind::UNKNOWN) return;

    TypePtr result = getBinaryOpResultType(l, r, node->op);
    if (result->kind == TypeKind::UNKNOWN) {
        reportError(node, "Invalid binary operation '" + l->toString() + "' and '" + r->toString() + "'");
    }
}

void SemanticAnalyzer::visitAssignment(const BinaryOpNode* node) 
{
    if (node->right) visit(node->right.get());

    const Symbol* s = getRootSymbol(node->left.get(), symbols);
    TypePtr lType = typeRegistry.getUnknownType();

    if (auto id = dynamic_cast<const IdentifierNode*>(node->left.get())) {
        s = symbols.lookup(id->name);
        if (s) lType = s->type;
        else reportError(node, "Undefined '" + id->name + "'");
    } else if (dynamic_cast<const MemberAccessNode*>(node->left.get())) {
        visit(node->left.get());
        lType = resolveType(node->left.get());
    }

    const ExpressionNode* lhs = node->left.get();
    
    if (auto mem = dynamic_cast<const MemberAccessNode*>(lhs)) {
        // CHECK: Swizzle duplication (e.g. v.xx = vec2(1.0) is INVALID)
        // Only applies if the member is a swizzle (len <= 4 and chars are xyzw/rgba/stpq)
        // We assume valid chars because visitMemberAccess checked that.
        
        std::string s = mem->member;
        if (s.length() <= 4) { // Heuristic: it's likely a swizzle
            
            bool isSwizzleChars = true;
            const std::string validSet = "xyzwrugbstpq"; // Combined sets
            for (char c : s) {
                if (validSet.find(c) == std::string::npos) {
                    isSwizzleChars = false;
                    break;
                }
            }

            if (isSwizzleChars && s.length() <= 4) {
                for(size_t i=0; i<s.length(); ++i) {
                    for(size_t j=i+1; j<s.length(); ++j) {
                        if (s[i] == s[j]) {
                            reportError(node, "Invalid Write Mask: Component '" + 
                            std::string(1, s[i]) + "' is assigned twice.");
                        }
                    }
                }
            }
        }
    }

    // 3. Permission Checks (Const, Uniform, Varying)
    if (s) {
        if (s->category == SymbolType::Const) {
            reportError(node->left.get(), "Cannot assign to constant '" + s->name + "'");
        } 
        else if (s->category == SymbolType::Uniform) {
            reportError(node->left.get(), "Cannot assign to uniform '" + s->name + "'");
        }
        else if (s->category == SymbolType::Varying) {
            
            if (currentProcessorFunction == ShaderStage::Light) {
                reportError(node->left.get(), "Varying '" + s->name + "' cannot be assigned in the light processor.");
            }
            if (currentProcessorFunction != ShaderStage::Vertex) {
                reportError(node->left.get(), "Varyings are read-only in the fragment processor.");
            }
        }
    }

    TypePtr rType = resolveType(node->right.get());
    if (lType->kind != TypeKind::UNKNOWN && rType->kind != TypeKind::UNKNOWN && *lType != *rType) {
        reportError(node, "Type mismatch in assignment");
    }
}

void gdshader_lsp::SemanticAnalyzer::visitArrayAccess(const ArrayAccessNode *node)
{
    if (node->base) visit(node->base.get());
    if (node->index) visit(node->index.get());

    TypePtr baseType = resolveType(node->base.get());
    TypePtr indexType = resolveType(node->index.get());

    // 1. Validate Base is Array or Vector
    if (baseType->kind != TypeKind::ARRAY && baseType->kind != TypeKind::VECTOR && baseType->kind != TypeKind::MATRIX) {
        reportError(node, "Type '" + baseType->toString() + "' is not indexable.");
    }

    // 2. Validate Index is Integer
    if (indexType->name != "int" && indexType->name != "uint") {
        reportError(node->index.get(), "Array index must be an integer.");
    }
}

void SemanticAnalyzer::visitUnaryOp(const UnaryOpNode* node) {
    if (node->operand) visit(node->operand.get());
}

/**
 * @brief Visits any function call, even builtins. We analyze the arguments first, so that we can resolve types and overloads.
 * 
 * @param node 
 */
void SemanticAnalyzer::visitFunctionCall(const FunctionCallNode* node) 
{
    std::vector<TypePtr> argTypes;
    for (const auto& arg : node->arguments) {
        visit(arg.get());
        argTypes.push_back(resolveType(arg.get()));
    }

    std::string name = node->functionName;
    
    // 1. Constructor Check
    // (If the name matches a known type, it's a constructor)
    if (typeRegistry.getType(name)->kind != TypeKind::UNKNOWN) {
        validateConstructor(node, name);
        return; 
    }

    // 2. Function Lookup
    // This now finds BOTH User functions AND Built-ins (because of registerGlobalFunctions)
    std::vector<const Symbol*> candidates = symbols.lookupFunctions(name);

    if (candidates.empty()) {
        reportError(node, "Unknown function '" + name + "'");
        return;
    }

    // 3. Overload Resolution
    const Symbol* match = nullptr;
    
    for (const auto* sym : candidates) {
        if (sym->parameterTypes.size() != argTypes.size()) continue;

        bool signatureMatches = true;
        for (size_t i = 0; i < argTypes.size(); i++) {
            // Strict matching (you can add implicit casting here later)
            if (argTypes[i]->kind != TypeKind::UNKNOWN && *sym->parameterTypes[i] != *argTypes[i]) {
                signatureMatches = false;
                break;
            }
        }
        
        if (signatureMatches) {
            match = sym;
            break; 
        }
    }
    
    if (!match) {
        // Improve error message to show what was expected
        std::string argsStr = "";
        for(auto& t : argTypes) argsStr += t->toString() + ", ";
        if (!argsStr.empty()) {
            argsStr.pop_back(); 
            argsStr.pop_back();
        } 
        reportError(node, "No matching function for '" + name + "(" + argsStr + ")'");
    }
}

void SemanticAnalyzer::visitMemberAccess(const MemberAccessNode* node) 
{
    if (node->base) visit(node->base.get());
    TypePtr baseT = resolveType(node->base.get());

    if (baseT->kind == TypeKind::UNKNOWN) return;

    // FIX: Allow .length() on arrays
    if (baseT->kind == TypeKind::ARRAY && node->member == "length") {
        return; // Valid
    }

    TypePtr memberT = typeRegistry.getMemberType(baseT, node->member);
    if (memberT->kind == TypeKind::UNKNOWN) {
        reportError(node, "Invalid member '" + node->member + "' on type '" + baseT->toString() + "'");
    }
}

// -------------------------------------------------------------------------
// HELPERS
// -------------------------------------------------------------------------

void SemanticAnalyzer::validateConstructor(const FunctionCallNode* node, const std::string& typeName) 
{
    TypePtr target = typeRegistry.getType(typeName);
    
    if (target->kind == TypeKind::STRUCT) {
        
        if (node->arguments.size() != target->members.size()) {
            reportError(node, "Constructor for '" + typeName + "' expects " + 
                        std::to_string(target->members.size()) + " arguments, but got " + 
                        std::to_string(node->arguments.size()));
            return;
        }

        for(size_t i = 0; i < target->members.size(); ++i) {
            TypePtr argType = resolveType(node->arguments[i].get());
            TypePtr expectedType = target->members[i].second;

            // Strict Type Check (Structs don't usually allow implicit casting in constructors)
            // This works for Arrays too because of your Type::operator==
            if (*argType != *expectedType) {
                reportError(node->arguments[i].get(), 
                    "Type mismatch in struct constructor argument " + std::to_string(i+1) + 
                    ". Expected '" + expectedType->toString() + "', found '" + argType->toString() + "'");
            }
        }
        return;
    }

    if (target->kind == TypeKind::VECTOR) {
        int expected = target->componentCount;
        int provided = 0;

        for (const auto& arg : node->arguments) {
            TypePtr argT = resolveType(arg.get());
            
            // 1. Basic Type Check
            // Ensure base types match (e.g. no bools in vec3 constructor)
            if (argT->baseType && target->baseType && *argT->baseType != *target->baseType) {
                // Exception: vec3(float) is valid (splatting)
                if (node->arguments.size() == 1 && argT->kind == TypeKind::SCALAR) return; 
                
                reportError(arg.get(), "Type mismatch in constructor.");
            }

            // 2. Count Components
            provided += (argT->kind == TypeKind::SCALAR) ? 1 : argT->componentCount;
        }

        // Exception: Splatting (vec3(1.0) -> 1,1,1)
        if (provided == 1 && expected > 1) return;

        if (provided != expected) {
            reportError(node, "Invalid constructor. Expected " + std::to_string(expected) + 
                " components, but found " + std::to_string(provided));
        }
        return;
    }
}

bool gdshader_lsp::SemanticAnalyzer::isProcessorFunction(const std::string &name)
{
    return name == "vertex" || name == "fragment" || name == "light" || 
           name == "start" || name == "process" || name == "sky" || name == "fog";
}

bool gdshader_lsp::SemanticAnalyzer::allPathsReturn(const ASTNode *node)
{
    if (!node) return false;

    if (dynamic_cast<const ReturnNode*>(node)) return true;
    if (dynamic_cast<const DiscardNode*>(node)) return true;
    
    if (auto block = dynamic_cast<const BlockNode*>(node)) {
        for (const auto& stmt : block->statements) {
            if (allPathsReturn(stmt.get())) return true;
        }
        return false;
    }

    if (auto ifNode = dynamic_cast<const IfNode*>(node)) {
        // BOTH branches must return
        return ifNode->elseBranch && 
               allPathsReturn(ifNode->thenBranch.get()) && 
               allPathsReturn(ifNode->elseBranch.get());
    }

    // Note: Loops (For/While) are generally considered "not returning" 
    // because the loop body might never run.
    
    return false;
}

TypePtr gdshader_lsp::SemanticAnalyzer::getBinaryOpResultType(TypePtr l, TypePtr r, TokenType op)
{
    if (l->kind == TypeKind::UNKNOWN || r->kind == TypeKind::UNKNOWN) return typeRegistry.getUnknownType();

    // --- 1. Comparison Operators (Always return bool) ---
    bool isComparison = (op == TokenType::TOKEN_EQ_EQ || op == TokenType::TOKEN_NOT_EQ ||
                         op == TokenType::TOKEN_LESS || op == TokenType::TOKEN_GREATER ||
                         op == TokenType::TOKEN_LESS_EQ || op == TokenType::TOKEN_GREATER_EQ);

    if (isComparison) {
        // Types must generally be comparable (e.g., match exactly, or implicit cast)
        // For strictness: assume they must match types (vec3 == vec3) or be vector/scalar compatible.
        // For now, let's allow it if they are somewhat compatible.
        return typeRegistry.getType("bool"); 
    }

    // --- 2. Arithmetic Exact Match (int+int, vec3+vec3, mat4*mat4) ---
    if (*l == *r) {
        return l; 
    }

    // --- 3. Vector-Scalar Mixing (vec3 * 2.0) ---
    bool lVec = (l->kind == TypeKind::VECTOR);
    bool rVec = (r->kind == TypeKind::VECTOR);
    bool lScalar = (l->kind == TypeKind::SCALAR);
    bool rScalar = (r->kind == TypeKind::SCALAR);

    if (lVec && rScalar) {
        // Check if scalar type matches vector base (float * vec3 vs int * vec3)
        if (l->baseType && *l->baseType == *r) return l; 
    }
    if (rVec && lScalar) {
        if (r->baseType && *r->baseType == *l) return r;
    }

    // --- 4. Matrix Math (The Hard Part) ---
    bool lMat = (l->kind == TypeKind::MATRIX);
    bool rMat = (r->kind == TypeKind::MATRIX);

    // Matrix * Scalar (Component-wise)
    if ((lMat && rScalar) || (rMat && lScalar)) {
         // Assuming float scalar for matrices
         if ((lScalar && l->name == "float") || (rScalar && r->name == "float")) {
             return lMat ? l : r;
         }
    }

    // Matrix * Vector (Linear Algebra: Transform)
    // mat4 (4x4) * vec4 (4x1) -> vec4
    if (lMat && rVec) {
        if (l->componentCount == r->componentCount) return r; 
    }

    // Vector * Matrix (Linear Algebra: Post-multiply)
    // vec4 (1x4) * mat4 (4x4) -> vec4
    if (lVec && rMat) {
        if (l->componentCount == r->componentCount) return l;
    }

    // Matrix * Matrix 
    // mat4 * mat4 -> mat4
    if (lMat && rMat) {
        if (l->componentCount == r->componentCount) return l;
    }

    return typeRegistry.getUnknownType();
}

const Symbol *gdshader_lsp::SemanticAnalyzer::getRootSymbol(const ExpressionNode *node, const SymbolTable &symbols)
{
    if (auto id = dynamic_cast<const IdentifierNode*>(node)) {
        return symbols.lookup(id->name);
    }
    return nullptr;
}

void SemanticAnalyzer::loadBuiltinsForFunction(const std::string& funcName) 
{
    ShaderStage scope = ShaderStage::Global;
    if (funcName == "vertex") scope = ShaderStage::Vertex;
    else if (funcName == "fragment") scope = ShaderStage::Fragment;
    else if (funcName == "light") scope = ShaderStage::Light;
    else if (funcName == "start") scope = ShaderStage::Start;
    else if (funcName == "process") scope = ShaderStage::Process;
    
    if (scope != ShaderStage::Global) {
        const auto& builtins = get_builtins(currentShaderType, scope);
        for (const auto& b : builtins) {
            TypePtr t = typeRegistry.getType(b.type);
            // Builtins have no specific line, so -1
            symbols.add({b.name, t, {t}, SymbolType::Builtin, -1, 0, b.doc});
        }
    }
}

TypePtr gdshader_lsp::SemanticAnalyzer::resolveType(const ExpressionNode *node)
{
    if (!node) return typeRegistry.getType("void");

    // 1. Literal?
    if (auto lit = dynamic_cast<const LiteralNode*>(node)) {
        if (lit->type == TokenType::TOKEN_NUMBER) return (lit->value.find('.') != std::string::npos) ? typeRegistry.getType("float") : typeRegistry.getType("int");
        if (lit->type == TokenType::KEYWORD_TRUE || lit->type == TokenType::KEYWORD_FALSE) return typeRegistry.getType("bool");
        return typeRegistry.getType("void");
    }

    // 2. Identifier? Lookup in Symbol Table
    if (auto id = dynamic_cast<const IdentifierNode*>(node)) {
        if (const Symbol* s = symbols.lookup(id->name)) {
            return s->type;
        }
        return typeRegistry.getUnknownType();
    }

    // 3. Binary Op? (Simplify: float + float = float)
    if (auto bin = dynamic_cast<const BinaryOpNode*>(node)) {
        TypePtr l = resolveType(bin->left.get());
        TypePtr r = resolveType(bin->right.get());
        
        return getBinaryOpResultType(l, r, bin->op);
    }

    if (auto idx = dynamic_cast<const ArrayAccessNode*>(node)) {
        TypePtr base = resolveType(idx->base.get());
        if (base->kind == TypeKind::ARRAY) return base->baseType;
        if (base->kind == TypeKind::VECTOR) return base->baseType;
        if (base->kind == TypeKind::MATRIX) return typeRegistry.getType("vec" + std::to_string(base->componentCount)); // mat4[0] -> vec4
        return typeRegistry.getUnknownType();
    }

    // 4. Function Call? Lookup return type
    if (auto call = dynamic_cast<const FunctionCallNode*>(node)) {
        
        // A. Constructors
        TypePtr t = typeRegistry.getType(call->functionName);
        if (t->kind != TypeKind::UNKNOWN) return t;

        // B. Functions (User & Builtin)
        auto candidates = symbols.lookupFunctions(call->functionName);
        if (!candidates.empty()) {
            // Ideally, we would run the exact same overload resolution as visitFunctionCall here.
            // For now, returning the type of the first candidate is a safe "good enough" heuristic 
            // because overloads in GLSL usually return the same type category (vec for vec, float for float).
            return candidates[0]->type; 
        }
        
        return typeRegistry.getUnknownType();
    }
    
    // 5. Member Access (The Tricky Part)
    if (auto mem = dynamic_cast<const MemberAccessNode*>(node)) {
        if (mem->member == "length") return typeRegistry.getType("int");
        TypePtr base = resolveType(mem->base.get());
        return typeRegistry.getMemberType(base, mem->member);
    }

    if (auto tern = dynamic_cast<const TernaryNode*>(node)) {
        return resolveType(tern->trueExpr.get());
    }

    return typeRegistry.getUnknownType();
}

void SemanticAnalyzer::reportError(const ASTNode* node, const std::string& msg) {
    if (node) diagnostics.push_back({node->line, node->column, msg});
}

void SemanticAnalyzer::registerGlobalFunctions()
{
    auto registerVariant = [&](const BuiltinFunction& f, const std::string& genericReplacement = "") {
        
        // 1. Resolve Return Type
        std::string retTypeName = f.returnType;
        if (!genericReplacement.empty()) {
            // Replace "vec_type" with actual type (e.g. "vec3")
            if (retTypeName == "vec_type" || retTypeName == "gentype") retTypeName = genericReplacement;
        }
        
        TypePtr returnType = typeRegistry.getType(retTypeName);
        // Fallback: If registry doesn't know it (e.g. "bvec"), default to void to prevent crash, 
        // but ideally your registry has everything.
        if (returnType->kind == TypeKind::UNKNOWN && retTypeName != "void") {
             // Optional: Log warning here
        }

        // 2. Resolve Argument Types
        std::vector<TypePtr> params;
        for (const std::string& argRaw : f.argTypes) {
            std::string argName = argRaw;
            if (!genericReplacement.empty()) {
                 if (argName == "vec_type" || argName == "gentype") argName = genericReplacement;
            }
            params.push_back(typeRegistry.getType(argName));
        }

        // 3. Create and Add Symbol
        // We use line -1 for builtins
        Symbol s{f.name, returnType, params, SymbolType::Builtin, -1, 0, f.doc};
        
        // We don't check for redefinitions here because overloads are allowed for functions
        symbols.add(s); 
    };

    for (const auto& func : GLOBAL_FUNCTIONS) {
        
        bool isGeneric = false;
        if (func.returnType == "vec_type" || func.returnType == "gentype") isGeneric = true;
        for(const auto& arg : func.argTypes) {
            if (arg == "vec_type" || arg == "gentype") isGeneric = true;
        }

        if (isGeneric) {
            // Expand Generics: Register for float, vec2, vec3, vec4
            // (You can add int/ivec/uint expansion here too if needed)
            registerVariant(func, "float");
            registerVariant(func, "vec2");
            registerVariant(func, "vec3");
            registerVariant(func, "vec4");
        } else {
            // Register exactly as defined (e.g. texture(sampler2D, vec2))
            registerVariant(func);
        }
    }
}

} // end of namespace