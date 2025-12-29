
#include "gdshader/semantics/semantic_analyzer.hpp"

namespace gdshader_lsp {

AnalysisResult SemanticAnalyzer::analyze(const ProgramNode* ast) 
{
    symbols = SymbolTable(); // Clear
    diagnostics.clear();
    typeRegistry = TypeRegistry();

    currentShaderType = ShaderType::Spatial; // Reset default
    currentProcessorFunction = ShaderStage::Global;

    if (ast) {
        visitProgram(ast);
    }
    
    return { std::move(symbols), std::move(typeRegistry), diagnostics };
}

// -------------------------------------------------------------------------
// TOP LEVEL
// -------------------------------------------------------------------------

void SemanticAnalyzer::visitShaderType(const ShaderTypeNode* node) {
    if (node->shaderType == "canvas_item") currentShaderType = ShaderType::CanvasItem;
    else if (node->shaderType == "particles") currentShaderType = ShaderType::Particles;
    else if (node->shaderType == "sky") currentShaderType = ShaderType::Sky;
    else if (node->shaderType == "fog") currentShaderType = ShaderType::Fog;
    else currentShaderType = ShaderType::Spatial;
}

void SemanticAnalyzer::visitUniform(const UniformNode* node) 
{
    Symbol s{node->name, node->type, {node->type}, SymbolType::Uniform, node->line, node->column, node->hint};
    if (!symbols.add(s)) reportError(node, "Redefinition of uniform '" + s.name + "'");
    if (node->defaultValue) visit(node->defaultValue.get());

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
            valid = (node->type == "vec3" || node->type == "vec4" || node->type == "sampler2D");
        } 
        else if (hintName == "hint_range") {
            // Valid for: int, float
            valid = (node->type == "int" || node->type == "float");
        }
        else if (hintName == "hint_normal" || 
                 hintName == "hint_default_white" || 
                 hintName == "hint_default_black" ||
                 hintName == "hint_default_transparent" ||
                 hintName == "hint_screen_texture" || 
                 hintName == "hint_depth_texture") {
            // Valid for: sampler2D
            valid = (node->type == "sampler2D");
        }

        if (!valid) {
            reportError(node, "Hint '" + hintName + "' is not valid for uniform type '" + node->type + "'");
        }
    }

}

void SemanticAnalyzer::visitVarying(const VaryingNode* node) 
{
    Symbol s{node->name, node->type, {node->type}, SymbolType::Varying, node->line, node->column, ""};
    if (!symbols.add(s)) reportError(node, "Redefinition of varying '" + s.name + "'");
}

void SemanticAnalyzer::visitConst(const ConstNode* node) 
{
    Symbol s{node->name, node->type, {node->type}, SymbolType::Const, node->line, node->column, ""};
    if (!symbols.add(s)) reportError(node, "Redefinition of const '" + s.name + "'");
    if (node->value) visit(node->value.get());
}

void SemanticAnalyzer::visitStruct(const StructNode* node) 
{
    Symbol s{node->name, node->name, {""}, SymbolType::Struct, node->line, node->column, ""};
    if (!symbols.add(s)) reportError(node, "Redefinition of struct '" + s.name + "'");
    
    // REGISTER THE TYPE
    std::unordered_map<std::string, std::string> members;
    for(const auto& m : node->members) {
        members[m.name] = m.type;
    }
    typeRegistry.registerStruct(node->name, members);
}

void SemanticAnalyzer::visitFunction(const FunctionNode* node) 
{
    std::vector<std::string> paramTypes;
    for (const auto& arg : node->arguments) {
        paramTypes.push_back(arg.type);
    }

    // 2. Create Symbol with signature
    Symbol funcSym{
        node->name, 
        node->returnType, 
        paramTypes,        // <--- NEW
        SymbolType::Function, 
        node->line, 
        node->column, 
        ""
    };

    if (!symbols.add(funcSym)) reportError(node, "Redefinition of function '" + node->name + "'");
    symbols.pushScope(node->line);

    ShaderStage previousStage = currentProcessorFunction;
    bool previousReturnFlag = currentFunctionHasReturn;

    if (node->name == "vertex") currentProcessorFunction = ShaderStage::Vertex;
    else if (node->name == "fragment") currentProcessorFunction = ShaderStage::Fragment;
    else if (node->name == "light") currentProcessorFunction = ShaderStage::Light;
    else currentProcessorFunction = ShaderStage::Global;

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
 
    // Add arguments
    for (const auto& arg : node->arguments) {
        Symbol argSym{arg.name, arg.type, {arg.type}, SymbolType::Variable, node->line, node->column, ""};
        symbols.add(argSym);
    }

    if (node->body) {
        // Visit statements directly to avoid pushing another scope for the block
        for (const auto& stmt : node->body->statements) {
            visit(stmt.get());
        }
    }

    if (node->returnType != "void" && !currentFunctionHasReturn) {
        // This is a simple check. It doesn't handle "if/else" branches perfectly,
        // but it catches functions that have ZERO return statements (like your test case).
        reportError(node, "Function '" + node->name + "' must return a value of type '" + node->returnType + "'");
    }
       
    currentProcessorFunction = previousStage;
    currentFunctionHasReturn = previousReturnFlag;
    
    int endLine = node->line; // Default to function start line
    
    if (node->body) {
        if (!node->body->statements.empty()) {
            endLine = node->body->statements.back()->line;
        } else {
            endLine = node->body->endLine;
        }
    }

    symbols.popScope(endLine);
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

void SemanticAnalyzer::visitProgram(const ProgramNode* node) {
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

void SemanticAnalyzer::visitBlock(const BlockNode* node) {
    symbols.pushScope(node->line);
    for (const auto& stmt : node->statements) {
        visit(stmt.get());
    }
    symbols.popScope(node->endLine);
}

void SemanticAnalyzer::visitVarDecl(const VariableDeclNode* node) 
{
    Symbol s{node->name, node->type, {node->type}, SymbolType::Variable, node->line, node->column, ""};
    if (!symbols.add(s)) reportError(node, "Redefinition of variable '" + s.name + "'");
    
    if (node->initializer) {
        visit(node->initializer.get());
        
        std::string initType = resolveType(node->initializer.get());
        
        // The Core Check
        if (initType != "unknown" && initType != node->type) {
            reportError(node, 
                "Type mismatch: Cannot initialize variable of type '" + node->type + 
                "' with value of type '" + initType + "'");
        }
    }
}

void SemanticAnalyzer::visitIf(const IfNode* node) {
    if (node->condition) visit(node->condition.get());
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

void SemanticAnalyzer::visitWhile(const WhileNode* node) {
    if (node->condition) visit(node->condition.get());
    if (node->body) visit(node->body.get());
}

void SemanticAnalyzer::visitReturn(const ReturnNode* node) 
{
    std::string actualType = "void";
    currentFunctionHasReturn = true;
    
    if (node->value) {
        visit(node->value.get());
        actualType = resolveType(node->value.get());
    }

    if (currentExpectedReturnType == "void") {
        if (actualType != "void") {
            reportError(node, "Void function cannot return a value");
        }
    } else {
        if (actualType == "void") {
            reportError(node, "Function must return a value of type '" + currentExpectedReturnType + "'");
        } else if (actualType != "unknown" && actualType != currentExpectedReturnType) {
            // Implicit casting is generally forbidden in return statements too
            reportError(node, 
                "Type mismatch: Expected return type '" + currentExpectedReturnType + 
                "' but found '" + actualType + "'");
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
        std::string type = resolveType(node->condition.get());
        if (type != "bool" && type != "unknown") {
            reportError(node->condition.get(), "Do-While condition must be 'bool', found '" + type + "'");
        }
    }
}

void SemanticAnalyzer::visitSwitch(const SwitchNode* node) 
{
    std::string exprType = "unknown";
    
    // 1. Validate Switch Expression
    if (node->expression) {
        visit(node->expression.get());
        exprType = resolveType(node->expression.get());
        
        // GLSL/Godot Switch only allows integers
        if (exprType != "int" && exprType != "uint" && exprType != "unknown") {
            reportError(node->expression.get(), "Switch expression must be 'int' or 'uint', found '" + exprType + "'");
        }
    }

    // 2. Validate Cases
    for (const auto& c : node->cases) {

        if (!c->isDefault && c->value) {
            visit(c->value.get());
            std::string caseType = resolveType(c->value.get());
            
            if (exprType != "unknown" && caseType != "unknown" && caseType != exprType) {
                reportError(c->value.get(), "Case value type '" + caseType + "' does not match switch type '" + exprType + "'");
            }
            
            if (!isConstantExpression(c->value.get())) {
                reportError(c->value.get(), "Case label must be a constant expression.");
            }
        }

        for (const auto& stmt : c->statements) {
            visit(stmt.get());
        }
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

void SemanticAnalyzer::visitIdentifier(const IdentifierNode* node) {
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

    std::string lType = resolveType(node->left.get());
    std::string rType = resolveType(node->right.get());

    // If children are already "unknown", an error was likely already reported 
    // (e.g., "Undefined identifier"). Stop here to prevent error cascading.
    if (lType == "unknown" || rType == "unknown") return;

    // 4. Validate the operation
    // We call resolveType on the node itself. 
    std::string resultType = resolveType(node);

    if (resultType == "unknown") {
        reportError(node, 
            "Type mismatch: Invalid binary operation between '" + lType + "' and '" + rType + "'");
    }
}

void SemanticAnalyzer::visitAssignment(const BinaryOpNode* node) 
{
    if (node->right) visit(node->right.get());

    std::string lhsType = "unknown";
    const Symbol* s = nullptr; // We need to track the symbol for permission checks

    // 2. Resolve LHS
    if (auto id = dynamic_cast<const IdentifierNode*>(node->left.get())) {
        s = symbols.lookup(id->name);
        if (s) {
            lhsType = s->typeName;
        } else {
             reportError(id, "Undefined identifier '" + id->name + "'");
             return;
        }
    } 
    else if (dynamic_cast<const MemberAccessNode*>(node->left.get())) {
        visit(node->left.get()); // Standard visit
        lhsType = resolveType(node->left.get());
        
        // Advanced: If you want to check if the ROOT of the member access 
        // is a const/uniform (e.g., 'my_uniform.x = 5'), you would need 
        // to dig down to the base IdentifierNode here. 
        // For now, we focus on direct identifier assignment.
    } 
    else {
        reportError(node, "Invalid assignment target");
        return;
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
            // --- MISSING LOGIC ADDED HERE ---
            
            // Rule 1: No varyings in light() [cite: 23]
            if (currentProcessorFunction == ShaderStage::Light) {
                reportError(node->left.get(), "Varying '" + s->name + "' cannot be assigned in the light processor.");
            }
            // Rule 2: Varyings only in main processors (Vertex/Fragment)
            // This catches assignments in custom functions like 'custom_writer()'
            else if (currentProcessorFunction != ShaderStage::Vertex && 
                     currentProcessorFunction != ShaderStage::Fragment) {
                reportError(node->left.get(), "Varying '" + s->name + "' can only be assigned in vertex() or fragment() functions.");
            }
        }
    }

    // 4. Resolve RHS Type
    std::string rhsType = resolveType(node->right.get());

    // 5. Compare Types
    if (lhsType != "unknown" && rhsType != "unknown" && lhsType != rhsType) {
        reportError(node, 
            "Type mismatch: Cannot assign value of type '" + rhsType +
            "' to variable of type '" + lhsType + "'");
    }
}

void SemanticAnalyzer::visitUnaryOp(const UnaryOpNode* node) {
    if (node->operand) visit(node->operand.get());
}

void SemanticAnalyzer::visitFunctionCall(const FunctionCallNode* node) 
{
    /// 1. Check Arguments
    for (const auto& arg : node->arguments) {
        visit(arg.get());
    }

    std::string name = node->functionName;
    
    if (typeRegistry.hasType(name)) {
        validateConstructor(node, name);
        return; // It's a type constructor
    }

    bool userExists = (symbols.lookup(name) != nullptr);
    bool builtinExists = false;
    if (!userExists) {
        for(const auto& f : GLOBAL_FUNCTIONS) {
            if(f.name == name) { builtinExists = true; break; }
        }
    }

    if (userExists || builtinExists) {
        validateFunctionCall(node, name);
    } else {
        reportError(node, "Unknown function '" + name + "'");
    };
}

void SemanticAnalyzer::visitMemberAccess(const MemberAccessNode* node) 
{
    // 1. Check the Base (recursively)
    if (node->base) visit(node->base.get());
    std::string baseType = resolveType(node->base.get());

    if (baseType == "unknown" || baseType == "void") return; // Prevents cascading errors

    std::string memberType = typeRegistry.getMemberType(baseType, node->member);

    if (memberType == "unknown") {
        // Distinguish between Vector and Struct error messages for clarity
        if (baseType.find("vec") != std::string::npos) {
             reportError(node, "Invalid swizzle '" + node->member + "' for type '" + baseType + "'");
        } else {
             reportError(node, "Type '" + baseType + "' has no member named '" + node->member + "'");
        }
    }
}

// -------------------------------------------------------------------------
// HELPERS
// -------------------------------------------------------------------------

void SemanticAnalyzer::validateConstructor(const FunctionCallNode* node, const std::string& typeName) 
{
    // 1. Basic Info
    int targetCount = getComponentCount(typeName);
    std::string targetBase = getElementBaseType(typeName);

    int providedComponents = 0;
    
    // 2. Special Case: Scalar Construction (Casting)
    // float(int_val) or int(float_val) -> Valid
    if (targetCount == 1) {
        if (node->arguments.size() != 1) {
            reportError(node, "Scalar constructor '" + typeName + "' requires exactly 1 argument.");
        }
        return; // Casting is allowed between scalars, so we stop here.
    }

    for (size_t i = 0; i < node->arguments.size(); ++i) 
    {
        std::string argType = resolveType(node->arguments[i].get());

        // 1. REJECT Nonsensical Types immediately
        if (argType == "void") {
            reportError(node, "Cannot use 'void' value in constructor");
            return;
        } 

        if (argType == "string") {
            reportError(node, "Cannot convert 'string' to '" + typeName + "'");
            return;
        }

        // 2. Ensure Argument is Numeric (Scalar, Vector, or Matrix)
        bool isNumeric = (argType == "int" || argType == "uint" || argType == "bool" || argType == "float") ||
                         (argType.find("vec") != std::string::npos) || 
                         (argType.find("mat") != std::string::npos);

        if (!isNumeric) {
            reportError(node, "Invalid argument type '" + argType + "' in constructor for '" + typeName + "'");
            return;
        }

        std::string argBase = getElementBaseType(argType);

        // Rule A: Strict Base Type Matching
        // vec3 (float) cannot be built with ints (ivec3 or int)
        // EXCEPT: If the argument is a single literal number, resolveType might have guessed wrong, 
        // but strict typing says: vec3(1) is error. vec3(1.0) is ok.
        if (argBase != targetBase) {
            reportError(node, 
                "Type mismatch in constructor: '" + typeName + "' requires '" + targetBase + 
                "' arguments, but found '" + argType + "' (" + argBase + ")");
            return;
        }
        
        if (argBase != targetBase && targetCount > 1) { 
            // Allow casting only for Scalar constructors (e.g. float(int))
            // Disallow for Vector constructors (e.g. vec3(int))
            reportError(node, "Type mismatch: '" + typeName + "' expects '" + targetBase + "' components, found '" + argType + "'");
            return;
        }

        providedComponents += getComponentCount(argType);
    }

    // Rule B: Component Count
    // Valid scenarios for vec3:
    // 1. vec3(float) -> Splatting (1 arg, 1 component) -> OK
    // 2. vec3(float, float, float) -> (3 args, 3 components) -> OK
    // 3. vec3(vec2, float) -> (2 args, 2+1 components) -> OK
    
    bool isSplat = (node->arguments.size() == 1) && (providedComponents == 1);
    
    if (!isSplat && providedComponents != targetCount) {
        reportError(node, 
            "Invalid constructor: '" + typeName + "' expects " + std::to_string(targetCount) + 
            " components, but received " + std::to_string(providedComponents));
    }
}

void gdshader_lsp::SemanticAnalyzer::validateFunctionCall(const FunctionCallNode *node, const std::string &funcName)
{
    std::vector<std::string> expectedTypes;
    bool isBuiltin = false;

    // 1. Resolve Expected Types
    // A. Check User Functions first
    if (const Symbol* s = symbols.lookup(funcName)) {
        if (s->category == SymbolType::Function) {
            expectedTypes = s->parameterTypes;
        } else {
            // It's a variable, not a function (e.g. calling a float? invalid)
            reportError(node, "'" + funcName + "' is not a function.");
            return;
        }
    } 
    // B. Check Built-ins
    else {
        for (const auto& f : GLOBAL_FUNCTIONS) {
            if (f.name == funcName) {
                expectedTypes = f.argTypes;
                isBuiltin = true;
                break;
            }
        }
        if (expectedTypes.empty() && !isBuiltin) return; // Unknown function
    }

    // 2. Validate Argument Count
    if (node->arguments.size() != expectedTypes.size()) {
        reportError(node, "Function '" + funcName + "' expects " + 
            std::to_string(expectedTypes.size()) + " arguments, but got " + 
            std::to_string(node->arguments.size()));
        return;
    }

    // 3. Validate Argument Types
    std::string deducedVecType = "";

    for (size_t i = 0; i < expectedTypes.size(); ++i) {
        std::string actual = resolveType(node->arguments[i].get());
        std::string expected = expectedTypes[i];

        // Generic "vec_type" Logic ---
        if (isBuiltin && expected == "vec_type") {
            
            // A. Must be a valid vector or scalar
            if (!isVecType(actual) && actual != "unknown") {
                reportError(node, "Argument " + std::to_string(i + 1) + 
                    " must be float or vector, but found '" + actual + "'");
                continue;
            }

            // B. Consistency Check
            // If this is the first vec_type we've seen, lock it in.
            // If we've seen one before, this argument MUST match it.
            // (e.g. pow(vec3, vec2) is invalid, must be pow(vec3, vec3))
            if (deducedVecType.empty()) {
                deducedVecType = actual;
            } else if (actual != "unknown" && actual != deducedVecType) {
                reportError(node, "Argument " + std::to_string(i + 1) + 
                    " type mismatch: Expected '" + deducedVecType + 
                    "' (to match previous argument) but found '" + actual + "'");
            }
            continue; 
        }

        if (actual != "unknown" && actual != expected) {
            reportError(node, "Argument " + std::to_string(i + 1) + " type mismatch: Expected '" + 
                expected + "', got '" + actual + "'");
        }
    }
}

bool gdshader_lsp::SemanticAnalyzer::isProcessorFunction(const std::string &name)
{
    return name == "vertex" || name == "fragment" || name == "light" || 
           name == "start" || name == "process" || name == "sky" || name == "fog";
}

bool gdshader_lsp::SemanticAnalyzer::isVecType(const std::string &type)
{
    return type == "float" || 
           type == "vec2" || type == "vec3" || type == "vec4";
}

bool gdshader_lsp::SemanticAnalyzer::isConstantExpression(const ExpressionNode *node)
{
    if (!node) return false;
    // 1. Literal is constant
    if (dynamic_cast<const LiteralNode*>(node)) return true;
    
    // 2. Identifier is constant ONLY if it is a 'const' variable
    if (auto id = dynamic_cast<const IdentifierNode*>(node)) {
        const Symbol* s = symbols.lookup(id->name);
        return s && s->category == SymbolType::Const;
    }

    // 3. Binary Op is constant if both sides are constant
    if (auto bin = dynamic_cast<const BinaryOpNode*>(node)) {
        return isConstantExpression(bin->left.get()) && isConstantExpression(bin->right.get());
    }

    // 4. Unary Op
    if (auto un = dynamic_cast<const UnaryOpNode*>(node)) {
        return isConstantExpression(un->operand.get());
    }
    
    // Function calls (even built-ins) are usually NOT considered constant expressions in GLSL case labels
    return false;
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
            // We use line -1 to indicate builtin
            symbols.add({b.name, b.type, {b.type}, SymbolType::Builtin, -1, 0, b.doc});
        }
    }
}

std::string gdshader_lsp::SemanticAnalyzer::resolveType(const ExpressionNode *node)
{
    if (!node) return "void";

    // 1. Literal?
    if (auto lit = dynamic_cast<const LiteralNode*>(node)) {
        if (lit->type == TokenType::TOKEN_NUMBER) return (lit->value.find('.') != std::string::npos) ? "float" : "int";
        if (lit->type == TokenType::KEYWORD_TRUE || lit->type == TokenType::KEYWORD_FALSE) return "bool";
        return "void";
    }

    // 2. Identifier? Lookup in Symbol Table
    if (auto id = dynamic_cast<const IdentifierNode*>(node)) {
        if (const Symbol* s = symbols.lookup(id->name)) {
            return s->typeName;
        }
        return "unknown";
    }

    // 3. Binary Op? (Simplify: float + float = float)
    if (auto bin = dynamic_cast<const BinaryOpNode*>(node)) {
        switch (bin->op) {
            case TokenType::TOKEN_EQ_EQ:
            case TokenType::TOKEN_NOT_EQ:
            case TokenType::TOKEN_LESS:
            case TokenType::TOKEN_GREATER:
            case TokenType::TOKEN_LESS_EQ:
            case TokenType::TOKEN_GREATER_EQ:
            case TokenType::TOKEN_AND:
            case TokenType::TOKEN_OR:
                return "bool";
            default: break;
        }

        std::string leftType = resolveType(bin->left.get());
        std::string rightType = resolveType(bin->right.get());

        if (leftType == "unknown" || rightType == "unknown") return "unknown";
        if (leftType == rightType) return leftType;

        bool lVec = leftType.find("vec") != std::string::npos;
        bool rVec = rightType.find("vec") != std::string::npos;
        
        if (lVec && rightType == "float") return leftType;
        if (rVec && leftType == "float") return rightType;
        
        // (You can expand this for matrices later)

        return "unknown";

    }

    // 4. Function Call? Lookup return type
    if (auto call = dynamic_cast<const FunctionCallNode*>(node)) {
        // Is it a user function?
        if (const Symbol* s = symbols.lookup(call->functionName)) {
            return s->typeName;
        }

        // Is it a Builtin Function? (Check GLOBAL_FUNCTIONS list)
        for(const auto& f : GLOBAL_FUNCTIONS) {
            if(f.name == call->functionName) {
                
                // --- NEW: Dynamic Return Type Resolution ---
                if (f.returnType == "vec_type") {
                    // Infer return type from the first argument
                    // e.g., sin(vec3) -> returns vec3
                    if (!call->arguments.empty()) {
                        return resolveType(call->arguments[0].get());
                    }
                    return "float"; // Fallback if no args (shouldn't happen for these math funcs)
                }
                // -------------------------------------------
                
                return f.returnType;
            }
        }

        // Is it a Constructor? (vec3(...))
        if(typeRegistry.hasType(call->functionName)) {
            return call->functionName;
        }
    }
    
    // 5. Member Access (The Tricky Part)
    if (auto mem = dynamic_cast<const MemberAccessNode*>(node)) 
    {
        if (mem->member == "length") {
            return "int"; // length() returns int
        }

        std::string baseType = resolveType(mem->base.get());

        if (baseType == "unknown" || baseType == "void") return "unknown";
        
        std::string resultType = typeRegistry.getMemberType(baseType, mem->member);
        
        if (resultType == "unknown") {
            // We can report a specific error here if we want, or let visitMemberAccess do it
            return "unknown"; 
        }
        return resultType;
    }

    return "unknown";
}

void SemanticAnalyzer::reportError(const ASTNode* node, const std::string& msg) {
    if (node) diagnostics.push_back({node->line, node->column, msg});
}

}