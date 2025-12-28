
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

void SemanticAnalyzer::visitUniform(const UniformNode* node) {
    Symbol s{node->name, node->type, SymbolType::Uniform, node->line, node->column, node->hint};
    if (!symbols.add(s)) reportError(node, "Redefinition of uniform '" + s.name + "'");
    if (node->defaultValue) visit(node->defaultValue.get());
}

void SemanticAnalyzer::visitVarying(const VaryingNode* node) {
    Symbol s{node->name, node->type, SymbolType::Varying, node->line, node->column, ""};
    if (!symbols.add(s)) reportError(node, "Redefinition of varying '" + s.name + "'");
}

void SemanticAnalyzer::visitConst(const ConstNode* node) {
    Symbol s{node->name, node->type, SymbolType::Const, node->line, node->column, ""};
    if (!symbols.add(s)) reportError(node, "Redefinition of const '" + s.name + "'");
    if (node->value) visit(node->value.get());
}

void SemanticAnalyzer::visitStruct(const StructNode* node) {
    Symbol s{node->name, node->name, SymbolType::Struct, node->line, node->column, ""};
    if (!symbols.add(s)) reportError(node, "Redefinition of struct '" + s.name + "'");
    
    // REGISTER THE TYPE
    std::unordered_map<std::string, std::string> members;
    for(const auto& m : node->members) {
        members[m.name] = m.type;
    }
    typeRegistry.registerStruct(node->name, members);
}

void SemanticAnalyzer::visitFunction(const FunctionNode* node) {
    Symbol funcSym{node->name, node->returnType, SymbolType::Function, node->line, node->column, ""};
    if (!symbols.add(funcSym)) reportError(node, "Redefinition of function '" + node->name + "'");

    symbols.pushScope(node->line);
    
    // Load built-ins if this is a processor function (vertex, fragment, etc.)
    loadBuiltinsForFunction(node->name);

    // Add arguments
    for (const auto& arg : node->arguments) {
        Symbol argSym{arg.name, arg.type, SymbolType::Variable, node->line, node->column, ""};
        symbols.add(argSym);
    }

    if (node->body) {
        // Visit statements directly to avoid pushing another scope for the block
        for (const auto& stmt : node->body->statements) {
            visit(stmt.get());
        }
    }
    
    int endLine = node->body->statements.empty() ? node->line : node->body->statements.back()->line;
    symbols.popScope(endLine);
}


void SemanticAnalyzer::visit(const ASTNode* node) {
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

void SemanticAnalyzer::visitVarDecl(const VariableDeclNode* node) {
    Symbol s{node->name, node->type, SymbolType::Variable, node->line, node->column, ""};
    if (!symbols.add(s)) reportError(node, "Redefinition of variable '" + s.name + "'");
    
    if (node->initializer) {
        visit(node->initializer.get());
        // TODO: Type check initializer vs variable type
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

void SemanticAnalyzer::visitReturn(const ReturnNode* node) {
    if (node->value) visit(node->value.get());
}

void SemanticAnalyzer::visitExpressionStatement(const ExpressionStatementNode* node) {
    if (node->expr) visit(node->expr.get());
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

void SemanticAnalyzer::visitBinaryOp(const BinaryOpNode* node) {
    // Check for assignment to const/read-only
    if (node->op == TokenType::TOKEN_EQUAL) {
        visitAssignment(node);
        return;
    }
    
    if (node->left) visit(node->left.get());
    if (node->right) visit(node->right.get());
}

void SemanticAnalyzer::visitAssignment(const BinaryOpNode* node) {
    // 1. Check RHS
    if (node->right) visit(node->right.get());

    // 2. Check LHS validity
    // Must be an l-value (identifier, member access, array access)
    if (auto id = dynamic_cast<const IdentifierNode*>(node->left.get())) {
        const Symbol* s = symbols.lookup(id->name);
        if (s) {
            if (s->category == SymbolType::Const) {
                reportError(id, "Cannot assign to constant '" + id->name + "'");
            } else if (s->category == SymbolType::Uniform) {
                reportError(id, "Cannot assign to uniform '" + id->name + "'");
            }
            // TODO: Check if Builtin is Read-Only (most 'in' builtins are)
        } else {
             reportError(id, "Undefined identifier '" + id->name + "'");
        }
    } else if (dynamic_cast<const MemberAccessNode*>(node->left.get())) {
        visit(node->left.get()); // Recursively check if base is defined
    } else {
        // visit(node->left.get()); // Fallback
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

    // 2. Check User-Defined Functions
    if (symbols.lookup(name)) {
        return; // Found in symbol table
    }

    // 3. Check Built-in Functions (e.g. sin, texture)
    bool isBuiltin = false;
    for (const auto& f : GLOBAL_FUNCTIONS) {
        if (f.name == name) {
            isBuiltin = true;
            // TODO: Validate argument count here based on f.arguments
            break;
        }
    }
    if (isBuiltin) return;

    // 4. Check Constructors (e.g. vec3(...))
    if (typeRegistry.hasType(name)) {
        return; // It's a type constructor
    }

    // 5. Report Error
    reportError(node, "Unknown function or type '" + name + "'");
}

void SemanticAnalyzer::visitMemberAccess(const MemberAccessNode* node) 
{
    // 1. Check the Base (recursively)
    if (node->base) visit(node->base.get());

    // 2. Resolve the Type of the Base
    std::string baseType = resolveType(node->base.get());

    if (baseType == "unknown" || baseType == "void") return; // Prevents cascading errors

    // 3. Check if the member exists on that type
    if (!typeRegistry.hasMember(baseType, node->member)) {
        reportError(node, "Type '" + baseType + "' has no member '" + node->member + "'");
    }
}

// -------------------------------------------------------------------------
// HELPERS
// -------------------------------------------------------------------------

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
            symbols.add({b.name, b.type, SymbolType::Builtin, -1, 0, b.doc});
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
        return resolveType(bin->left.get()); // Rough approximation
    }

    // 4. Function Call? Lookup return type
    if (auto call = dynamic_cast<const FunctionCallNode*>(node)) {
        // Is it a user function?
        if (const Symbol* s = symbols.lookup(call->functionName)) {
            return s->typeName;
        }
        // Is it a Builtin Function? (Check GLOBAL_FUNCTIONS list)
        for(const auto& f : GLOBAL_FUNCTIONS) {
            if(f.name == call->functionName) return f.returnType;
        }
        // Is it a Constructor? (vec3(...))
        if(typeRegistry.hasType(call->functionName)) {
            return call->functionName;
        }
    }
    
    // 5. Member Access? (The Tricky Part)
    if (auto mem = dynamic_cast<const MemberAccessNode*>(node)) {
        std::string baseType = resolveType(mem->base.get());
        
        // If it is a vector (vec3), swizzling returns a smaller vector or scalar
        // vec3 v; v.xy -> vec2
        if (baseType.find("vec") != std::string::npos && typeRegistry.hasMember(baseType, mem->member)) {
            int len = mem->member.length();
            if (len == 1) return "float";
            if (len == 2) return "vec2";
            if (len == 3) return "vec3";
            if (len == 4) return "vec4";
        }
        
        // If struct, we need to look up the specific member type (Requires struct registry lookup)
        // For now, return unknown or implement full struct lookup
    }

    return "unknown";
}

void SemanticAnalyzer::reportError(const ASTNode* node, const std::string& msg) {
    if (node) diagnostics.push_back({node->line, node->column, msg});
}

}