#ifndef SEMANTIC_ANALYZER_HPP
#define SEMANTIC_ANALYZER_HPP

#include "gdshader/ast/ast.h"
#include "gdshader/semantics/symbol_table.hpp"
#include "gdshader/semantics/type_registry.hpp"
#include "gdshader/parser/parser.hpp"

#include "gdshader/builtins.hpp"

namespace gdshader_lsp {

class SemanticAnalyzer {

private:

    SymbolTable symbols;
    TypeRegistry typeRegistry;

    std::vector<Diagnostic> diagnostics;

    // State
    ShaderType currentShaderType = ShaderType::Spatial; // Default
    Scope currentProcessorFunction = Scope::Global;

    void visit(const ASTNode* node);

    // Top Level

    void visitProgram(const ProgramNode* node);
    void visitShaderType(const ShaderTypeNode* node);
    void visitUniform(const UniformNode* node);
    void visitVarying(const VaryingNode* node);
    void visitConst(const ConstNode* node);
    void visitStruct(const StructNode* node);
    void visitFunction(const FunctionNode* node);
    
    // Statements

    void visitBlock(const BlockNode* node);
    void visitVarDecl(const VariableDeclNode* node);
    void visitIf(const IfNode* node);
    void visitFor(const ForNode* node);
    void visitWhile(const WhileNode* node);
    void visitReturn(const ReturnNode* node);
    void visitExpressionStatement(const ExpressionStatementNode* node);

    // Expressions

    void visitExpression(const ExpressionNode* node);
    void visitIdentifier(const IdentifierNode* node);
    void visitBinaryOp(const BinaryOpNode* node);
    void visitUnaryOp(const UnaryOpNode* node);
    void visitFunctionCall(const FunctionCallNode* node);
    void visitMemberAccess(const MemberAccessNode* node);
    void visitAssignment(const BinaryOpNode* node); // Special case of BinaryOp

    // Helper

    void reportError(const ASTNode* node, const std::string& msg);
    void loadBuiltinsForFunction(const std::string& funcName);
    std::string resolveType(const ExpressionNode* node);

public:
    // Returns a populated symbol table (moved out) and a list of errors
    std::pair<SymbolTable, std::vector<Diagnostic>> analyze(const ProgramNode* ast);

};

}

#endif