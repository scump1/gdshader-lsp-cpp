#ifndef PARSER_HPP
#define PARSER_HPP

#include <vector>
#include <memory>
#include <string>
#include <optional>

#include "gdshader/lexer/lexer.h"
#include "gdshader/lexer/lexer_types.h"
#include "gdshader/ast/ast.h"

namespace gdshader_lsp {

struct Diagnostic {
    int line;
    int column;
    std::string message;
};

class Parser {
public:
    Parser(Lexer& lexer);

    // Main entry point
    std::unique_ptr<ProgramNode> parse();

    // Retrieve errors after parsing
    const std::vector<Diagnostic>& getDiagnostics() const { return diagnostics; }

private:
    Lexer& lexer;
    Token current_token;
    Token previous_token; // Useful for getting locations of consumed tokens
    
    std::vector<Diagnostic> diagnostics;
    bool panicMode = false;

    // --- Core Helpers ---
    void advance();
    void consume(TokenType type, const std::string& message);
    bool match(TokenType type);
    bool check(TokenType type);
    void reportError(const std::string& message);
    void reportErrorAt(const Token& token, const std::string& message);
    void synchronize(); // Error recovery

    // --- Top Level Parsing ---
    std::unique_ptr<ASTNode> parseTopLevelDecl();
    std::unique_ptr<ASTNode> parseShaderType();
    std::unique_ptr<ASTNode> parseRenderMode();
    std::unique_ptr<ASTNode> parseUniform();
    std::unique_ptr<ASTNode> parseVarying();
    std::unique_ptr<ASTNode> parseConst();
    std::unique_ptr<ASTNode> parseStruct();
    
    // Handles both Functions ("void foo() {}") and Global Vars ("vec3 x;")
    std::unique_ptr<ASTNode> parseTypeIdentifierDecl(); 

    // --- Function Parsing ---
    std::unique_ptr<FunctionNode> parseFunction(const std::string& type, const std::string& name);
    std::unique_ptr<BlockNode> parseBlock();
    
    // --- Statement Parsing ---
    std::unique_ptr<StatementNode> parseStatement();
    std::unique_ptr<StatementNode> parseVarDecl(); // int x = 5;
    std::unique_ptr<StatementNode> parseIf();
    std::unique_ptr<StatementNode> parseFor();
    std::unique_ptr<StatementNode> parseWhile();
    std::unique_ptr<StatementNode> parseReturn();
    std::unique_ptr<StatementNode> parseExpressionStatement();
    std::unique_ptr<StatementNode> parseDoWhile();
    std::unique_ptr<StatementNode> parseSwitch();
    std::unique_ptr<StatementNode> parseBreak();
    std::unique_ptr<StatementNode> parseContinue();

    // --- Expression Parsing (Precedence Climbing) ---
    std::unique_ptr<ExpressionNode> parseExpression();
    std::unique_ptr<ExpressionNode> parseAssignment();
    std::unique_ptr<ExpressionNode> parseTernary();
    std::unique_ptr<ExpressionNode> parseLogicOr();
    std::unique_ptr<ExpressionNode> parseLogicAnd();
    std::unique_ptr<ExpressionNode> parseEquality();
    std::unique_ptr<ExpressionNode> parseComparison();
    std::unique_ptr<ExpressionNode> parseTerm();       // + -
    std::unique_ptr<ExpressionNode> parseFactor();     // * / %
    std::unique_ptr<ExpressionNode> parseUnary();      // - !
    std::unique_ptr<ExpressionNode> parseCallOrAccess(); // Postfix: (), [], .
    std::unique_ptr<ExpressionNode> parsePrimary();
    
    // Helper for parsing types (e.g. "vec3", "void", "Sampler2D")
    std::string parseTypeString();
    bool isTypeStart();
};

} // namespace gdshader_lsp

#endif // PARSER_HPP