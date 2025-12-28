
#ifndef GDSHADER_SERVER_HPP 
#define GDSHADER_SERVER_HPP

#include <iostream>
#include <thread>
#include <memory>
#include <vector>
#include <unordered_map>

#include <lsp/io/socket.h>
#include <lsp/connection.h>

#include <lsp/messagehandler.h>
#include <lsp/messages.h>
#include <lsp/types.h>

#include "gdshader/parser/parser.hpp" // parser includes ast.h and lexer.h
#include "gdshader/semantics/symbol_table.hpp"

namespace gdshader_lsp {

struct Document {
    std::string text;
    std::unique_ptr<ProgramNode> ast;
    SymbolTable symbols;
};

// -------------------------------------------------------------------------
// SERVER SESSION
// Each client connection spawns an instance of this class.
// This is where your compiler state lives.
// -------------------------------------------------------------------------

class GdShaderServer {

private:

    lsp::io::Socket socket;
    lsp::Connection connection;
    lsp::MessageHandler handler;

    std::unordered_map<std::string, Document> documents;

    void registerHandlers();
    void publish_diagnostics(const std::string& uri, const std::vector<std::string>& errors);

    void compileAndPublish(const lsp::DocumentUri& uri, const std::string& code);

public:

    GdShaderServer(lsp::io::Socket s);
    ~GdShaderServer();

    void run();

};

}

#endif // GDSHADER_SERVER_HPP