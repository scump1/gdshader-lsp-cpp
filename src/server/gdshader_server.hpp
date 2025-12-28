
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

namespace gdshader_lsp {

struct Document {
    // Compiler
    // Root AST Node
    std::string text;
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

    void registerHandlers();

    void publish_diagnostics(const std::string& uri, const std::vector<std::string>& errors);

    std::unordered_map<std::string, Document> documents;

public:

    GdShaderServer(lsp::io::Socket s);
    ~GdShaderServer();

    void run();

};

}

#endif // GDSHADER_SERVER_HPP