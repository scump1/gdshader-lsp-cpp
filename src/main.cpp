
#include <iostream>
#include <thread>
#include <memory>
#include <vector>

#include "server/gdshader_server.hpp"

// -------------------------------------------------------------------------
// MAIN ENTRY POINT
// -------------------------------------------------------------------------
int main(int argc, char* argv[]) 
{
    // Standard LSP port is often arbitrarily chosen, e.g., 6005
    int port = 6005;
    
    // Simple argument parsing for port
    if (argc > 1 && std::string(argv[1]).rfind("--port=", 0) == 0) {
        port = std::stoi(std::string(argv[1]).substr(7));
    }

    std::cout << "Starting Godot Shader LSP on port " << port << "..." << std::endl;

    try {
        auto listener = lsp::io::SocketListener(port);

        while (listener.isReady()) {
            // Block until a client (IDE) connects
            auto socket = listener.listen();
            
            if (!socket.isOpen()) break;

            std::cout << "Client connected!" << std::endl;

            // Spawn a thread to handle this connection independently
            std::thread([socket = std::move(socket)]() mutable {
                gdshader_lsp::GdShaderServer server(std::move(socket));
                server.run();
            }).detach();
        }
    } catch (const std::exception& e) {
        std::cerr << "Fatal error: " << e.what() << std::endl;
        return 1;
    }

    return 0;
}