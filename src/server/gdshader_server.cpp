
#include "server/gdshader_server.hpp"

using namespace gdshader_lsp;

GdShaderServer::GdShaderServer(lsp::io::Socket s) : socket(std::move(s)), connection(socket), handler(connection)
{
    registerHandlers();
}

GdShaderServer::~GdShaderServer()
{
}

void GdShaderServer::run() 
{
    try {
        // FIX 2: Check 'socket.isOpen()', not 'connection.isOpen()'
        while (socket.isOpen()) {
            handler.processIncomingMessages();
        }
    } catch (const std::exception& e) {
        std::cerr << "Session ended: " << e.what() << std::endl;
    }
}

void GdShaderServer::registerHandlers() {
    
    // --- LIFECYCLE: INITIALIZE ---
    // The client sending its capabilities and asking for yours.
    handler.add<lsp::requests::Initialize>(
        [this](lsp::requests::Initialize::Params&& params) {
            
            return lsp::requests::Initialize::Result{
                .capabilities = {
                    .textDocumentSync = lsp::TextDocumentSyncOptions{
                        .openClose = true,
                        .change = lsp::TextDocumentSyncKind::Full 
                    },
                    .completionProvider = lsp::CompletionOptions{
                        .triggerCharacters = std::vector<std::string>{".", ":"} 
                    }
                },
                .serverInfo = lsp::InitializeResultServerInfo{
                    .name = "gdshader-lsp",
                    .version = "0.1.0"
                }
            };
        }
    );

    // --- SYNCHRONIZATION: DID OPEN ---
    handler.add<lsp::notifications::TextDocument_DidOpen>(
        [this](lsp::notifications::TextDocument_DidOpen::Params&& params) {
            lsp::DocumentUri uri = params.textDocument.uri;
            std::string code = params.textDocument.text;
            
            // TODO: 
            // 1. Create a new DocumentState (Arena + AST) for this URI
            // 2. Parse 'code'
            // 3. publishDiagnostics(uri, parser_errors)
        }
    );

    // --- SYNCHRONIZATION: DID CHANGE ---
    handler.add<lsp::notifications::TextDocument_DidChange>(
        [this](lsp::notifications::TextDocument_DidChange::Params&& params) {
            lsp::DocumentUri uri = params.textDocument.uri;

            // TODO: 
            // 1. Retrieve DocumentState for this URI
            // 2. state.arena.reset()  <-- Fast wipe!
            // 3. state.ast = parser.parse(newCode, state.arena)
            // 4. publishDiagnostics(uri, parser_errors)
        }
    );

    // --- SYNCHRONIZATION: DID CLOSE ---
    handler.add<lsp::notifications::TextDocument_DidClose>(
        [this](lsp::notifications::TextDocument_DidClose::Params&& params) {
            lsp::DocumentUri uri = params.textDocument.uri;
            
        }
    );

    // --- FEATURE: COMPLETION ---
    // Called when the user requests auto-completion (ctrl+space or trigger char).
    handler.add<lsp::requests::TextDocument_Completion>(
        [this](lsp::requests::TextDocument_Completion::Params&& params) {
            // 1. Locate the AST node at params.position.line / params.position.character
            // 2. Determine Scope (Vertex? Fragment? Light?)
            
            std::vector<lsp::CompletionItem> items;

            // Example: Hardcoded suggestion
            items.push_back(lsp::CompletionItem{
                .label = "ALBEDO",
                .kind = lsp::CompletionItemKind::Variable,
                .detail = "vec3",
                .documentation = "Output color for the fragment shader."
            });

            return lsp::requests::TextDocument_Completion::Result(items);
        }
    );
    
    // --- LIFECYCLE: SHUTDOWN/EXIT ---
    handler.add<lsp::requests::Shutdown>([]() { return nullptr; });
    handler.add<lsp::notifications::Exit>([]() { exit(0); });
}

void gdshader_lsp::GdShaderServer::publish_diagnostics(const std::string &uri, const std::vector<std::string> &errors)
{
}