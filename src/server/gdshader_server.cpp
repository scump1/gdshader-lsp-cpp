
#include "server/gdshader_server.hpp"

#include "gdshader/semantics/semantic_analyzer.hpp"

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
            (void)params;
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
            std::cout << "Opened document " << std::string(params.textDocument.uri.path()) << "..." << std::endl;
            compileAndPublish(params.textDocument.uri, params.textDocument.text);
        }
    );

    // --- SYNCHRONIZATION: DID CHANGE ---
    handler.add<lsp::notifications::TextDocument_DidChange>(
        [this](lsp::notifications::TextDocument_DidChange::Params&& params) {
            
            // Safety check: The vector should strictly have 1 element in Full Sync,
            // but it is good practice to check.
            if (params.contentChanges.empty() || params.contentChanges.size() > 1) return;

            auto uri = params.textDocument.uri;
            const auto& changeEvent = params.contentChanges.back();

            std::visit([this, &uri](auto&& change) {
                compileAndPublish(uri, change.text);
            }, changeEvent);
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

void gdshader_lsp::GdShaderServer::compileAndPublish(const lsp::DocumentUri& uri, const std::string &code)
{
    std::string mapKey = std::string(uri.path());

    // Debug output
    // std::cout << "Compiling " << mapKey << "..." << std::endl;

    // 1. Run Lexer & Parser
    Lexer lexer(code);
    Parser parser(lexer);
    
    auto ast = parser.parse();
    auto errors = parser.getDiagnostics();

    if (ast) {
        SemanticAnalyzer analyzer;
        auto result = analyzer.analyze(ast.get());
        
        // Save the Symbol Table for Autocomplete later!
        // You need to add 'SymbolTable syms' to your 'Document' struct in .hpp
        documents[mapKey].symbols = std::move(result.first);

        // Append Semantic Errors (e.g. "Undefined Variable") to Syntax Errors
        auto semanticErrors = result.second;
        errors.insert(errors.end(), semanticErrors.begin(), semanticErrors.end());
    }

    // 2. Update State (using the mapKey)
    Document& doc = documents[mapKey];
    doc.text = code;
    doc.ast = std::move(ast);

    // 3. Convert Diagnostics
    std::vector<lsp::Diagnostic> lspDiagnostics;
    for (const auto& err : errors) {
        lspDiagnostics.push_back(lsp::Diagnostic{
            .range = lsp::Range{
                .start = lsp::Position{(unsigned)err.line, (unsigned)err.column},
                .end   = lsp::Position{(unsigned)err.line, (unsigned)err.column + 1}
            },
            .message = err.message,
            .severity = lsp::DiagnosticSeverity::Error,
            .source = "gdshader"
        });
    }

    // 4. Send to Editor
    lsp::notifications::TextDocument_PublishDiagnostics::Params params;
    
    // CRITICAL FIX:
    // Use the original URI object from the request. 
    // This preserves the exact encoding (e.g. spaces vs %20) the client used.
    params.uri = uri; 
    
    params.diagnostics = lspDiagnostics;

    handler.sendNotification<lsp::notifications::TextDocument_PublishDiagnostics>(std::move(params));
    
    std::cout << "  Found " << errors.size() << " errors." << std::endl;
}