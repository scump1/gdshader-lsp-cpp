
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

    // --- FEATURE: HOVER ---
    handler.add<lsp::requests::TextDocument_Hover>(
        [this](lsp::requests::TextDocument_Hover::Params&& params) -> lsp::requests::TextDocument_Hover::Result
        {
            std::string uri = std::string(params.textDocument.uri.path());

            int line = params.position.line;
            int col = params.position.character; 

            if (documents.find(uri) == documents.end()) return nullptr;
            const auto& doc = documents.at(uri);

            // 1. Find the Scope
            const Scope* scope = doc.symbols.findScopeAt(line);

            // 2. We need to find the "Word" under the cursor.
            // Since we don't have the AST node exactly at this char easily accessible 
            // without a spatial tree, we can use a simple helper to extract the identifier.
            std::string identifier = getWordAtPosition(doc.text, line, col);
            
            if (identifier.empty()) return nullptr;

            const Symbol* sym = nullptr;
            const Scope* walker = scope;
            while (walker) {
                if (walker->symbols.count(identifier)) {
                    sym = &walker->symbols.at(identifier);
                    break;
                }
                walker = walker->parent;
            }

            if (!sym) return nullptr;

            // 4. Construct Response
            lsp::MarkupContent content;
            content.kind = lsp::MarkupKind::Markdown;
            
            // Format:
            // ```glsl
            // vec3 ALBEDO
            // ```
            // Base color...
            std::string docString = "```glsl\n" + sym->typeName + " " + sym->name + "\n```\n";
            if (!sym->doc_string.empty()) {
                docString += "---\n" + sym->doc_string;
            }
            content.value = docString;

            lsp::Hover hover;
            hover.contents = content;
            
            // 2. Return it (implicitly converts to Result/Nullable<Hover>)
            return hover;
        }
    );

    // --- FEATURE: COMPLETION ---
    // Called when the user requests auto-completion (ctrl+space or trigger char).
    handler.add<lsp::requests::TextDocument_Completion>(
        [this](lsp::requests::TextDocument_Completion::Params&& params) 
        {
            // 1. Locate the AST node at params.position.line / params.position.character
            // 2. Determine Scope (Vertex? Fragment? Light?)
            
            std::string uri = std::string(params.textDocument.uri.path());
            int line = params.position.line;
            int col = params.position.character;
            
            // 1. Get the parsed document
            if (documents.find(uri) == documents.end()) return lsp::requests::TextDocument_Completion::Result{};
            const auto& doc = documents[uri];

            std::string textLine = getLine(doc.text, line);
            char prevChar = (col > 0 && (size_t)col <= textLine.size()) ? textLine[col - 1] : '\0';

            if (prevChar == '.') {
                // 1. Extract variable name
                std::string varName = getWordBeforeDot(textLine, col - 1);
                
                if (varName.empty()) return lsp::requests::TextDocument_Completion::Result{};

                // 2. Lookup Variable in Symbol Table
                const Scope* scope = doc.symbols.findScopeAt(line);
                const Symbol* sym = nullptr;
                const Scope* walker = scope;
                
                // Walk up the scope tree to find the variable
                while (walker) {
                    if (walker->symbols.count(varName)) {
                        sym = &walker->symbols.at(varName);
                        break;
                    }
                    walker = walker->parent;
                }

                // If variable not found, return empty
                if (!sym) return lsp::requests::TextDocument_Completion::Result{};

                // 3. Lookup Type Definition in Registry
                std::vector<lsp::CompletionItem> items;
                const TypeInfo* typeInfo = doc.types.getTypeInfo(sym->typeName);

                if (typeInfo) {
                    // CASE A: It's a Struct -> Add Members
                    if (typeInfo->is_struct) {
                        for (const auto& member : typeInfo->members) {
                            items.push_back(lsp::CompletionItem{
                                .label = member.first, // Member Name
                                .kind = lsp::CompletionItemKind::Field,
                                .detail = member.second // Member Type
                            });
                        }
                    }
                    // CASE B: It's a Vector -> Add Swizzles
                    else if (typeInfo->is_vector) {
                        // Add standard components
                        const char* coords[] = { "x", "y", "z", "w" };
                        const char* colors[] = { "r", "g", "b", "a" };
                        const char* tex[]    = { "s", "t", "p", "q" };
                        
                        // Determine size based on type name (vec2, vec3, etc.)
                        int size = 4; 
                        if (sym->typeName.find("2") != std::string::npos) size = 2;
                        else if (sym->typeName.find("3") != std::string::npos) size = 3;

                        for(int i=0; i<size; i++) {
                            items.push_back({ .label = coords[i], .kind = lsp::CompletionItemKind::Field, .detail = "float" });
                            items.push_back({ .label = colors[i], .kind = lsp::CompletionItemKind::Field, .detail = "float" });
                            items.push_back({ .label = tex[i],    .kind = lsp::CompletionItemKind::Field, .detail = "float" });
                        }
                        
                        // Optional: Add common swizzles like "xy", "rgb" if desired
                        if (size >= 2) items.push_back({ .label = "uv", .kind = lsp::CompletionItemKind::Field, .detail = "vec2" });
                        if (size >= 3) items.push_back({ .label = "rgb", .kind = lsp::CompletionItemKind::Field, .detail = "vec3" });
                    }
                }
                return lsp::requests::TextDocument_Completion::Result(items);

            } else {

                const Scope* scope = doc.symbols.findScopeAt(line);
                auto symbols = doc.symbols.getVisibleSymbols(scope);

                std::vector<lsp::CompletionItem> items;

                for(const auto& sym : symbols) {
                    items.push_back(lsp::CompletionItem{
                        .label = sym.name,
                        .kind = (sym.category == SymbolType::Function) ? lsp::CompletionItemKind::Function : lsp::CompletionItemKind::Variable,
                        .detail = sym.typeName,
                        .documentation = sym.doc_string
                    });
                }

                return lsp::requests::TextDocument_Completion::Result(items);
            }
        }
    );
    
    // --- LIFECYCLE: SHUTDOWN/EXIT ---
    handler.add<lsp::requests::Shutdown>([]() { return nullptr; });
    handler.add<lsp::notifications::Exit>([]() { exit(0); });
}

void gdshader_lsp::GdShaderServer::compileAndPublish(const lsp::DocumentUri& uri, const std::string &code)
{
    std::string mapKey = std::string(uri.path());
    Document& doc = documents[mapKey];

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
        doc.symbols = std::move(result.symbols);
        doc.types   = std::move(result.types);

        // Append Semantic Errors (e.g. "Undefined Variable") to Syntax Errors
        auto semanticErrors = result.diagnostics;
        errors.insert(errors.end(), semanticErrors.begin(), semanticErrors.end());
    }

    // 2. Update State (using the mapKey)
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

std::string gdshader_lsp::GdShaderServer::getWordAtPosition(const std::string &source, int line, int col)
{
    size_t currentLine = 0;
    size_t start = 0;
    size_t end = source.find('\n');
    
    while (currentLine < (size_t)line && end != std::string::npos) {
        start = end + 1;
        end = source.find('\n', start);
        currentLine++;
    }
    
    if (currentLine != (size_t)line) return "";

    std::string lineText = source.substr(start, end - start);
    
    // 2. Expand outwards from col to find start/end of identifier
    if ((size_t)col >= lineText.size()) return "";

    // Check if cursor is on a valid identifier char
    auto isIdChar = [](char c) { return isalnum(c) || c == '_'; };
    
    if (!isIdChar(lineText[col])) return "";

    int wordStart = col;
    while (wordStart > 0 && isIdChar(lineText[wordStart - 1])) {
        wordStart--;
    }

    int wordEnd = col;
    while ((size_t)wordEnd < lineText.size() && isIdChar(lineText[wordEnd])) {
        wordEnd++;
    }

    return lineText.substr(wordStart, wordEnd - wordStart);
}

std::string gdshader_lsp::GdShaderServer::getWordBeforeDot(const std::string& lineText, int dotPos) 
{
    // Safety check
    if (dotPos <= 0 || (size_t)dotPos >= lineText.size()) return "";

    // Start looking immediately before the dot
    int i = dotPos - 1;

    // 1. Skip optional whitespace (handle "test . x")
    while (i >= 0 && std::isspace(lineText[i])) {
        i--;
    }

    if (i < 0) return ""; // Found nothing before the dot

    // 2. Mark the end of the identifier
    int end = i;

    // 3. Scan backwards capturing valid identifier chars (alnum + _)
    while (i >= 0) {
        char c = lineText[i];
        if (std::isalnum(c) || c == '_') {
            i--;
        } else {
            // Hit a non-identifier char (space, operator, etc.)
            break;
        }
    }

    // 'i' is now at the character *before* the word.
    int start = i + 1;

    if (start > end) return ""; // No identifier found (e.g. "123." or just ".")

    return lineText.substr(start, end - start + 1);
}

std::string gdshader_lsp::GdShaderServer::getLine(const std::string &source, int targetLine)
{
    if (source.empty()) return "";

    size_t start = 0;
    size_t end = source.find('\n');
    int currentLine = 0;

    // Iterate until we find the start of our target line
    while (end != std::string::npos && currentLine < targetLine) {
        start = end + 1;
        end = source.find('\n', start);
        currentLine++;
    }

    // If we reached EOF before the line index, return empty
    if (currentLine != targetLine) return "";

    // Extract the line
    size_t count = (end == std::string::npos) ? std::string::npos : (end - start);
    std::string line = source.substr(start, count);

    // Trim carriage return '\r' if present (Windows line endings)
    if (!line.empty() && line.back() == '\r') {
        line.pop_back();
    }

    return line;
}