
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
            lsp::Hover hover;
            std::string path = std::string(params.textDocument.uri.path());
            if (documents.find(path) == documents.end()) return hover;

            auto& doc = documents[path];
            int line = params.position.line;
            int col = params.position.character;

            std::string word = getWordAtPosition(doc.text, line, col);
            if (word.empty()) return hover;

            bool isMember = (col > 0 && getWordBeforeDot(doc.text, col) != ""); 

            if (isMember) {
                // 1. Find the base variable (e.g. "my_instance" from "my_instance.test")
                std::string baseName = getWordBeforeDot(doc.text, col); // You need to implement/expose this helper
                // 2. Look up base
                const Symbol* baseSym = doc.symbols.lookupAt(baseName, line);
                if (baseSym && baseSym->type) {
                    // 3. Look up member in the base type
                    TypePtr memberT = doc.types.getMemberType(baseSym->type, word);
                    if (memberT->kind != TypeKind::UNKNOWN) {
                        std::string content = "**" + baseSym->name + "**\n\n";
                        content += "Type: `" + baseSym->type->toString() + "`\n";
                        if (!baseSym->doc_string.empty()) {
                            content += "\n" + baseSym->doc_string;
                        }
                        
                        hover.contents = {
                            lsp::MarkupContent {
                                .kind = lsp::MarkupKind::Markdown,
                                .value = content
                            }
                        };
                    }
                }
            } else {

                const Symbol* sym = doc.symbols.lookupAt(word, line);

                if (sym) {
                    std::string content = "**" + sym->name + "**\n\n";
                    content += "Type: `" + sym->type->toString() + "`\n";
                    if (!sym->doc_string.empty()) {
                        content += "\n" + sym->doc_string;
                    }
                    
                    hover.contents = {
                        lsp::MarkupContent {
                            .kind = lsp::MarkupKind::Markdown,
                            .value = content
                        }
                    };
                }
            }
            return hover;
        }
    );

    // --- FEATURE: COMPLETION ---
    handler.add<lsp::requests::TextDocument_Completion>(
        [this](lsp::requests::TextDocument_Completion::Params&& params) -> lsp::requests::TextDocument_Completion::Result
        {            
            lsp::CompletionList result;
            result.isIncomplete = false;

            std::string path = std::string(params.textDocument.uri.path());
            if (documents.find(path) == documents.end()) return result;

            auto& doc = documents[path];
            int line = params.position.line;
            int col = params.position.character;

            // 1. Get context (Line content)
            std::string lineText = getLine(doc.text, line);
            if (col > (int)lineText.length()) col = lineText.length();
            
            // Check trigger char
            bool isDotTrigger = false;
            if (col > 0 && lineText[col-1] == '.') isDotTrigger = true;

            // --- CASE A: DOT COMPLETION (myVec.|) ---
            if (isDotTrigger) {
                std::string varName = getWordBeforeDot(lineText, col);
                const Symbol* sym = doc.symbols.lookupAt(varName, line);

                if (sym && sym->type) {
                    TypePtr t = sym->type;
                    
                    // 1. Vector Swizzling (Basic)
                    if (t->kind == TypeKind::VECTOR) {
                        std::vector<std::string> swizzles = {"x", "y", "z", "w", "r", "g", "b", "a"};
                        for(int i=0; i<t->componentCount * 2; ++i) { // Crude limit to valid comps
                            if (i >= (int)swizzles.size()) break;
                             result.items.push_back(lsp::CompletionItem{
                                .label = swizzles[i],
                                .kind = lsp::CompletionItemKind::Field
                            });
                        }
                    }
                    // 2. Struct Members
                    else if (t->kind == TypeKind::STRUCT) {
                        for(const auto& member : t->members) {
                            result.items.push_back(lsp::CompletionItem{
                                .label = member.first,
                                .kind = lsp::CompletionItemKind::Field,
                                .detail = member.second->toString()
                            });
                        }
                    }
                }
                return result;
            }

            std::vector<Symbol> visible = doc.symbols.getVisibleSymbolsAt(line);
            
            for (const auto& s : visible) {
                lsp::CompletionItem item;
                item.label = s.name;
                
                if (s.category == SymbolType::Function || s.category == SymbolType::Builtin) {
                    item.kind = lsp::CompletionItemKind::Function;
                    item.detail = s.type->toString(); // Return type
                    item.insertText = s.name + "($0)";
                    item.insertTextFormat = lsp::InsertTextFormat::Snippet;
                } 
                else if (s.category == SymbolType::Struct) {
                    item.kind = lsp::CompletionItemKind::Struct;
                }
                else {
                    item.kind = lsp::CompletionItemKind::Variable;
                    item.detail = s.type->toString();
                }

                result.items.push_back(item);
            }

            return result;
        }
    );
    
    // --- FEATURE: DEFINITION ---
    handler.add<lsp::requests::TextDocument_Definition>(
        [this](lsp::requests::TextDocument_Definition::Params&& params) 
        -> lsp::requests::TextDocument_Definition::Result
        {
            std::string path = std::string(params.textDocument.uri.path());
            if (documents.find(path) == documents.end()) return nullptr;

            auto& doc = documents[path];
            int line = params.position.line;
            int col = params.position.character;

            std::string word = getWordAtPosition(doc.text, line, col);
            if (word.empty()) return nullptr;

            const Symbol* sym = doc.symbols.lookupAt(word, line);

            // If found and it's not a built-in (line -1)
            if (sym && sym->line >= 0) {
                lsp::Location loc;
                loc.uri = params.textDocument.uri;
                
                // Construct range (highlight the specific line)
                loc.range.start = lsp::Position{(unsigned)sym->line, (unsigned)sym->column};
                loc.range.end   = lsp::Position{(unsigned)sym->line, (unsigned)sym->column + (unsigned)sym->name.length()};
                
                return loc;
            }

            return nullptr;
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

        int len = (err.length > 0) ? err.length : 1;

        lsp::DiagnosticSeverity severity;
        if (err.level == DiagnosticLevel::Warning) {
            severity = lsp::DiagnosticSeverity::Warning;
        } else {
            severity = lsp::DiagnosticSeverity::Error;
        }

        lspDiagnostics.push_back(lsp::Diagnostic{
            .range = lsp::Range{
                .start = lsp::Position{(unsigned)err.line, (unsigned)err.column},
                .end   = lsp::Position{(unsigned)err.line, (unsigned)(err.column + len)}
            },
            .message = err.message,
            .severity = severity,
            .source = "gdshader"
        });
    }

    // 4. Send to Editor
    lsp::notifications::TextDocument_PublishDiagnostics::Params params;

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