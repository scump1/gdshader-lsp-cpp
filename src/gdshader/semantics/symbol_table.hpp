#ifndef SYMBOL_TABLE_HPP
#define SYMBOL_TABLE_HPP

#include <string>
#include <vector>
#include <unordered_map>
#include <memory>
#include <optional>

namespace gdshader_lsp {

enum class SymbolType {
    Variable,
    Uniform,
    Varying,
    Const,
    Function,
    Struct,
    Builtin
};

struct Symbol {
    std::string name;
    std::string typeName; // e.g., "vec3", "void", "MyStruct"
    std::vector<std::string> parameterTypes;
    SymbolType category;
    
    // For "Go to Definition"
    int line;
    int column;
    std::string doc_string;
};

struct Scope {
    Scope* parent = nullptr;
    std::vector<std::unique_ptr<Scope>> children;
    std::unordered_map<std::string, Symbol> symbols;
    
    // Range this scope covers (0-based)
    int startLine = 0;
    int endLine = 0;

    Scope(Scope* p = nullptr) : parent(p) {}
};

class SymbolTable {
public:
    SymbolTable();

    // Scope Management
    void pushScope(int startLine);
    void popScope(int endLine);

    // CRUD
    bool add(const Symbol& symbol);
    const Symbol* lookup(const std::string& name) const;

    const Scope* findScopeAt(int line) const;
    
    // Helper for Autocomplete: Get all symbols visible in current scope stack
    std::vector<Symbol> getVisibleSymbols(const Scope* scope) const;

private:
   
    std::unique_ptr<Scope> root;
    Scope* current;

};

}

#endif