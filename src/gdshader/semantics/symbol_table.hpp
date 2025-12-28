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
    SymbolType category;
    
    // For "Go to Definition"
    int line;
    int column;
    std::string doc_string;
};

class SymbolTable {
public:
    SymbolTable();

    // Scope Management
    void pushScope();
    void popScope();

    // CRUD
    bool add(const Symbol& symbol);
    const Symbol* lookup(const std::string& name) const;
    
    // Helper for Autocomplete: Get all symbols visible in current scope stack
    std::vector<Symbol> getAllVisibleSymbols() const;

private:
    // Stack of scopes. Each scope is a map of Name -> Symbol
    std::vector<std::unordered_map<std::string, Symbol>> scopes;
};

}

#endif