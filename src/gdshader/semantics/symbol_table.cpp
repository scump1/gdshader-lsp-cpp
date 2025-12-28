
#include "gdshader/semantics/symbol_table.hpp"

namespace gdshader_lsp {

SymbolTable::SymbolTable() {
    // Start with a global scope
    pushScope();
}

void SymbolTable::pushScope() {
    scopes.emplace_back();
}

void SymbolTable::popScope() {
    if (!scopes.empty()) {
        scopes.pop_back();
    }
}

bool SymbolTable::add(const Symbol& symbol) {
    if (scopes.empty()) return false;

    auto& currentScope = scopes.back();
    if (currentScope.find(symbol.name) != currentScope.end()) {
        return false; // Already declared in this scope (Redefinition error)
    }

    currentScope[symbol.name] = symbol;
    return true;
}

const Symbol* SymbolTable::lookup(const std::string& name) const {
    // Look from top of stack (innermost) to bottom (global)
    for (auto it = scopes.rbegin(); it != scopes.rend(); ++it) {
        auto found = it->find(name);
        if (found != it->end()) {
            return &found->second;
        }
    }
    return nullptr;
}

std::vector<Symbol> SymbolTable::getAllVisibleSymbols() const 
{
    std::vector<Symbol> results;

    // Iterate backwards so we see innermost variables first? 
    // Or just dump everything. For LSP completion, duplicates (shadowing) 
    // are usually filtered by the client or we just send the innermost.
    
    std::unordered_map<std::string, bool> added;
    
    for (auto it = scopes.rbegin(); it != scopes.rend(); ++it) {
        for (const auto& pair : *it) {
            if (!added[pair.first]) {
                results.push_back(pair.second);
                added[pair.first] = true;
            }
        }
    }
    return results;
}

}