
#include "gdshader/semantics/symbol_table.hpp"

namespace gdshader_lsp {

SymbolTable::SymbolTable() {
    root = std::make_unique<Scope>(nullptr);
    current = root.get();
}

void SymbolTable::pushScope(int startLine) {
    auto newScope = std::make_unique<Scope>(current);
    newScope->startLine = startLine;
    
    // Store raw pointer before moving ownership
    Scope* next = newScope.get();
    current->children.push_back(std::move(newScope));
    current = next;
}

void SymbolTable::popScope(int endLine) {
    if (current->parent) {
        current->endLine = endLine;
        current = current->parent;
    }
}

bool SymbolTable::add(const Symbol& symbol) {
    if (current->symbols.count(symbol.name)) return false;
    current->symbols[symbol.name] = symbol;
    return true;
}

const Symbol* SymbolTable::lookup(const std::string& name) const {
    // Walk up from current (used during analysis phase)
    Scope* walker = current;
    while (walker) {
        auto it = walker->symbols.find(name);
        if (it != walker->symbols.end()) return &it->second;
        walker = walker->parent;
    }
    return nullptr;
}

const Scope* SymbolTable::findScopeAt(int line) const {
    const Scope* candidate = root.get();
    
    // Drill down as deep as possible
    while (true) {
        bool foundChild = false;
        for (const auto& child : candidate->children) {
            // Check if line is inside child's range
            // Note: We might need precise column checks later, but line is usually enough
            if (line >= child->startLine && line <= child->endLine) {
                candidate = child.get();
                foundChild = true;
                break;
            }
        }
        if (!foundChild) break;
    }
    return candidate;
}

std::vector<Symbol> SymbolTable::getVisibleSymbols(const Scope* scope) const 
{
    std::vector<Symbol> results;
    const Scope* walker = scope;
    
    while (walker) {
        for (const auto& pair : walker->symbols) {
            // TODO: check for shadowing if you want strict correctness
            results.push_back(pair.second);
        }
        walker = walker->parent;
    }
    return results;
}

}