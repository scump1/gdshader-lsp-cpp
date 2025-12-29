
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

bool SymbolTable::add(const Symbol& symbol) 
{
    auto& store = current->symbols[symbol.name];
    
    if (!store.empty()) {
        // A symbol with this name exists. 
        
        // Rule 1: If the new symbol OR existing symbol is NOT a function, it's a collision.
        // (e.g. "float x;" and "void x() {}" cannot coexist, nor can "float x;" and "float x;")
        if (symbol.category != SymbolType::Function || store[0].category != SymbolType::Function) {
            return false; // Redefinition Error
        }

        // Rule 2: Function Overloading
        // We must check if a function with the SAME signature already exists.
        for (const auto& existing : store) {
            if (existing.parameterTypes == symbol.parameterTypes) {
                return false; // Exact signature redefinition Error
            }
        }
    }

    // Safe to add
    store.push_back(symbol);
    return true;
}

const Symbol* SymbolTable::lookup(const std::string& name) const {
    Scope* walker = current;
    while (walker) {
        auto it = walker->symbols.find(name);
        if (it != walker->symbols.end() && !it->second.empty()) {
            // Return the first one found.
            // For variables, this is the only one. 
            // For functions, this returns one of the overloads (usually the first defined).
            return &it->second[0];
        }
        walker = walker->parent;
    }
    return nullptr;
}

std::vector<const Symbol*> SymbolTable::lookupFunctions(const std::string& name) const 
{
    Scope* walker = current;
    while (walker) {
        auto it = walker->symbols.find(name);
        if (it != walker->symbols.end() && !it->second.empty()) {
            // Found the scope where this function is defined. 
            // Return pointers to ALL overloads in this scope.
            std::vector<const Symbol*> results;
            results.reserve(it->second.size());
            
            for (const auto& s : it->second) {
                results.push_back(&s);
            }
            return results;
        }
        walker = walker->parent;
    }
    return {};
}

const Scope* SymbolTable::findScopeAt(int line) const 
{
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

const Symbol* SymbolTable::lookupAt(const std::string& name, int line) const {
    const Scope* searchScope = findScopeAt(line);
    
    // Walk up the scope tree
    while (searchScope) {
        auto it = searchScope->symbols.find(name);
        if (it != searchScope->symbols.end() && !it->second.empty()) {
            return &it->second[0];
        }
        searchScope = searchScope->parent;
    }
    return nullptr;
}

std::vector<Symbol> SymbolTable::getVisibleSymbolsAt(int line) const 
{
    std::vector<Symbol> results;
    const Scope* walker = findScopeAt(line);
    
    // We use a set to avoid duplicates when shadowing variables
    std::unordered_map<std::string, bool> seen;

    while (walker) {
        for (const auto& pair : walker->symbols) {
            if (!seen.count(pair.first)) {
                // Add all overloads
                for(const auto& s : pair.second) {
                    results.push_back(s);
                }
                seen[pair.first] = true;
            }
        }
        walker = walker->parent;
    }
    return results;
}

}