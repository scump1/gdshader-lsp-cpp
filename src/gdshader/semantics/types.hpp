#ifndef TYPES_HPP
#define TYPES_HPP

#include <string>
#include <vector>
#include <memory>
#include <algorithm>

namespace gdshader_lsp {

enum class TypeKind {
    SCALAR,     // int, float, bool, void
    VECTOR,     // vec3, ivec2
    MATRIX,     // mat4
    STRUCT,     // User defined structs
    ARRAY,      // float[5]
    SAMPLER,    // sampler2D
    UNKNOWN     // Error state
};

struct Type;
using TypePtr = std::shared_ptr<Type>;

struct Type {
    TypeKind kind = TypeKind::UNKNOWN;
    std::string name; // "vec3", "MyStruct", "int"
    
    // For Arrays: "float[5]" -> baseType="float", arraySize=5
    // For Vectors: "vec3" -> baseType="float", componentCount=3
    TypePtr baseType = nullptr; 
    int arraySize = 0; 
    int componentCount = 0; // 1 for scalar, 2-4 for vector, 4/9/16 for matrix

    // For Structs: Members (Ordered for constructor validation)
    std::vector<std::pair<std::string, TypePtr>> members;

    // --- Comparison Logic ---
    bool operator==(const Type& other) const {
        if (this == &other) return true;
        if (kind != other.kind) return false;

        // Arrays are structurally typed: float[5] == float[5]
        if (kind == TypeKind::ARRAY) {
            return arraySize == other.arraySize && 
                   (baseType && other.baseType && *baseType == *other.baseType);
        }

        // Structs/Builtins are nominative: "MyStruct" == "MyStruct"
        // (In GLSL/GDShader, two structs with same fields but different names are NOT compatible)
        return name == other.name;
    }

    bool operator!=(const Type& other) const { return !(*this == other); }
    
    // Helper for diagnostics
    std::string toString() const {
        if (kind == TypeKind::ARRAY) {
            return (baseType ? baseType->toString() : "unknown") + 
                   "[" + std::to_string(arraySize) + "]";
        }
        return name;
    }
};

} // namespace gdshader_lsp

#endif