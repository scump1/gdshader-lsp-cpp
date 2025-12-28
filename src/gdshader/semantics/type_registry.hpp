#ifndef TYPE_REGISTRY_HPP
#define TYPE_REGISTRY_HPP

#include <string>
#include <unordered_map>
#include <vector>
#include <iostream>

namespace gdshader_lsp {

struct TypeInfo {
    std::string name;
    
    // For structs: map<member_name, type_name>
    std::unordered_map<std::string, std::string> members;
    
    // Flags
    bool is_vector = false;    // Enables swizzling (xyzw, rgba)
    bool is_matrix = false;    // Enables array-like access
    bool is_struct = false;
};

class TypeRegistry {

public:
    TypeRegistry() {
        registerBuiltins();
    }

    void registerStruct(const std::string& name, const std::unordered_map<std::string, std::string>& members) {
        TypeInfo info;
        info.name = name;
        info.members = members;
        info.is_struct = true;
        types[name] = info;
    }

    bool hasType(const std::string& name) const {
        return types.find(name) != types.end();
    }

    // Validates if "member" exists on "typeName".
    // Handles Swizzling for vectors!
    bool hasMember(const std::string& typeName, const std::string& member) const {
        auto it = types.find(typeName);
        if (it == types.end()) return false;

        const auto& info = it->second;

        // 1. Struct Member Lookup
        if (info.members.count(member)) return true;

        // 2. Vector Swizzling (e.g., .xyz, .rg)
        if (info.is_vector) {
            return isValidSwizzle(member);
        }

        return false;
    }

    const TypeInfo* getTypeInfo(const std::string& name) const {
        auto it = types.find(name);
        if (it != types.end()) return &it->second;
        return nullptr;
    }

private:
    std::unordered_map<std::string, TypeInfo> types;

    bool isValidSwizzle(const std::string& swizzle) const {
        if (swizzle.length() > 4) return false;
        
        // Sets of valid components
        const std::string xyzw = "xyzw";
        const std::string rgba = "rgba";
        const std::string stpq = "stpq";

        bool use_xyzw = true, use_rgba = true, use_stpq = true;

        for (char c : swizzle) {
            if (xyzw.find(c) == std::string::npos) use_xyzw = false;
            if (rgba.find(c) == std::string::npos) use_rgba = false;
            if (stpq.find(c) == std::string::npos) use_stpq = false;
        }

        // Must strictly adhere to one set (cannot mix .xg)
        return use_xyzw || use_rgba || use_stpq;
    }

    void registerBuiltins() {
        // Scalars
        types["float"] = { .name = "float", .members = {} };
        types["int"]   = { .name = "int",   .members = {} };
        types["bool"]  = { .name = "bool",  .members = {} };
        types["void"]  = { .name = "void",  .members = {} };

        auto makeVec = [&](std::string n) { 
            types[n] = { .name = n, .members = {}, .is_vector = true }; 
        };
        makeVec("vec2"); makeVec("vec3"); makeVec("vec4");
        makeVec("ivec2"); makeVec("ivec3"); makeVec("ivec4");
        makeVec("bvec2"); makeVec("bvec3"); makeVec("bvec4");

        // Matrices
        types["mat3"] = { .name = "mat3", .members = {}, .is_matrix = true };
        types["mat4"] = { .name = "mat4", .members = {}, .is_matrix = true };
        
        // Samplers
        types["sampler2D"] = { .name = "sampler2D", .members = {} };
    }
};

}

#endif