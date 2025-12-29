#ifndef TYPE_REGISTRY_HPP
#define TYPE_REGISTRY_HPP

#include <string>
#include <unordered_map>
#include <vector>
#include <iostream>

#include "gdshader/builtins.hpp"

namespace gdshader_lsp {

struct TypeInfo {
    std::string name;
    
    // For structs: map<member_name, type_name>
    std::unordered_map<std::string, std::string> members;
    
    // Flags
    bool is_vector = false;    // Enables swizzling (xyzw, rgba)
    int vector_length = 0;

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
            return isValidSwizzle(member, info.vector_length);
        }

        return false;
    }

    std::string getMemberType(const std::string& baseTypeName, const std::string& memberName) {
        auto it = types.find(baseTypeName);
        if (it == types.end()) return "unknown";

        const TypeInfo& info = it->second;

        // 1. Struct Member
        if (info.is_struct) {
            auto memIt = info.members.find(memberName);
            if (memIt != info.members.end()) {
                return memIt->second; // Return the actual type (e.g., "vec3")
            }
        }

        // 2. Vector Swizzling
        if (info.is_vector) {
            // Validation: Check if components exist for this size
            int vectorSize = getComponentCount(baseTypeName);
            if (isValidSwizzle(memberName, vectorSize)) {
                // Determine resulting type based on swizzle length
                if (memberName.length() == 1) return getElementBaseType(baseTypeName); // float
                if (memberName.length() == 2) return "vec2"; // (or ivec2 etc)
                if (memberName.length() == 3) return "vec3";
                if (memberName.length() == 4) return "vec4";
            }
        }
        
        return "unknown";
    }

    const TypeInfo* getTypeInfo(const std::string& name) const {
        auto it = types.find(name);
        if (it != types.end()) return &it->second;
        return nullptr;
    }

private:
    std::unordered_map<std::string, TypeInfo> types;

    bool isValidSwizzle(const std::string& swizzle, int vectorSize) const {
        if (swizzle.length() > 4) return false;
        
        const std::string xyzw = "xyzw";
        const std::string rgba = "rgba";
        const std::string stpq = "stpq"; // Texture coords

        int setIdx = -1; // 0=xyzw, 1=rgba, 2=stpq

        for (char c : swizzle) {
            // Find which set this char belongs to and its index (0-3)
            size_t idx = -1;
            int currentSet = -1;

            if ((idx = xyzw.find(c)) != std::string::npos) currentSet = 0;
            else if ((idx = rgba.find(c)) != std::string::npos) currentSet = 1;
            else if ((idx = stpq.find(c)) != std::string::npos) currentSet = 2;

            if (currentSet == -1) return false; // Invalid char
            if (setIdx != -1 && setIdx != currentSet) return false; // Mixing sets (e.g. .xg) 
            if (idx >= (size_t)vectorSize) return false; // Accessing .z on vec2

            setIdx = currentSet;
        }
        return true;
    }

    void registerBuiltins() {
        // Scalars
        types["float"] = { .name = "float", .members = {} };
        types["int"]   = { .name = "int",   .members = {} };
        types["bool"]  = { .name = "bool",  .members = {} };
        types["void"]  = { .name = "void",  .members = {} };

        auto makeVec = [&](std::string n) { 

            int vector_len = 0;

            if (n.find("2") != std::string::npos) {
                vector_len = 2;
            } else if (n.find("3") != std::string::npos) {
                vector_len = 3;
            } else if (n.find("4") != std::string::npos) {
                vector_len = 4;
            }

            types[n] = { .name = n, .members = {}, .is_vector = true, .vector_length = vector_len }; 
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