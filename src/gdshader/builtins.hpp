#ifndef BUILTINS_HPP
#define BUILTINS_HPP

#include <vector>
#include <string>
#include <map>

namespace gdshader_lsp {

// -------------------------------------------------------------------------
// ENUMS
// -------------------------------------------------------------------------

enum class ShaderType {
    Spatial,
    CanvasItem,
    Particles,
    Sky,
    Fog,
    Unknown
};

enum class Scope {
    Global,
    Vertex,
    Fragment,
    Light,
    Start,   // Particles
    Process, // Particles
    Sky,     // Sky
    Fog      // Fog
};

// -------------------------------------------------------------------------
// BUILT-IN DATA
// -------------------------------------------------------------------------

struct BuiltinVariable {
    std::string name;
    std::string type; // e.g., "vec3", "float"
    std::string doc;
};

// Helper to define built-ins quickly
using BuiltinList = std::vector<BuiltinVariable>;

// -------------------------------------------------------------------------
// DEFINITIONS
// -------------------------------------------------------------------------

// SPATIAL (3D)
static const BuiltinList SPATIAL_VERTEX = {
    {"VERTEX", "vec3", "Vertex position in view space."},
    {"NORMAL", "vec3", "Vertex normal in view space."},
    {"TANGENT", "vec3", "Vertex tangent in view space."},
    {"BINORMAL", "vec3", "Vertex binormal in view space."},
    {"UV", "vec2", "UV coordinates."},
    {"UV2", "vec2", "UV2 coordinates."},
    {"COLOR", "vec4", "Vertex color."},
    {"POINT_SIZE", "float", "Point size for point rendering."},
    {"INSTANCE_ID", "int", "Instance ID."},
    {"INSTANCE_CUSTOM", "vec4", "Custom instance data."},
    {"MODEL_MATRIX", "mat4", "Model space to world space transform."},
    {"MODELVIEW_MATRIX", "mat4", "Model space to view space transform."},
    {"PROJECTION_MATRIX", "mat4", "View space to clip space transform."},
    {"CAMERA_POSITION_WORLD", "vec3", "Camera position in world space."},
    {"TIME", "float", "Global time in seconds."}
};

static const BuiltinList SPATIAL_FRAGMENT = {
    {"ALBEDO", "vec3", "Base color (default white)."},
    {"ALPHA", "float", "Opacity (0.0 - 1.0)."},
    {"METALLIC", "float", "Metallic value (0.0 - 1.0)."},
    {"ROUGHNESS", "float", "Roughness value (0.0 - 1.0)."},
    {"SPECULAR", "float", "Specular intensity (0.0 - 1.0)."},
    {"RIM", "float", "Rim lighting intensity."},
    {"CLEARCOAT", "float", "Clearcoat intensity."},
    {"ANISOTROPY", "float", "Anisotropy strength."},
    {"EMISSION", "vec3", "Emission color."},
    {"NORMAL", "vec3", "Normal map value (view space)."},
    {"NORMAL_MAP", "vec3", "Normal map texture value."},
    {"NORMAL_MAP_DEPTH", "float", "Normal map depth."},
    {"AO", "float", "Ambient occlusion."},
    {"SSS_STRENGTH", "float", "Subsurface scattering strength."},
    {"TRANSMISSION", "vec3", "Transmission color."},
    {"BACKLIGHT", "vec3", "Backlight color."},
    {"SCREEN_UV", "vec2", "Screen UV coordinates."},
    {"FRAGCOORD", "vec4", "Fragment coordinates in window space."},
    {"FRONT_FACING", "bool", "True if front face."},
    {"TIME", "float", "Global time."}
};

static const BuiltinList SPATIAL_LIGHT = {
    {"DIFFUSE_LIGHT", "vec3", "Output diffuse light."},
    {"SPECULAR_LIGHT", "vec3", "Output specular light."},
    {"LIGHT", "vec3", "Direction to light."},
    {"LIGHT_COLOR", "vec3", "Color of the light."},
    {"ATTENUATION", "float", "Shadow and distance attenuation."},
    {"ALBEDO", "vec3", "Base albedo from fragment function."},
    {"ROUGHNESS", "float", "Roughness from fragment function."},
    {"FRAGCOORD", "vec4", "Fragment coordinates."}
};

// CANVAS ITEM (2D)
static const BuiltinList CANVAS_VERTEX = {
    {"VERTEX", "vec2", "Vertex position."},
    {"UV", "vec2", "UV coordinates."},
    {"COLOR", "vec4", "Vertex color."},
    {"MODULATE", "vec4", "Modulate color."},
    {"POINT_SIZE", "float", "Point size."},
    {"TEXTURE_PIXEL_SIZE", "vec2", "Size of a pixel in the texture."},
    {"TIME", "float", "Global time."}
};

static const BuiltinList CANVAS_FRAGMENT = {
    {"COLOR", "vec4", "Output color."},
    {"UV", "vec2", "UV coordinates."},
    {"SCREEN_UV", "vec2", "Screen UV coordinates."},
    {"TEXTURE", "sampler2D", "The default texture."},
    {"TEXTURE_PIXEL_SIZE", "vec2", "Size of a pixel in the texture."},
    {"SCREEN_TEXTURE", "sampler2D", "Screen texture."},
    {"SCREEN_PIXEL_SIZE", "vec2", "Size of a pixel in the screen."},
    {"POINT_COORD", "vec2", "Point coordinates."},
    {"FRAGCOORD", "vec4", "Fragment coordinates."},
    {"NORMAL", "vec3", "Normal value."},
    {"NORMAL_MAP", "vec3", "Normal map."}
};

static const BuiltinList CANVAS_LIGHT = {
    {"LIGHT", "vec4", "Output color for this light."},
    {"LIGHT_COLOR", "vec4", "Color of the light."},
    {"LIGHT_ENERGY", "float", "Energy of the light."},
    {"LIGHT_POSITION", "vec3", "Position of the light."},
    {"SHADOW_MODULATE", "vec4", "Shadow color."},
    {"COLOR", "vec4", "Base color from fragment function."}
};

// PARTICLES
static const BuiltinList PARTICLES_PROCESS = {
    {"COLOR", "vec4", "Particle color."},
    {"VELOCITY", "vec3", "Particle velocity."},
    {"MASS", "float", "Particle mass."},
    {"ACTIVE", "bool", "If false, particle is removed."},
    {"RESTART", "bool", "If true, particle restarts."},
    {"CUSTOM", "vec4", "Custom particle data."},
    {"TRANSFORM", "mat4", "Particle transform."},
    {"LIFETIME", "float", "Particle lifetime."},
    {"DELTA", "float", "Time since last frame."},
    {"TIME", "float", "Global time."},
    {"COLLISION_NORMAL", "vec3", "Collision normal."},
    {"COLLISION_DEPTH", "float", "Collision depth."},
    {"ATTRACTOR_FORCE", "vec3", "Force from attractors."}
};

// SKY
static const BuiltinList SKY_PROCESS = {
    {"COLOR", "vec3", "Output color."},
    {"ALPHA", "float", "Output alpha."},
    {"EYEDIR", "vec3", "Eye direction."},
    {"SKY_COORDS", "vec2", "Sky spherical coordinates."},
    {"SCREEN_UV", "vec2", "Screen UV."},
    {"TIME", "float", "Global time."},
    {"PI", "float", "Const PI."},
    {"TAU", "float", "Const TAU."},
    {"E", "float", "Const E."}
};

// FOG
static const BuiltinList FOG_PROCESS = {
    {"DENSITY", "float", "Output fog density."},
    {"ALBEDO", "vec3", "Output fog color."},
    {"EMISSION", "vec3", "Output emission."},
    {"WORLD_POSITION", "vec3", "World position of the cell."},
    {"OBJECT_POSITION", "vec3", "Object position."},
    {"SDF", "vec3", "Signed Distance Field value."},
    {"UVW", "vec3", "3D Texture UVW."},
    {"SIZE", "vec3", "Size of fog volume."},
    {"TIME", "float", "Global time."}
};

// -------------------------------------------------------------------------
// LOOKUP UTILITIES
// -------------------------------------------------------------------------

static const BuiltinList EMPTY_LIST = {};

inline const BuiltinList& get_builtins(ShaderType type, Scope scope) {
    if (type == ShaderType::Spatial) {
        if (scope == Scope::Vertex) return SPATIAL_VERTEX;
        if (scope == Scope::Fragment) return SPATIAL_FRAGMENT;
        if (scope == Scope::Light) return SPATIAL_LIGHT;
    } 
    else if (type == ShaderType::CanvasItem) {
        if (scope == Scope::Vertex) return CANVAS_VERTEX;
        if (scope == Scope::Fragment) return CANVAS_FRAGMENT;
        if (scope == Scope::Light) return CANVAS_LIGHT;
    }
    else if (type == ShaderType::Particles) {
        if (scope == Scope::Start || scope == Scope::Process) return PARTICLES_PROCESS;
    }
    else if (type == ShaderType::Sky) {
        if (scope == Scope::Sky) return SKY_PROCESS;
    }
    else if (type == ShaderType::Fog) {
        if (scope == Scope::Fog) return FOG_PROCESS;
    }
    
    return EMPTY_LIST;
}

} // namespace gdshader_lsp

#endif // BUILTINS_HPP