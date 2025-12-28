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

enum class ShaderStage {
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

struct BuiltinFunction {
    std::string name;
    std::string returnType;
    std::string arguments; // Display string for now (e.g., "float x, float y")
    std::string doc;
};

// Helper to define built-ins quickly
using BuiltinList = std::vector<BuiltinVariable>;
using BuiltinFuncList = std::vector<BuiltinFunction>;

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
// FUNCTION DEFINITIONS
// -------------------------------------------------------------------------

static const BuiltinFuncList GLOBAL_FUNCTIONS = {
    // Trigonometry
    {"radians", "float", "float degrees", "Converts degrees to radians."},
    {"degrees", "float", "float radians", "Converts radians to degrees."},
    {"sin", "float", "float angle", "Sine."},
    {"cos", "float", "float angle", "Cosine."},
    {"tan", "float", "float angle", "Tangent."},
    {"asin", "float", "float x", "Arc-sine."},
    {"acos", "float", "float x", "Arc-cosine."},
    {"atan", "float", "float y, float x", "Arc-tangent."},
    {"sinh", "float", "float x", "Hyperbolic sine."},
    {"cosh", "float", "float x", "Hyperbolic cosine."},
    {"tanh", "float", "float x", "Hyperbolic tangent."},

    // Exponents and Logarithms
    {"pow", "float", "float x, float y", "Returns x raised to the power of y."},
    {"exp", "float", "float x", "Returns the natural exponentiation of x."},
    {"log", "float", "float x", "Returns the natural logarithm of x."},
    {"exp2", "float", "float x", "Returns 2 raised to the power of x."},
    {"log2", "float", "float x", "Returns the base 2 logarithm of x."},
    {"sqrt", "float", "float x", "Returns the square root of x."},
    {"inversesqrt", "float", "float x", "Returns the inverse square root of x."},

    // Common Math
    {"abs", "float", "float x", "Returns the absolute value of x."},
    {"sign", "float", "float x", "Returns 1.0 if x > 0, -1.0 if x < 0, or 0.0 if x == 0."},
    {"floor", "float", "float x", "Returns a value equal to the nearest integer that is less than or equal to x."},
    {"ceil", "float", "float x", "Returns a value equal to the nearest integer that is greater than or equal to x."},
    {"round", "float", "float x", "Returns a value equal to the nearest integer to x."},
    {"roundEven", "float", "float x", "Returns a value equal to the nearest integer to x. Halves round to even."},
    {"trunc", "float", "float x", "Returns a value equal to the nearest integer to x whose absolute value is not larger than the absolute value of x."},
    {"fract", "float", "float x", "Returns x - floor(x)."},
    {"mod", "float", "float x, float y", "Computes the value of x modulo y."},
    {"modf", "float", "float x, out float i", "Returns the fractional part of x and sets i to the integer part."},
    {"min", "float", "float a, float b", "Returns the smaller of the two values."},
    {"max", "float", "float a, float b", "Returns the larger of the two values."},
    {"clamp", "float", "float x, float minVal, float maxVal", "Constrains a value to lie between two further values."},
    {"mix", "float", "float x, float y, float a", "Linear interpolate between two values."},
    {"step", "float", "float edge, float x", "Generate a step function by comparing two values."},
    {"smoothstep", "float", "float edge0, float edge1, float x", "Hermite interpolation between two values."},
    
    // Geometric
    {"length", "float", "vec_type x", "Calculates the length of a vector."},
    {"distance", "float", "vec_type p0, vec_type p1", "Calculates the distance between two points."},
    {"dot", "float", "vec_type x, vec_type y", "Calculates the dot product of two vectors."},
    {"cross", "vec3", "vec3 x, vec3 y", "Calculates the cross product of two vectors."},
    {"normalize", "vec_type", "vec_type x", "Calculates the reflection direction for an incident vector."},
    {"reflect", "vec3", "vec3 I, vec3 N", "Calculates the reflection direction."},
    {"refract", "vec3", "vec3 I, vec3 N, float eta", "Calculates the refraction direction."},
    {"faceforward", "vec3", "vec3 N, vec3 I, vec3 Nref", "Returns a vector pointing in the same direction as another."},

    // Matrix
    {"determinant", "float", "mat_type m", "Returns the determinant of a matrix."},
    {"inverse", "mat_type", "mat_type m", "Returns the inverse of a matrix."},
    {"transpose", "mat_type", "mat_type m", "Returns the transpose of a matrix."},

    // Texture
    {"texture", "vec4", "sampler2D sampler, vec2 uv", "Performs a texture lookup."},
    {"textureProj", "vec4", "sampler2D sampler, vec3 uv", "Performs a texture lookup with projection."},
    {"textureLod", "vec4", "sampler2D sampler, vec2 uv, float lod", "Performs a texture lookup with explicit level of detail."},
    {"texelFetch", "vec4", "sampler2D sampler, ivec2 P, int lod", "Performs a lookup of a single texel from a texture."},
    {"textureSize", "ivec2", "sampler2D sampler, int lod", "Returns the size of the texture."},

    // Derivatives
    {"dFdx", "float", "float p", "Returns the partial derivative of p with respect to the window x coordinate."},
    {"dFdy", "float", "float p", "Returns the partial derivative of p with respect to the window y coordinate."},
    {"fwidth", "float", "float p", "Returns the sum of the absolute value of derivatives in x and y."},
};

// -------------------------------------------------------------------------
// LOOKUP UTILITIES
// -------------------------------------------------------------------------

static const BuiltinList EMPTY_LIST = {};

inline const BuiltinList& get_builtins(ShaderType type, ShaderStage scope) {
    if (type == ShaderType::Spatial) {
        if (scope == ShaderStage::Vertex) return SPATIAL_VERTEX;
        if (scope == ShaderStage::Fragment) return SPATIAL_FRAGMENT;
        if (scope == ShaderStage::Light) return SPATIAL_LIGHT;
    } 
    else if (type == ShaderType::CanvasItem) {
        if (scope == ShaderStage::Vertex) return CANVAS_VERTEX;
        if (scope == ShaderStage::Fragment) return CANVAS_FRAGMENT;
        if (scope == ShaderStage::Light) return CANVAS_LIGHT;
    }
    else if (type == ShaderType::Particles) {
        if (scope == ShaderStage::Start || scope == ShaderStage::Process) return PARTICLES_PROCESS;
    }
    else if (type == ShaderType::Sky) {
        if (scope == ShaderStage::Sky) return SKY_PROCESS;
    }
    else if (type == ShaderType::Fog) {
        if (scope == ShaderStage::Fog) return FOG_PROCESS;
    }
    
    return EMPTY_LIST;
}

inline const BuiltinFuncList& get_builtin_functions() {
    return GLOBAL_FUNCTIONS;
}

} // namespace gdshader_lsp

#endif // BUILTINS_HPP