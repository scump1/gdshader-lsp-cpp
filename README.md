# GDShader LSP

A high-performance **Language Server Protocol (LSP)** implementation for the Godot Shading Language (`.gdshader`), written in C++.

This tool provides advanced code intelligence, strict type checking, and real-time error reporting for Godot shaders, designed to integrate seamlessly into editors like VSCode, Emacs, or Godot itself (via GDExtension).

## Features

### Semantic Analysis & Type Safety
Unlike basic syntax highlighters, this LSP understands the *meaning* of your code.
* **Strict Type Checking**: Catches errors like `float x = 5;` (implicit cast) or `vec3 * int` mismatches immediately.
* **Validation**:
    * **Swizzling**: Ensures components exist (`vec2.z` → error) and sets aren't mixed (`.xg` → error).
    * **Structs**: Validates member access and constructor arguments (order & type).
    * **Constructors**: Enforces strict GLSL/Godot constructor rules (e.g., `vec3(1.0, 2.0)` ✅ vs `vec3(1, 2)` ❌).
    * **Functions**: Checks argument counts and types against signatures for both **Built-ins** (`pow`, `texture`) and **User-defined** functions.
* **Godot-Specific Rules**:
    * Validates processor signatures (`void vertex()`, `void fragment()`).
    * Enforces `discard` keyword usage (Fragment processor only).
    * Restricts `varying` assignments (Vertex/Fragment only, never in Light processor).
    * Validates Uniform Hints (e.g., checks if `hint_range` is used on a `float/int`).

### ⚡ LSP Capabilities
* **Diagnostics**: Real-time error reporting with squiggly underlines.
* **Auto-Completion**:
    * Smart suggestions for variables in scope.
    * Struct members and Vector swizzling (e.g., typing `my_vec.` suggests `xy`, `rgb`).
    * Built-in functions and types.
* **Go to Definition**: Jump instantly to where variables, structs, or functions are defined.
* **Hover Documentation**:
    * Shows types and function signatures (e.g., `vec3 mix(vec3, vec3, float)`).
    * Displays documentation for built-in Godot functions.
    * Shows values of `const` variables.

### Native Syntax Highlighting (Godot Client)
* Includes a GDExtension-based syntax highlighter.
* **Adaptive Theming**: Automatically pulls colors from your **Godot Editor Settings**, ensuring your shaders look consistent with your current theme (Light/Dark/Custom).

---

## Building

This project uses **SCons** as its build system.

### Prerequisites
* **C++ Compiler**: GCC, Clang, or MSVC supporting C++17.
* **SCons**: Install via Python (`pip install scons`).

### Compiling the LSP Server
To build the standalone LSP executable:

```bash
# Debug build
scons platform=<platform> target=template_debug

# Release build
scons platform=<platform> target=template_release