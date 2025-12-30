# GDShader LSP

A high-performance **Language Server Protocol (LSP)** implementation for the Godot Shading Language (`.gdshader`), written in C++.

This tool provides advanced code intelligence, strict type checking, and real-time error reporting for Godot shaders, designed to integrate seamlessly into editors like VSCode, Emacs, or Godot itself (via GDExtension).

## Features

This LSP is feature complete. It handles all requests like any other full-featured language server. From autocompletion to hover, go-to definitions and even syntax highliting. 

### Dev Insights

We are not using a standalone memory allocation system (like an arena allocator). Shader files tend to be rather short, mostly under 1000 lines. The cpp built-in allocs are perfectly capable of handling these scenarios. In stress test, even with files around 5000 lines, many preprocessors and includes, compiling with many errors, we never exceeded the 25ms mark on the full pipeline from client -> server -> client.

---

## Building

This project uses **SCons** as its build system.

### Prerequisites
* **C++ Compiler**: GCC, Clang, or MSVC supporting C++17.
* **SCons**: Install via Python (`pip install scons`).

### Compiling the LSP Server
To build the standalone LSP executable:

```bash
scons platform=<platform>
```

Where platform = [linux, windos, macos].