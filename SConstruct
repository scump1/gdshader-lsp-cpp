import os

env = Environment(ENV=os.environ)

# -------------------------------------------------------------------------
# COMPILER CONFIGURATION
# -------------------------------------------------------------------------
env.Append(CXXFLAGS=['-std=c++20', '-Wall', '-Wextra', '-O2'])

# -------------------------------------------------------------------------
# INCLUDE PATHS
# -------------------------------------------------------------------------
env.Append(CPPPATH=[
    'src',
    'extern/lsp-framework',          
    'extern/lsp-framework/build',   
])

# -------------------------------------------------------------------------
# LIBRARIES
# -------------------------------------------------------------------------
env.Append(LIBPATH=['extern/lsp-framework/build']) 
env.Append(LIBS=['lsp'])

if env['PLATFORM'] == 'posix':
    env.Append(LIBS=['pthread'])

# -------------------------------------------------------------------------
# BUILD DIRECTORY SETUP
# -------------------------------------------------------------------------
# This magic line maps 'src' to 'build'.
# duplicate=0: Compiles from 'src' but writes .o files to 'build', 
# without copying the actual .cpp files.
env.VariantDir('build', 'src', duplicate=0)

# -------------------------------------------------------------------------
# COMPILATION DATABASE
# -------------------------------------------------------------------------
env.Tool('compilation_db')
env.CompilationDatabase('compile_commands.json')

# -------------------------------------------------------------------------
# SOURCE DISCOVERY
# -------------------------------------------------------------------------
sources = []
# Walk the actual 'src' directory to find files
for root, dirs, files in os.walk('src'):
    for file in files:
        if file.endswith('.cpp'):
            # Get path relative to 'src' (e.g., "server/gdshader_server.cpp")
            rel_path = os.path.relpath(os.path.join(root, file), 'src')
            
            # Tell SCons to compile the 'build' version of this file
            # (e.g., "build/server/gdshader_server.cpp")
            sources.append(os.path.join('build', rel_path))

# -------------------------------------------------------------------------
# BUILD TARGET
# -------------------------------------------------------------------------
# Output the executable inside 'build/'
env.Program(target='build/gdshader-lsp', source=sources)