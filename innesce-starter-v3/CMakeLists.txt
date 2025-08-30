cmake_minimum_required(VERSION 3.22)
project(innesce LANGUAGES CXX)

option(INNSCE_ENABLE_LLVM "Build the LLVM backend" OFF)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# Warnings
if (MSVC)
  add_compile_options(/W4 /permissive- /EHsc-)
else()
  add_compile_options(-Wall -Wextra -Wpedantic -fno-exceptions)
endif()

# Core library (no LLVM)
add_library(innesce_core
  src/core/lanes.cpp
)
target_include_directories(innesce_core PUBLIC
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src>
  $<INSTALL_INTERFACE:include>
)
if (WIN32)
  target_link_libraries(innesce_core PUBLIC synchronization)
endif()

# Frontend (always build)
add_library(innesce_frontend
  src/frontend/lexer.cpp
  src/frontend/parser.cpp
  src/frontend/sema.cpp
)
target_include_directories(innesce_frontend PUBLIC src)
target_link_libraries(innesce_frontend PUBLIC innesce_core)

# Optional LLVM backend
if (INNSCE_ENABLE_LLVM)
  list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")
  include(FindLLVMHelper)
  add_compile_definitions(INNSCE_HAVE_LLVM=1)
  add_library(innesce_backend_llvm
    src/backend/llvm/ir_builder.cpp
    src/backend/llvm/codegen.cpp
  )
  target_include_directories(innesce_backend_llvm PUBLIC ${LLVM_INCLUDE_DIRS} src)
  target_link_libraries(innesce_backend_llvm PUBLIC ${LLVM_LIBRARIES})
  target_compile_definitions(innesce_backend_llvm PUBLIC ${LLVM_DEFINITIONS})
endif()

# CLI
add_executable(innescec cli/innescec.cpp)
target_include_directories(innescec PRIVATE src)
target_link_libraries(innescec PRIVATE innesce_core innesce_frontend)
if (INNSCE_ENABLE_LLVM)
  target_link_libraries(innescec PRIVATE innesce_backend_llvm)
endif()

# Tests
enable_testing()
add_subdirectory(tests)
