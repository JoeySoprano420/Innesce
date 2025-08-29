
cmake_minimum_required(VERSION 3.16)
project(innesce LANGUAGES CXX)
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

option(INNSCE_ENABLE_LLVM "Enable LLVM backend" ON)

add_executable(innescec
  src/main.cpp
  src/frontend/lexer.cpp
  src/frontend/parser.cpp
  src/frontend/sema.cpp
)

target_include_directories(innescec PRIVATE src)

if (INNSCE_ENABLE_LLVM)
  find_package(LLVM REQUIRED CONFIG)
  message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
  message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")
  separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
  add_definitions(${LLVM_DEFINITIONS_LIST})
  include_directories(${LLVM_INCLUDE_DIRS})
  llvm_map_components_to_libnames(REQ_LLVM_LIBS core native orcjit mcjit support target)
  target_sources(innescec PRIVATE
    src/backend/llvm/codegen.cpp
    src/runtime/time.cpp
    src/runtime/fs.cpp
    src/runtime/net.cpp
    src/runtime/rand.cpp
    src/runtime/fs_ext.cpp
    src/runtime/net_ext.cpp
    src/runtime/rand_ext.cpp
  )
  target_link_libraries(innescec PRIVATE ${REQ_LLVM_LIBS})
  target_compile_definitions(innescec PRIVATE INNSCE_ENABLE_LLVM=1)
endif()
