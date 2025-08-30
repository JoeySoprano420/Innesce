# Helper to locate LLVM in a friendly way
find_package(LLVM 17 QUIET CONFIG)
if (NOT LLVM_FOUND)
  message(FATAL_ERROR "LLVM not found. Set -DLLVM_DIR to the LLVM 17+ CMake config directory.")
endif()

message(STATUS "Found LLVM: ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

include_directories(SYSTEM ${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})
