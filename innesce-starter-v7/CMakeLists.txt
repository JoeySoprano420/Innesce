
cmake_minimum_required(VERSION 3.16)
project(innesce LANGUAGES CXX)
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

add_executable(innescec
  src/main.cpp
  src/frontend/lexer.cpp
  src/frontend/parser.cpp
  src/frontend/sema.cpp
)
target_include_directories(innescec PRIVATE src)
