cmake_minimum_required(VERSION 2.8.2)
project(vector LANGUAGES CXX )

set (CMAKE_CXX_STANDARD 17)
set (CMAKE_CXX_STANDARD_REQUIRED ON)
# output binary tO build
set (CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/build)

add_subdirectory(lib)
add_subdirectory(src)

