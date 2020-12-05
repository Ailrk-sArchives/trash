cmake_minimum_required(VERSION 3.5)

project(demo)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

set(EXECUTABLE_OUTPUT_PATH ${PROJECT_BINARY_DIR}/bin)

add_subdirectory(lambda)
add_subdirectory(locks)
add_subdirectory(ptr)
add_subdirectory(stl)
add_subdirectory(typededuction)
add_subdirectory(tp)
add_subdirectory(class)
add_subdirectory(misc)
add_subdirectory(concurrent)
