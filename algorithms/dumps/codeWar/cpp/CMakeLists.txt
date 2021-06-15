cmake_minimum_required(VERSION 2.8.2)
project(Cpp)

include_directories(${library_INCLUDE_DIRS})

add_subdirectory(src)

add_executable()

target_link_libraries( ${library_LIBRARIES})

