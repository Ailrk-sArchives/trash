cmake_minimum_required(VERSION 2.8.2)
project(language_laywer)

include_directories(${library_INCLUDE_DIRS})

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON CACHE INTERNAL "")

add_executable(language_laywer
  "main.cc"
  "union-with-deleted-default-constructor-and-destructor.cc"
  )

target_link_libraries(language_laywer ${library_LIBRARIES})

