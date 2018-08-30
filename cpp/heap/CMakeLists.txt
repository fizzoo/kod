cmake_minimum_required(VERSION 3.6)

set(CMAKE_CXX_STANDARD 11)

file(GLOB SOURCES "*.cpp")


project (heap)
add_executable (heap ${SOURCES})
