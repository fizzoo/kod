cmake_minimum_required(VERSION 3.6)

set(CMAKE_CXX_STANDARD 11)

file(GLOB SOURCES "*.cpp")


project (ceil)
add_executable (ceil ${SOURCES})
