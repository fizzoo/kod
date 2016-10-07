cmake_minimum_required(VERSION 3.6)

set(CMAKE_CXX_STANDARD 11)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -DFETT}" )

file(GLOB SOURCES "*.cpp")


project (young)
add_executable (young ${SOURCES})
