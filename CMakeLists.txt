# This CMake script is adapted from original Makefile.
# Created by Yuan Tong.
# This CMake script is placed in the Public Domain, and No warranty is given.

cmake_minimum_required(VERSION 2.8.6)

project(csapp_datalab C)

set(CMAKE_C_STANDARD 90)

set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -m32")

add_definitions(
        -Wall

        # I have to do that
        -Wno-int-in-bool-context

        # silent both IDE and compiler
        -Wno-unknown-pragmas

        # Warnings from testbed. Not my fault.
        -Wno-strict-aliasing
        -Wno-unused-but-set-variable
        -Wno-maybe-uninitialized
)

link_libraries(m)

if(CMAKE_BUILD_TYPE STREQUAL Release)
    message(WARNING "Compiler optimization enabled. Results may be different from expectation.")
endif()

add_executable(btest btest.c bits.c decl.c tests.c btest.h bits.h)
add_executable(ishow ishow.c)
add_executable(fshow fshow.c)