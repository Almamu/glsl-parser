cmake_minimum_required(VERSION 3.12)
project(glsl-parser)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-rtti -fno-exceptions -Wall -Wextra -Wformat")
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

add_library(libglsl-parser
        ast.cpp
        lexer.cpp
        parser.cpp
        printer.cpp
        util.cpp
        ast.h
        lexer.h
        parser.h
        printer.h
        util.h
)

add_executable(glsl-parser
        ast.cpp
        lexer.cpp
        parser.cpp
        printer.cpp
        main.cpp
        util.cpp
        ast.h
        lexer.h
        parser.h
        printer.h
        util.h
)