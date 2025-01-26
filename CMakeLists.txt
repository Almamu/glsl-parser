project(glsl-parser)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-rtti -fno-exceptions -Wall -Wextra -Wformat")

add_executable(glsl-parser
        ast.cpp
        lexer.cpp
        parser.cpp
        main.cpp
        util.cpp
)