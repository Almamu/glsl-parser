#!/bin/bash

shopt -s globstar
clang-tidy --format-style file ast.* lexer.* main.* parser.* printer.* util.* -p cmake-build-debug