#!/bin/bash

shopt -s globstar
clang-tidy --format-style file *.cpp *.h -p cmake-build-debug