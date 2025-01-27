#ifndef PRINTER_H
#define PRINTER_H

#include "parser.h"
#include <string>

namespace glsl {
    std::string printTU(astTU *tu);
}

#endif