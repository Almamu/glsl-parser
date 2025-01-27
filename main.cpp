#include <cstdio>  // fread, fclose, fprintf, stderr
#include <cstring> // strcmp, memcpy

#include "parser.h"
#include "printer.h"

using namespace glsl;

struct sourceFile {
    const char *fileName;
    FILE *file;
    int shaderType;
};

int main(int argc, char **argv) {
    int shaderType = -1;
    vector<sourceFile> sources;
    while (argc > 1) {
        ++argv;
        --argc;
        if (argv[0][0] == '-' && argv[0][1]) {
            const char *what = argv[0] + 1;
            if (!strcmp(what, "c"))
                shaderType = astTU::kCompute;
            else if (!strcmp(what, "v"))
                shaderType = astTU::kVertex;
            else if (!strcmp(what, "tc"))
                shaderType = astTU::kTessControl;
            else if (!strcmp(what, "te"))
                shaderType = astTU::kTessEvaluation;
            else if (!strcmp(what, "g"))
                shaderType = astTU::kGeometry;
            else if (!strcmp(what, "f"))
                shaderType = astTU::kFragment;
            else {
                fprintf(stderr, "unknown option: `%s'\n", argv[0]);
                return 1;
            }
        } else {
            // Treat as fragment shader by default
            if (shaderType == -1)
                shaderType = astTU::kFragment;
            sourceFile source{};
            if (!strcmp(argv[0], "-")) {
                source.fileName = "<stdin>";
                source.file = stdin;
                source.shaderType = shaderType;
                sources.push_back(source);
            } else {
                source.fileName = argv[0];
                if ((source.file = fopen(argv[0], "r"))) {
                    source.shaderType = shaderType;
                    sources.push_back(source);
                } else {
                    fprintf(stderr, "failed to read shader file: `%s' (ignoring)\n", argv[0]);
                }
            }
        }
    }

    for (auto & source : sources) {
        vector<char> contents;
        // Read contents of file
        if (source.file != stdin) {
            fseek(source.file, 0, SEEK_END);
            contents.resize(ftell(source.file));
            fseek(source.file, 0, SEEK_SET);
            fread(&contents[0], 1, contents.size(), source.file);
            fclose(source.file);
        } else {
            char buffer[1024];
            size_t c;
            while ((c = fread(buffer, 1, sizeof(buffer), stdin))) {
                contents.reserve(contents.size() + c);
                contents.insert(contents.end(), buffer, buffer + c);
            }
        }
        vector<const char*> builtinVariables;
        builtinVariables.push_back("gl_FragColor");
        builtinVariables.push_back("gl_Position");
        contents.push_back('\0');
        parser p(&contents[0], source.fileName);
        astTU *tu = p.parse(source.shaderType, &builtinVariables);
        if (tu) {
            std::string result = printTU(tu);
            printf("%s\n", result.c_str());
        } else {
            fprintf(stderr, "%s\n", p.error());
        }
    }
    return 0;
}
