# glsl-parser

**glsl-parser** is an offline GLSL parser which can be used to do many things with GLSL source code.

The straight-forward API allows you to parse GLSL into an abstact-syntax-tree in only a couple lines, for example

    glsl::parser parse(sourceCode);
    glsl::astTU *translationUnit = parse.parse(astTU::kFragment);
    if (translationUnit) {
        // Do something with the AST here
    } else {
        // A parse error occured
        fprintf(stderr, "%s\n", parse.error());
    }

A test-suite and GLSL source-generator is included to get you started.

Check out the superior diagnostics [here](EXAMPLE_ERRORS.md)

### Known limitations
  * Does not support preprocessor directives
    * Does support `#version` and `#extension` though.
    * Supports `#ifdef`, `#if`, `#elif`, `#else` and `#endif` although this implementation might be iffy
  * Does not handle new-line termination with the backslash character `\`
  * Not all of GLSL is supported, if you run into a missing feature open an issue.
  * None of the builtin functions or variables are provided, you must provide those yourself.

### Possible uses
  * Verify a shader without running it
  * Find hard-to-find syntax or semantic errors with the superior diagnostics
  * Extend GLSL
  * Transcompile GLSL to other languages
  * Quickly check or verify something
  * Optimize shaders
  * Generate introspection information

### Portable and embeddable
  * Only uses *std::vector* from the standard library
  * Exception free
  * Small (~90 KB)
  * Permissive (MIT)

**WARNING**: The printer part uses a global variable as buffer to generate the new file so it's not thread-safe.
Using a context and passing it around might be something to look into in the future,
but should be enough for our purposes for now.

**NOTE**: This version is updated and customized for usage in linux-wallpaperengine and might not be what you're looking
to use in the long run. Thanks to the original creator [graphitemaster](https://github.com/graphitemaster/glsl-parser)
for the library that served as base for this.