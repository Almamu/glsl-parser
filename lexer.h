#pragma once

#include <string>
#include "util.h"

namespace glsl {

#define KEYWORD(...)
#define OPERATOR(...)
#define TYPENAME(...)

// Types
#define TYPE(X) kType_##X,
enum tokenTypes {
    kType_Unknown,
    #include "lexemes.h"
};
#undef TYPE
#define TYPE(...)

// Keywords
#undef KEYWORD
#define KEYWORD(X) kKeyword_##X,
enum keywordTypes {
    #include "lexemes.h"
};
#undef KEYWORD
#define KEYWORD(...)

// Operators
#undef OPERATOR
#define OPERATOR(X, ...) kOperator_##X,
enum operatorTypes {
    #include "lexemes.h"
};
#undef OPERATOR
#define OPERATOR(...)

enum {
    kCore,
    kCompatibility,
    kES
};

enum {
    kEnable,
    kRequire,
    kWarn,
    kDisable
};

struct keywordInfo {
    const char *name;
    int type;
};

struct operatorInfo {
    const char *name;
    const char *string;
    int precedence;
};

struct directive {
    enum {
        kVersion,
        kExtension,
        kInclude,
        kIf,
        kIfDef,
        kIfNDef,
        kElse,
        kElIf,
        kEndIf,
        kDefine,
    } type;

    //int type; // kVersion, kExtension

    union {
        struct {
            int version;
            int type; // kCore, kCompatibility, kES
        } asVersion;
        struct {
            char* name;
            int behavior; // kEnable, kRequire, kWarn, kDisable
        } asExtension;
        struct {
            char* file;
        } asInclude;
        struct {
            char* name;
        } asDefine;
        struct {
        } asIf;
        struct {
        } asIfDef;
        struct {
        } asIfNDef;
        struct {
        } asElse;
        struct {
        } asEndIf;
    };
};

struct token {
    [[nodiscard]] int precedence() const;

private:
    token();
    friend struct lexer;
    friend struct parser;
    tokenTypes m_type;
    union {
        char *asIdentifier;
        directive asDirective;
        int asInt;
        keywordTypes asKeyword;
        operatorTypes asOperator;
        unsigned asUnsigned;
        float asFloat;
        double asDouble;
    };
};

struct location {
    location();
    size_t column;
    size_t line;
    size_t position;
private:
    friend struct lexer;
    void advanceColumn(size_t count = 1);
    void advanceLine();
};

struct lexer {
    explicit lexer(const char *data);

    token peek(bool ignore_eol = true, bool ignore_whitespace = true);

    [[nodiscard]] const char *error() const;

    void backup();
    void restore();

    [[nodiscard]] size_t line() const;
    [[nodiscard]] size_t column() const;

protected:
    friend struct parser;

    [[nodiscard]] size_t position() const;

    [[nodiscard]] char at(int offset = 0) const;

    void read(token &out);
    void read(token &out, bool, bool ignore_eol = true, bool ignore_whitespace = true);

    void skipWhitespace(bool allowNewlines = false);

    vector<char> readNumeric(bool isOctal, bool isHex);

private:
    std::string m_data;
    size_t m_length;
    const char *m_error;
    location m_location;
    location m_backup;
};

inline size_t lexer::position() const {
    return m_location.position;
}

inline size_t lexer::line() const {
    return m_location.line;
}

inline size_t lexer::column() const {
    return m_location.column;
}

}
