#include <cstring> // strcmp, memcpy
#include <string>

#include "parser.h"
#include "util.h"

namespace glsl {

parser::parser(const char *source, const char *fileName, parserIncludeResolver* includeResolver, vector<const char*>* builtinVariables)
    : m_ast(nullptr)
    , m_lexer(source)
    , m_fileName(fileName)
    , m_builtinVariables(builtinVariables)
    , m_includeResolver(includeResolver)
{
    m_oom = strnew("Out of memory");
    m_error = strnew("");
}

parser::~parser() {
    delete m_ast;
    for (auto & m_string : m_strings)
        free(m_string);
    for (auto & i : m_memory)
        i.destroy();
}

#define IS_TYPE(TOKEN, TYPE) \
    ((TOKEN).m_type == (TYPE))
#define IS_KEYWORD(TOKEN, KEYWORD) \
    (IS_TYPE((TOKEN), kType_keyword) && (TOKEN).asKeyword == (KEYWORD))
#define IS_OPERATOR(TOKEN, OPERATOR) \
    (IS_TYPE((TOKEN), kType_operator) && (TOKEN).asOperator == (OPERATOR))

#define GC_NEW new(&m_memory)

bool parser::isType(int type) const {
    return IS_TYPE(m_token, type);
}

bool parser::isKeyword(int keyword) const {
    return IS_KEYWORD(m_token, keyword);
}

bool parser::isOperator(int oper) const {
    return IS_OPERATOR(m_token, oper);
}

bool parser::isEndCondition(endCondition condition) const {
    return ((condition & kEndConditionSemicolon)    && isType(kType_semicolon))
        || ((condition & kEndConditionParanthesis)  && isOperator(kOperator_paranthesis_end))
        || ((condition & kEndConditionBracket)      && isOperator(kOperator_bracket_end))
        || ((condition & kEndConditionColon)        && isOperator(kOperator_colon))
        || ((condition & kEndConditionComma)        && isOperator(kOperator_comma))
        || ((condition & kEndConditionLineFeed)     && (isType(kType_eof) || isType(kType_end_of_line)));
}

// Constant expression evaluator
bool parser::isConstant(astExpression *expression) const {
    if (isConstantValue(expression))
        return true;
    else if (expression->type == astExpression::kVariableIdentifier) {
        astVariable *reference = ((astVariableIdentifier*)expression)->variable;
        if (reference->type != astVariable::kGlobal)
            return false;
        astExpression *initialValue = ((astGlobalVariable*)reference)->initialValue;
        if (!initialValue)
            return false;
        return isConstant(initialValue);
    } else if (expression->type == astExpression::kUnaryPlus || expression->type == astExpression::kUnaryMinus) {
        return isConstant(((astUnaryExpression *) expression)->operand);
    } else if (expression->type == astExpression::kOperation) {
        auto *operation = (astOperationExpression*)expression;
        return isConstant(operation->operand1) && isConstant(operation->operand2);
    }
    return false;
}

bool parser::isConstantValue(astExpression *expression) {
    return expression->type == astExpression::kIntConstant ||
           expression->type == astExpression::kUIntConstant ||
           expression->type == astExpression::kFloatConstant ||
           expression->type == astExpression::kDoubleConstant ||
           expression->type == astExpression::kBoolConstant;
}

#define ICONST(X) ((astIntConstant*)(X))
#define UCONST(X) ((astUIntConstant*)(X))
#define FCONST(X) ((astFloatConstant*)(X))
#define DCONST(X) ((astDoubleConstant*)(X))
#define BCONST(X) ((astBoolConstant*)(X))

#define ICONST_NEW(X) GC_NEW astIntConstant(X)
#define UCONST_NEW(X) GC_NEW astUIntConstant(X)
#define FCONST_NEW(X) GC_NEW astFloatConstant(X)
#define DCONST_NEW(X) GC_NEW astDoubleConstant(X)
#define BCONST_NEW(X) GC_NEW astBoolConstant(X)

#define IVAL(X) (ICONST(X)->value)
#define UVAL(X) (UCONST(X)->value)
#define FVAL(X) (FCONST(X)->value)
#define DVAL(X) (DCONST(X)->value)
#define BVAL(X) (BCONST(X)->value)

astConstantExpression *parser::evaluate(astExpression *expression) {
    if (!expression)
        return nullptr;
    else if (isConstantValue(expression))
        return expression;
    else if (expression->type == astExpression::kVariableIdentifier)
        return evaluate(((astGlobalVariable*)((astVariableIdentifier*)expression)->variable)->initialValue);
    else if (expression->type == astExpression::kUnaryMinus) {
        astExpression *operand = evaluate(((astUnaryExpression*)expression)->operand);
        if (!operand)
            return nullptr;
        switch (operand->type) {
        case astExpression::kIntConstant:    return ICONST_NEW(-IVAL(operand));
        case astExpression::kFloatConstant:  return FCONST_NEW(-FVAL(operand));
        case astExpression::kDoubleConstant: return DCONST_NEW(-DVAL(operand));
        default:
            fatal("invalid operation in constant expression");
            return nullptr;
        }
    } else if (expression->type == astExpression::kUnaryPlus) {
        astExpression *operand = evaluate(((astUnaryExpression*)expression)->operand);
        if (!operand)
            return nullptr;
        switch (operand->type) {
            case astExpression::kIntConstant:
            case astExpression::kUIntConstant:
            case astExpression::kFloatConstant:
            case astExpression::kDoubleConstant:
                return operand;
            default:
                fatal("invalid operation in constant expression");
                return nullptr;
        }
    } else if (expression->type == astExpression::kOperation) {
        int operation = ((astOperationExpression*)expression)->operation;
        astExpression *lhs = evaluate(((astBinaryExpression*)expression)->operand1);
        astExpression *rhs = evaluate(((astBinaryExpression*)expression)->operand2);
        if (!lhs)
            return nullptr;
        if (!rhs)
            return nullptr;
        switch (lhs->type) {
            case astExpression::kIntConstant:
                switch (operation) {
                case kOperator_multiply:       return ICONST_NEW(IVAL(lhs) * IVAL(rhs));
                case kOperator_divide:         return ICONST_NEW(IVAL(lhs) / IVAL(rhs));
                case kOperator_modulus:        return ICONST_NEW(IVAL(lhs) % IVAL(rhs));
                case kOperator_plus:           return ICONST_NEW(IVAL(lhs) + IVAL(rhs));
                case kOperator_minus:          return ICONST_NEW(IVAL(lhs) - IVAL(rhs));
                case kOperator_shift_left:     return ICONST_NEW(IVAL(lhs) << IVAL(rhs));
                case kOperator_shift_right:    return ICONST_NEW(IVAL(lhs) >> IVAL(rhs));
                case kOperator_less:           return BCONST_NEW(IVAL(lhs) < IVAL(rhs));
                case kOperator_greater:        return BCONST_NEW(IVAL(lhs) > IVAL(rhs));
                case kOperator_less_equal:     return BCONST_NEW(IVAL(lhs) <= IVAL(rhs));
                case kOperator_greater_equal:  return BCONST_NEW(IVAL(lhs) >= IVAL(rhs));
                case kOperator_equal:          return BCONST_NEW(IVAL(lhs) == IVAL(rhs));
                case kOperator_not_equal:      return BCONST_NEW(IVAL(lhs) != IVAL(rhs));
                case kOperator_bit_and:        return ICONST_NEW(IVAL(lhs) & IVAL(rhs));
                case kOperator_bit_xor:        return ICONST_NEW(IVAL(lhs) ^ IVAL(rhs));
                case kOperator_logical_and:    return BCONST_NEW(IVAL(lhs) && IVAL(rhs));
                case kOperator_logical_xor:    return BCONST_NEW(!IVAL(lhs) != !IVAL(rhs));
                case kOperator_logical_or:     return BCONST_NEW(IVAL(lhs) || IVAL(rhs));
                default:
                    fatal("invalid operation in constant expression");
                    return nullptr;
                }
                break;
            case astExpression::kUIntConstant:
                switch (operation) {
                case kOperator_multiply:       return UCONST_NEW(UVAL(lhs) * UVAL(rhs));
                case kOperator_divide:         return UCONST_NEW(UVAL(lhs) / UVAL(rhs));
                case kOperator_modulus:        return UCONST_NEW(UVAL(lhs) % UVAL(rhs));
                case kOperator_plus:           return UCONST_NEW(UVAL(lhs) + UVAL(rhs));
                case kOperator_minus:          return UCONST_NEW(UVAL(lhs) - UVAL(rhs));
                case kOperator_shift_left:     return UCONST_NEW(UVAL(lhs) << UVAL(rhs));
                case kOperator_shift_right:    return UCONST_NEW(UVAL(lhs) >> UVAL(rhs));
                case kOperator_less:           return BCONST_NEW(UVAL(lhs) < UVAL(rhs));
                case kOperator_greater:        return BCONST_NEW(UVAL(lhs) > UVAL(rhs));
                case kOperator_less_equal:     return BCONST_NEW(UVAL(lhs) <= UVAL(rhs));
                case kOperator_greater_equal:  return BCONST_NEW(UVAL(lhs) >= UVAL(rhs));
                case kOperator_equal:          return BCONST_NEW(UVAL(lhs) == UVAL(rhs));
                case kOperator_not_equal:      return BCONST_NEW(UVAL(lhs) != UVAL(rhs));
                case kOperator_bit_and:        return UCONST_NEW(UVAL(lhs) & UVAL(rhs));
                case kOperator_bit_xor:        return UCONST_NEW(UVAL(lhs) ^ UVAL(rhs));
                case kOperator_logical_and:    return BCONST_NEW(UVAL(lhs) && UVAL(rhs));
                case kOperator_logical_xor:    return BCONST_NEW(!UVAL(lhs) != !UVAL(rhs));
                case kOperator_logical_or:     return BCONST_NEW(UVAL(lhs) || UVAL(rhs));
                default:
                    fatal("invalid operation in constant expression");
                    return nullptr;
                }
                break;
            case astExpression::kFloatConstant:
                switch (operation) {
                case kOperator_multiply:       return FCONST_NEW(FVAL(lhs) * FVAL(rhs));
                case kOperator_divide:         return FCONST_NEW(FVAL(lhs) / FVAL(rhs));
                case kOperator_plus:           return FCONST_NEW(FVAL(lhs) + FVAL(rhs));
                case kOperator_minus:          return FCONST_NEW(FVAL(lhs) - FVAL(rhs));
                case kOperator_less:           return BCONST_NEW(FVAL(lhs) < FVAL(rhs));
                case kOperator_greater:        return BCONST_NEW(FVAL(lhs) > FVAL(rhs));
                case kOperator_less_equal:     return BCONST_NEW(FVAL(lhs) <= FVAL(rhs));
                case kOperator_greater_equal:  return BCONST_NEW(FVAL(lhs) >= FVAL(rhs));
                case kOperator_equal:          return BCONST_NEW(FVAL(lhs) == FVAL(rhs));
                case kOperator_not_equal:      return BCONST_NEW(FVAL(lhs) != FVAL(rhs));
                case kOperator_logical_and:    return BCONST_NEW(FVAL(lhs) && FVAL(rhs));
                case kOperator_logical_xor:    return BCONST_NEW(!FVAL(lhs) != !FVAL(rhs));
                case kOperator_logical_or:     return BCONST_NEW(FVAL(lhs) || FVAL(rhs));
                default:
                    fatal("invalid operation in constant expression");
                    return nullptr;
                }
                break;
            case astExpression::kDoubleConstant:
                switch (operation) {
                case kOperator_multiply:       return DCONST_NEW(DVAL(lhs) * DVAL(rhs));
                case kOperator_divide:         return DCONST_NEW(DVAL(lhs) / DVAL(rhs));
                case kOperator_plus:           return DCONST_NEW(DVAL(lhs) + DVAL(rhs));
                case kOperator_minus:          return DCONST_NEW(DVAL(lhs) - DVAL(rhs));
                case kOperator_less:           return BCONST_NEW(DVAL(lhs) < DVAL(rhs));
                case kOperator_greater:        return BCONST_NEW(DVAL(lhs) > DVAL(rhs));
                case kOperator_less_equal:     return BCONST_NEW(DVAL(lhs) <= DVAL(rhs));
                case kOperator_greater_equal:  return BCONST_NEW(DVAL(lhs) >= DVAL(rhs));
                case kOperator_equal:          return BCONST_NEW(DVAL(lhs) == DVAL(rhs));
                case kOperator_not_equal:      return BCONST_NEW(DVAL(lhs) != DVAL(rhs));
                case kOperator_logical_and:    return BCONST_NEW(DVAL(lhs) && DVAL(rhs));
                case kOperator_logical_xor:    return BCONST_NEW(!DVAL(lhs) != !DVAL(rhs));
                case kOperator_logical_or:     return BCONST_NEW(DVAL(lhs) || DVAL(rhs));
                default:
                    fatal("invalid operation in constant expression");
                    return nullptr;
                }
                break;
            case astExpression::kBoolConstant:
                switch (operation) {
                case kOperator_equal:          return BCONST_NEW(BVAL(lhs) == BVAL(rhs));
                case kOperator_not_equal:      return BCONST_NEW(BVAL(lhs) != BVAL(rhs));
                case kOperator_logical_and:    return BCONST_NEW(BVAL(lhs) && BVAL(rhs));
                case kOperator_logical_xor:    return BCONST_NEW(!BVAL(lhs) != !BVAL(rhs));
                case kOperator_logical_or:     return BCONST_NEW(BVAL(lhs) || BVAL(rhs));
                default:
                    fatal("invalid operation in constant expression");
                    return nullptr;
                }
                break;
            default:
                fatal("invalid expression");
                return nullptr;
        }
    } else {
        return evaluate(expression);
    }
    return nullptr;
}

void parser::fatal(const char *fmt, ...) {
    // Format banner
    char *banner = nullptr;
    int bannerLength = allocfmt(&banner, "%s:%zu:%zu: error: ", m_fileName, m_lexer.line(), m_lexer.column());
    if (bannerLength == -1) {
        m_error = m_oom;
        return;
    }

    // Format message
    char *message = nullptr;
    va_list va;
    va_start(va, fmt);
    int messageLength = allocvfmt(&message, fmt, va);
    if (messageLength == -1) {
        va_end(va);
        m_error = m_oom;
        return;
    }
    va_end(va);

    // Concatenate the two things
    char *concat = (char *)malloc(bannerLength + messageLength + 1);
    if (!concat) {
        free(banner);
        free(message);
        m_error = m_oom;
        return;
    }

    memcpy(concat, banner, bannerLength);
    memcpy(concat + bannerLength, message, messageLength + 1); // +1 for '\0'
    free(banner);
    free(message);

    m_error = concat;
    m_strings.push_back(m_error);
}

#undef TYPENAME
#define TYPENAME(X) case kKeyword_##X:
bool parser::isBuiltin() const {
    if (!isType(kType_keyword))
        return false;
    switch (m_token.asKeyword) {
    #include "lexemes.h"
        return true;
    default:
        break;
    }
    return false;
}
#undef TYPENAME
#define TYPENAME(...)

/// The parser entry point
astTU *parser::parse(int type) {
    m_ast = new astTU(type);
    m_scopes.push_back(scope());
    m_defines.push_back(defineScope());

    if (m_builtinVariables) {
        for (auto & i : *m_builtinVariables) {
            auto *define = GC_NEW astDefineStatement();

            define->name = strnew(i);

            m_defines.back().push_back(define);
        }
    }

    for (;;) {
        m_lexer.read(m_token, true);

        if (m_lexer.error()) {
            fatal("%s", m_lexer.error());
            return nullptr;
        }

        if (isType(kType_eof)) {
            break;
        }

        if (isType(kType_directive)) {
            if (m_token.asDirective.type == directive::kVersion) {
                if (m_ast->versionDirective) {
                    fatal("Multiple version directives not allowed");
                    return nullptr;
                }
                auto *directive = GC_NEW astVersionDirective();
                directive->version = m_token.asDirective.asVersion.version;
                directive->type = m_token.asDirective.asVersion.type;
                m_ast->versionDirective = directive;
                m_ast->nodes.push_back(directive);
                continue;
            } else if (m_token.asDirective.type == directive::kExtension) {
                auto *extension = GC_NEW astExtensionDirective();
                extension->behavior = m_token.asDirective.asExtension.behavior;
                extension->name = strnew(m_token.asDirective.asExtension.name);
                m_ast->extensionDirectives.push_back(extension);
                m_ast->nodes.push_back(extension);
                continue;
            } else if (m_token.asDirective.type == directive::kInclude) {
                if (!parseIncludeDirective()) {
                    return nullptr;
                }
                continue;
            } else if (m_token.asDirective.type == directive::kDefine) {
                astDefineStatement* define = parseDefineDirective();

                if (!define) {
                    return nullptr;
                }

                m_ast->statements.push_back(define);
                m_ast->nodes.push_back(define);
                continue;
            } else if (m_token.asDirective.type == directive::kIfDef) {
                astIfDefDirectiveStatement *ifdef = parseIfDefDirective();

                if (!ifdef) {
                    return nullptr;
                }

                m_ast->statements.push_back(ifdef);
                m_ast->nodes.push_back(ifdef);
                continue;
            } else if (m_token.asDirective.type == directive::kIfNDef) {
                astIfNDefDirectiveStatement *ifndef = parseIfNDefDirective();

                if (!ifndef) {
                    return nullptr;
                }

                m_ast->statements.push_back(ifndef);
                m_ast->nodes.push_back(ifndef);
                continue;
            } else if (m_token.asDirective.type == directive::kIf) {
                astIfDirectiveStatement *ifdef = parseIfDirective();

                if (!ifdef) {
                    return nullptr;
                }

                m_ast->statements.push_back(ifdef);
                m_ast->nodes.push_back(ifdef);
                continue;
            } else {
                fatal("unexpected directive %d", m_token.asDirective.type);
                return nullptr;
            }
        }

        vector<topLevel> items;
        if (!parseTopLevel(items, &m_ast->nodes))
            return nullptr;

        if (isType(kType_semicolon)) {
            for (auto & parse : items) {
                auto *global = GC_NEW astGlobalVariable();
                global->storage = parse.storage;
                global->auxiliary = parse.auxiliary;
                global->memory = parse.memory;
                global->precision = parse.precision;
                global->interpolation = parse.interpolation;
                global->baseType = parse.type;
                global->name = strnew(parse.name);
                global->isInvariant = parse.isInvariant;
                global->isPrecise = parse.isPrecise;
                global->layoutQualifiers = parse.layoutQualifiers;
                if (parse.initialValue) {
                    if (!(global->initialValue = evaluate(parse.initialValue)))
                        return nullptr;
                }
                global->isArray = parse.isArray;
                global->arraySizes = parse.arraySizes;
                m_ast->globals.push_back(global);
                m_ast->nodes.push_back(global);
                m_scopes.back().push_back(global);
            }
        } else if (isOperator(kOperator_paranthesis_begin)) {
            astFunction *function = parseFunction(items.front());
            if (!function)
                return nullptr;
            m_ast->functions.push_back(function);
            m_ast->nodes.push_back(function);
        } else if (isType(kType_whitespace) || isType(kType_end_of_line)) {
            continue; // whitespace tokens will be used later for the preprocessor
        } else {
            fatal("syntax error at top level %d", m_token.asKeyword);
            return nullptr;
        }
    }
    return m_ast;
}

bool parser::parseStorage(topLevel &current) {
    // const, in, out, attribute, uniform, varying, buffer, shared
    if (isKeyword(kKeyword_const)) {
        current.storage = kConst;
        if (!next()) return false; // skip 'const'
    } else if (isKeyword(kKeyword_in)) {
        current.storage = kIn;
        if (!next()) return false; // skip 'in'
    } else if (isKeyword(kKeyword_out)) {
        current.storage = kOut;
        if (!next()) return false; // skip 'out'
    } else if (isKeyword(kKeyword_attribute)) {
        current.storage = kAttribute;
        if (!next()) return false; // skip 'attribute'
    } else if (isKeyword(kKeyword_uniform)) {
        current.storage = kUniform;
        if (!next()) return false; // skip 'uniform'
    } else if (isKeyword(kKeyword_varying)) {
        current.storage = kVarying;
        if (!next()) return false; // skip 'varying'
    } else if (isKeyword(kKeyword_buffer)) {
        current.storage = kBuffer;
        if (!next()) return false; // skip 'buffer'
    } else if (isKeyword(kKeyword_shared)) {
        current.storage = kShared;
        if (!next()) return false; // skip 'shared'
    }
    return true;
}

bool parser::parseAuxiliary(topLevel &current) {
    // centroid, sample, patch
    if (isKeyword(kKeyword_centroid)) {
        current.auxiliary = kCentroid;
        if (!next()) return false; // skip 'centroid'
    } else if (isKeyword(kKeyword_sample)) {
        current.auxiliary = kSample;
        if (!next()) return false; // skip 'sample'
    } else if (isKeyword(kKeyword_patch)) {
        current.auxiliary = kPatch;
        if (!next()) return false; // skip 'patch'
    }
    return true;
}

bool parser::parseInterpolation(topLevel &current) {
    // smooth, flat, noperspective
    if (isKeyword(kKeyword_smooth)) {
        current.interpolation = kSmooth;
        if (!next()) return false; // skip 'smooth'
    } else if (isKeyword(kKeyword_flat)) {
        current.interpolation = kFlat;
        if (!next()) return false; // skip 'flat'
    } else if (isKeyword(kKeyword_noperspective)) {
        current.interpolation = kNoPerspective;
        if (!next()) return false; // skip 'noperspective'
    }
    return true;
}

bool parser::parsePrecision(topLevel &current) {
    // highp, mediump, lowp
    if (isKeyword(kKeyword_highp)) {
        current.precision = kHighp;
        if (!next()) return false; // skip 'highp'
    } else if (isKeyword(kKeyword_mediump)) {
        current.precision = kMediump;
        if (!next()) return false; // skip 'mediump'
    } else if (isKeyword(kKeyword_lowp)) {
        current.precision = kLowp;
        if (!next()) return false; // skip 'lowp'
    }
    return true;
}

bool parser::parseInvariant(topLevel &current) {
    // invariant
    if (isKeyword(kKeyword_invariant)) {
        current.isInvariant = true;
        if (!next()) return false; // skip 'invariant'
    }
    return true;
}

bool parser::parsePrecise(topLevel &current) {
    // precise
    if (isKeyword(kKeyword_precise)) {
        current.isPrecise = true;
        if (!next()) return false; // skip 'precise'
    }
    return true;
}

bool parser::parseMemory(topLevel &current) {
    // coherent, volatile, restrict, readonly, writeonly
    if (isKeyword(kKeyword_coherent)) {
        current.memory |= kCoherent;
        if (!next()) return false; // skip 'coherent'
    } else if (isKeyword(kKeyword_volatile)) {
        current.memory |= kVolatile;
        if (!next()) return false; // skip 'volatile'
    } else if (isKeyword(kKeyword_restrict)) {
        current.memory |= kRestrict;
        if (!next()) return false; // skip 'restrict'
    } else if (isKeyword(kKeyword_readonly)) {
        current.memory |= kReadOnly;
        if (!next()) return false; // skip 'readonly'
    } else if (isKeyword(kKeyword_writeonly)) {
        current.memory |= kWriteOnly;
        if (!next()) return false; // skip 'writeonly;
    }
    return true;
}

static struct {
    const char *qualifier;
    bool isAssign;
} kLayoutQualifiers[] = {
    { "shared",                     false },
    { "packed",                     false },
    { "std140",                     false },
    { "row_major",                  false },
    { "column_major",               false },
    { "binding",                    true  },
    { "offset",                     true  },
    { "align",                      true  },
    { "location",                   true  },
    { "component",                  true  },
    { "index",                      true  },
    { "triangles",                  false },
    { "quads",                      false },
    { "isolines",                   false },
    { "equal_spacing",              false },
    { "fractional_even_spacing",    false },
    { "fractional_odd_spacing",     false },
    { "cw",                         false },
    { "ccw",                        false },
    { "point_mode",                 false },
    { "points",                     false },
    { "lines",                      false },
    { "lines_adjacency",            false },
    { "triangles_adjacency",        false },
    { "invocations",                true  },
    { "origin_upper_left",          false },
    { "pixel_center_integer",       false },
    { "early_fragment_tests",       false },
    { "local_size_x",               true  },
    { "local_size_y",               true  },
    { "local_size_z",               true  },
    { "xfb_buffer",                 true  },
    { "xfb_stride",                 true  },
    { "xfb_offset",                 true  },
    { "vertices",                   true  },
    { "line_strip",                 false },
    { "triangle_strip",             false },
    { "max_vertices",               true  },
    { "stream",                     true  },
    { "depth_any",                  false },
    { "depth_greater",              false },
    { "depth_less",                 false },
    { "depth_unchanged",            false }
};

bool parser::parseLayout(topLevel &current) {
    vector<astLayoutQualifier*> &qualifiers = current.layoutQualifiers;
    if (isKeyword(kKeyword_layout)) {
        if (!next()) // skip 'layout'
            return false;
        if (!isOperator(kOperator_paranthesis_begin)) {
            fatal("expected `(' after `layout'");
            return false;
        }
        if (!next()) // skip '('
            return false;
        while (!isOperator(kOperator_paranthesis_end)) {
            auto *qualifier = GC_NEW astLayoutQualifier();

            // "The tokens used for layout-qualifier-name are identifiers,
            //  not keywords, however, the shared keyword is allowed as a
            //  layout-qualifier-id."
            if (!isType(kType_identifier) && !isKeyword(kKeyword_shared))
                return false;

            int found = -1;
            qualifier->name = strnew(isType(kType_identifier) ? m_token.asIdentifier : "shared");
            for (size_t i = 0; i < sizeof(kLayoutQualifiers)/sizeof(kLayoutQualifiers[0]); i++) {
                if (strcmp(qualifier->name, kLayoutQualifiers[i].qualifier) != 0)
                    continue;
                found = int(i);
                break;
            }

            if (found == -1) {
                fatal("unknown layout qualifier `%s'", qualifier->name);
                return false;
            }

            if (!next()) // skip identifier or 'shared' keyword
                return false;

            if (isOperator(kOperator_assign)) {
                if (!kLayoutQualifiers[found].isAssign) {
                    fatal("unexpected layout qualifier value on `%s' layout qualifier", qualifier->name);
                    return false;
                }
                if (!next()) // skip '='
                    return false;
                if (!(qualifier->initialValue = parseExpression(kEndConditionComma | kEndConditionParanthesis)))
                    return false;
                if (!isConstant(qualifier->initialValue)) {
                    // TODO: check integer-constant-expression
                    fatal("value for layout qualifier `%s' is not a valid constant expression",
                        qualifier->name);
                    return false;
                }
                if (!(qualifier->initialValue = evaluate(qualifier->initialValue)))
                    return false;
            } else if (kLayoutQualifiers[found].isAssign) {
                fatal("expected layout qualifier value for `%s' layout qualifier", qualifier->name);
                return false;
            }

            if (isOperator(kOperator_comma)) {
                if (!next()) // skip ','
                    return false;
            }
            qualifiers.push_back(qualifier);
        }
        if (!next()) // skip ')'
            return false;
    }
    return true;
}

static bool isInterfaceBlockStorage(int storage) {
    return storage == kIn
        || storage == kOut
        || storage == kUniform
        || storage == kBuffer;
}

static bool isReservedKeyword(int keyword) {
    return keyword == kKeyword_common
        || keyword == kKeyword_partition
        || keyword == kKeyword_active
        || keyword == kKeyword_asm
        || keyword == kKeyword_class
        || keyword == kKeyword_union
        || keyword == kKeyword_enum
        || keyword == kKeyword_typedef
        || keyword == kKeyword_template
        || keyword == kKeyword_this
        || keyword == kKeyword_resource
        || keyword == kKeyword_goto
        || keyword == kKeyword_inline
        || keyword == kKeyword_noinline
        || keyword == kKeyword_public
        || keyword == kKeyword_static
        || keyword == kKeyword_extern
        || keyword == kKeyword_external
        || keyword == kKeyword_interface
        || keyword == kKeyword_long
        || keyword == kKeyword_short
        || keyword == kKeyword_half
        || keyword == kKeyword_fixed
        || keyword == kKeyword_unsigned
        || keyword == kKeyword_superp
        || keyword == kKeyword_input
        || keyword == kKeyword_output
        || keyword == kKeyword_hvec2
        || keyword == kKeyword_hvec3
        || keyword == kKeyword_hvec4
        || keyword == kKeyword_fvec2
        || keyword == kKeyword_fvec3
        || keyword == kKeyword_fvec4
        || keyword == kKeyword_sampler3DRect
        || keyword == kKeyword_filter
        || keyword == kKeyword_sizeof
        || keyword == kKeyword_cast
        || keyword == kKeyword_namespace
        || keyword == kKeyword_using;
}

bool parser::parseTopLevelItem(topLevel &level, vector<astBase*>* nodes, topLevel *continuation, bool allow_undefined) {
    vector<topLevel> items;
    while (!isBuiltin() && !isType(kType_identifier)) {
        // If this is an empty file don't get caught in this loop indefinitely
        token peek = m_lexer.peek();
        if (IS_TYPE(peek, kType_eof))
            return false;
        if (isType(kType_end_of_line)) {
            m_lexer.read(m_token, true);
            continue;
        }

        topLevel item;
        if (continuation)
            item = *continuation;

        if (!parseStorage(item))       return false;
        if (!parseAuxiliary(item))     return false;
        if (!parseInterpolation(item)) return false;
        if (!parsePrecision(item))     return false;
        if (!parseInvariant(item))     return false;
        if (!parsePrecise(item))       return false;
        if (!parseMemory(item))        return false;
        if (!parseLayout(item))        return false;

        if (isType(kType_keyword) && isReservedKeyword(m_token.asKeyword)) {
            fatal("cannot use a reserved keyword");
            return false;
        }

        // Check for interface block.
        if (isType(kType_identifier) && isInterfaceBlockStorage(item.storage)) {
            // if (!next()) return false; // skip identifier
            astInterfaceBlock *unique = parseInterfaceBlock(item.storage);
            if (!unique)
                return false;
            m_ast->interfaceBlocks.push_back(unique);
            nodes->push_back(unique);
            if (isType(kType_semicolon)) {
                return true;
            } else {
                level.type = unique;
            }
        } else if (isKeyword(kKeyword_struct)) {
            if (!next()) return false; // skip struct
            astStruct *unique = parseStruct();
            if (!unique)
                return false;
            m_ast->structures.push_back(unique);
            nodes->push_back(unique);
            if (isType(kType_semicolon))
            {
                return true;
            } else {
                level.type = unique;
            }
        } else {
            items.push_back(item);
        }
    }

    if (continuation) {
        level = *continuation;
        // erase anything that is not an array size on the type, e.g
        // int[2] a[2], b; should produce: int a[2][2]; int b[2];
        level.arraySizes.erase(level.arraySizes.begin() + level.arrayOnTypeOffset, level.arraySizes.end());
    }

    for (auto & next : items) {
        const int storage = level.storage != kStorageUnknown ? level.storage : next.storage;
        if (m_ast->type == astTU::kVertex && storage == kIn) {
            // "It's a compile-time error to use any auxiliary or interpolation
            //  qualifiers on a vertex shader input"
            if (level.auxiliary != kAuxiliaryUnknown || next.auxiliary != kAuxiliaryUnknown) {
                fatal("cannot use auxiliary storage qualifier on vertex shader input");
                return false;
            } else if (level.interpolation != -1 || next.interpolation != -1) {
                fatal("cannot use interpolation qualifier on vertex shader input");
                return false;
            }
        }
        if (m_ast->type == astTU::kFragment && storage == kOut) {
            // "It's a compile-time error to use auxiliary storage qualifiers or
            //  interpolation qualifiers on an output in a fragment shader."
            if (level.auxiliary != kAuxiliaryUnknown || next.auxiliary != kAuxiliaryUnknown) {
                fatal("cannot use auxiliary storage qualifier on fragment shader output");
                return false;
            } else if (level.interpolation != -1 || next.interpolation != -1) {
                fatal("cannot use interpolation qualifier on fragment shader output");
                return false;
            }
        }
        if (m_ast->type != astTU::kTessEvaluation && storage == kIn) {
            // "Applying the patch qualifier to inputs can only be done in tessellation
            //  evaluation shaders. It is a compile-time error to use patch with inputs
            //  in any other stage."
            if (level.auxiliary == kPatch || next.auxiliary == kPatch) {
                fatal("applying `patch' qualifier to input can only be done in tessellation evaluation shaders");
                return false;
            }
        }
        if (m_ast->type != astTU::kTessControl && storage == kOut) {
            // "Applying patch to an output can only be done in a tessellation control
            //  shader. It is a compile-time errot to use patch on outputs in any
            //  other stage."
            if (level.auxiliary == kPatch || next.auxiliary == kPatch) {
                fatal("applying `patch' qualifier to output can only be done in tessellation control shaders");
                return false;
            }
        }
        if (next.storage != kStorageUnknown && level.storage != kStorageUnknown) {
            fatal("multiple storage qualifiers in declaration");
            return false;
        } else if (next.auxiliary != kAuxiliaryUnknown && level.auxiliary != kAuxiliaryUnknown) {
            fatal("multiple auxiliary storage qualifiers in declaration");
            return false;
        } else if (next.interpolation != -1 && level.interpolation != -1) {
            fatal("multiple interpolation qualifiers in declaration");
            return false;
        } if (next.precision != -1 && level.precision != -1) {
            fatal("multiple precision qualifiers in declaration");
            return false;
        }
        level.storage = next.storage;
        level.auxiliary = next.auxiliary;
        level.interpolation = next.interpolation;
        level.precision = next.precision;
        level.memory |= next.memory;

        for (size_t i = 0; i < next.layoutQualifiers.size(); i++) {
            // "When the same layout-qualifier-name occurs multiple times, in a single declaration, the
            //  last occurrence overrides the former occurrence(s)"
            for (size_t j = 0; i < level.layoutQualifiers.size(); j++) {
                if (next.layoutQualifiers[i]->name == level.layoutQualifiers[j]->name)
                    level.layoutQualifiers.erase(level.layoutQualifiers.begin() + j);
            }
            level.layoutQualifiers.push_back(next.layoutQualifiers[i]);
        }
    }

    // "It's a compile-time error to use interpolation qualifiers with patch"
    if (level.auxiliary == kPatch && level.interpolation != -1) {
        fatal("cannot use interpolation qualifier with auxiliary storage qualifier `patch'");
        return false;
    }

    if (!continuation && !level.type) {
        if (isType(kType_identifier)) {
            level.type = findType(m_token.asIdentifier);
            if (level.type && !next()) // skip identifier
                return false;
        } else {
            level.type = parseBuiltin();
            if (!next()) // skip typename
                return false;
        }

        if (level.type) {
            // Could be an array
            while (isOperator(kOperator_bracket_begin)) {
                level.isArray = true;
                astConstantExpression *arraySize = parseArraySize();
                if (!arraySize)
                    return false;
                level.arraySizes.insert(level.arraySizes.begin(), arraySize);
                level.arrayOnTypeOffset++;
                if (!next()) // skip ']'
                    return false;
            }
        }
    }

    if (!level.type) {
        fatal("expected typename");
        return false;
    }

    if (isType(kType_identifier)) {
        level.name = strnew(m_token.asIdentifier);
        if (!next())// skip identifier
            return false;
    }

    while (isOperator(kOperator_bracket_begin)) {
        level.isArray = true;
        level.arraySizes.push_back(parseArraySize(allow_undefined));
        if (!next()) // skip ']'
            return false;
    }

    if (level.storage == kConst || level.storage == kUniform) {
        // Can have a constant expression assignment
        if (isOperator(kOperator_assign)) {
            if (!next()) // skip '='
                return false;
            if (!(level.initialValue = parseExpression(kEndConditionSemicolon)))
                return false;
            if (!isConstant(level.initialValue)) {
                fatal("not a valid constant expression");
                return false;
            }
        } else if (level.storage != kUniform) {
            fatal("const-qualified variable declared but not initialized");
            return false;
        }
    }

    // If it isn't a function or prototype than the use of void is not legal
    if (!isOperator(kOperator_paranthesis_begin)) {
        if (level.type->typeType == astType::kBuiltin && ((astBuiltin*)level.type)->type == kKeyword_void) {
            fatal("`void' cannot be used in declaration");
            return false;
        }
    }

    // if it doesn't have a name than it's illegal
    if (strnil(level.name)) {
        fatal("expected name for declaration");
        return false;
    }

    return true;
}

bool parser::parseTopLevel(vector<topLevel> &items, vector<astBase*>* nodes, bool allow_undefined) {
    topLevel item;
    if (!parseTopLevelItem(item, nodes, nullptr, allow_undefined))
        return false;
    if (item.type)
        items.push_back(item);
    while (!items.empty() && isOperator(kOperator_comma)) {
        if (!next())
            return false; // skip ','
        topLevel nextItem;
        if (!parseTopLevelItem(nextItem, nodes, &items.front()))
            return false;
        if (nextItem.type)
            items.push_back(nextItem);
    }
    return true;
}

template<typename T>
T *parser::parseBlock(const char* type) {
    T *unique = GC_NEW T;

    if (isType(kType_identifier)) {
        unique->name = strnew(m_token.asIdentifier);
        if (!next()) return nullptr; // skip identifier
    }

    if (!isType(kType_scope_begin)) {
        fatal("expected '{' for %s definition", type);
        return nullptr;
    }

    if (!next()) return nullptr; // skip '{'

    vector<topLevel> items;
    while (!isType(kType_scope_end)) {
        if (!parseTopLevel(items, &m_ast->nodes))
            return nullptr;
        if (!next())
            return nullptr;
    }

    for (auto & parse : items) {
        auto *field = GC_NEW astVariable(astVariable::kField);
        field->baseType = parse.type;
        field->name = strnew(parse.name);
        field->isPrecise = parse.isPrecise;
        field->isArray = parse.isArray;
        field->arraySizes = parse.arraySizes;
        unique->fields.push_back(field);
    }

    if (!next()) return nullptr; // skip '}'

    return unique;
}

astStruct *parser::parseStruct() {
    return parseBlock<astStruct>("structure");
}

astInterfaceBlock *parser::parseInterfaceBlock(int storage) {
    astInterfaceBlock* unique = nullptr;
    switch (storage) {
        case kIn:
            unique = parseBlock<astInterfaceBlock>("input block");
            break;
        case kOut:
            unique = parseBlock<astInterfaceBlock>("outout block");
            break;
        case kUniform:
            unique = parseBlock<astInterfaceBlock>("uniform block");
            break;
        case kBuffer:
            unique = parseBlock<astInterfaceBlock>("buffer block");
            break;
        default:
            return nullptr;
    }

    // When there's no identifier then implicitly declare these as globals
    // in their respective places.
    if (!isType(kType_identifier)) {
        for (auto & field : unique->fields) {
            // Check if the variable already exists
            astVariable *variable = field;
            if (findVariable(variable->name)) {
                fatal("'%s` is already declared in this scope", variable->name);
                return nullptr;
            }
            m_scopes.back().push_back(field);
        }
    }

    unique->storage = storage;
    return unique;
}

astExpression *parser::parseBinary(int lhsPrecedence, astExpression *lhs, endCondition condition, bool allow_undefined) {
    // Precedence climbing
    while (!isEndCondition(condition)) {
        int binaryPrecedence = m_token.precedence();
        if (binaryPrecedence < lhsPrecedence)
            break;

        astBinaryExpression *expression = createExpression();
        if (!next(!(condition & kEndConditionLineFeed)))
            return nullptr;

        astExpression *rhs = parseUnary(condition, allow_undefined);
        if (!rhs)
            return nullptr;
        if (!next(!(condition & kEndConditionLineFeed)))
            return nullptr;

        if (((astExpression*)expression)->type == astExpression::kAssign) {
            astExpression *find = lhs;
            while (find->type == astExpression::kArraySubscript
                || find->type == astExpression::kFieldOrSwizzle)
            {
                find = (find->type == astExpression::kArraySubscript)
                    ? ((astArraySubscript*)find)->operand
                    : ((astFieldOrSwizzle*)find)->operand;
            }
            if (find->type == astExpression::kVariableIdentifier) {
                astVariable *variable = ((astVariableIdentifier*)lhs)->variable;
                if (variable->type == astVariable::kGlobal) {
                    auto *global = (astGlobalVariable*)variable;
                    // "It's a compile-time error to write to a variable declared as an input"
                    if (global->storage == kIn) {
                        fatal("cannot write to a variable declared as input");
                        return nullptr;
                    }
                    // "It's a compile-time error to write to a const variable outside of its declaration."
                    if (global->storage == kConst) {
                        fatal("cannot write to a const variable outside of its declaration");
                        return nullptr;
                    }
                }
            } else if (find->type != astExpression::kDefineIdentifier && (allow_undefined && find->type != astExpression::kUnknownIdentifier)) {
                fatal("not a valid lvalue");
                return nullptr;
            }
        }

        int rhsPrecedence = m_token.precedence();

        // climb
        if (binaryPrecedence < rhsPrecedence) {
            if (!(rhs = parseBinary(binaryPrecedence + 1, rhs, condition, allow_undefined)))
                return nullptr;
        }

        expression->operand1 = lhs;
        expression->operand2 = rhs;
        lhs = expression;
    }
    return lhs;
}

astExpression *parser::parseUnaryPrefix(endCondition condition, bool allow_undefined) {
    if (isOperator(kOperator_paranthesis_begin)) {
        if (!next()) return nullptr; // skip '('
        return parseExpression(kEndConditionParanthesis, allow_undefined);
    } else if (isOperator(kOperator_logical_not)) {
        if (!next()) return nullptr; // skip '!'
        return GC_NEW astUnaryLogicalNotExpression(parseUnary(condition));
    } else if (isOperator(kOperator_bit_not)) {
        if (!next()) return nullptr; // skip '~'
        return GC_NEW astUnaryBitNotExpression(parseUnary(condition));
    } else if (isOperator(kOperator_plus)) {
        if (!next()) return nullptr; // skip '+'
        return GC_NEW astUnaryPlusExpression(parseUnary(condition));
    } else if (isOperator(kOperator_minus)) {
        if (!next()) return nullptr; // skip '-'
        return GC_NEW astUnaryMinusExpression(parseUnary(condition, allow_undefined));
    } else if (isOperator(kOperator_increment)) {
        if (!next()) return nullptr; // skip '++'
        return GC_NEW astPrefixIncrementExpression(parseUnary(condition));
    } else if (isOperator(kOperator_decrement)) {
        if (!next()) return nullptr; // skip '--'
        return GC_NEW astPrefixDecrementExpression(parseUnary(condition));
    } else if (isBuiltin()) {
        return parseConstructorCall(allow_undefined);
    } else if (isType(kType_identifier)) {
        token peek = m_lexer.peek(!(condition & kEndConditionLineFeed));
        if (IS_OPERATOR(peek, kOperator_paranthesis_begin)) {
            astType *type = findType(m_token.asIdentifier);
            if (type)
                return parseConstructorCall(allow_undefined);
            else
                return parseFunctionCall(allow_undefined);
        } else {
            astVariable *find = findVariable(m_token.asIdentifier);
            if (find)
                return GC_NEW astVariableIdentifier(find);
            astDefineStatement* define = findDefine(m_token.asIdentifier);
            if (define)
                return GC_NEW astDefineIdentifier(define);
            if (allow_undefined)
                return GC_NEW astUnknownIdentifier(strnew(m_token.asIdentifier));
            fatal("`%s' was not declared in this scope", m_token.asIdentifier);
            return nullptr;
        }
    } else if (isKeyword(kKeyword_true)) {
        return BCONST_NEW(true);
    } else if (isKeyword(kKeyword_false)) {
        return BCONST_NEW(false);
    } else if (isType(kType_constant_int)) {
        return ICONST_NEW(m_token.asInt);
    } else if (isType(kType_constant_uint)) {
        return UCONST_NEW(m_token.asUnsigned);
    } else if (isType(kType_constant_float)) {
        return FCONST_NEW(m_token.asFloat);
    } else if (isType(kType_constant_double)) {
        return DCONST_NEW(m_token.asDouble);
    } else if (condition == kEndConditionBracket) {
        return nullptr;
    }
    fatal("syntax error during unary prefix");
    return nullptr;
}

astType* parser::getType(astExpression *expression)
{
    switch (expression->type) {
        case astExpression::kVariableIdentifier:
            return ((astVariableIdentifier*)expression)->variable->baseType;
        case astExpression::kFieldOrSwizzle:
            return getType(((astFieldOrSwizzle*)expression)->operand);
        case astExpression::kArraySubscript:
            return getType(((astArraySubscript*)expression)->operand);
        case astExpression::kFunctionCall:
            for (auto & function : m_ast->functions) {
                if (strcmp(function->name, ((astFunctionCall*)expression)->name) != 0)
                    continue;
                return function->returnType;
            }
            break;
        case astExpression::kConstructorCall:
            return ((astConstructorCall*)expression)->type;

        default:
            return nullptr;
    }

    return nullptr;
}

astExpression *parser::parseUnary(endCondition condition, bool allow_undefined) {
    astExpression *operand = parseUnaryPrefix(condition, allow_undefined);
    if (!operand)
        return nullptr;
    for (;;) {
        token peek = m_lexer.peek(!(condition & kEndConditionLineFeed));
        if (IS_OPERATOR(peek, kOperator_dot)) {
            if (!next()) return nullptr; // skip last
            if (!next()) return nullptr; // skip '.'
            if (!isType(kType_identifier)) {
                fatal("expected field identifier or swizzle after `.'");
                return nullptr;
            }
            auto *expression = GC_NEW astFieldOrSwizzle();

            astType *type = getType(operand);
            if (type && type->typeType != astType::kBuiltin) {
                astVariable *field = nullptr;
                auto *kind = (astStruct*)type;
                for (auto & i : kind->fields) {
                    if (strcmp(i->name, m_token.asIdentifier) != 0)
                        continue;
                    field = i;
                    break;
                }
                if (!field) {
                    fatal("field `%s' does not exist in structure `%s'", m_token.asIdentifier, kind->name);
                    return nullptr;
                }
            }

            expression->operand = operand;
            expression->name = strnew(m_token.asIdentifier);
            operand = expression;
        } else if (IS_OPERATOR(peek, kOperator_increment)) {
            if (!next()) return nullptr; // skip last
            operand = GC_NEW astPostIncrementExpression(operand);
        } else if (IS_OPERATOR(peek, kOperator_decrement)) {
            if (!next()) return nullptr; // skip last
            operand = GC_NEW astPostDecrementExpression(operand);
        } else if (IS_OPERATOR(peek, kOperator_bracket_begin)) {
            if (!next()) return nullptr; // skip last
            if (!next()) return nullptr; // skip '['
            auto *expression = GC_NEW astArraySubscript();
            astExpression *find = operand;
            while (find->type == astExpression::kArraySubscript)
                find = ((astArraySubscript*)find)->operand;
            if (find->type != astExpression::kVariableIdentifier) {
                fatal("cannot be subscripted");
                return nullptr;
            }
            expression->operand = operand;
            if (!(expression->index = parseExpression(kEndConditionBracket)))
                return nullptr;
            if (isConstant(expression->index)) {
                if (!(expression->index = evaluate(expression->index)))
                    return nullptr;
            }
            operand = expression;
        } else if (IS_OPERATOR(peek, kOperator_questionmark)) {
            if (!next()) return nullptr; // skip last
            if (!next()) return nullptr; // skip '?'
            auto *expression = GC_NEW astTernaryExpression();
            expression->condition = operand;
            expression->onTrue = parseExpression(kEndConditionColon, allow_undefined);
            if (!isOperator(kOperator_colon)) {
                fatal("expected `:' for else case in ternary statement");
                return nullptr;
            }
            if (!next()) return nullptr; // skip ':'
            if (!(expression->onFalse = parseUnary(condition))) {
                fatal("expected expression after `:' in ternary statement");
                return nullptr;
            }
            operand = expression;
        } else {
            break;
        }
    }
    return operand;
}

astExpression *parser::parseExpression(endCondition condition, bool allow_undefined) {
    astExpression *lhs = parseUnary(condition, allow_undefined);
    if (!lhs)
        return nullptr;
    if (!next(!(condition & kEndConditionLineFeed))) // skip last
        return nullptr;
    return parseBinary(0, lhs, condition, allow_undefined);
}

astExpressionStatement *parser::parseExpressionStatement(endCondition condition, bool allow_undefined) {
    astExpression *expression = parseExpression(condition, allow_undefined);
    return expression ? GC_NEW astExpressionStatement(expression) : nullptr;
}

astConstantExpression *parser::parseArraySize(bool allow_undefined) {
    if (!next()) // skip '['
        return nullptr;
    return parseExpression(kEndConditionBracket, allow_undefined);
}

astCompoundStatement *parser::parseCompoundStatement() {
    auto *statement = GC_NEW astCompoundStatement();
    if (!next()) // skip '{'
        return nullptr;
    while (!isType(kType_scope_end) && !(isType(kType_directive) && (m_token.asDirective.type == directive::kEndIf || m_token.asDirective.type == directive::kElse || m_token.asDirective.type == directive::kElIf))) {
        astStatement *nextStatement = parseStatement();
        if (!nextStatement) return nullptr;
        statement->statements.push_back(nextStatement);
        if (!next()) // skip ';'
            return nullptr;
    }
    return statement;
}

astIfStatement *parser::parseIfStatement() {
    auto *statement = GC_NEW astIfStatement();
    if (!next()) // skip 'if'
        return nullptr;
    if (!isOperator(kOperator_paranthesis_begin)) {
        fatal("expected `(' after `if'");
        return nullptr;
    }
    if (!next()) // skip '('
        return nullptr;
    if (!(statement->condition = parseExpression(kEndConditionParanthesis)))
        return nullptr;
    if (!next()) // skip ')'
        return nullptr;
    statement->thenStatement = parseStatement();
    token peek = m_lexer.peek();
    if (IS_KEYWORD(peek, kKeyword_else)) {
        if (!next()) // skip ';' or '}'
            return nullptr;
        if (!next()) // skip 'else'
            return nullptr;
        if (!(statement->elseStatement = parseStatement()))
            return nullptr;
    }
    return statement;
}

astSwitchStatement *parser::parseSwitchStatement() {
    auto *statement = GC_NEW astSwitchStatement();
    if (!next()) // skip 'switch'
        return nullptr;
    if (!isOperator(kOperator_paranthesis_begin)) {
        fatal("expected `(' after `switch'");
        return nullptr;
    }
    if (!next()) // skip '('
        return nullptr;
    if (!(statement->expression = parseExpression(kEndConditionParanthesis)))
        return nullptr;
    if (!next()) // skip next
        return nullptr;
    if (!isType(kType_scope_begin)) {
        fatal("expected `{' after `)'");
        return nullptr;
    }
    if (!next()) // skip '{'
        return nullptr;

    vector<int> seenInts;
    vector<unsigned int> seenUInts;
    bool hadDefault = false;
    while (!isType(kType_scope_end)) {
        astStatement *nextStatement = parseStatement();
        if (!nextStatement) return nullptr;
        if (nextStatement->type == astStatement::kCaseLabel) {
            auto *caseLabel = (astCaseLabelStatement*)nextStatement;
            if (!caseLabel->isDefault) {
                if (!isConstant(caseLabel->condition)) {
                    fatal("case label is not a valid constant expression");
                    return nullptr;
                }
                astConstantExpression *value = evaluate(caseLabel->condition);
                // "It is a compile-time error to have two case label constant-expression of equal value"
                if (value->type == astExpression::kIntConstant) {
                    const int val = IVAL(value);
                    if (glsl::find(seenInts.begin(), seenInts.end(), val) != seenInts.end()) {
                        fatal("duplicate case label `%d'", val);
                        return nullptr;
                    }
                    seenInts.push_back(val);
                } else if (value->type == astExpression::kUIntConstant) {
                    const unsigned int val = UVAL(value);
                    if (glsl::find(seenUInts.begin(), seenUInts.end(), val) != seenUInts.end()) {
                        fatal("duplicate case label `%u'", val);
                        return nullptr;
                    }
                    seenUInts.push_back(val);
                } else {
                    fatal("case label must be scalar `int' or `uint'");
                    return nullptr;
                }
            } else {
                // "It's a compile-time error to have more than one default"
                if (hadDefault) {
                    fatal("duplicate `default' case label");
                    return nullptr;
                }
                hadDefault = true;
            }
        }
        statement->statements.push_back(nextStatement);
        if (!next())
            return nullptr;
    }

    // TODO: verify scope of where switches are found
    return statement;
}

astCaseLabelStatement *parser::parseCaseLabelStatement() {
    auto *statement = GC_NEW astCaseLabelStatement();
    if (isKeyword(kKeyword_default)) {
        statement->isDefault = true;
        if (!next()) // skip 'default'
            return nullptr;
        if (!isOperator(kOperator_colon)) {
            fatal("expected `:' after `default' in case label");
            return nullptr;
        }
    } else {
        if (!next()) // skip 'case'
            return nullptr;
        statement->condition = parseExpression(kEndConditionColon);
    }
    return statement;
}

astForStatement *parser::parseForStatement() {
    auto *statement = GC_NEW astForStatement();
    if (!next()) // skip 'for'
        return nullptr;
    if (!isOperator(kOperator_paranthesis_begin)) {
        fatal("expected `(' after `for'");
        return nullptr;
    }
    if (!next()) // skip '('
        return nullptr;
    if (!isType(kType_semicolon))
        if (!(statement->init = parseDeclarationOrExpressionStatement(kEndConditionSemicolon)))
            return nullptr;
    if (!next()) // skip ';'
        return nullptr;
    if (!isType(kType_semicolon))
        if (!(statement->condition = parseExpression(kEndConditionSemicolon)))
            return nullptr;
    if (!next()) // skip ';'
        return nullptr;
    if (!isOperator(kOperator_paranthesis_end)) {
        if (!(statement->loop = parseExpression(kEndConditionParanthesis)))
            return nullptr;
    }
    if (!next()) // skip ')'
        return nullptr;
    statement->body = parseStatement();
    return statement;
}

astContinueStatement *parser::parseContinueStatement() {
    auto *statement = GC_NEW astContinueStatement();
    if (!next()) // skip 'continue'
        return nullptr;
    return statement;
}

astBreakStatement *parser::parseBreakStatement() {
    auto *statement = GC_NEW astBreakStatement();
    if (!next())
        return nullptr; // skip 'break'
    if (!isType(kType_semicolon)) {
        fatal("expected semicolon after break statement");
        return nullptr;
    }
    return statement;
}

astDiscardStatement *parser::parseDiscardStatement() {
    auto *statement = GC_NEW astDiscardStatement();
    if (!next()) // skip 'discard'
        return nullptr;
    if (!isType(kType_semicolon)) {
        fatal("expected semicolon after discard statement");
        return nullptr;
    }
    return statement;
}

astReturnStatement *parser::parseReturnStatement() {
    auto *statement = GC_NEW astReturnStatement();
    if (!next()) // skip 'return'
        return nullptr;
    if (!isType(kType_semicolon)) {
        if (!(statement->expression = parseExpression(kEndConditionSemicolon)))
            return nullptr;
        if (!isType(kType_semicolon)) {
            fatal("expected semicolon after return statement");
            return nullptr;
        }
    }
    return statement;
}

astDoStatement *parser::parseDoStatement() {
    auto *statement = GC_NEW astDoStatement();
    if (!next()) // skip 'do'
        return nullptr;
    if (!(statement->body = parseStatement()))
        return nullptr;
    if (!next())
        return nullptr;
    if (!isKeyword(kKeyword_while)) {
        fatal("expected `while' after `do'");
        return nullptr;
    }
    if (!next()) // skip 'while'
        return nullptr;
    if (!isOperator(kOperator_paranthesis_begin)) {
        fatal("expected `(' after `while'");
        return nullptr;
    }
    if (!next()) // skip '('
        return nullptr;
    if (!(statement->condition = parseExpression(kEndConditionParanthesis)))
        return nullptr;
    if (!next())
        return nullptr;
    return statement;
}

astWhileStatement *parser::parseWhileStatement() {
    auto *statement = GC_NEW astWhileStatement();
    if (!next()) // skip 'while'
        return nullptr;
    if (!isOperator(kOperator_paranthesis_begin)) {
        fatal("expected `(' after `while'");
        return nullptr;
    }
    if (!next()) // skip '('
        return nullptr;
    if (!(statement->condition = parseDeclarationOrExpressionStatement(kEndConditionParanthesis)))
        return nullptr;
    if (!next())
        return nullptr;
    if (!(statement->body = parseStatement()))
        return nullptr;
    return statement;
}

astDeclarationStatement *parser::parseDeclarationStatement(endCondition condition, bool allow_undefined) {
    m_lexer.backup();

    bool isConst = false;
    if (isKeyword(kKeyword_const)) {
        isConst = true;
        if (!next()) // skip 'const'
            return nullptr;
    }

    astType *type = nullptr;
    if (isBuiltin()) {
        type = parseBuiltin();
    } else if (isType(kType_identifier)) {
        type = findType(m_token.asIdentifier);
    }

    if (!type) {
        m_lexer.restore();
        return nullptr;
    }

    if (!next())
        return nullptr;

    auto *statement = GC_NEW astDeclarationStatement();
    for (;;) {
        size_t paranthesisCount = 0;
        while (isOperator(kOperator_paranthesis_begin)) {
            paranthesisCount++;
            if (!next()) // skip ','
                return nullptr;
        }
        if (!isType(kType_identifier)) {
            m_lexer.restore();
            return nullptr;
        }

        const char *name = strnew(m_token.asIdentifier);
        if (!next()) // skip identifier
            return nullptr;

        for (size_t i = 0; i < paranthesisCount; i++) {
            if (!isOperator(kOperator_paranthesis_end)) {
                m_lexer.restore();
                return nullptr;
            }
            if (!next())
                return nullptr;
        }

        if (statement->variables.empty() && !isOperator(kOperator_assign)
            && !isOperator(kOperator_comma) && !isEndCondition(condition))
        {
            m_lexer.restore();
            return nullptr;
        }

        astExpression *initialValue = nullptr;
        if (isOperator(kOperator_assign)) {
            if (!next()) // skip '='
                return nullptr;
            if (!(initialValue = parseExpression(kEndConditionComma | condition, allow_undefined)))
                return nullptr;
        }

        auto *variable = GC_NEW astFunctionVariable();
        variable->isConst = isConst;
        variable->baseType = type;
        variable->name = strnew(name);
        variable->initialValue = initialValue;
        statement->variables.push_back(variable);
        m_scopes.back().push_back(variable);

        if (isEndCondition(condition)) {
            break;
        } else if (isOperator(kOperator_comma)) {
            if (!next()) // skip ','
                return nullptr;
        } else if (isOperator(kOperator_bracket_begin)) {
            while (isOperator(kOperator_bracket_begin)) {
                variable->isArray = true;
                astConstantExpression *arraySize = parseArraySize();
                if (!arraySize)
                    return nullptr;
                variable->arraySizes.push_back(arraySize);
                if (!next()) // skip ']'
                    return nullptr;
            }
        } else {
            fatal("syntax error during declaration statement");
            return nullptr;
        }
    }

    return statement;
}

astSimpleStatement *parser::parseDeclarationOrExpressionStatement(endCondition condition, bool allow_undefined) {
    astSimpleStatement *declaration = parseDeclarationStatement(condition, allow_undefined);
    if (declaration) {
        return declaration;
    } else {
        return parseExpressionStatement(condition, allow_undefined);
    }
}

astDefineStatement* parser::parseDefineDirective() {
    if (m_token.asDirective.type == directive::kDefine) {
        auto* define = GC_NEW astDefineStatement();
        define->name = strnew(m_token.asDirective.asDefine.name);

        // add the define to the scope
        m_defines.back().push_back(define);

        token token = m_lexer.peek(false, false);

        if (IS_TYPE(token, kType_end_of_line)) {
            return define;
        }

        if (!IS_TYPE(token, kType_whitespace)) {
            if (!next()) // skip '('
                return nullptr;
            while (!isOperator(kOperator_paranthesis_end)) {
                if (isType(kType_identifier)) {
                    define->parameters.push_back(strnew (m_token.asIdentifier));
                }
                if (!next())
                    return nullptr;
            }
        }

        if (!next(false)) {
            return nullptr;
        }

        define->value = parseExpression(kEndConditionLineFeed, true);

        if (!define->value) {
            return nullptr;
        }

        return define;
    } else {
        fatal("expected a define directive");
        return nullptr;
    }
}

bool parser::parseIfDirectiveVariations(astIfDirectiveStatement* out, bool is_in_root) {
    if (!next(false)) {
        return false;
    }

    out->value = parseExpression(kEndConditionLineFeed, true);

    if (!out->value) {
        return false;
    }

    while (!isType(kType_directive) ||
           (m_token.asDirective.type != directive::kEndIf &&
            m_token.asDirective.type != directive::kElse &&
            m_token.asDirective.type != directive::kElIf)) {
        if (isType(kType_end_of_line)) {
            m_lexer.read(m_token, true);
            continue;
        }

        // top level parsing has some issues as it doesn't accept statements
        vector<topLevel> items;
        if (is_in_root) {
            // directives have to be handled like in the root too
            if (isType(kType_directive)) {
                if (m_token.asDirective.type == directive::kVersion) {
                    if (m_ast->versionDirective) {
                        fatal("Multiple version directives not allowed");
                        return false;
                    }
                    auto *directive = GC_NEW astVersionDirective();
                    directive->version = m_token.asDirective.asVersion.version;
                    directive->type = m_token.asDirective.asVersion.type;
                    m_ast->versionDirective = directive;
                    m_ast->nodes.push_back(directive);
                    continue;
                } else if (m_token.asDirective.type == directive::kExtension) {
                    auto *extension = GC_NEW astExtensionDirective();
                    extension->behavior = m_token.asDirective.asExtension.behavior;
                    extension->name = strnew(m_token.asDirective.asExtension.name);
                    m_ast->extensionDirectives.push_back(extension);
                    m_ast->nodes.push_back(extension);
                    continue;
                } else if (m_token.asDirective.type == directive::kInclude) {
                    if (!parseIncludeDirective()) {
                        return false;
                    }
                    continue;
                } else if (m_token.asDirective.type == directive::kDefine) {
                    astDefineStatement* define = parseDefineDirective();

                    if (!define) {
                        return false;
                    }

                    m_ast->statements.push_back(define);
                    m_ast->nodes.push_back(define);
                    continue;
                } else if (m_token.asDirective.type == directive::kIfDef) {
                    astIfDefDirectiveStatement *ifdef = parseIfDefDirective();

                    if (!ifdef) {
                        return false;
                    }

                    m_ast->statements.push_back(ifdef);
                    m_ast->nodes.push_back(ifdef);
                    continue;
                } else if (m_token.asDirective.type == directive::kIfNDef) {
                    astIfNDefDirectiveStatement *ifndef = parseIfNDefDirective();

                    if (!ifndef) {
                        return false;
                    }

                    m_ast->statements.push_back(ifndef);
                    m_ast->nodes.push_back(ifndef);
                    continue;
                } else if (m_token.asDirective.type == directive::kIf) {
                    astIfDirectiveStatement *ifdef = parseIfDirective();

                    if (!ifdef) {
                        return false;
                    }

                    m_ast->statements.push_back(ifdef);
                    m_ast->nodes.push_back(ifdef);
                    continue;
                } else {
                    fatal("unexpected directive %d", m_token.asDirective.type);
                    return false;
                }
            }

            // top level parsing might fail here and it's okay?
            if (!parseTopLevel(items, &out->thenNodes, true)) {
                return false;
            }
        }

        if (isType(kType_semicolon)) {
            for (auto & parse : items) {
                auto *global = GC_NEW astGlobalVariable();
                global->storage = parse.storage;
                global->auxiliary = parse.auxiliary;
                global->memory = parse.memory;
                global->precision = parse.precision;
                global->interpolation = parse.interpolation;
                global->baseType = parse.type;
                global->name = strnew(parse.name);
                global->isInvariant = parse.isInvariant;
                global->isPrecise = parse.isPrecise;
                global->layoutQualifiers = parse.layoutQualifiers;
                if (parse.initialValue) {
                    if (!(global->initialValue = evaluate(parse.initialValue)))
                        return false;
                }
                global->isArray = parse.isArray;
                global->arraySizes = parse.arraySizes;
                out->thenNodes.push_back(global);
                m_scopes.back().push_back(global);
            }
        } else if (isOperator(kOperator_paranthesis_begin)) {
            astFunction *function = parseFunction(items.front());
            if (!function)
                return false;
            out->thenNodes.push_back(function);
        } else {
            astStatement* statement = parseStatement(true);
            if (!statement)
                return false;
            out->thenNodes.push_back(statement);
        }

        if (!next()) {
            return false;
        }
    }

    while (m_token.asDirective.type == directive::kElse || m_token.asDirective.type == directive::kElIf) {
        astStatement* conditionStatement = parseStatement();

        if (!conditionStatement) {
            fatal("cannot parse directive as statement");
            return false;
        }

        out->elseNodes.push_back(conditionStatement);

        if (!next()) {
            return false;
        }

        while (!isType(kType_directive) ||
               (m_token.asDirective.type != directive::kEndIf &&
                m_token.asDirective.type != directive::kElse &&
                m_token.asDirective.type != directive::kElIf)) {
            if (isType(kType_end_of_line)) {
                m_lexer.read(m_token, true);
                continue;
            }

            vector<topLevel> items;

            if (is_in_root) {
                // directives have to be handled like in the root too
                if (isType(kType_directive)) {
                    if (m_token.asDirective.type == directive::kVersion) {
                        if (m_ast->versionDirective) {
                            fatal("Multiple version directives not allowed");
                            return false;
                        }
                        auto *directive = GC_NEW astVersionDirective();
                        directive->version = m_token.asDirective.asVersion.version;
                        directive->type = m_token.asDirective.asVersion.type;
                        m_ast->versionDirective = directive;
                        m_ast->nodes.push_back(directive);
                        continue;
                    } else if (m_token.asDirective.type == directive::kExtension) {
                        auto *extension = GC_NEW astExtensionDirective();
                        extension->behavior = m_token.asDirective.asExtension.behavior;
                        extension->name = strnew(m_token.asDirective.asExtension.name);
                        m_ast->extensionDirectives.push_back(extension);
                        m_ast->nodes.push_back(extension);
                        continue;
                    } else if (m_token.asDirective.type == directive::kInclude) {
                        if (!parseIncludeDirective()) {
                            return false;
                        }
                        continue;
                    } else if (m_token.asDirective.type == directive::kDefine) {
                        astDefineStatement* define = parseDefineDirective();

                        if (!define) {
                            return false;
                        }

                        m_ast->statements.push_back(define);
                        m_ast->nodes.push_back(define);
                        continue;
                    } else if (m_token.asDirective.type == directive::kIfDef) {
                        astIfDefDirectiveStatement *ifdef = parseIfDefDirective();

                        if (!ifdef) {
                            return false;
                        }

                        m_ast->statements.push_back(ifdef);
                        m_ast->nodes.push_back(ifdef);
                        continue;
                    } else if (m_token.asDirective.type == directive::kIfNDef) {
                        astIfNDefDirectiveStatement *ifndef = parseIfNDefDirective();

                        if (!ifndef) {
                            return false;
                        }

                        m_ast->statements.push_back(ifndef);
                        m_ast->nodes.push_back(ifndef);
                        continue;
                    } else if (m_token.asDirective.type == directive::kIf) {
                        astIfDirectiveStatement *ifdef = parseIfDirective();

                        if (!ifdef) {
                            return false;
                        }

                        m_ast->statements.push_back(ifdef);
                        m_ast->nodes.push_back(ifdef);
                        continue;
                    } else {
                        fatal("unexpected directive %d", m_token.asDirective.type);
                        return false;
                    }
                }
                // top level parsing might fail here and it's okay?
                if (!parseTopLevel(items, &out->thenNodes)) {
                    return false;
                }
            }

            if (isType(kType_semicolon)) {
                for (auto & parse : items) {
                    auto *global = GC_NEW astGlobalVariable();
                    global->storage = parse.storage;
                    global->auxiliary = parse.auxiliary;
                    global->memory = parse.memory;
                    global->precision = parse.precision;
                    global->interpolation = parse.interpolation;
                    global->baseType = parse.type;
                    global->name = strnew(parse.name);
                    global->isInvariant = parse.isInvariant;
                    global->isPrecise = parse.isPrecise;
                    global->layoutQualifiers = parse.layoutQualifiers;
                    if (parse.initialValue) {
                        if (!(global->initialValue = evaluate(parse.initialValue)))
                            return false;
                    }
                    global->isArray = parse.isArray;
                    global->arraySizes = parse.arraySizes;
                    out->elseNodes.push_back(global);
                    m_scopes.back().push_back(global);
                }
            } else if (isOperator(kOperator_paranthesis_begin)) {
                astFunction *function = parseFunction(items.front());
                if (!function)
                    return false;
                out->elseNodes.push_back(function);
            } else {
                astStatement* statement = parseStatement(true);
                if (!statement)
                    return false;
                out->elseNodes.push_back(statement);
            }

            if (!next()) {
                return false;
            }
        }
    }

    if (m_token.asDirective.type != directive::kEndIf) {
        fatal("ending an ifdef with an unexpected directive, must be endif");
        return false;
    }

    if (!next(false, true, true)) {
        return false;
    }

    // always add it on the else nodes as this simplifies parsing things
    out->elseNodes.push_back(GC_NEW astEndIfDirectiveStatement());

    return true;
}

astIfDirectiveStatement* parser::parseIfDirective(bool is_in_root) {
    if (m_token.asDirective.type == directive::kIf) {
        auto* ifdef = GC_NEW astIfDirectiveStatement();

        if (!parseIfDirectiveVariations(ifdef, is_in_root)) {
            return nullptr;
        }

        return ifdef;
    } else {
        fatal("expected a if directive");
        return nullptr;
    }
}

astIfDefDirectiveStatement* parser::parseIfDefDirective(bool is_in_root) {
    if (m_token.asDirective.type == directive::kIfDef) {
        auto* ifdef = GC_NEW astIfDefDirectiveStatement();

        if (!parseIfDirectiveVariations(ifdef, is_in_root)) {
            return nullptr;
        }

        return ifdef;
    } else {
        fatal("expected a ifdef directive");
        return nullptr;
    }
}

astIfNDefDirectiveStatement* parser::parseIfNDefDirective(bool is_in_root) {
    if (m_token.asDirective.type == directive::kIfNDef) {
        auto* ifdef = GC_NEW astIfNDefDirectiveStatement();

        if (!parseIfDirectiveVariations(ifdef, is_in_root)) {
            return nullptr;
        }

        return ifdef;
    } else {
        fatal("expected a ifdef directive");
        return nullptr;
    }
}

bool parser::parseIncludeDirective() {
    if (!m_includeResolver) {
        fatal("found an include directive but no include resolver was set");
        return false;
    }

    const char* file = m_includeResolver->resolve(m_token.asDirective.asInclude.file);

    if (!file) {
        fatal("include file '%s' not found", m_token.asDirective.asInclude.file);
        return false;
    }

    location end = m_lexer.m_location;

    // go back to the beginning of the #include directive
    // should be the previous # character
    while (m_lexer.at() != '#') {
        m_lexer.m_location.position --;
    }

    location begin = m_lexer.m_location;

    // remove from the contents the include directive and add the new contents
    m_lexer.m_data.erase(begin.position, end.position);
    m_lexer.m_data.insert(begin.position, file, file + strlen(file));

    // the file contents have to be freed as the include resolver should be allocating memory for it
    free((void *) file);

    // update file length with the new length
    m_lexer.m_length = m_lexer.m_data.size();

    // file can be parsed as it was being parsed
    return true;
}

astStatement* parser::parseDirective() {
    if (m_token.asDirective.type == directive::kVersion) {
        fatal("version directive is only possible at the top level");
        return nullptr;
    } else if (m_token.asDirective.type == directive::kInclude) {
        if (!parseIncludeDirective()) {
            return nullptr;
        }

        return GC_NEW astEmptyStatement();
    } else if (m_token.asDirective.type == directive::kDefine) {
        return parseDefineDirective();
    } else if (m_token.asDirective.type == directive::kIf) {
        return parseIfDirective(false);
    } else if (m_token.asDirective.type == directive::kIfDef) {
        return parseIfDefDirective(false);
    } else if (m_token.asDirective.type == directive::kIfNDef) {
        return parseIfNDefDirective(false);
    } else if (m_token.asDirective.type == directive::kEndIf) {
        if (!next())
            return nullptr;

        return GC_NEW astEndIfDirectiveStatement();
    } else if (m_token.asDirective.type == directive::kElse) {
        return GC_NEW astElseDirectiveStatement();
    } else if (m_token.asDirective.type == directive::kElIf) {
        auto* elif = GC_NEW astElseDirectiveStatement();

        if (!next())
            return nullptr;

        elif->value = parseExpression(kEndConditionLineFeed, true);

        return elif;
    } else {
        fatal("rest of directives not implemented yet!");
        return nullptr;
    }
}

astStatement *parser::parseStatement(bool allow_undefined) {
    if (isType(kType_scope_begin)) {
        return parseCompoundStatement();
    } else if (isKeyword(kKeyword_if)) {
        return parseIfStatement();
    } else if (isKeyword(kKeyword_switch)) {
        return parseSwitchStatement();
    } else if (isKeyword(kKeyword_case) || isKeyword(kKeyword_default)) {
        return parseCaseLabelStatement();
    } else if (isKeyword(kKeyword_for)) {
        return parseForStatement();
    } else if (isKeyword(kKeyword_do)) {
        return parseDoStatement();
    } else if (isKeyword(kKeyword_while)) {
        return parseWhileStatement();
    } else if (isKeyword(kKeyword_continue)) {
        return parseContinueStatement();
    } else if (isKeyword(kKeyword_break)) {
        return parseBreakStatement();
    } else if (isKeyword(kKeyword_discard)) {
        return parseDiscardStatement();
    } else if (isKeyword(kKeyword_return)) {
        return parseReturnStatement();
    } else if (isType(kType_semicolon)) {
        return GC_NEW astEmptyStatement();
    } else if (isType(kType_directive)) {
        return parseDirective();
    } else {
        return parseDeclarationOrExpressionStatement(kEndConditionSemicolon, allow_undefined);
    }
}

astFunction *parser::parseFunction(const topLevel &parse) {
    auto *function = GC_NEW astFunction();
    function->returnType = parse.type;
    function->name = strnew(parse.name);

    if (!next()) // skip '('
        return nullptr;
    while (!isOperator(kOperator_paranthesis_end)) {
        auto *parameter = GC_NEW astFunctionParameter();
        while (!isOperator(kOperator_comma) && !isOperator(kOperator_paranthesis_end)) {
            if (isKeyword(kKeyword_in)) {
                parameter->storage = kIn;
            } else if (isKeyword(kKeyword_out)) {
                parameter->storage = kOut;
            } else if (isKeyword(kKeyword_inout)) {
                parameter->storage = kInOut;
            } else if (isKeyword(kKeyword_highp)) {
                parameter->precision = kHighp;
            } else if (isKeyword(kKeyword_mediump)) {
                parameter->precision = kMediump;
            } else if (isKeyword(kKeyword_lowp)) {
                parameter->precision = kLowp;
            } else if (isKeyword(kKeyword_coherent)) {
                parameter->memory = kCoherent;
            } else if (isKeyword(kKeyword_volatile)) {
                parameter->memory = kVolatile;
            } else if (isKeyword(kKeyword_restrict)) {
                parameter->memory = kRestrict;
            } else if (isKeyword(kKeyword_readonly)) {
                parameter->memory = kReadOnly;
            } else if (isKeyword(kKeyword_writeonly)) {
                parameter->memory = kWriteOnly;
            } else if (isType(kType_identifier)) {
                // TODO: user defined types
                parameter->name = strnew(m_token.asIdentifier);
            } else if (isOperator(kOperator_bracket_begin)) {
                while (isOperator(kOperator_bracket_begin)) {
                    parameter->isArray = true;
                    astConstantExpression *arraySize = parseArraySize();
                    if (!arraySize)
                        return nullptr;
                    parameter->arraySizes.push_back(arraySize);
                }
            } else {
                parameter->baseType = parseBuiltin();
                if (parameter->baseType && parameter->baseType->typeType == astType::kBuiltin) {
                    auto *builtin = (astBuiltin*)parameter->baseType;
                    if (builtin->type == kKeyword_void && !strnil(parameter->name)) {
                        fatal("`void' parameter cannot be named");
                        return nullptr;
                    }
                }
            }
            if (!next())
                return nullptr;
        }

        if (!parameter->baseType) {
            fatal("expected type");
            return nullptr;
        }
        function->parameters.push_back(parameter);
        if (isOperator(kOperator_comma)) {
            if (!next())// skip ','
                return nullptr;
        }
    }
    if (!next()) // skip ')'
        return nullptr;

    // If there is just one 'void' than silently drop it
    if (function->parameters.size() == 1) {
        if (function->parameters[0]->baseType->typeType == astType::kBuiltin) {
            auto *builtin = (astBuiltin*)function->parameters[0]->baseType;
            if (builtin->type == kKeyword_void)
                function->parameters.pop_back();
        }
    }

    // "It is a compile-time or link-time error to declare or define a function main with any other parameters or
    //  return type."
    if (!strcmp(function->name, "main")) {
        if (!function->parameters.empty()) {
            fatal("`main' cannot have parameters");
            return nullptr;
        }
        if (function->returnType->typeType != astType::kBuiltin || ((astBuiltin*)function->returnType)->type != kKeyword_void) {
            fatal("`main' must be declared to return void");
            return nullptr;
        }
    }

    if (isType(kType_scope_begin)) {
        function->isPrototype = false;
        if (!next()) // skip '{'
            return nullptr;

        m_scopes.push_back(scope());
        m_defines.push_back(defineScope());
        for (auto & parameter : function->parameters)
            m_scopes.back().push_back(parameter);
        while (!isType(kType_scope_end)) {
            astStatement *statement = parseStatement();
            if (!statement)
                return nullptr;
            function->statements.push_back(statement);
            if (!next())// skip ';'
                return nullptr;
        }

        m_scopes.pop_back();
        m_defines.pop_back();
    } else if (isType(kType_semicolon)) {
        function->isPrototype = true;
    } else {
        fatal("expected `{' or `;'");
        return nullptr;
    }
    return function;
}

// TODO: cleanup
#undef TYPENAME
#define TYPENAME(X) case kKeyword_##X:
astBuiltin *parser::parseBuiltin() {
    if (!isType(kType_keyword)) {
        fatal("expected keyword");
        return nullptr;
    }

    switch (m_token.asKeyword) {
        #include "lexemes.h"
            for (auto & m_builtin : m_builtins) {
                if (m_builtin->type == m_token.asKeyword) {
                    return m_builtin;
                }
            }
            m_builtins.push_back(GC_NEW astBuiltin(m_token.asKeyword));
            return m_builtins.back();
            break;
        default:
            break;
    }
    
    fatal("internal compiler error: attempted to parse as builtin type");
    return nullptr;
}
#undef TYPENAME

astConstructorCall *parser::parseConstructorCall(bool allow_undefined) {
    auto *expression = GC_NEW astConstructorCall();
    if (!(expression->type = parseBuiltin()))
        return nullptr;
    if (!next())
        return nullptr;
    if (!isOperator(kOperator_paranthesis_begin)) {
        fatal("expected `(' for constructor call");
        return nullptr;
    }
    if (!next()) // skip '('
        return nullptr;
    while (!isOperator(kOperator_paranthesis_end)) {
        astExpression *parameter = parseExpression(kEndConditionComma | kEndConditionParanthesis, allow_undefined);
        if (!parameter)
            return nullptr;
        expression->parameters.push_back(parameter);
        if (isOperator(kOperator_comma)) {
            if (!next()) // skip ','
                return nullptr;
        }
    }
    return expression;
}

astFunctionCall *parser::parseFunctionCall(bool allow_undefined) {
    auto *expression = GC_NEW astFunctionCall();
    expression->name = strnew(m_token.asIdentifier);
    if (!next()) // skip identifier
        return nullptr;
    if (!isOperator(kOperator_paranthesis_begin)) {
        fatal("expected `(' for function call");
        return nullptr;
    }
    if (!next()) return nullptr; // skip '('
    while (!isOperator(kOperator_paranthesis_end)) {
        astExpression *parameter = parseExpression(kEndConditionComma | kEndConditionParanthesis, allow_undefined);
        if (!parameter)
            return nullptr;
        expression->parameters.push_back(parameter);
        if (isOperator(kOperator_comma)) {
            if (!next()) // skip ','
                return nullptr;
        }
    }
    return expression;
}

bool parser::next(bool ignore_eol, bool ignore_whitespace, bool eof_valid) {
    m_lexer.read(m_token, true, ignore_eol, ignore_whitespace);
    if (!eof_valid && isType(kType_eof)) {
        fatal("premature end of file");
        return false;
    }
    if (m_lexer.error()) {
        fatal("%s", m_lexer.error());
        return false;
    }
    return true;
}

astBinaryExpression *parser::createExpression() {
    if (!isType(kType_operator)) {
        fatal("internal compiler error: attempted to create binary expression in wrong context");
        return nullptr;
    }

    switch (m_token.asOperator) {
        case kOperator_multiply:
        case kOperator_divide:
        case kOperator_modulus:
        case kOperator_plus:
        case kOperator_minus:
        case kOperator_shift_left:
        case kOperator_shift_right:
        case kOperator_less:
        case kOperator_greater:
        case kOperator_less_equal:
        case kOperator_greater_equal:
        case kOperator_equal:
        case kOperator_not_equal:
        case kOperator_bit_and:
        case kOperator_bit_xor:
        case kOperator_logical_and:
        case kOperator_logical_xor:
        case kOperator_logical_or:
            return GC_NEW astOperationExpression(m_token.asOperator);
        case kOperator_assign:
        case kOperator_add_assign:
        case kOperator_sub_assign:
        case kOperator_multiply_assign:
        case kOperator_divide_assign:
        case kOperator_modulus_assign:
        case kOperator_shift_left_assign:
        case kOperator_shift_right_assign:
        case kOperator_bit_and_assign:
        case kOperator_bit_xor_assign:
        case kOperator_bit_or_assign:
            return GC_NEW astAssignmentExpression(m_token.asOperator);
        case kOperator_comma:
            return GC_NEW astSequenceExpression();
        default:
            return nullptr;
    }
}

astType *parser::findType(const char *identifier) {
    for (auto & structure : m_ast->structures) {
        if (strcmp(structure->name, identifier) != 0)
            continue;
        return (astType*)structure;
    }
    return nullptr;
}

astVariable *parser::findVariable(const char *identifier) {
    for (size_t scopeIndex = m_scopes.size(); scopeIndex > 0; scopeIndex--) {
        scope &s = m_scopes[scopeIndex - 1];
        for (auto & variableIndex : s) {
            if (!strcmp(variableIndex->name, identifier))
                return variableIndex;
        }
    }
    return nullptr;
}

astDefineStatement *parser::findDefine(const char* define) {
    for (size_t scopeIndex = m_defines.size(); scopeIndex > 0; scopeIndex--) {
        vector<astDefineStatement*> &s = m_defines[scopeIndex - 1];
        for (auto & variableIndex : s) {
            if (!strcmp(variableIndex->name, define))
                return variableIndex;
        }
    }
    return nullptr;
}

const char *parser::error() const {
    return m_error;
}

}
