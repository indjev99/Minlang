#include <iostream>
#include <string>
#include <vector>
#include <cassert>
#include <unordered_map>

void errorAssert(bool cond, const std::string& type, const std::string& descr, int line)
{
    if (cond) return;
    std::cerr << type << " errorAssert at line " << line << ": " << descr << std::endl;
    exit(0);
}

enum Operation
{
    ArAdd,
    ArSub,
    ArMul,
    ArDiv,
    ArMod,
    CmpEq,
    CmpNeq,
    CmpLt,
    CmpGt,
    CmpLeq,
    CmpGeq,
    LogAnd,
    LogOr,
    LogNot,
    BitAnd,
    BitOr,
    BitXor,
    BitNot,
    BitLShift,
    BitRShift
};

const std::unordered_map<std::string, Operation> operationMap = {
    {"+", ArAdd},
    {"-", ArSub},
    {"*", ArMul},
    {"/", ArDiv},
    {"%", ArMod},
    {"==", CmpEq},
    {"!=", CmpNeq},
    {"<", CmpLt},
    {">", CmpGt},
    {"<=", CmpLeq},
    {">=", CmpGeq},
    {"&&", LogAnd},
    {"||", LogOr},
    {"!", LogNot},
    {"&", BitAnd},
    {"|", BitOr},
    {"^", BitXor},
    {"~", BitNot},
    {"<<", BitLShift},
    {">>", BitRShift}
};

const std::unordered_map<Operation, std::string> operationStrings = {
    {ArAdd , "+"},
    {ArSub , "-"},
    {ArMul , "*"},
    {ArDiv , "/"},
    {ArMod , "%"},
    {CmpEq , "=="},
    {CmpNeq, "!="},
    {CmpLt, "<"},
    {CmpGt, ">"},
    {CmpLeq, "<="},
    {CmpGeq, ">="},
    {LogAnd, "&&"},
    {LogOr, "||"},
    {LogNot, "!"},
    {BitAnd, "&"},
    {BitOr, "|"},
    {BitXor, "^"},
    {BitNot, "~"},
    {BitLShift, "<<"},
    {BitRShift, ">>"}
};

enum TokenType
{
    Name,
    Number,
    Operator,
    Assign,
    Semicol,
    Comma,
    LBrack,
    RBrack,
    If,
    Then,
    Elif,
    Else,
    While,
    Do,
    End,
    Var,
    Fun
};

const std::unordered_map<std::string, TokenType> tokenMap = {
    {"=", Assign},
    {";", Semicol},
    {",", Comma},
    {"(", LBrack},
    {")", RBrack},
    {"if", If},
    {"then", Then},
    {"elif", Elif},
    {"else", Else},
    {"while", While},
    {"do", Do},
    {"end", End},
    {"var", Var},
    {"fun", Fun}
};

const std::unordered_map<TokenType, std::string> tokenStrings = {
    {Assign, "="},
    {Semicol, ";"},
    {Comma, ","},
    {LBrack, "("},
    {RBrack, ")"},
    {If, "if"},
    {Then, "then"},
    {Elif, "elif"},
    {Else, "else"},
    {While, "while"},
    {Do, "do"},
    {End, "end"},
    {Var, "var"},
    {Fun, "fun"}
};

struct Token
{
    TokenType type;
    std::string name;
    long long number;
    Operation operation;
    int line;

    Token(const std::string& str, int line)
        : line(line)
    {
        assert(!str.empty());

        auto it = tokenMap.find(str);
        if (it != tokenMap.end()) type = it->second;
        else if (isalpha(str[0]))
        {
            type = Name;
            name = str;
        }
        else if (isdigit(str[0]))
        {
            type = Number;
            number = stoi(str);
        }
        else
        {
            auto it = operationMap.find(str);
            errorAssert(it != operationMap.end(), "Lex", "Invalid operator", line);

            type = Operator;
            operation = it->second;
        }
    }

    void print() const
    {
        switch (type)
        {
        case Name:
            std::cerr << name;
            break;
        case Number:
            std::cerr << number;
            break;
        case Operator:
            std::cerr << operationStrings.at(operation);
            break;
        default:
            std::cerr << tokenStrings.at(type);
            break;
        }
    }
};

struct TokenStream
{
    std::vector<Token> tokens;

    void print() const
    {
        int prevLine = 0;
        for (const Token& token : tokens)
        {
            if (token.line > prevLine) std::cerr << "\n";
            prevLine = token.line;
            token.print();
            std::cerr << " ";
        }
        std::cerr << "\n";
    }
};

void lexOperators(TokenStream& tokenStream, const std::string& str, int line)
{
    size_t prefLen;
    for (prefLen = str.size(); prefLen >= 1; --prefLen)
    {
        if (operationMap.find(str.substr(0, prefLen)) != operationMap.end()) break;
    }

    errorAssert(prefLen > 0, "Lex", "Invalid operator", line);

    tokenStream.tokens.emplace_back(str.substr(0, prefLen), line);
    lexOperators(tokenStream, str.substr(prefLen, str.size() - prefLen), line);
}

TokenStream lexProgram(std::istream& in)
{
    enum SymbolSet
    {
        ANY,
        WORD,
        NUMBER,
        OPERATOR,
        SPACE,
    };

    int line = 0;
    std::string curr = "";
    SymbolSet currSymbSet = ANY;

    TokenStream tokenStream;

    while (in.good())
    {
        char c = in.get();

        SymbolSet cSet;
        if (isspace(c)) cSet = SPACE;
        else if (c == '_' || isalpha(c)) cSet = WORD; 
        else if (isdigit(c)) cSet = currSymbSet == WORD ? WORD : NUMBER;
        else cSet = OPERATOR;

        errorAssert(cSet != WORD || currSymbSet == NUMBER, "Lex", "Invalid digit", line);

        if (cSet != currSymbSet && curr != "")
        {
            tokenStream.tokens.emplace_back(curr, line);
            curr = "";
            currSymbSet = ANY;
        }

        if (cSet != SPACE)
        {
            curr += c;
            currSymbSet = cSet;
        }

        if (c == '\n') ++line;
    }

    return tokenStream;
}

enum ASTNodeType
{
    GetVar,
    ConstNumber,
    BinOperator,
    UnOperator,
    Application,
    Assignment,
    IfThenElse,
    WhileDo,
    Sequence,
    VarDef,
    FunDef
};

bool isExpr(ASTNodeType type)
{
    return type == GetVar || type == ConstNumber || type == BinOperator || type == UnOperator || type == Application;
}

bool isStmt(ASTNodeType type)
{
    return type == Assignment || type == IfThenElse || type == WhileDo;
}

bool isDef(ASTNodeType type)
{
    return type == VarDef || type == FunDef;
}

struct ASTNode
{
    ASTNodeType type;

    std::string name;
    long long number;
    Operation operation;
    std::vector<std::string> argNames;

    std::vector<ASTNode> children;

    ASTNode(ASTNodeType type)
        : type(type)
    {}

    ASTNode(ASTNodeType type, const std::string& name)
        : type(type)
        , name(name)
    {}

    ASTNode(ASTNodeType type, long long number)
        : type(type)
        , number(number)
    {}

    ASTNode(ASTNodeType type, Operation operation)
        : type(type)
        , operation(operation)
    {}
};

ASTNode parseProgram(const TokenStream& tokenStream)
{
    enum DefState
    {
        ExpectDefName,
        ExpectDefBrack,
        ExepctDefParam,
        ExepctDefSep
    };

    DefState = false;
    std::vector<ASTNode> nodeStack;

    for (size_t i = 0; i < tokenStream.tokens.size(); ++i)
    {
        const Token& token = tokenStream.tokens[i];

        if (defInProgress)
        {
            if (token.type == Name)
            {
                if (nodeStack.back().type == VarDef)
                {
                    errorAssert(nodeStack.back().name == "", "Parse", "Multiple names in var definition", token.line);
                    nodeStack.back().name = token.name;
                }
                else if (nodeStack.back().type == FunDef)
                {
                    if (nodeStack.back().name == "") nodeStack.back().name = token.name;
                    else nodeStack.back().argNames.push_back(token.name);
                }
            }
            else if (token.type == Assign)
            {
                defInProgress = false;
                if (nodeStack.back().type == VarDef) nodeStack.emplace_back(Assignment, nodeStack.back().name);
            }
            else errorAssert(false, "Parse", "Invalid token in definition", token.line);

            continue;
        }

        switch (token.type)
        {
        case Name:
            nodeStack.emplace_back(GetVar, token.name);
            break;

        case Number:
            nodeStack.emplace_back(ConstNumber, token.number);
            break;

        case Operator:
            // TODO:
            break;

        case Assign:
            // TODO:
            break;

        case Semicol:
            // TODO:
            break;

        case LBrack:
            // TODO:
            break;

        case RBrack:
            // TODO:
            break;

        case If:
            // TODO:
            break;

        case Then:
            // TODO:
            break;

        case Elif:
            // TODO:
            break;

        case Else:
            // TODO:
            break;

        case While:
            // TODO:
            break;

        case Do:
            // TODO:
            break;

        case End:
            // TODO:
            break;

        case Var:
            nodeStack.emplace_back(VarDef);
            defInProgress = true;
            break;

        case Fun:
            nodeStack.emplace_back(FunDef);
            defInProgress = true;
            break;
        }
    }

    return ASTNode(ConstNumber, 0);
}

int main()
{
    TokenStream tokenStream = lexProgram(std::cin);

    tokenStream.print();

    return 0;
}
