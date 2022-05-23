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
    Bracketed,
    Sequence,
    VarDef,
    FunDef,
    Program
};

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
        None,
        DefName,
        DefParamList,
        DefParam,
        DefParamSep,
        DefAssign
    };

    DefState defState = None;
    std::vector<ASTNode> nodeStack;

    nodeStack.emplace_back(Program);

    for (const Token& token : tokenStream.tokens)
    {
        if (defState != None)
        {
            if (defState == DefName)
            {
                errorAssert(token.type == Name, "Parse", "Expected name in definition", token.line);
                nodeStack.back().name = token.name;
                defState = nodeStack.back().type == FunDef ? DefParamList : DefAssign;
            }
            else if (defState == DefParamList)
            {
                errorAssert(token.type == LBrack, "Parse", "Expected start of parameter list in definition", token.line);
                defState = DefParam;
            }
            else if (defState == DefParam)
            {
                if (nodeStack.back().argNames.empty())
                    errorAssert(token.type == Comma || token.type == RBrack, "Parse", "Expected separator or end of parameter list in defition", token.line);
                else errorAssert(token.type == Comma, "Parse", "Expected separator in defition", token.line);
                if (token.type == Name)
                {
                    nodeStack.back().argNames.push_back(token.name);
                    defState = DefParamSep;
                }
                else if (token.type == RBrack) defState = DefAssign;
            }
            else if (defState == DefParamSep)
            {
                errorAssert(token.type == Comma || token.type == RBrack, "Parse", "Expected separator or end of parameter list in defition", token.line);
                if (token.type == Comma) defState = DefParam;
                else if (token.type == RBrack) defState = DefAssign;
            }
            else if (defState == DefAssign)
            {
                errorAssert(token.type == Assign, "Parse", "Expected assignment in defition", token.line);
                defState = None;
            }
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
            // TODO: Match until < priority Operator iif Left associative, or <= priority Operaotr iff Right associative
            break;

        case Assign:
        {
            errorAssert(nodeStack.back().type == GetVar, "Parse", "LHS of assignment needs to be a variable", token.line);
            std::string name = nodeStack.back().name;
            nodeStack.pop_back();
            nodeStack.emplace_back(Assignment, name);
            break;
        }

        case Semicol:
            // TODO: Match until Assignment or whole expression
            break;

        case Comma:
            // TODO: Match until Application
            break;

        case LBrack:
            if (nodeStack.back().type == GetVar)
            {
                std::string name = nodeStack.back().name;
                nodeStack.pop_back();
                nodeStack.emplace_back(Application, name);
            }
            else nodeStack.emplace_back(Bracketed);
            break;

        case RBrack:
            // TODO: Match until Bracketed or Application (pass through expression if children > 0)
            break;

        case If:
            nodeStack.emplace_back(IfThenElse);
            break;

        case Then:
            // TODO: Match until IfThenElse (children % 2 == 1)
            break;

        case Elif:
            // TODO: Match until IfThenElse (children % 2 == 0 && children >= 2 -- maybe insert empty Sequence)
            break;

        case Else:
            // TODO: Match until IfThenElse (children % 2 == 0 && children >= 2 -- maybe insert empty Sequence)
            break;

        case While:
            nodeStack.emplace_back(WhileDo);
            break;

        case Do:
            // TODO: Match until WhileDo (children == 1)
            break;

        case End:
            // TODO: Match unti WhileDo (children == 2) or IfThenElse (children % 2 == 1 && children >= 3 -- maybe insert empty Sequences)
            break;

        case Var:
            nodeStack.emplace_back(VarDef);
            defState = DefName;
            break;

        case Fun:
            nodeStack.emplace_back(FunDef);
            defState = DefName;
            break;
        }
    }

    return ASTNode(ConstNumber, 0);
}

int main()
{
    TokenStream tokenStream = lexProgram(std::cin);
    tokenStream.print();

    ASTNode astRoot = parseProgram(tokenStream);

    return 0;
}
