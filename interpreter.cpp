#include <iostream>
#include <string>
#include <vector>
#include <cassert>
#include <unordered_map>

void error(const std::string& type, int line)
{
    std::cerr << type << " error at line " << line << "." << std::endl;
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
            if (it == operationMap.end()) error("Lex", line);

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
            std::cerr << operationStrings.find(operation)->second;
            break;
        default:
            std::cerr << tokenStrings.find(type)->second;
            break;
        }
    }
};

struct TokenStream
{
    std::vector<Token> tokens;

    void print()
    {
        int prevLine = 0;
        for (const auto& token : tokens)
        {
            if (token.line > prevLine) std::cerr << "\n";
            prevLine = token.line;
            token.print();
            std::cerr << " ";
        }
        std::cerr << "\n";
    }
};

TokenStream lexProgram(std::istream& in)
{
    enum SymbolSet
    {
        ANY,
        WORD,
        NUMBER,
        OPERATOR,
        SINGLE,
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
        else if (c == '(' || c == ')' || c == ';') cSet = SINGLE; 
        else if (c == '_' || isalpha(c)) cSet = WORD; 
        else if (isdigit(c)) cSet = currSymbSet == WORD ? WORD : NUMBER;
        else cSet = OPERATOR;

        if (cSet == WORD && currSymbSet == NUMBER)
        {
            error("Lex", line);
        }

        if (cSet != currSymbSet && curr != "")
        {
            tokenStream.tokens.emplace_back(curr, line);
            curr = "";
            currSymbSet = ANY;
        }

        if (cSet == SINGLE)
        {
            tokenStream.tokens.emplace_back(std::string(1, c), line);
        }
        else if (cSet != SPACE)
        {
            curr += c;
            currSymbSet = cSet;
        }

        if (c == '\n') ++line;
    }

    return tokenStream;
}

int main()
{
    TokenStream tokenStream = lexProgram(std::cin);

    tokenStream.print();

    return 0;
}
