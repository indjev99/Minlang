#include <algorithm>
#include <cassert>
#include <iostream>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

void errorAssert(bool cond, const std::string& type, const std::string& descr, int line)
{
    if (cond) return;
    std::cerr << type << " error at line " << line << ": " << descr << std::endl;
    exit(0);
}

enum Operator
{
    UnPlus,
    UnMinus,
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

const std::unordered_map<std::string, Operator> operatorMap = {
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

const std::unordered_map<Operator, std::string> operatorStrings = {
    {UnPlus , "+"},
    {UnMinus , "-"},
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

const std::unordered_map<Operator, int> operatorPrec = {
    {UnPlus, 0},
    {UnMinus, 0},
    {ArAdd, 9},
    {ArSub, 9},
    {ArMul, 10},
    {ArDiv, 10},
    {ArMod, 10},
    {CmpEq, 6},
    {CmpNeq, 6},
    {CmpLt, 7},
    {CmpGt, 7},
    {CmpLeq, 7},
    {CmpGeq, 7},
    {LogAnd, 2},
    {LogOr, 1},
    {LogNot, 0},
    {BitAnd, 5},
    {BitOr, 3},
    {BitXor, 4},
    {BitNot, 0},
    {BitLShift, 8},
    {BitRShift, 8}
};

const std::unordered_set<Operator> binOperators = {
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
    BitAnd,
    BitOr,
    BitXor,
    BitLShift,
    BitRShift
};

const std::unordered_map<Operator, Operator> unOperators = {
    {ArAdd, LogNot},
    {ArSub, BitNot},
    {LogNot, LogNot},
    {BitNot, BitNot}
};

enum TokenType
{
    Name,
    Number,
    Op,
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
    Return,
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
    {"return", Return},
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
    {Return, "return"},
    {Var, "var"},
    {Fun, "fun"}
};

struct Token
{
    TokenType type;
    std::string name;
    long long number;
    Operator oper;
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
            auto it = operatorMap.find(str);
            errorAssert(it != operatorMap.end(), "Lex", "Invalid oper", line);

            type = Op;
            oper = it->second;
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
        case Op:
            std::cerr << operatorStrings.at(oper);
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
        if (operatorMap.find(str.substr(0, prefLen)) != operatorMap.end()) break;
    }

    errorAssert(prefLen > 0, "Lex", "Invalid oper", line);

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
    Bracketed,
    IgnoreValue,
    Assignment,
    IfThenElse,
    WhileDo,
    ReturnVal,
    Sequence,
    VarDef,
    FunDef,
    Program
};

bool isExpr(ASTNodeType type)
{
    return type == GetVar || type == ConstNumber || type == BinOperator || type == UnOperator || type == Application || type == Bracketed;
}

struct ASTNode
{
    ASTNodeType type;

    std::string name;
    long long number;
    Operator oper;
    std::vector<std::string> argNames;

    std::vector<ASTNode> children;
    int remaining;

    ASTNode(ASTNodeType type, int remaining)
        : type(type)
        , remaining(remaining)
    {}

    ASTNode(ASTNodeType type, const std::string& name, int remaining)
        : type(type)
        , name(name)
        , remaining(remaining)
    {}

    ASTNode(ASTNodeType type, long long number, int remaining)
        : type(type)
        , number(number)
        , remaining(remaining)
    {}

    ASTNode(ASTNodeType type, Operator oper, int remaining)
        : type(type)
        , oper(oper)
        , remaining(remaining)
    {}

    void addChild(const ASTNode& child)
    {
        children.push_back(child);
        --remaining;
    }

    void addChild(ASTNode&& child)
    {
        children.emplace_back(std::move(child));
        --remaining;
    }

    bool isExpr() const
    {
        return ::isExpr(type);
    }

    bool isComplete() const
    {
        return remaining == 0;
    }
};

void popSequence(std::vector<ASTNode>& nodeStack, int line)
{
    ASTNode seq(Sequence, -1);
    while (nodeStack.back().isComplete())
    {
        errorAssert(!nodeStack.back().isExpr(), "Parse", "Unexpected complete expression in sequence", line);
        seq.addChild(std::move(nodeStack.back()));
        nodeStack.pop_back();
    }

    seq.remaining = 0;

    std::reverse(seq.children.begin(), seq.children.end());
    if (seq.children.size() == 1)
    {
        seq = std::move(seq.children.back());
        seq.children.clear();
    }

    nodeStack.back().addChild(std::move(seq));
}

ASTNode popOperators(std::vector<ASTNode>& nodeStack, int prec, int line)
{
    ASTNode right = std::move(nodeStack.back());
    nodeStack.pop_back();

    errorAssert(right.isComplete() && right.isExpr(), "Parse", "Incplete expression", line);

    while (nodeStack.back().type == BinOperator || nodeStack.back().type == UnOperator)
    {
        errorAssert(!nodeStack.back().isComplete(), "Parse", "Unexpected complete expression", line);
        nodeStack.back().addChild(std::move(right));
        right = std::move(nodeStack.back());
        nodeStack.pop_back();
    }

    assert(right.isComplete() && right.isExpr());
    return right;
}

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

    nodeStack.emplace_back(Program, -1);

    for (const Token& token : tokenStream.tokens)
    {
        int line = token.line;
        ASTNode& last = nodeStack.back();

        if (defState != None)
        {
            if (defState == DefName)
            {
                errorAssert(token.type == Name, "Parse", "Expected name in definition", line);
                last.name = token.name;
                defState = last.type == FunDef ? DefParamList : DefAssign;
            }
            else if (defState == DefParamList)
            {
                errorAssert(token.type == LBrack, "Parse", "Expected opening bracket in definition", line);
                defState = DefParam;
            }
            else if (defState == DefParam)
            {
                if (last.argNames.empty())
                    errorAssert(token.type == Name || token.type == RBrack, "Parse", "Expected parameter name or right bracket list in defition", line);
                else errorAssert(token.type == Name, "Parse", "Expected parameter name in defition", line);
                if (token.type == Name)
                {
                    last.argNames.push_back(token.name);
                    defState = DefParamSep;
                }
                else if (token.type == RBrack) defState = DefAssign;
            }
            else if (defState == DefParamSep)
            {
                errorAssert(token.type == Comma || token.type == RBrack, "Parse", "Expected comma or right bracket list in defition", line);
                if (token.type == Comma) defState = DefParam;
                else if (token.type == RBrack) defState = DefAssign;
            }
            else if (defState == DefAssign)
            {
                errorAssert(token.type == Assign, "Parse", "Expected assignment in defition", line);
                defState = None;
            }
            continue;   
        }

        switch (token.type)
        {
        case Name:
            errorAssert(last.isComplete() && last.isExpr(), "Parse", "Unexpected complete expression before name", line);
            nodeStack.emplace_back(GetVar, token.name, 0);
            break;

        case Number:
            errorAssert(last.isComplete() && last.isExpr(), "Parse", "Unexpected complete expression before number", line);
            nodeStack.emplace_back(ConstNumber, token.number, 0);
            break;

        case Op:
        {
            auto itBin = binOperators.find(token.oper);
            auto itUn = unOperators.find(token.oper);
            bool okBin = last.isComplete() && last.isExpr();
            bool okUn = !okBin;
            errorAssert(itUn != unOperators.end() || okBin, "Parse", "Expected complete expression before binary operator", line);
            errorAssert(itBin != binOperators.end() || okUn, "Parse", "Unexpected complete expression before binary operator", line);
            if (itUn != unOperators.end() && okUn) nodeStack.emplace_back(UnOperator, itUn->second, 1);
            else
            {
                ASTNode left = popOperators(nodeStack, operatorPrec.at(token.oper), line);
                nodeStack.emplace_back(BinOperator, token.oper, 2);
                nodeStack.back().addChild(std::move(left));
            }
            break;
        }

        case Assign:
        {
            errorAssert(last.type == GetVar, "Parse", "LHS of assignment needs to be a variable", line);
            std::string name = last.name;
            nodeStack.pop_back();
            nodeStack.emplace_back(Assignment, name, 1);
            break;
        }

        case Semicol:
        {
            ASTNode expr = popOperators(nodeStack, 0, line);
            ASTNode& last = nodeStack.back();
            errorAssert(
                (!last.isComplete() && (last.type == Assignment || last.type == ReturnVal)) || (last.isComplete() && !last.isExpr()),
                "Parse", "Unexpected semicolon", line
            );
            if (last.isComplete()) last.addChild(std::move(expr));
            else
            {
                nodeStack.emplace_back(IgnoreValue, 1);
                nodeStack.back().addChild(std::move(expr));
            }
            break;
        }

        case Comma:
        {
            ASTNode expr = popOperators(nodeStack, 0, line);
            ASTNode& last = nodeStack.back();
            errorAssert(!last.isComplete() && last.type == Application, "Parse", "Unexpected comma", line);
            last.addChild(std::move(expr));
            break;
        }

        case LBrack:
            if (last.type == GetVar)
            {
                std::string name = last.name;
                nodeStack.pop_back();
                nodeStack.emplace_back(Application, name, -1);
            }
            else
            {
                errorAssert(last.isComplete() && last.isExpr(), "Parse", "Unexpected complete expression before opening bracket", line);
                nodeStack.emplace_back(Bracketed, 1);
            }
            break;

        case RBrack:
        {
            ASTNode expr = popOperators(nodeStack, 0, line);
            ASTNode& last = nodeStack.back();
            errorAssert(!last.isComplete() && (last.type == Bracketed || last.type == Application), "Parse", "Unmatched closing bracket", line);
            if (last.type == Bracketed)
            {
                nodeStack.pop_back();
                nodeStack.emplace_back(std::move(expr));
            }
            else
            {
                last.addChild(std::move(expr));
                last.remaining = 0;
            }
            break;
        }

        case If:
            nodeStack.emplace_back(IfThenElse, 3);
            break;

        case Then:
        {
            ASTNode expr = popOperators(nodeStack, 0, line);
            ASTNode& last = nodeStack.back();
            errorAssert(last.remaining == 3 && last.type == IfThenElse, "Parse", "Unexpected then", line);
            last.addChild(std::move(expr));
            break;
        }

        case Elif:
        {
            popSequence(nodeStack, line);
            ASTNode& last = nodeStack.back();
            errorAssert(last.remaining == 1 && last.type == IfThenElse, "Parse", "Unexpected elif", line);
            last.remaining += 2;
            break;
        }

        case Else:
        {
            popSequence(nodeStack, line);
            ASTNode& last = nodeStack.back();
            errorAssert(last.remaining == 1 && last.type == IfThenElse, "Parse", "Unexpected else", line);
            break;
        }

        case While:
            nodeStack.emplace_back(WhileDo, 2);
            break;

        case Do:
        {
            ASTNode expr = popOperators(nodeStack, 0, line);
            ASTNode& last = nodeStack.back();
            errorAssert(last.remaining == 2 && last.type == WhileDo, "Parse", "Unexpected do", line);
            last.addChild(std::move(expr));
            break;
        }

        case End:
        {
            popSequence(nodeStack, line);
            ASTNode& last = nodeStack.back();
            errorAssert(last.isComplete() && (last.type == IfThenElse || last.type == WhileDo || last.type == FunDef), "Parse", "Unexpected end", line);
            break;
        }

        case Return:
            nodeStack.emplace_back(ReturnVal, 1);
            break;

        case Var:
            nodeStack.emplace_back(VarDef, 0);
            defState = DefName;
            break;

        case Fun:
            nodeStack.emplace_back(FunDef, 1);
            defState = DefName;
            break;
        }
    }

    return ASTNode(ConstNumber, 0, 0);
}

int main()
{
    TokenStream tokenStream = lexProgram(std::cin);
    tokenStream.print();

    ASTNode astRoot = parseProgram(tokenStream);

    return 0;
}
