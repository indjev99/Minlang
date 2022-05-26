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

typedef long long ValueT;

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
    {UnPlus, 20},
    {UnMinus, 20},
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
    {LogNot, 20},
    {BitAnd, 5},
    {BitOr, 3},
    {BitXor, 4},
    {BitNot, 20},
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
    {ArAdd, UnPlus},
    {ArSub, UnMinus},
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
    Fun,
    Eof
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
    {Fun, "fun"},
    {Eof, ""}
};

struct Token
{
    TokenType type;
    std::string name;
    ValueT number;
    Operator oper;
    int line;

    Token(TokenType type, int line)
        : type(type)
        , line(line)
    {}

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
            number = stoll(str);
        }
        else
        {
            auto it = operatorMap.find(str);
            assert(it != operatorMap.end());
            type = Op;
            oper = it->second;
        }
    }

    void print(std::ostream& out) const
    {
        switch (type)
        {
        case Name:
            out << name;
            break;
        case Number:
            out << number;
            break;
        case Op:
            out << operatorStrings.at(oper);
            break;
        default:
            out << tokenStrings.at(type);
            break;
        }
    }
};

struct TokenStream
{
    std::vector<Token> tokens;

    void print(std::ostream& out) const
    {
        int prevLine = 1;
        for (const Token& token : tokens)
        {
            if (token.line > prevLine) out << "\n";
            prevLine = token.line;
            token.print(out);
            out << " ";
        }
        out << "\n";
    }
};

void lexOperators(TokenStream& tokenStream, const std::string& str, int line)
{
    if (str.empty()) return;

    size_t prefLen;
    for (prefLen = str.size(); prefLen >= 1; --prefLen)
    {
        std::string pref = str.substr(0, prefLen);
        if (operatorMap.find(pref) != operatorMap.end() || tokenMap.find(pref) != tokenMap.end()) break;
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

    int line = 1;
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

        errorAssert(cSet != WORD || currSymbSet != NUMBER, "Lex", "Invalid digit", line);

        if (cSet != currSymbSet && curr != "")
        {
            if (currSymbSet == OPERATOR) lexOperators(tokenStream, curr, line);
            else tokenStream.tokens.emplace_back(curr, line);
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

    tokenStream.tokens.emplace_back(Eof, line + 1);

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
    Body,
    VarDef,
    FunDef,
    Program
};

bool isExpr(ASTNodeType type)
{
    return type == GetVar || type == ConstNumber || type == BinOperator || type == UnOperator || type == Application || type == Bracketed;
}

void printIndent(std::ostream& out, int indent)
{
    while (indent--) out << "  ";
}

struct ASTNode
{
    ASTNodeType type;

    std::string name;
    ValueT number;
    Operator oper;
    std::vector<std::string> paramNames;

    std::vector<ASTNode> children;
    int remaining;

    int line;

    ASTNode(ASTNodeType type, int remaining, int line)
        : type(type)
        , remaining(remaining)
        , line(line)
    {}

    ASTNode(ASTNodeType type, const std::string& name, int remaining, int line)
        : type(type)
        , name(name)
        , remaining(remaining)
        , line(line)
    {}

    ASTNode(ASTNodeType type, ValueT number, int remaining, int line)
        : type(type)
        , number(number)
        , remaining(remaining)
        , line(line)
    {}

    ASTNode(ASTNodeType type, Operator oper, int remaining, int line)
        : type(type)
        , oper(oper)
        , remaining(remaining)
        , line(line)
    {}

    bool isExpr() const
    {
        return ::isExpr(type);
    }

    bool isComplete() const
    {
        return remaining == 0;
    }

    bool needsExpr() const
    {
        return (
            ((type == UnOperator || type == Bracketed || type == IgnoreValue || type == Assignment || type == ReturnVal || type == VarDef) && remaining == 1)
            || (type == BinOperator && (remaining == 2 || remaining == 1))
            || (type == Application && remaining == -1)
            || (type == IfThenElse && remaining == 3)
            || (type == WhileDo && remaining == 2)
        );
    }

    bool needsBody() const
    {
        return (
            (type == IfThenElse && (remaining == 2 || remaining == 1))
            || ((type == WhileDo || type == FunDef) && remaining == 1)
            || (type == Program && remaining == -1)
        );
    }

    bool needsStmt() const
    {
        return type == Body && remaining == -1;
    }

    void addChild(ASTNode&& child)
    {
        assert(remaining != 0);
        if (remaining > 0) --remaining;
        children.emplace_back(std::move(child));
    }

    void addExpr(ASTNode&& expr)
    {
        assert(needsExpr() && expr.isExpr() && expr.isComplete());
        addChild(std::move(expr));
    }

    void addBody(ASTNode&& body)
    {
        assert(needsBody() && body.type == Body && body.isComplete());
        addChild(std::move(body));
    }

    void addStmt(ASTNode&& stmt)
    {
        assert(needsStmt() && !stmt.isExpr() && stmt.type != Body && stmt.type != Program);
        addChild(std::move(stmt));
    }

    bool isEmptyNode() const
    {
        return type == Body && remaining == 0 && children.size() == 0 && line == 0;
    }

    static const ASTNode emptyNode;

    void print(std::ostream& out, int indent = 0) const
    {
        switch (type)
        {
        case GetVar:
            out << name;
            break;

        case ConstNumber:
            out << number;
            break;

        case BinOperator:
            out << "(";
            if (children.size() > 0) children[0].print(out, indent + 1);
            out << " " << operatorStrings.at(oper) << " ";
            if (children.size() > 1) children[1].print(out, indent + 1);
            out << ")";
            break;

        case UnOperator:
            out << operatorStrings.at(oper);
            if (children.size() > 0) children[0].print(out, indent + 1);
            break;

        case Application:
            out << name << "(";
            for (size_t i = 0; i < children.size(); ++i)
            {
                if (i > 0) out << ", ";
                children[i].print(out, indent + 1);
            }
            out << ")";
            break;

        case Bracketed:
            out << "(";
            if (children.size() > 0) children[0].print(out, indent + 1);
            out << ")";
            break;

        case IgnoreValue:
            printIndent(out, indent);
            if (children.size() > 0) children[0].print(out, indent + 1);
            out << ";" << "\n";
            break;

        case Assignment:
            printIndent(out, indent);
            out << name << " = ";
            if (children.size() > 0) children[0].print(out, indent + 1);
            out << ";" << "\n";
            break;

        case IfThenElse:
            for (size_t i = 0; i < children.size() + remaining; ++i)
            {
                if (i == 0)
                {
                    printIndent(out, indent);
                    out << "if" << "\n";
                    printIndent(out, indent + 1);
                    if (children.size() > i) children[i].print(out, indent + 1);
                    out << "\n";
                }
                else if (i + 1 == children.size() + remaining)
                {
                    printIndent(out, indent);
                    out << "else" << "\n";
                    if (children.size() > i) children[i].print(out, indent + 1);
                }
                else if (i % 2 == 0)
                {
                    printIndent(out, indent);
                    out << "elif" << "\n";
                    printIndent(out, indent + 1);
                    if (children.size() > i) children[i].print(out, indent + 1);
                    out << "\n";
                }
                else
                {
                    printIndent(out, indent);
                    out << "then" << "\n";
                    if (children.size() > i) children[i].print(out, indent + 1);
                }
            }
            printIndent(out, indent);
            out << "end" << "\n";
            break;

        case WhileDo:
            printIndent(out, indent);
            out << "while" << "\n";
            printIndent(out, indent + 1);
            if (children.size() > 0) children[0].print(out, indent + 1);
            out << "\n";
            printIndent(out, indent);
            out << "do" << "\n";
            if (children.size() > 1) children[1].print(out, indent + 1);
            printIndent(out, indent);
            out << "end" << "\n";
            break;

        case ReturnVal:
            printIndent(out, indent);
            out << "return ";
            if (children.size() > 0) children[0].print(out, indent + 1);
            out << ";" << "\n";
            break;

        case Body:
            for (const ASTNode& child : children)
            {
                child.print(out, indent);
            }
            break;

        case VarDef:
            printIndent(out, indent);
            out << "var " << name << " = ";
            if (children.size() > 0) children[0].print(out, indent + 1);
            out << ";" << "\n";
            break;

        case FunDef:
            printIndent(out, indent);
            out << "fun " << name << "(";
            for (size_t i = 0; i < paramNames.size(); ++i)
            {
                if (i > 0) out << ", ";
                out << paramNames[i];
            }
            out << ") =" << "\n";
            if (children.size() > 0) children[0].print(out, indent + 1);
            printIndent(out, indent);
            out << "end" << "\n";
            break;

        case Program:
            for (size_t i = 0; i < children.size(); ++i)
            {
                if (i > 0 && (children[i].type == FunDef || children[i - 1].type == FunDef)) out << "\n";
                children[i].print(out, indent);
            }
            out << "\n";
            break;
        }
    }
};

const ASTNode ASTNode::emptyNode(Body, 0, 0);

bool popBody(std::vector<ASTNode>& nodeStack, int line)
{
    ASTNode body(Body, -1, line);
    while (nodeStack.back().isComplete() && !nodeStack.back().isExpr())
    {
        body.addStmt(std::move(nodeStack.back()));
        nodeStack.pop_back();
    }

    if (!nodeStack.back().needsBody()) return false;

    body.remaining = 0;
    std::reverse(body.children.begin(), body.children.end());
    nodeStack.back().addBody(std::move(body));

    return true;
}

ASTNode popOperators(std::vector<ASTNode>& nodeStack, int prec, int line)
{
    ASTNode right = std::move(nodeStack.back());
    nodeStack.pop_back();

    if (!right.isComplete() || !right.isExpr()) return ASTNode::emptyNode;

    while ((nodeStack.back().type == BinOperator || nodeStack.back().type == UnOperator) && operatorPrec.at(nodeStack.back().oper) >= prec)
    {
        assert(!nodeStack.back().isComplete());
        nodeStack.back().addExpr(std::move(right));
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

    nodeStack.emplace_back(Program, -1, 1);

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
                if (last.paramNames.empty())
                    errorAssert(token.type == Name || token.type == RBrack, "Parse", "Expected parameter name or right bracket list in defition", line);
                else errorAssert(token.type == Name, "Parse", "Expected parameter name in defition", line);
                if (token.type == Name)
                {
                    last.paramNames.push_back(token.name);
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
            errorAssert(!last.isComplete() || !last.isExpr(), "Parse", "Unexpected name", line);
            nodeStack.emplace_back(GetVar, token.name, 0, line);
            break;

        case Number:
            errorAssert(!last.isComplete() || !last.isExpr(), "Parse", "Unexpected number", line);
            nodeStack.emplace_back(ConstNumber, token.number, 0, line);
            break;

        case Op:
        {
            auto itBin = binOperators.find(token.oper);
            auto itUn = unOperators.find(token.oper);
            bool okBin = last.isComplete() && last.isExpr();
            bool okUn = !okBin;
            errorAssert(itUn != unOperators.end() || okBin, "Parse", "Unexpected binary operator", line);
            errorAssert(itBin != binOperators.end() || okUn, "Parse", "Unexpected unary operator", line);
            if (itUn != unOperators.end() && okUn) nodeStack.emplace_back(UnOperator, itUn->second, 1, line);
            else
            {
                ASTNode left = popOperators(nodeStack, operatorPrec.at(token.oper), line);
                assert(!left.isEmptyNode());
                nodeStack.emplace_back(BinOperator, token.oper, 2, left.line);
                nodeStack.back().addExpr(std::move(left));
            }
            break;
        }

        case Assign:
        {
            errorAssert(last.type == GetVar, "Parse", "Unexpected assignment", line);
            ASTNode& prev = nodeStack[nodeStack.size() - 2];
            errorAssert(!prev.isExpr() && (prev.isComplete() || prev.needsBody()), "Parse", "Unexpected assignment", line);
            last.type = Assignment;
            last.remaining = 1;
            break;
        }

        case Semicol:
        {
            ASTNode expr = popOperators(nodeStack, 0, line);
            ASTNode& last = nodeStack.back();
            errorAssert(
                !expr.isEmptyNode() &&
                ((last.needsExpr() && (last.type == Assignment || last.type == ReturnVal || last.type == VarDef))
                || last.needsBody() || (last.isComplete() && !last.isExpr()))
                , "Parse", "Unexpected semicolon", line
            );
            if (last.needsExpr()) last.addExpr(std::move(expr));
            else
            {
                nodeStack.emplace_back(IgnoreValue, 1, expr.line);
                nodeStack.back().addExpr(std::move(expr));
            }
            break;
        }

        case Comma:
        {
            ASTNode expr = popOperators(nodeStack, 0, line);
            ASTNode& last = nodeStack.back();
            errorAssert(!expr.isEmptyNode() && !last.isComplete() && last.type == Application, "Parse", "Unexpected comma", line);
            last.addExpr(std::move(expr));
            break;
        }

        case LBrack:
            if (last.type == GetVar)
            {
                last.type = Application;
                last.remaining = -1;
            }
            else
            {
                errorAssert(!last.isComplete() || !last.isExpr(), "Parse", "Unexpected opening bracket", line);
                nodeStack.emplace_back(Bracketed, 1, line);
            }
            break;

        case RBrack:
        {
            if (!last.isComplete() && last.type == Application && last.children.empty())
            {
                last.remaining = 0;
                break;
            }

            ASTNode expr = popOperators(nodeStack, 0, line);
            ASTNode& last = nodeStack.back();
            errorAssert(!expr.isEmptyNode() && !last.isComplete() && (last.type == Bracketed || last.type == Application), "Parse", "Unmatched closing bracket", line);
            if (last.type == Bracketed)
            {
                nodeStack.pop_back();
                nodeStack.emplace_back(std::move(expr));
            }
            else
            {
                last.addExpr(std::move(expr));
                last.remaining = 0;
            }
            break;
        }

        case If:
            errorAssert(!last.isExpr() && (last.isComplete() || last.needsBody()), "Parse", "Unexpected if", line);
            nodeStack.emplace_back(IfThenElse, 3, line);
            break;

        case Then:
        {
            ASTNode expr = popOperators(nodeStack, 0, line);
            ASTNode& last = nodeStack.back();
            errorAssert(!expr.isEmptyNode() && last.remaining == 3 && last.type == IfThenElse, "Parse", "Unexpected then", line);
            last.addExpr(std::move(expr));
            break;
        }

        case Elif:
        {
            bool succ = popBody(nodeStack, line);
            ASTNode& last = nodeStack.back();
            errorAssert(succ && last.remaining == 1 && last.type == IfThenElse, "Parse", "Unexpected elif", line);
            last.remaining += 2;
            break;
        }

        case Else:
        {
            bool succ = popBody(nodeStack, line);
            ASTNode& last = nodeStack.back();
            errorAssert(succ && last.remaining == 1 && last.type == IfThenElse, "Parse", "Unexpected else", line);
            break;
        }

        case While:
            errorAssert(!last.isExpr() && (last.isComplete() || last.needsBody()), "Parse", "Unexpected while", line);
            nodeStack.emplace_back(WhileDo, 2, line);
            break;

        case Do:
        {
            ASTNode expr = popOperators(nodeStack, 0, line);
            ASTNode& last = nodeStack.back();
            errorAssert(!expr.isEmptyNode() && last.remaining == 2 && last.type == WhileDo, "Parse", "Unexpected do", line);
            last.addExpr(std::move(expr));
            break;
        }

        case End:
        {
            bool succ = popBody(nodeStack, line);
            ASTNode& last = nodeStack.back();
            if (succ && last.type == IfThenElse && last.remaining == 1) last.addBody(ASTNode(Body, 0, line));
            errorAssert(succ && last.isComplete() && (last.type == IfThenElse || last.type == WhileDo || last.type == FunDef), "Parse", "Unexpected end", line);
            break;
        }

        case Return:
            errorAssert(!last.isExpr() && (last.isComplete() || last.needsBody()), "Parse", "Unexpected return", line);
            nodeStack.emplace_back(ReturnVal, 1, line);
            break;

        case Var:
            errorAssert(!last.isExpr() && (last.isComplete() || last.needsBody()), "Parse", "Unexpected variable definition", line);
            nodeStack.emplace_back(VarDef, 1, line);
            defState = DefName;
            break;

        case Fun:
            errorAssert(!last.isExpr() && (last.isComplete() || last.needsBody()), "Parse", "Unexpected function definition", line);
            nodeStack.emplace_back(FunDef, 1, line);
            defState = DefName;
            break;

        case Eof:
        {
            bool succ = popBody(nodeStack, line);
            ASTNode& last = nodeStack.back();
            errorAssert(succ && last.type == Program, "Parse", "Unexpected end of file", line);
            assert(last.children.size() == 1);
            if (last.children.back().type == Body) last.children = std::move(last.children.back().children);
            last.remaining = 0;
            break;
        }
        }
    }

    return nodeStack.back();
}

enum InstrType
{
    IPush,
    IPop,
    IBinOp,
    IUnOp,
    IGlobal,
    ILocal,
    ILoad,
    IStore,
    IJmp,
    IJzr,
    IJnz,
    IAddr,
    ICall,
    IReturn,
    IRead,
    IPrint,
    IExit
};

std::unordered_map<InstrType, std::string> intrStrings = {
    {IPush, "push"},
    {IPop, "pop"},
    {IBinOp, "binOp"},
    {IUnOp, "unOp"},
    {IGlobal, "global"},
    {ILocal, "local"},
    {ILoad, "load"},
    {IStore, "store"},
    {IJmp, "jmp"},
    {IJzr, "jzr"},
    {IJnz, "jnz"},
    {IAddr, "addr"},
    {ICall, "call"},
    {IReturn, "return"},
    {IRead, "read"},
    {IPrint, "print"},
    {IExit, "exit"}
};

struct Instr
{
    InstrType type;
    ValueT arg;

    Instr(InstrType type)
        : type(type)
    {}

    Instr(InstrType type, ValueT arg)
        : type(type)
        , arg(arg)
    {}

    void print(std::ostream& out) const
    {
        std::cerr << intrStrings.at(type);
        if (type != IPop && type != ILoad && type != IStore && type != IReturn && type != IRead && type != IPrint && type != IExit)
            std::cerr << " " << arg;
        std::cerr << "\n";
    }
};

struct InstrStream
{
    std::vector<Instr> instrs;

    void print(std::ostream& out) const
    {
        for (size_t i = 0; i < instrs.size(); ++i)
        {
            std::cerr << i << ": ";
            instrs[i].print(std::cerr);
        }
        std::cerr << "\n";
    }
};

enum DefType
{
    DefFun,
    DefGlobalVar,
    DefLocalVar
};

struct Definition
{
    DefType type;
    ValueT addr;
    ValueT params;

    Definition(DefType type, ValueT addr)
        : type(type)
        , addr(addr)
    {}

    Definition(DefType type, ValueT addr, ValueT params)
        : type(type)
        , addr(addr)
        , params(params)
    {}
};

struct Env
{
    std::vector<ValueT> frameOffsets;

    std::unordered_map<std::string, std::vector<Definition>> defs;
    std::vector<std::unordered_set<std::string>> scopeDefs;

    std::unordered_map<ValueT, ValueT> labelAddrs;
    ValueT currLabel = 0;

    void pushFrame(ValueT params)
    {
        frameOffsets.push_back(-params);
    }

    void popFrame(ValueT params)
    {
        assert(frameOffsets.back() == -params);
        frameOffsets.pop_back();
    }

    void pushScope()
    {
        scopeDefs.emplace_back();
    }

    void popScope()
    {
        for (const std::string& name : scopeDefs.back())
        {
            auto it = defs.find(name);
            assert(it != defs.end());
            if (it->second.back().type == DefLocalVar || it->second.back().type == DefGlobalVar) --frameOffsets.back();
            it->second.pop_back();
            if (it->second.empty()) defs.erase(it);
        }
        scopeDefs.pop_back();
    }

    void defineFun(const std::string& name, ValueT addr, ValueT params, int line)
    {
        errorAssert(scopeDefs.back().find(name) == scopeDefs.back().end(), "Compile", "Multiple definitions of function", line);
        defs.insert({name, {}});
        defs[name].emplace_back(DefFun, addr, params);
        scopeDefs.back().insert(name);
    }

    void defineVar(const std::string& name, DefType type, int line)
    {
        errorAssert(scopeDefs.back().find(name) == scopeDefs.back().end(), "Compile", "Multiple definitions of variable in the same scope", line);
        defs.insert({name, {}});
        defs[name].emplace_back(type, frameOffsets.back()++);
        scopeDefs.back().insert(name);
    }

    const Definition& getDef(const std::string& name, int line)
    {
        auto it = defs.find(name);
        errorAssert(it != defs.end(), "Compile", "Undefined variable or function", line);
        return it->second.back();
    }

    ValueT newLabel()
    {
        return currLabel++;
    }

    void setLabelAddr(ValueT label, ValueT addr)
    {
        labelAddrs[label] = addr;
    }

    ValueT getLabelAddr(ValueT label)
    {
        auto it = labelAddrs.find(label);
        assert(it != labelAddrs.end());
        return it->second;
    }
};

void compileASTNode(const ASTNode& node, Env& env, InstrStream& instrStream)
{
    assert(node.isComplete());

    switch (node.type)
    {
    case GetVar:
    {
        assert(node.children.empty());
        const Definition& def = env.getDef(node.name, node.line);
        errorAssert(def.type == DefGlobalVar || def.type == DefLocalVar, "Compile", "Cannot get value of function", node.line);
        instrStream.instrs.emplace_back(def.type == DefGlobalVar ? IGlobal : ILocal, def.addr);
        instrStream.instrs.emplace_back(ILoad);
        break;
    }

    case ConstNumber:
        assert(node.children.empty());
        instrStream.instrs.emplace_back(IPush, node.number);
        break;

    case BinOperator:
        assert(node.children.size() == 2);
        assert(node.children[0].isExpr());
        assert(node.children[1].isExpr());
        if (node.oper == LogAnd || node.oper == LogOr)
        {
            ValueT endOfSecond = env.newLabel();
            instrStream.instrs.emplace_back(IPush, node.oper == LogAnd ? 0 : 1);
            compileASTNode(node.children[0], env, instrStream);
            instrStream.instrs.emplace_back(node.oper == LogAnd ? IJzr : IJnz, endOfSecond);
            instrStream.instrs.emplace_back(IUnOp, LogNot);
            compileASTNode(node.children[1], env, instrStream);
            env.setLabelAddr(endOfSecond, instrStream.instrs.size());
        }
        else
        {
            compileASTNode(node.children[0], env, instrStream);
            compileASTNode(node.children[1], env, instrStream);
        }
        instrStream.instrs.emplace_back(IBinOp, node.oper);
        break;

    case UnOperator:
        assert(node.children.size() == 1);
        assert(node.children[0].isExpr());
        compileASTNode(node.children[0], env, instrStream);
        instrStream.instrs.emplace_back(IUnOp, node.oper);
        break;

    case Application:
    {
        const Definition& def = env.getDef(node.name, node.line);
        errorAssert(def.type == DefFun, "Compile", "Cannot call variable", node.line);
        errorAssert(def.params == (ValueT) node.children.size(), "Compile", "Wrong number of arguments in function call", node.line);
        for (const ASTNode& child : node.children)
        {
            assert(child.isExpr());
            compileASTNode(child, env, instrStream);
        }
        instrStream.instrs.emplace_back(IAddr, def.addr);
        instrStream.instrs.emplace_back(ICall, node.children.size());
        break;
    }

    case Bracketed:
        assert(node.children.size() == 1);
        assert(node.children[0].isExpr());
        compileASTNode(node.children[0], env, instrStream);
        break;

    case IgnoreValue:
        assert(node.children.size() == 1);
        assert(node.children[0].isExpr());
        compileASTNode(node.children[0], env, instrStream);
        instrStream.instrs.emplace_back(IPop);
        break;

    case Assignment:
    {
        assert(node.children.size() == 1);
        assert(node.children[0].isExpr());
        const Definition& def = env.getDef(node.name, node.line);
        errorAssert(def.type == DefGlobalVar || def.type == DefLocalVar, "Compile", "Cannot assign to function", node.line);
        instrStream.instrs.emplace_back(def.type == DefGlobalVar ? IGlobal : ILocal, def.addr);
        compileASTNode(node.children[0], env, instrStream);
        instrStream.instrs.emplace_back(IStore);
        break;
    }

    case IfThenElse:
    {
        assert(node.children.size() % 2 == 1 && node.children.size() >= 3);
        ValueT endOfIf = env.newLabel();
        ValueT endOfThen;
        for (size_t i = 0; i < node.children.size(); ++i)
        {
            const ASTNode& child = node.children[i];
            if (i % 2 == 0 && i < node.children.size() - 1)
            {
                assert(child.isExpr());
                endOfThen = env.newLabel();
                compileASTNode(child, env, instrStream);
                instrStream.instrs.emplace_back(IJzr, endOfThen);
            }
            else if (i % 2 == 1)
            {
                assert(child.type == Body);
                compileASTNode(child, env, instrStream);
                instrStream.instrs.emplace_back(IJmp, endOfIf);
                env.setLabelAddr(endOfThen, instrStream.instrs.size());
            }
            else
            {
                assert(child.type == Body);
                compileASTNode(child, env, instrStream);
                env.setLabelAddr(endOfIf, instrStream.instrs.size());
            }
        }
        break;
    }

    case WhileDo:
    {
        assert(node.children.size() == 2);
        assert(node.children[0].isExpr());
        assert(node.children[1].type == Body);
        ValueT condOfWhile = env.newLabel();
        ValueT startOfWhile = env.newLabel();
        instrStream.instrs.emplace_back(IJmp, condOfWhile);
        env.setLabelAddr(startOfWhile, instrStream.instrs.size());
        compileASTNode(node.children[1], env, instrStream);
        compileASTNode(node.children[0], env, instrStream);
        env.setLabelAddr(condOfWhile, instrStream.instrs.size());
        instrStream.instrs.emplace_back(IJnz, startOfWhile);
        break;
    }

    case ReturnVal:
        assert(node.children.size() == 1);
        assert(node.children[0].isExpr());
        compileASTNode(node.children[0], env, instrStream);
        instrStream.instrs.emplace_back(IReturn);
        break;

    case Body:
    {
        int numVars = 0;
        env.pushScope();
        for (const ASTNode& child : node.children)
        {
            assert(!child.isExpr() && child.type != Body && child.type != Program);
            errorAssert(child.type != FunDef, "Compile", "Illegal nested function definition", child.line);
            compileASTNode(child, env, instrStream);
            if (child.type == VarDef)
            {
                ++numVars;
                env.defineVar(child.name, DefLocalVar, child.line);
            }
        }
        env.popScope();
        std::fill_n(std::back_inserter(instrStream.instrs), numVars, Instr(IPop));
        break;
    }

    case VarDef:
        assert(node.children.size() == 1);
        assert(node.children[0].isExpr());
        compileASTNode(node.children[0], env, instrStream);
        break;

    case FunDef:
        assert(node.children.size() == 1);
        assert(node.children[0].type == Body);
        env.pushFrame(node.paramNames.size());
        env.pushScope();
        for (size_t i = 0; i < node.paramNames.size(); ++i)
        {
            env.defineVar(node.paramNames[i], DefLocalVar, node.line);
        }
        env.defineVar("(return address)", DefLocalVar, node.line);
        env.defineVar("(dynamic link)", DefLocalVar, node.line);
        env.defineVar("(stack pointer)", DefLocalVar, node.line);
        compileASTNode(node.children[0], env, instrStream);
        instrStream.instrs.emplace_back(IPush, 0);
        instrStream.instrs.emplace_back(IReturn);
        env.popScope();
        env.popFrame(node.paramNames.size());
        break;

    case Program:
    {
        env.pushFrame(0);
        env.pushScope();

        env.defineFun("read", env.newLabel(), 0, node.line);
        env.defineFun("print", env.newLabel(), 1, node.line);
        env.defineFun("exit", env.newLabel(), 1, node.line);

        for (const ASTNode& child : node.children)
        {
            assert(!child.isExpr() && child.type != Body && child.type != Program);
            errorAssert(child.type == VarDef || child.type == FunDef, "Compile", "Illegal statement outside function body", child.line);
            if (child.type == FunDef) env.defineFun(child.name, env.newLabel(), child.paramNames.size(), child.line);
        }

        for (const ASTNode& child : node.children)
        {
            if (child.type == FunDef) continue;
            compileASTNode(child, env, instrStream);
            if (child.type == VarDef) env.defineVar(child.name, DefGlobalVar, child.line);
        }

        errorAssert(env.defs.find("main") != env.defs.end(), "Compile", "`main` must be defined", node.line);
        const Definition& def = env.getDef("main", node.line);
        errorAssert(def.type == DefFun, "Compile", "`main` must be a function", node.line);
        errorAssert(def.params == 0, "Compile", "`main` must take no arguments", node.line);
        instrStream.instrs.emplace_back(IAddr, def.addr);
        instrStream.instrs.emplace_back(ICall, 0);
        instrStream.instrs.emplace_back(IExit);

        env.setLabelAddr(env.getDef("read", node.line).addr, instrStream.instrs.size());
        instrStream.instrs.emplace_back(IRead);
        instrStream.instrs.emplace_back(IReturn);

        env.setLabelAddr(env.getDef("print", node.line).addr, instrStream.instrs.size());
        instrStream.instrs.emplace_back(ILocal, 0);
        instrStream.instrs.emplace_back(ILoad);
        instrStream.instrs.emplace_back(IPrint);
        instrStream.instrs.emplace_back(IPush, 0);
        instrStream.instrs.emplace_back(IReturn);

        env.setLabelAddr(env.getDef("exit", node.line).addr, instrStream.instrs.size());
        instrStream.instrs.emplace_back(ILocal, 0);
        instrStream.instrs.emplace_back(ILoad);
        instrStream.instrs.emplace_back(IExit);
        instrStream.instrs.emplace_back(IPush, 0);
        instrStream.instrs.emplace_back(IReturn);

        for (const ASTNode& child : node.children)
        {
            if (child.type != FunDef) continue;
            env.setLabelAddr(env.getDef(child.name, child.line).addr, instrStream.instrs.size());
            compileASTNode(child, env, instrStream);
        }

        for (Instr& instr : instrStream.instrs)
        {
            if (instr.type != IAddr && instr.type != IJmp && instr.type != IJzr && instr.type != IJnz) continue;
            instr.arg = env.getLabelAddr(instr.arg);
        }

        break;
    }
    }
}

InstrStream compileProgram(const ASTNode& node)
{
    Env env;
    InstrStream instrStream;
    compileASTNode(node, env, instrStream);
    return instrStream;
}

ValueT safePopBack(std::vector<ValueT>& stack, ValueT pc)
{
    errorAssert(!stack.empty(), "Runtime", "Tried to pop from empty stack", pc);
    ValueT back = stack.back();
    stack.pop_back();
    return back;
}

ValueT executeProgram(const InstrStream& instrStream, std::istream& in, std::ostream& out)
{
    ValueT pc = 0;
    ValueT bp = 0;
    std::vector<ValueT> stack;

    while (pc >= 0 && pc < (ValueT) instrStream.instrs.size())
    {
        const Instr& instr = instrStream.instrs[pc];
        switch (instr.type)
        {
        case IPush:
            stack.push_back(instr.arg);
            break;

        case IPop:
            safePopBack(stack, pc);
            break;

        case IBinOp:
        {
            ValueT right = stack.back();
            stack.pop_back();
            ValueT left = safePopBack(stack, pc);
            switch (instr.arg)
            {
            case ArAdd:
                stack.push_back(left + right);
                break;

            case ArSub:
                stack.push_back(left - right);
                break;

            case ArMul:
                stack.push_back(left * right);
                break;

            case ArDiv:
                errorAssert(right != 0, "Runtime", "Division by 0", pc);
                stack.push_back(left / right);
                break;

            case ArMod:
                errorAssert(right != 0, "Runtime", "Modulo by 0", pc);
                stack.push_back(left % right);
                break;

            case CmpEq:
                stack.push_back(left == right);
                break;

            case CmpNeq:
                stack.push_back(left != right);
                break;

            case CmpLt:
                stack.push_back(left < right);
                break;

            case CmpGt:
                stack.push_back(left > right);
                break;

            case CmpLeq:
                stack.push_back(left <= right);
                break;

            case CmpGeq:
                stack.push_back(left >= right);
                break;

            case LogAnd:
                stack.push_back(left && right);
                break;

            case LogOr:
                stack.push_back(left || right);
                break;

            case BitAnd:
                stack.push_back(left & right);
                break;

            case BitOr:
                stack.push_back(left | right);
                break;

            case BitXor:
                stack.push_back(left ^ right);
                break;

            case BitLShift:
                stack.push_back(left << right);
                break;

            case BitRShift:
                stack.push_back(left >> right);
                break;
            }
            break;
        }

        case IUnOp:
        {
            ValueT val = safePopBack(stack, pc);
            switch (instr.arg)
            {
            case UnPlus:
                stack.push_back(+val);
                break;

            case UnMinus:
                stack.push_back(-val);
                break;

            case LogNot:
                stack.push_back(!val);
                break;

            case BitNot:
                stack.push_back(~val);
                break;
            }
            break;
        }

        case IGlobal:
            stack.push_back(instr.arg);
            break;

        case ILocal:
            stack.push_back(bp + instr.arg);
            break;

        case ILoad:
        {
            ValueT ptr = safePopBack(stack, pc);
            errorAssert(ptr >= 0 && ptr < (ValueT) stack.size(), "Runtime", "Loading from invalid pointer", pc);
            stack.push_back(stack[ptr]);
            break;
        }

        case IStore:
        {
            ValueT val = safePopBack(stack, pc);
            ValueT ptr = safePopBack(stack, pc);
            errorAssert(ptr >= 0 && ptr < (ValueT) stack.size(), "Runtime", "Storing to invalid pointer", pc);
            stack[ptr] = val;
            break;
        }

        case IJmp:
            pc = instr.arg;
            break;

        case IJzr:
        {
            ValueT val = safePopBack(stack, pc);
            if (val == 0) pc = instr.arg;
            break;
        }

        case IJnz:
        {
            ValueT val = safePopBack(stack, pc);
            if (val != 0) pc = instr.arg;
            break;
        }

        case IAddr:
            stack.push_back(instr.arg);
            break;

        case ICall:
        {
            ValueT newPc = safePopBack(stack, pc);
            stack.push_back(pc);
            stack.push_back(bp);
            stack.push_back(stack.size() - instr.arg - 2);
            pc = newPc;
            bp = stack.size() - 3;
            break;
        }

        case IReturn:
        {
            ValueT val = safePopBack(stack, pc);
            ValueT oldBp = bp;
            pc = stack[oldBp];
            bp = stack[oldBp + 1];
            errorAssert(stack[oldBp + 2] >= 0 && stack[oldBp + 2] <= (ValueT) stack.size(), "Runtime", "Invalid stack pointer", pc);
            stack.resize(stack[oldBp + 2]);
            stack.push_back(val);
            break;
        }

        case IRead:
            stack.push_back(0);
            in >> stack.back();
            break;

        case IPrint:
        {
            ValueT val = safePopBack(stack, pc);
            out << val << "\n";
            break;
        }

        case IExit:
        {
            ValueT val = safePopBack(stack, pc);
            return val;
            break;
        }
        }

        ++pc;
    }

    errorAssert(false, "Runtime", "Program counter went out of bounds", pc);
    return 0;
}

int main()
{
    TokenStream tokenStream = lexProgram(std::cin);

    std::cerr << "Lexed program:\n\n";
    tokenStream.print(std::cerr);

    ASTNode astRoot = parseProgram(tokenStream);

    std::cerr << "Parsed program:\n\n";
    astRoot.print(std::cerr);

    InstrStream instrStream = compileProgram(astRoot);

    std::cerr << "Compiled program:\n\n";
    instrStream.print(std::cerr);

    ValueT retCode = executeProgram(instrStream, std::cin, std::cout);

    std::cerr << "Executed program:\n\n";
    std::cerr << retCode << "\n";

    return 0;
}
