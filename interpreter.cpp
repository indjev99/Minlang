#include <algorithm>
#include <cassert>
#include <fstream>
#include <iostream>
#include <map>
#include <memory>
#include <set>
#include <string>
#include <vector>

void errorAssert(bool cond, const std::string& type, const std::string& descr, int line)
{
    if (cond) return;
    std::cerr << type << " error at line " << line << ": " << descr << std::endl;
    exit(0);
}

typedef long long ValueT;

template <class T>
std::map<std::string, T> computeInverseMap(const std::map<T, std::string>& tStrings)
{
    std::map<std::string, T> tMap;
    for (auto& [t, str] : tStrings)
    {
        tMap.insert({str, t});
    }
    std::cerr << "Inverse: " << std::endl;
    for (auto& [str, t] : tMap)
    {
        std::cerr << str << " -> " << t << std::endl;
    }
    return tMap;
}

enum Operator
{
    ArAdd,
    ArSub,
    ArMul,
    ArDiv,
    ArMod,
    UnPlus,
    UnMinus,
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
    BitRShift,
    AddrOf,
    Deref
};

const std::map<Operator, std::string> operatorStrings = {
    {ArAdd , "+"},
    {ArSub , "-"},
    {ArMul , "*"},
    {ArDiv , "/"},
    {ArMod , "%"},
    {UnPlus , "+"},
    {UnMinus , "-"},
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
    {BitRShift, ">>"},
    {AddrOf, "&"},
    {Deref, "*"}
};

const std::map<std::string, Operator> operatorMap = computeInverseMap(operatorStrings);

const int minOperatorPrec = 0;
const int maxOperatorPrec = 100;

const std::map<Operator, int> operatorPrec = {
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
    {BitRShift, 8},
    {AddrOf, 20},
    {Deref, 20}
};

const std::set<Operator> binOperators = {
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

const std::set<Operator> unOperators = {
    UnPlus,
    UnMinus,
    LogNot,
    BitNot,
    AddrOf,
    Deref
};

std::map<Operator, Operator> computeOperatorToUnMap()
{
    std::map<Operator, Operator> operatorToUnMap;
    for (Operator unOp : unOperators)
    {
        std::string str = operatorStrings.at(unOp);
        Operator op = operatorMap.at(str);
        operatorToUnMap.insert({op, unOp});
    }
    return operatorToUnMap;
}

const std::map<Operator, Operator> operatorToUnMap = computeOperatorToUnMap();

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
    LSub,
    RSub,
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

const std::map<TokenType, std::string> tokenStrings = {
    {Assign, "="},
    {Semicol, ";"},
    {Comma, ","},
    {LBrack, "("},
    {RBrack, ")"},
    {LSub, "["},
    {RSub, "]"},
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

const std::map<std::string, TokenType> tokenMap = computeInverseMap(tokenStrings);

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
    bool comment = false;

    TokenStream tokenStream;

    while (in.good())
    {
        char c = in.get();

        if (c == '#') comment = true;
        else if (c == '\n') comment = false;

        SymbolSet cSet;
        if (comment || isspace(c)) cSet = SPACE;
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
    Subscript,
    AddrOfOp,
    DerefOp,
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

bool isAddrExpr(ASTNodeType type)
{
    return type == GetVar || type == Subscript || type == DerefOp;
}

bool isExpr(ASTNodeType type)
{
    return isAddrExpr(type) || type == ConstNumber || type == BinOperator || type == UnOperator || type == Application || type == AddrOfOp;
}

bool isStmt(ASTNodeType type)
{
    return type == IgnoreValue || type == Assignment || type == IfThenElse || type == WhileDo || type == ReturnVal || type == VarDef || type == FunDef;
}

bool isBody(ASTNodeType type)
{
    return type == Body;
}

bool isProgram(ASTNodeType type)
{
    return type == Program;
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

    bool isAddrExpr() const
    {
        return ::isAddrExpr(type);
    }

    bool isExpr() const
    {
        return ::isExpr(type);
    }

    bool isStmt() const
    {
        return ::isStmt(type);
    }

    bool isBody() const
    {
        return ::isBody(type);
    }

    bool isProgram() const
    {
        return ::isProgram(type);
    }

    bool isComplete() const
    {
        return remaining == 0;
    }

    bool needsExpr() const
    {
        return (
            ((type == UnOperator || type == Bracketed || type == IgnoreValue || type == ReturnVal || type == VarDef) && remaining == 1)
            || ((type == BinOperator || type == Subscript || type == Assignment) && (remaining == 2 || remaining == 1))
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
        assert(remaining != 0 && child.isComplete());
        if (remaining > 0) --remaining;
        children.emplace_back(std::move(child));
    }

    void addExpr(ASTNode&& expr)
    {
        assert(needsExpr() && expr.isExpr());
        addChild(std::move(expr));
    }

    void addStmt(ASTNode&& stmt)
    {
        assert(needsStmt() && stmt.isStmt());
        addChild(std::move(stmt));
    }

    void addBody(ASTNode&& body)
    {
        assert(needsBody() && body.isBody());
        addChild(std::move(body));
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

        case AddrOfOp:
        case DerefOp:
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

        case Subscript:
            if (children.size() > 0) children[0].print(out, indent + 1);
            out << "[";
            if (children.size() > 1) children[1].print(out, indent + 1);
            out << "]";
            break;

        case IgnoreValue:
            printIndent(out, indent);
            if (children.size() > 0) children[0].print(out, indent + 1);
            out << ";" << "\n";
            break;

        case Assignment:
            printIndent(out, indent);
            if (children.size() > 0) children[0].print(out, indent + 1);
            out << " = ";
            if (children.size() > 1) children[1].print(out, indent + 1);
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

        default:
            assert(false);
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

ASTNode transformExpr(ASTNode&& expr, int line)
{
    std::cerr << expr.type << std::endl;
    expr.print(std::cerr, 0);
    std::cerr << std::endl;
    assert(expr.isExpr() && expr.isComplete());
    if (expr.type == UnOperator && expr.oper == AddrOf)
    {
        errorAssert(expr.children[0].isAddrExpr(), "Parse", "Unexpected AddrOfOp operator", line);
        expr.type = AddrOfOp;
    }
    else if (expr.type == UnOperator && expr.oper == Deref) expr.type = DerefOp;
    return expr;
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
    return transformExpr(std::move(right), line);
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
            auto itUn = operatorToUnMap.find(token.oper);
            bool isBin = itBin != binOperators.end();
            bool isUn = itUn != operatorToUnMap.end();
            assert(isBin || isUn);
            bool okBin = last.isComplete() && last.isExpr();
            bool okUn = !okBin;
            errorAssert(isUn || okBin, "Parse", "Unexpected binary operator", line);
            errorAssert(isBin || okUn, "Parse", "Unexpected unary operator", line);
            if (isUn && okUn) nodeStack.emplace_back(UnOperator, itUn->second, 1, line);
            else if (isBin && okBin)
            {
                ASTNode expr = popOperators(nodeStack, operatorPrec.at(token.oper), line);
                assert(!expr.isEmptyNode());
                nodeStack.emplace_back(BinOperator, token.oper, 2, line);
                nodeStack.back().addExpr(std::move(expr));
            }
            else assert(false);
            break;
        }

        case Assign:
        {
            ASTNode expr = popOperators(nodeStack, minOperatorPrec, line);
            errorAssert(!expr.isEmptyNode() && expr.isAddrExpr(), "Parse", "Unexpected assignment", line);
            ASTNode& last = nodeStack.back();
            errorAssert(!last.isExpr() && (last.isComplete() || last.needsBody()), "Parse", "Unexpected assignment", line);
            nodeStack.emplace_back(Assignment, 2, line);
            nodeStack.back().addExpr(std::move(expr));
            break;
        }

        case Semicol:
        {
            ASTNode expr = popOperators(nodeStack, minOperatorPrec, line);
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
            ASTNode expr = popOperators(nodeStack, minOperatorPrec, line);
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

            ASTNode expr = popOperators(nodeStack, minOperatorPrec, line);
            ASTNode& last = nodeStack.back();
            errorAssert(!expr.isEmptyNode() && !last.isComplete() && (last.type == Bracketed || last.type == Application), "Parse", "Unexpected closing bracket", line);
            if (last.type == Bracketed)
            {
                nodeStack.pop_back();
                nodeStack.emplace_back(std::move(expr));
            }
            else if (last.type == Application)
            {
                last.addExpr(std::move(expr));
                last.remaining = 0;
            }
            else assert(false);
            break;
        }

        case LSub:
        {
            ASTNode expr = popOperators(nodeStack, maxOperatorPrec, line);
            errorAssert(!expr.isEmptyNode(), "Parse", "Unexpected opening square bracket", line);
            nodeStack.emplace_back(Subscript, 2, line);
            nodeStack.back().addExpr(std::move(expr));
            break;
        }

        case RSub:
        {
            ASTNode expr = popOperators(nodeStack, minOperatorPrec, line);
            ASTNode& last = nodeStack.back();
            errorAssert(!expr.isEmptyNode() && !last.isComplete() && last.type == Subscript, "Parse", "Unexpected closing square", line);
            last.addExpr(std::move(expr));
            break;
        }

        case If:
            errorAssert(!last.isExpr() && (last.isComplete() || last.needsBody()), "Parse", "Unexpected if", line);
            nodeStack.emplace_back(IfThenElse, 3, line);
            break;

        case Then:
        {
            ASTNode expr = popOperators(nodeStack, minOperatorPrec, line);
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
            ASTNode expr = popOperators(nodeStack, minOperatorPrec, line);
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

        default:
            errorAssert(false, "Parse", "Unsupported token type", line);
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
    IAlloc,
    IFree,
    IRead,
    IPrint,
    IFlush,
    IExit
};

std::map<InstrType, std::string> intrStrings = {
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
    {IAlloc, "alloc"},
    {IFree, "free"},
    {IRead, "read"},
    {IPrint, "print"},
    {IFlush, "flush"},
    {IExit, "exit"}
};

bool hasArg(InstrType type)
{
    return type == IPush || type == IBinOp || type == IUnOp || type == IGlobal || type == ILocal || type == IJmp || type == IJzr || type == IJnz || type == IAddr || type == ICall;
}

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
        out << intrStrings.at(type);
        if (hasArg(type)) out << " " << arg;
        out << "\n";
    }
};

struct InstrStream
{
    std::vector<Instr> instrs;

    void print(std::ostream& out) const
    {
        for (size_t i = 0; i < instrs.size(); ++i)
        {
            out << i << ": ";
            instrs[i].print(out);
        }
        out << "\n";
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

    std::map<std::string, std::vector<Definition>> defs;
    std::vector<std::set<std::string>> scopeDefs;

    std::map<ValueT, ValueT> labelAddrs;
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

void compileAddrExpr(const ASTNode& node, Env& env, InstrStream& instrStream);
void compileExpr(const ASTNode& node, Env& env, InstrStream& instrStream);
void compileStmt(const ASTNode& node, Env& env, InstrStream& instrStream);
void compileBody(const ASTNode& node, Env& env, InstrStream& instrStream);

void compileAddrExpr(const ASTNode& node, Env& env, InstrStream& instrStream)
{
    assert(node.isAddrExpr() && node.isComplete());

    switch (node.type)
    {
    case GetVar:
    {
        assert(node.children.empty());
        const Definition& def = env.getDef(node.name, node.line);
        errorAssert(def.type == DefGlobalVar || def.type == DefLocalVar, "Compile", "Cannot get value of function", node.line);
        instrStream.instrs.emplace_back(def.type == DefGlobalVar ? IGlobal : ILocal, def.addr);
        break;
    }

    case Subscript:
        assert(node.children.size() == 2);
        assert(node.children[0].isExpr());
        assert(node.children[1].isExpr());
        compileExpr(node.children[0], env, instrStream);
        compileExpr(node.children[1], env, instrStream);
        instrStream.instrs.emplace_back(IPush, sizeof(ValueT));
        instrStream.instrs.emplace_back(IBinOp, ArMul);
        instrStream.instrs.emplace_back(IBinOp, ArAdd);
        break;

    case DerefOp:
        assert(node.children.size() == 1);
        assert(node.children[0].isExpr());
        compileExpr(node.children[0], env, instrStream);
        break;

    default:
        errorAssert(false, "Compile", "Unsupported node type", node.line);
    }
}

void compileExpr(const ASTNode& node, Env& env, InstrStream& instrStream)
{
    assert(node.isExpr() && node.isComplete());

    if (node.isAddrExpr())
    {
        compileAddrExpr(node, env, instrStream);
        instrStream.instrs.emplace_back(ILoad);
        return;
    }

    switch (node.type)
    {
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
            compileExpr(node.children[0], env, instrStream);
            instrStream.instrs.emplace_back(node.oper == LogAnd ? IJzr : IJnz, endOfSecond);
            instrStream.instrs.emplace_back(IUnOp, LogNot);
            compileExpr(node.children[1], env, instrStream);
            env.setLabelAddr(endOfSecond, instrStream.instrs.size());
        }
        else
        {
            compileExpr(node.children[0], env, instrStream);
            compileExpr(node.children[1], env, instrStream);
        }
        instrStream.instrs.emplace_back(IBinOp, node.oper);
        break;

    case UnOperator:
        assert(node.children.size() == 1);
        assert(node.children[0].isExpr());
        compileExpr(node.children[0], env, instrStream);
        instrStream.instrs.emplace_back(IUnOp, node.oper);
        break;

    case AddrOfOp:
        assert(node.children.size() == 1);
        assert(node.children[0].isAddrExpr());
        compileAddrExpr(node.children[0], env, instrStream);
        break;

    case Application:
    {
        const Definition& def = env.getDef(node.name, node.line);
        errorAssert(def.type == DefFun, "Compile", "Cannot call variable", node.line);
        errorAssert(def.params == (ValueT) node.children.size(), "Compile", "Wrong number of arguments in function call", node.line);
        for (const ASTNode& child : node.children)
        {
            assert(child.isExpr());
            compileExpr(child, env, instrStream);
        }
        instrStream.instrs.emplace_back(IAddr, def.addr);
        instrStream.instrs.emplace_back(ICall, node.children.size());
        break;
    }

    default:
        errorAssert(false, "Compile", "Unsupported node type", node.line);
    }
}

void compileStmt(const ASTNode& node, Env& env, InstrStream& instrStream)
{
    assert(node.isStmt() && node.isComplete());

    switch (node.type)
    {
    case IgnoreValue:
        assert(node.children.size() == 1);
        assert(node.children[0].isExpr());
        compileExpr(node.children[0], env, instrStream);
        instrStream.instrs.emplace_back(IPop);
        break;

    case Assignment:
        assert(node.children.size() == 2);
        assert(node.children[0].isAddrExpr());
        assert(node.children[1].isExpr());
        compileAddrExpr(node.children[0], env, instrStream);
        compileExpr(node.children[1], env, instrStream);
        instrStream.instrs.emplace_back(IStore);
        break;

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
                compileExpr(child, env, instrStream);
                instrStream.instrs.emplace_back(IJzr, endOfThen);
            }
            else if (i % 2 == 1)
            {
                assert(child.isBody());
                compileBody(child, env, instrStream);
                instrStream.instrs.emplace_back(IJmp, endOfIf);
                env.setLabelAddr(endOfThen, instrStream.instrs.size());
            }
            else
            {
                assert(child.isBody());
                compileBody(child, env, instrStream);
                env.setLabelAddr(endOfIf, instrStream.instrs.size());
            }
        }
        break;
    }

    case WhileDo:
    {
        assert(node.children.size() == 2);
        assert(node.children[0].isExpr());
        assert(node.children[1].isBody());
        ValueT condOfWhile = env.newLabel();
        ValueT startOfWhile = env.newLabel();
        instrStream.instrs.emplace_back(IJmp, condOfWhile);
        env.setLabelAddr(startOfWhile, instrStream.instrs.size());
        compileBody(node.children[1], env, instrStream);
        env.setLabelAddr(condOfWhile, instrStream.instrs.size());
        compileExpr(node.children[0], env, instrStream);
        instrStream.instrs.emplace_back(IJnz, startOfWhile);
        break;
    }

    case ReturnVal:
        assert(node.children.size() == 1);
        assert(node.children[0].isExpr());
        compileExpr(node.children[0], env, instrStream);
        instrStream.instrs.emplace_back(IReturn);
        break;

    case VarDef:
        assert(node.children.size() == 1);
        assert(node.children[0].isExpr());
        compileExpr(node.children[0], env, instrStream);
        break;

    case FunDef:
        assert(node.children.size() == 1);
        assert(node.children[0].isBody());
        env.pushFrame(node.paramNames.size());
        env.pushScope();
        for (size_t i = 0; i < node.paramNames.size(); ++i)
        {
            env.defineVar(node.paramNames[i], DefLocalVar, node.line);
        }
        env.defineVar("(return address)", DefLocalVar, node.line);
        env.defineVar("(old base pointer)", DefLocalVar, node.line);
        env.defineVar("(old stack pointer)", DefLocalVar, node.line);
        compileBody(node.children[0], env, instrStream);
        instrStream.instrs.emplace_back(IPush, 0);
        instrStream.instrs.emplace_back(IReturn);
        env.popScope();
        env.popFrame(node.paramNames.size());
        break;

    default:
        errorAssert(false, "Compile", "Unsupported node type", node.line);
    }
}

void compileBody(const ASTNode& node, Env& env, InstrStream& instrStream)
{
    int numVars = 0;
    env.pushScope();
    for (const ASTNode& child : node.children)
    {
        assert(!child.isExpr() && child.type != Body && child.type != Program);
        errorAssert(child.type != FunDef, "Compile", "Illegal nested function definition", child.line);
        compileStmt(child, env, instrStream);
        if (child.type == VarDef)
        {
            ++numVars;
            env.defineVar(child.name, DefLocalVar, child.line);
        }
    }
    env.popScope();
    std::fill_n(std::back_inserter(instrStream.instrs), numVars, Instr(IPop));
}

InstrStream compileProgram(const ASTNode& node)
{
    assert(node.isProgram() && node.isComplete());

    Env env;
    InstrStream instrStream;

    env.pushFrame(0);
    env.pushScope();

    env.defineFun("alloc", env.newLabel(), 1, node.line);
    env.defineFun("free", env.newLabel(), 1, node.line);
    env.defineFun("read", env.newLabel(), 0, node.line);
    env.defineFun("print", env.newLabel(), 1, node.line);
    env.defineFun("flush", env.newLabel(), 0, node.line);
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
        compileStmt(child, env, instrStream);
        if (child.type == VarDef) env.defineVar(child.name, DefGlobalVar, child.line);
    }

    errorAssert(env.defs.find("main") != env.defs.end(), "Compile", "`main` must be defined", node.line);
    const Definition& def = env.getDef("main", node.line);
    errorAssert(def.type == DefFun, "Compile", "`main` must be a function", node.line);
    errorAssert(def.params == 0, "Compile", "`main` must take no arguments", node.line);
    instrStream.instrs.emplace_back(IAddr, def.addr);
    instrStream.instrs.emplace_back(ICall, 0);
    instrStream.instrs.emplace_back(IExit);

    env.setLabelAddr(env.getDef("alloc", node.line).addr, instrStream.instrs.size());
    instrStream.instrs.emplace_back(ILocal, -1);
    instrStream.instrs.emplace_back(ILoad);
    instrStream.instrs.emplace_back(IAlloc);
    instrStream.instrs.emplace_back(IReturn);

    env.setLabelAddr(env.getDef("free", node.line).addr, instrStream.instrs.size());
    instrStream.instrs.emplace_back(ILocal, -1);
    instrStream.instrs.emplace_back(ILoad);
    instrStream.instrs.emplace_back(IFree);
    instrStream.instrs.emplace_back(IReturn);

    env.setLabelAddr(env.getDef("read", node.line).addr, instrStream.instrs.size());
    instrStream.instrs.emplace_back(IRead);
    instrStream.instrs.emplace_back(IReturn);

    env.setLabelAddr(env.getDef("print", node.line).addr, instrStream.instrs.size());
    instrStream.instrs.emplace_back(ILocal, -1);
    instrStream.instrs.emplace_back(ILoad);
    instrStream.instrs.emplace_back(IPrint);
    instrStream.instrs.emplace_back(IPush, 0);
    instrStream.instrs.emplace_back(IReturn);

    env.setLabelAddr(env.getDef("flush", node.line).addr, instrStream.instrs.size());
    instrStream.instrs.emplace_back(IFlush);
    instrStream.instrs.emplace_back(IPush, 0);
    instrStream.instrs.emplace_back(IReturn);

    env.setLabelAddr(env.getDef("exit", node.line).addr, instrStream.instrs.size());
    instrStream.instrs.emplace_back(ILocal, -1);
    instrStream.instrs.emplace_back(ILoad);
    instrStream.instrs.emplace_back(IExit);
    instrStream.instrs.emplace_back(IPush, 0);
    instrStream.instrs.emplace_back(IReturn);

    for (const ASTNode& child : node.children)
    {
        if (child.type != FunDef) continue;
        env.setLabelAddr(env.getDef(child.name, child.line).addr, instrStream.instrs.size());
        compileStmt(child, env, instrStream);
    }

    env.popScope();
    env.popFrame(0);

    for (Instr& instr : instrStream.instrs)
    {
        if (instr.type != IAddr && instr.type != IJmp && instr.type != IJzr && instr.type != IJnz) continue;
        instr.arg = env.getLabelAddr(instr.arg);
    }

    return instrStream;
}

const ValueT STACK_SIZE = 8 * 1024 * 1024;

ValueT executeProgram(const InstrStream& instrStream, std::istream& in, std::ostream& out)
{
    static_assert(sizeof(ValueT) == sizeof(ValueT*));
    static_assert(sizeof(ValueT) == sizeof(const Instr*));

    std::unique_ptr<ValueT[]> stack = std::make_unique<ValueT[]>(STACK_SIZE);
    
    const Instr* pcStart = instrStream.instrs.data();
    const Instr* pc = pcStart;

    ValueT* gp = stack.get();
    ValueT* bp = stack.get();
    ValueT* sp = stack.get();

    while (true)
    {
        const Instr& instr = *pc++;

        switch (instr.type)
        {
        case IPush:
            *sp++ = instr.arg;
            break;

        case IPop:
            --sp;
            break;

        case IBinOp:
        {
            ValueT right = *--sp;
            ValueT left = *--sp;
            switch (instr.arg)
            {
            case ArAdd:
                *sp++ = left + right;
                break;

            case ArSub:
                *sp++ = left - right;
                break;

            case ArMul:
                *sp++ = left * right;
                break;

            case ArDiv:
                *sp++ = left / right;
                break;

            case ArMod:
                *sp++ = left % right;
                break;

            case CmpEq:
                *sp++ = left == right;
                break;

            case CmpNeq:
                *sp++ = left != right;
                break;

            case CmpLt:
                *sp++ = left < right;
                break;

            case CmpGt:
                *sp++ = left > right;
                break;

            case CmpLeq:
                *sp++ = left <= right;
                break;

            case CmpGeq:
                *sp++ = left >= right;
                break;

            case LogAnd:
                *sp++ = left && right;
                break;

            case LogOr:
                *sp++ = left || right;
                break;

            case BitAnd:
                *sp++ = left & right;
                break;

            case BitOr:
                *sp++ = left | right;
                break;

            case BitXor:
                *sp++ = left ^ right;
                break;

            case BitLShift:
                *sp++ = left << right;
                break;

            case BitRShift:
                *sp++ = left >> right;
                break;

            default:
                errorAssert(false, "Runtime", "Unsupported binary operator", pc - pcStart - 1);
            }
            break;
        }

        case IUnOp:
        {
            ValueT val = *--sp;
            switch (instr.arg)
            {
            case UnPlus:
                *sp++ = +val;
                break;

            case UnMinus:
                *sp++ = -val;
                break;

            case LogNot:
                *sp++ = !val;
                break;

            case BitNot:
                *sp++ = ~val;
                break;

            default:
                errorAssert(false, "Runtime", "Unsupported unary operator", pc - pcStart - 1);
            }
            break;
        }

        case IGlobal:
            *sp++ = reinterpret_cast<ValueT>(gp + instr.arg);
            break;

        case ILocal:
            *sp++ = reinterpret_cast<ValueT>(bp + instr.arg);
            break;

        case ILoad:
        {
            ValueT ptr = *--sp;
            *sp++ = *reinterpret_cast<ValueT*>(ptr);
            break;
        }

        case IStore:
        {
            ValueT val = *--sp;
            ValueT ptr = *--sp;
            *reinterpret_cast<ValueT*>(ptr) = val;
            break;
        }

        case IJmp:
            pc = pcStart + instr.arg;
            break;

        case IJzr:
        {
            ValueT val = *--sp;
            if (val == 0) pc = pcStart + instr.arg;
            break;
        }

        case IJnz:
        {
            ValueT val = *--sp;
            if (val != 0) pc = pcStart + instr.arg;
            break;
        }

        case IAddr:
            *sp++ = reinterpret_cast<ValueT>(pcStart + instr.arg);
            break;

        case ICall:
        {
            const Instr* newPc = reinterpret_cast<const Instr*>(*--sp);
            ValueT* newBp = reinterpret_cast<ValueT*>(sp);
            *sp++ = reinterpret_cast<ValueT>(pc);
            *sp++ = reinterpret_cast<ValueT>(bp);
            *sp++ = reinterpret_cast<ValueT>(newBp - instr.arg);
            pc = newPc;
            bp = newBp;
            break;
        }

        case IReturn:
        {
            ValueT val = *--sp;
            ValueT* oldBp = reinterpret_cast<ValueT*>(bp);
            pc = reinterpret_cast<const Instr*>(oldBp[0]);
            bp = reinterpret_cast<ValueT*>(oldBp[1]);
            sp = reinterpret_cast<ValueT*>(oldBp[2]);
            *sp++ = val;
            break;
        }

        case IAlloc:
        {
            ValueT len = *--sp;
            *sp++ = reinterpret_cast<ValueT>(new ValueT[len]);
            break;
        }

        case IFree:
        {
            ValueT ptr = *--sp;
            delete[] reinterpret_cast<ValueT*>(ptr);
            break;
        }

        case IRead:
            in >> *sp++;
            break;

        case IPrint:
        {
            out << *--sp << "\n";
            break;
        }

        case IFlush:
            out << std::flush;
            break;

        case IExit:
        {
            out << std::flush;
            return *--sp;
            break;
        }

        default:
            errorAssert(false, "Runtime", "Unsupported instruction type", pc - pcStart - 1);
        }
    }
}

int main(int argc, char *argv[])
{
    std::ios::sync_with_stdio(false);
    std::cin.tie(nullptr);

    std::string fileName;
    if (argc >= 2) fileName = argv[1];
    else std::cin >> fileName;

    std::ifstream file(fileName);

    TokenStream tokenStream = lexProgram(file);

    std::cerr << "Lexed program:\n\n";
    tokenStream.print(std::cerr);

    ASTNode astRoot = parseProgram(tokenStream);

    std::cerr << "Parsed program:\n\n";
    astRoot.print(std::cerr);

    InstrStream instrStream = compileProgram(astRoot);

    std::cerr << "Compiled program:\n\n";
    instrStream.print(std::cerr);

    std::cerr << "Program IO:\n\n";

    ValueT retCode = executeProgram(instrStream, std::cin, std::cout);

    std::cerr << "\nProgram return code:\n\n";
    std::cerr << retCode << "\n";

    return 0;
}
