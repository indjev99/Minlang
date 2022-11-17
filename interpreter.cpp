#include <algorithm>
#include <cassert>
#include <fstream>
#include <iostream>
#include <map>
#include <memory>
#include <set>
#include <string>
#include <vector>

void errorAssert(bool cond, const std::string& kind, const std::string& descr, int line)
{
    if (cond) return;
    std::cerr << kind << " error at line " << line << ": " << descr << std::endl;
    exit(0);
}

typedef long long ValueT;

template <class T>
std::map<std::string, T> computeInverseMap(const std::map<T, std::string>& tStrings)
{
    std::map<std::string, T> tMap;
    for (auto& [t, str] : tStrings)
    {
        tMap.emplace(str, t);
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
        operatorToUnMap.emplace(op, unOp);
    }
    return operatorToUnMap;
}

const std::map<Operator, Operator> operatorToUnMap = computeOperatorToUnMap();

enum TokenKind
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

const std::map<TokenKind, std::string> tokenStrings = {
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

const std::map<std::string, TokenKind> tokenMap = computeInverseMap(tokenStrings);

struct Token
{
    TokenKind kind;
    std::string name;
    ValueT number;
    Operator oper;
    int line;

    Token(TokenKind kind, int line)
        : kind(kind)
        , line(line)
    {}

    Token(const std::string& str, int line)
        : line(line)
    {
        assert(!str.empty());

        auto it = tokenMap.find(str);
        if (it != tokenMap.end()) kind = it->second;
        else if (isalpha(str[0]))
        {
            kind = Name;
            name = str;
        }
        else if (isdigit(str[0]))
        {
            kind = Number;
            number = stoll(str);
        }
        else
        {
            auto it = operatorMap.find(str);
            assert(it != operatorMap.end());
            kind = Op;
            oper = it->second;
        }
    }

    void print(std::ostream& out) const
    {
        switch (kind)
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
            out << tokenStrings.at(kind);
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
    int lineComment = 0;
    int nestedComment = 0;
    char prevC = ' ';

    TokenStream tokenStream;

    while (in.good())
    {
        char c = in.get();

        if (c < 0) continue;

        if (c == '#') ++lineComment;
        else if (c == '\n') lineComment = 0;

        bool comment = lineComment > 0 || nestedComment > 0;

        if (comment)
        {
            if (prevC == '#' && c == '>')
            {
                ++nestedComment;
                --lineComment;
            }
            else if (prevC == '<' && c == '#')
            {
                --nestedComment;
                --lineComment;
            }

            prevC = c;
        }

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

    if (curr != "")
    {
        if (currSymbSet == OPERATOR) lexOperators(tokenStream, curr, line);
        else tokenStream.tokens.emplace_back(curr, line);
    }

    tokenStream.tokens.emplace_back(Eof, line + 1);

    return tokenStream;
}

enum TypeKind
{
    DummyT,
    Int64T,
    PointerT,
    FunctionT,
    CustomT
};

const std::map<TypeKind, std::string> baseTypeStrings = {
    {Int64T, "int"}
};

const std::map<std::string, TypeKind> baseTypeMap = computeInverseMap(baseTypeStrings);

struct TypeNode
{
    TypeKind kind;
    std::string name;
    std::vector<TypeNode> children;
    int remaining;

    TypeNode()
        : kind(DummyT)
        , remaining(0)
    {}

    TypeNode(TypeKind kind, int remaining)
        : kind(kind)
        , remaining(remaining)
    {}

    TypeNode(const std::string name)
        : remaining(0)
    {
        auto it = baseTypeMap.find(name);
        if (it != baseTypeMap.end()) kind = it->second;
        else
        {
            kind = CustomT;
            this->name = name;
        }
    }

    TypeNode(const TypeNode& other) = default;
    TypeNode(TypeNode&& other) = default;

    TypeNode& operator=(const TypeNode& other) = default;
    TypeNode& operator=(TypeNode&& other) = default;

    bool isValue() const
    {
        return kind != FunctionT;
    }

    bool isFunction() const
    {
        return kind == FunctionT;
    }

    bool isIntegral() const
    {
        return kind == Int64T;
    }

    bool isSubscriptable() const
    {
        return kind == PointerT && children[0].isValue();
    }

    bool isDereferenceable() const
    {
        return kind == PointerT;
    }

    bool structMatch(const TypeNode& other) const
    {
        if (kind != other.kind) return false;
        if (kind == CustomT && name != other.name) return false;
        if (children.size() != other.children.size()) return false;
        for (size_t i = 0; i < children.size(); ++i)
        {
            if (!children[i].structMatch(other.children[i])) return false;
        }
        return true;
    }

    bool convertableTo(const TypeNode& other) const
    {
        // TODO: Laxer check
        return structMatch(other);
    }

    bool isComplete() const
    {
        return kind != DummyT && remaining == 0;
    }

    void addChild(const TypeNode& child)
    {
        assert(remaining != 0 && child.isComplete());
        if (remaining > 0) --remaining;
        children.emplace_back(child);
    }

    void addChild(TypeNode&& child)
    {
        assert(remaining != 0 && child.isComplete());
        if (remaining > 0) --remaining;
        children.emplace_back(std::move(child));
    }

    void print(std::ostream& out) const
    {
        switch (kind)
        {
        case DummyT:
            out << "DUMMY";
            break;

        case PointerT:
            if (children.size() > 0) children[0].print(out);
            out << "*";
            break;

        case FunctionT:
            if (children.size() > 0) children[0].print(out);
            out << "(";
            for (size_t i = 1; i < children.size(); ++i)
            {
                if (i > 1) out << ", ";
                children[i].print(out);
            }
            out << ")";
            break;

        case CustomT:
            out << name;
            break;

        default:
            out << baseTypeStrings.at(kind);
        }
    }
};

enum ASTNodeKind
{
    GetVar,
    ConstNumber,
    BinOperator,
    UnOperator,
    FunCall,
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

bool isAddrExpr(ASTNodeKind kind)
{
    return kind == GetVar || kind == Subscript || kind == DerefOp;
}

bool isExpr(ASTNodeKind kind)
{
    return isAddrExpr(kind) || kind == ConstNumber || kind == BinOperator || kind == UnOperator || kind == FunCall || kind == AddrOfOp;
}

bool isStmt(ASTNodeKind kind)
{
    return kind == IgnoreValue || kind == Assignment || kind == IfThenElse || kind == WhileDo || kind == ReturnVal;
}

bool isDef(ASTNodeKind kind)
{
    return kind == VarDef || kind == FunDef;
}

bool isBody(ASTNodeKind kind)
{
    return kind == Body;
}

bool isProgram(ASTNodeKind kind)
{
    return kind == Program;
}

void printIndent(std::ostream& out, int indent)
{
    while (indent--) out << "  ";
}

struct Definition;

struct ASTNode
{
    ASTNodeKind kind;

    std::string name;
    ValueT number;
    Operator oper;
    TypeNode type;
    std::vector<std::string> paramNames;

    Definition* def = nullptr;

    std::vector<ASTNode> children;
    int remaining;

    int line;

    ASTNode(ASTNodeKind kind, int remaining, int line)
        : kind(kind)
        , remaining(remaining)
        , line(line)
    {}

    ASTNode(ASTNodeKind kind, const std::string& name, int remaining, int line)
        : kind(kind)
        , name(name)
        , remaining(remaining)
        , line(line)
    {}

    ASTNode(ASTNodeKind kind, ValueT number, int remaining, int line)
        : kind(kind)
        , number(number)
        , remaining(remaining)
        , line(line)
    {}

    ASTNode(ASTNodeKind kind, Operator oper, int remaining, int line)
        : kind(kind)
        , oper(oper)
        , remaining(remaining)
        , line(line)
    {}

    ASTNode(const ASTNode& other) = default;
    ASTNode(ASTNode&& other) = default;

    ASTNode& operator=(const ASTNode& other) = default;
    ASTNode& operator=(ASTNode&& other) = default;

    bool isAddrExpr() const
    {
        return ::isAddrExpr(kind);
    }

    bool isExpr() const
    {
        return ::isExpr(kind);
    }

    bool isStmt() const
    {
        return ::isStmt(kind);
    }

    bool isDef() const
    {
        return ::isDef(kind);
    }

    bool isBody() const
    {
        return ::isBody(kind);
    }

    bool isProgram() const
    {
        return ::isProgram(kind);
    }

    bool isComplete() const
    {
        return remaining == 0;
    }

    bool needsExpr() const
    {
        return (
            ((kind == UnOperator || kind == Bracketed || kind == IgnoreValue || kind == ReturnVal || kind == VarDef) && remaining == 1)
            || ((kind == BinOperator || kind == Subscript || kind == Assignment) && (remaining == 2 || remaining == 1))
            || (kind == FunCall && remaining == -1)
            || (kind == IfThenElse && remaining == 3)
            || (kind == WhileDo && remaining == 2)
        );
    }

    bool needsBody() const
    {
        return (
            (kind == IfThenElse && (remaining == 2 || remaining == 1))
            || ((kind == WhileDo || kind == FunDef) && remaining == 1)
            || (kind == Program && remaining == -1)
        );
    }

    bool needsStmtOrDef() const
    {
        return kind == Body && remaining == -1;
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

    void addStmtOrDef(ASTNode&& stmtOrDef)
    {
        assert(needsStmtOrDef() && (stmtOrDef.isStmt() || stmtOrDef.isDef()));
        addChild(std::move(stmtOrDef));
    }

    void addBody(ASTNode&& body)
    {
        assert(needsBody() && body.isBody());
        addChild(std::move(body));
    }

    bool isEmptyNode() const
    {
        return kind == Body && remaining == 0 && children.size() == 0 && line == 0;
    }

    static const ASTNode emptyNode;

    void print(std::ostream& out, int indent = 0) const
    {
        switch (kind)
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

        case FunCall:
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
            out << "var ";
            type.print(out);
            out << " " << name << " = ";
            if (children.size() > 0) children[0].print(out, indent + 1);
            out << ";" << "\n";
            break;

        case FunDef:
            printIndent(out, indent);
            out << "fun ";
            type.print(out);
            out << " " << name << "(";
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
                if (i > 0 && (children[i].kind == FunDef || children[i - 1].kind == FunDef)) out << "\n";
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
        body.addStmtOrDef(std::move(nodeStack.back()));
        nodeStack.pop_back();
    }

    if (!nodeStack.back().needsBody()) return false;

    body.remaining = 0;
    std::reverse(body.children.begin(), body.children.end());
    nodeStack.back().addBody(std::move(body));

    return true;
}

void transformExpr(ASTNode& expr)
{
    assert(expr.isExpr() && expr.isComplete());
    if (expr.kind == UnOperator && expr.oper == AddrOf)
    {
        errorAssert(expr.children[0].isAddrExpr(), "Parse", "Unexpected ampersand operator", expr.line);
        expr.kind = AddrOfOp;
    }
    else if (expr.kind == UnOperator && expr.oper == Deref) expr.kind = DerefOp;
}

ASTNode popOperators(std::vector<ASTNode>& nodeStack, int prec, int line)
{
    ASTNode right = std::move(nodeStack.back());
    nodeStack.pop_back();

    if (!right.isComplete() || !right.isExpr()) return ASTNode::emptyNode;
    transformExpr(right);

    while ((nodeStack.back().kind == BinOperator || nodeStack.back().kind == UnOperator) && operatorPrec.at(nodeStack.back().oper) >= prec)
    {
        assert(!nodeStack.back().isComplete());
        nodeStack.back().addExpr(std::move(right));
        right = std::move(nodeStack.back());
        nodeStack.pop_back();
        transformExpr(right);
    }

    assert(right.isComplete() && right.isExpr());
    return std::move(right);
}

bool isPointerOperator(Operator oper)
{
    return operatorStrings.at(oper) == operatorStrings.at(Deref);
}

ASTNode parseProgram(const TokenStream& tokenStream)
{
    enum DefState
    {
        None,
        DefBegin,
        DefType,
        DefName,
        DefParamList,
        DefParam,
        DefParamSep,
        DefAssign
    };

    DefState defState = None;
    std::vector<ASTNode> nodeStack;
    std::vector<TypeNode> typeStack;

    nodeStack.emplace_back(Program, -1, 1);

    for (const Token& token : tokenStream.tokens)
    {
        assert(!nodeStack.empty());

        int line = token.line;
        ASTNode& last = nodeStack.back();

        if (defState != None)
        {
            assert(!nodeStack.empty());

            switch (defState)
            {
            case DefBegin:
                assert(typeStack.empty());
                typeStack.emplace_back(DummyT, -1);
                defState = DefType;

            case DefType:
            {
                TypeNode& lastT = typeStack.back();
                switch (token.kind)
                {
                case Name:
                    if (!lastT.isComplete())
                    {
                        typeStack.emplace_back(token.name);
                    }
                    else
                    {
                        assert(typeStack.size() >= 2 && typeStack.front().kind == DummyT);
                        errorAssert(typeStack.size() == 2, "Parse", "Unexpected name in type", line);
                        last.type = std::move(lastT);
                        typeStack.clear();
                        last.name = token.name;
                        if (last.kind == VarDef)
                        {
                            errorAssert(last.type.isValue(), "Parse", "Expected value type in variable definition", line);
                            defState = DefAssign;
                        }
                        else if (last.kind == FunDef)
                        {
                            errorAssert(last.type.isFunction(), "Parse", "Expected function type in fuction definition", line);
                            defState = DefParamList;
                        }
                        else assert(false);
                    }
                    break;

                case Op:
                {
                    errorAssert(isPointerOperator(token.oper), "Parse", "Unexpected operator in type", line);
                    errorAssert(lastT.isComplete(), "Parse", "Unexpected asterisk in type", line);
                    TypeNode baseType = std::move(typeStack.back());
                    typeStack.pop_back();
                    typeStack.emplace_back(PointerT, 1);
                    typeStack.back().addChild(std::move(baseType));
                    break;
                }

                case LBrack:
                {
                    errorAssert(lastT.isComplete(), "Parse", "Unexpected opening bracket in type", line);
                    TypeNode outType = std::move(typeStack.back());
                    typeStack.pop_back();
                    errorAssert(outType.isValue(), "Parse", "Expected value type in return type", line);
                    typeStack.emplace_back(FunctionT, -1);
                    TypeNode& lastT = typeStack.back();
                    lastT.addChild(std::move(outType));
                    break;
                }

                case Comma:
                {
                    errorAssert(lastT.isComplete(), "Parse", "Unexpected comma in type", line);
                    TypeNode inType = std::move(typeStack.back());
                    typeStack.pop_back();
                    TypeNode& lastT = typeStack.back();
                    errorAssert(!lastT.isComplete(), "Parse", "Unexpected comma in type", line);
                    errorAssert(inType.isValue(), "Parse", "Expected value type in parameter type", line);
                    lastT.addChild(std::move(inType));
                    break;
                }

                case RBrack:
                {
                    if (lastT.isComplete())
                    {
                        TypeNode inType = std::move(typeStack.back());
                        typeStack.pop_back();
                        TypeNode& lastT = typeStack.back();
                        errorAssert(!lastT.isComplete(), "Parse", "Unexpected closing bracket in type", line);
                        errorAssert(inType.isValue(), "Parse", "Expected value type in parameter type", line);
                        lastT.addChild(std::move(inType));
                    }
                    TypeNode& lastT = typeStack.back();
                    errorAssert(lastT.isComplete() || lastT.remaining == -1, "Parse", "Unexpected closing bracket in type", line);
                    lastT.remaining = 0;
                    break;
                }

                default:
                    errorAssert(false, "Parse", "Unexpected token in type", line);
                }

                break;
            }

            case DefName:
                errorAssert(token.kind == Name, "Parse", "Expected name in definition", line);
                break;

            case DefParamList:
                errorAssert(token.kind == LBrack, "Parse", "Expected opening bracket in definition", line);
                defState = DefParam;
                break;

            case DefParam:
                if (last.paramNames.empty())
                    errorAssert(token.kind == Name || token.kind == RBrack, "Parse", "Expected parameter name or closing bracket in defition", line);
                else errorAssert(token.kind == Name, "Parse", "Expected parameter name in defition", line);
                if (token.kind == Name)
                {
                    last.paramNames.push_back(token.name);
                    defState = DefParamSep;
                }
                else if (token.kind == RBrack) defState = DefAssign;
                break;

            case DefParamSep:
                errorAssert(token.kind == Comma || token.kind == RBrack, "Parse", "Expected comma or closing bracket in defition", line);
                if (token.kind == Comma) defState = DefParam;
                else if (token.kind == RBrack)
                {
                    defState = DefAssign;
                    errorAssert(last.type.children.size() == last.paramNames.size() + 1, "Parse", "Unexpected number of parameters in function definition", line);
                }
                break;

            case DefAssign:
                errorAssert(token.kind == Assign, "Parse", "Expected assignment in defition", line);
                defState = None;
                break;

            default:
                errorAssert(false, "Parse", "Unknown definition state", line);
            }
            continue;   
        }

        switch (token.kind)
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
                ((last.needsExpr() && (last.kind == Assignment || last.kind == ReturnVal || last.kind == VarDef))
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
            errorAssert(!expr.isEmptyNode() && !last.isComplete() && last.kind == FunCall, "Parse", "Unexpected comma", line);
            last.addExpr(std::move(expr));
            break;
        }

        case LBrack:
            if (last.isComplete() && last.isExpr())
            {
                ASTNode expr = popOperators(nodeStack, maxOperatorPrec, line);
                assert(!expr.isEmptyNode());
                nodeStack.emplace_back(FunCall, -1, line);
                nodeStack.back().addExpr(std::move(expr));
            }
            else
            {
                nodeStack.emplace_back(Bracketed, 1, line);
            }
            break;

        case RBrack:
        {
            if (!last.isComplete() && last.kind == FunCall && last.children.size() == 1)
            {
                last.remaining = 0;
                break;
            }

            ASTNode expr = popOperators(nodeStack, minOperatorPrec, line);
            ASTNode& last = nodeStack.back();
            errorAssert(
                !expr.isEmptyNode() && !last.isComplete() && (last.kind == Bracketed || last.kind == FunCall),
                "Parse", "Unexpected closing bracket", line);
            if (last.kind == Bracketed)
            {
                nodeStack.pop_back();
                nodeStack.emplace_back(std::move(expr));
            }
            else if (last.kind == FunCall)
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
            errorAssert(!expr.isEmptyNode() && !last.isComplete() && last.kind == Subscript, "Parse", "Unexpected closing square", line);
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
            errorAssert(!expr.isEmptyNode() && last.remaining == 3 && last.kind == IfThenElse, "Parse", "Unexpected then", line);
            last.addExpr(std::move(expr));
            break;
        }

        case Elif:
        {
            bool succ = popBody(nodeStack, line);
            ASTNode& last = nodeStack.back();
            errorAssert(succ && last.remaining == 1 && last.kind == IfThenElse, "Parse", "Unexpected elif", line);
            last.remaining += 2;
            break;
        }

        case Else:
        {
            bool succ = popBody(nodeStack, line);
            ASTNode& last = nodeStack.back();
            errorAssert(succ && last.remaining == 1 && last.kind == IfThenElse, "Parse", "Unexpected else", line);
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
            errorAssert(!expr.isEmptyNode() && last.remaining == 2 && last.kind == WhileDo, "Parse", "Unexpected do", line);
            last.addExpr(std::move(expr));
            break;
        }

        case End:
        {
            bool succ = popBody(nodeStack, line);
            ASTNode& last = nodeStack.back();
            if (succ && last.kind == IfThenElse && last.remaining == 1) last.addBody(ASTNode(Body, 0, line));
            errorAssert(succ && last.isComplete() && (last.kind == IfThenElse || last.kind == WhileDo || last.kind == FunDef), "Parse", "Unexpected end", line);
            break;
        }

        case Return:
            errorAssert(!last.isExpr() && (last.isComplete() || last.needsBody()), "Parse", "Unexpected return", line);
            nodeStack.emplace_back(ReturnVal, 1, line);
            break;

        case Var:
            errorAssert(!last.isExpr() && (last.isComplete() || last.needsBody()), "Parse", "Unexpected variable definition", line);
            nodeStack.emplace_back(VarDef, 1, line);
            defState = DefBegin;
            break;

        case Fun:
            errorAssert(!last.isExpr() && (last.isComplete() || last.needsBody()), "Parse", "Unexpected function definition", line);
            nodeStack.emplace_back(FunDef, 1, line);
            defState = DefBegin;
            break;

        case Eof:
        {
            bool succ = popBody(nodeStack, line);
            ASTNode& last = nodeStack.back();
            errorAssert(succ && last.kind == Program, "Parse", "Unexpected end of file", line);
            assert(last.children.size() == 1);
            if (last.children.back().kind == Body) last.children = std::move(last.children.back().children);
            last.remaining = 0;
            break;
        }

        default:
            errorAssert(false, "Parse", "Unknown token kind", line);
        }
    }

    return nodeStack.back();
}

enum LocKind
{
    DefFun,
    DefGlobalVar,
    DefLocalVar
};

struct Location
{
    LocKind kind;
    ValueT addr;
};

struct Definition
{
    TypeNode type;
    Location loc;

    Definition(const TypeNode& type)
        : type(type)
    {}
};

struct Env
{
    std::vector<std::unique_ptr<Definition>> allDefs;
    std::map<std::string, std::vector<Definition*>> defs;
    std::vector<std::vector<std::string>> scopeDefs;
    TypeNode retType;

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
            assert(!it->second.empty());
            it->second.pop_back();
            if (it->second.empty()) defs.erase(it);
        }
        scopeDefs.pop_back();
    }

    void define(const std::string& name, const TypeNode& type)
    {
        scopeDefs.back().push_back(name);
        allDefs.push_back(std::make_unique<Definition>(type));
        defs[name].emplace_back(allDefs.back().get());
    }

    Definition* getDef(const std::string& name)
    {
        auto it = defs.find(name);
        if (it == defs.end()) return nullptr;
        return it->second.back();
    }
};

void checkDef(ASTNode& node, Env& env)
{
    assert(node.isDef() && node.isComplete());
    assert((node.kind == FunDef) == (node.type.isFunction()));

    errorAssert(node.type.kind != CustomT, "Check", "Custom types not supported", node.line);
    env.define(node.name, node.type);
    node.def = env.getDef(node.name);
}

void checkParams(ASTNode& node, Env& env)
{
    assert(node.kind == FunDef && node.isComplete());
    assert(node.type.isFunction());
    assert(node.type.children.size() == node.paramNames.size() + 1);

    for (size_t i = 0; i < node.paramNames.size(); ++i)
    {
        env.define(node.paramNames[i], node.type.children[i + 1]);
    }
}

void checkExpr(ASTNode& node, Env& env);

void checkAddrExpr(ASTNode& node, Env& env)
{
    assert(node.isAddrExpr() && node.isComplete());

    switch (node.kind)
    {
    case GetVar:
        assert(node.children.empty());
        node.def = env.getDef(node.name);
        errorAssert(node.def != nullptr, "Check", "Undefined variable or function", node.line);
        node.type = node.def->type;
        break;

    case Subscript:
        assert(node.children.size() == 2);
        checkExpr(node.children[0], env);
        errorAssert(node.children[0].type.isSubscriptable(), "Check", "Subscripting non-subsriptable object", node.line);
        checkExpr(node.children[1], env);
        errorAssert(node.children[1].type.isIntegral(), "Check", "Subscripting with non-integral object", node.line);
        assert(node.children[0].type.children.size() == 1);
        node.type = node.children[0].type.children[0];
        break;

    case DerefOp:
        assert(node.children.size() == 1);
        checkExpr(node.children[0], env);
        errorAssert(node.children[0].type.isDereferenceable(), "Check", "Dereferencing non-dereferenceable object", node.line);
        assert(node.children[0].type.children.size() == 1);
        node.type = node.children[0].type.children[0];
        break;

    default:
        errorAssert(false, "Check", "Unknown node kind", node.line);
    }
}

void checkExpr(ASTNode& node, Env& env)
{
    assert(node.isExpr() && node.isComplete());

    if (node.isAddrExpr())
    {
        return checkAddrExpr(node, env);
    }

    switch (node.kind)
    {
    case ConstNumber:
        assert(node.children.empty());
        node.type = TypeNode(Int64T, 0);
        break;

    case BinOperator:
        assert(node.children.size() == 2);
        checkExpr(node.children[0], env);
        checkExpr(node.children[1], env);
        // TODO: Handling of multiple numeric types
        if (node.oper == ArAdd || node.oper == ArSub)
        {
            errorAssert(
                node.children[0].type.isIntegral() || node.children[0].type.isSubscriptable(),
                "Check", "Invalid binary operator on non-integral, non-subscriptable object", node.line);
        }
        else
        {
            errorAssert(node.children[0].type.isIntegral(), "Check", "Invalid binary operator on non-integral object", node.line);
        }
        errorAssert(node.children[1].type.isIntegral(), "Check", "Invalid binary operator on non-integral object", node.line);
        node.type = node.children[0].type;
        break;

    case UnOperator:
        std::cerr << node.oper << std::endl;
        node.print(std::cerr, 0);
        std::cerr << std::endl;
        assert(node.children.size() == 1);
        checkExpr(node.children[0], env);
        // TODO: Handling of multiple numeric types
        errorAssert(node.children[0].type.isIntegral(), "Check", "Invalid unary operator on non-integral object", node.line);
        node.type = node.children[0].type;
        break;

    case AddrOfOp:
        assert(node.children.size() == 1);
        checkAddrExpr(node.children[0], env);
        node.type = TypeNode(PointerT, 1);
        node.type.addChild(node.children[0].type);
        break;

    case FunCall:
    {
        assert(node.children.size() >= 1);
        checkExpr(node.children[0], env);
        errorAssert(node.children[0].type.isFunction(), "Check", "Calling non-callable object", node.line);
        const TypeNode& funType = node.children[0].type;
        assert(funType.children.size() >= 1);
        errorAssert(
            node.children.size() == funType.children.size(),
            "Check", "Number of arguments does not match number of parameters in function call", node.line);
        node.type = funType.children[0];
        for (size_t i = 1; i < node.children.size(); ++i)
        {
            checkExpr(node.children[i], env);
            errorAssert(
                node.children[i].type.convertableTo(funType.children[i]),
                "Check", "Argument type does not match parameter type in function call", node.line);
        }   
        break;
    }

    default:
        errorAssert(false, "Check", "Unknown node kind", node.line);
    }
}

void checkBody(ASTNode& node, Env& env);

void checkStmt(ASTNode& node, Env& env)
{
    assert((node.isStmt() || node.kind == VarDef) && node.isComplete());

    switch (node.kind)
    {
    case IgnoreValue:
        assert(node.children.size() == 1);
        assert(node.children[0].isExpr());
        checkExpr(node.children[0], env);
        break;

    case Assignment:
    {
        assert(node.children.size() == 2);
        checkAddrExpr(node.children[0], env);
        checkExpr(node.children[1], env);
        errorAssert(
            node.children[1].type.convertableTo(node.children[0].type),
            "Check", "RHS type does not match LHS type in assignment", node.line);
        break;
    }

    case IfThenElse:
    {
        assert(node.children.size() % 2 == 1 && node.children.size() >= 3);
        for (size_t i = 0; i < node.children.size(); ++i)
        {
            ASTNode& child = node.children[i];
            if (i % 2 == 0 && i < node.children.size() - 1)
            {
                assert(child.isExpr());
                checkExpr(child, env);
            }
            else if (i % 2 == 1)
            {
                assert(child.isBody());
                checkBody(child, env);
            }
            else
            {
                assert(child.isBody());
                checkBody(child, env);
            }
        }
        break;
    }

    case WhileDo:
    {
        assert(node.children.size() == 2);
        checkExpr(node.children[0], env);
        checkBody(node.children[1], env);
        break;
    }

    case ReturnVal:
        assert(node.children.size() == 1);
        checkExpr(node.children[0], env);
        errorAssert(
            node.children[0].type.convertableTo(env.retType),
            "Check", "Return value type does not match function return type", node.line);
        break;

    case VarDef:
        assert(node.children.size() == 1);
        checkDef(node, env);
        checkExpr(node.children[0], env);
        // TODO: Add way to not initialize or to have uniqueptr
        // errorAssert(
        //     node.children[0].type.convertableTo(node.type),
        //     "Check", "RHS type does not match LHS type in variable definition", node.line);
        break;

    default:
        errorAssert(false, "Check", "Unknown node kind", node.line);
    }
}

void checkBody(ASTNode& node, Env& env)
{
    assert(node.isBody() && node.isComplete());

    env.pushScope();
    for (ASTNode& child : node.children)
    {
        assert(child.isStmt() || child.isDef());
        errorAssert(child.kind != FunDef, "Check", "Illegal nested function definition", child.line);
        if (child.isDef()) checkDef(child, env);
        checkStmt(child, env);
    }
    env.popScope();
}

Env checkProgram(ASTNode& node)
{
    assert(node.isProgram() && node.isComplete());

    Env env;

    env.pushScope();

    // env.define("alloc", );
    // env.define("free", );
    // env.define("read", );
    // env.define("print", );
    // env.define("flush", );
    // env.define("exit", );

    for (ASTNode& child : node.children)
    {
        assert(child.isStmt() || child.isDef());
        errorAssert(child.isDef(), "Check", "Illegal statement outside function body", child.line);
        env.define(child.name, child.type);
    }

    for (ASTNode& child : node.children)
    {
        if (child.kind == FunDef)
        {
            assert(child.children.size() >= 1);
            assert(child.type.children.size() >= 1);
            env.retType = child.type.children[0];
            env.pushScope();
            checkParams(child, env);
            checkBody(child.children[0], env);
            env.popScope();
        }
        else checkStmt(child, env);
    }

    env.popScope();

    return env;
}

/*
enum InstrKind
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

std::map<InstrKind, std::string> intrStrings = {
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

bool hasArg(InstrKind kind)
{
    return kind == IPush || kind == IBinOp || kind == IUnOp || kind == IGlobal || kind == ILocal || kind == IJmp || kind == IJzr || kind == IJnz || kind == IAddr || kind == ICall;
}

struct Instr
{
    InstrKind kind;
    ValueT arg;

    Instr(InstrKind kind)
        : kind(kind)
    {}

    Instr(InstrKind kind, ValueT arg)
        : kind(kind)
        , arg(arg)
    {}

    void print(std::ostream& out) const
    {
        out << intrStrings.at(kind);
        if (hasArg(kind)) out << " " << arg;
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

enum DefKind
{
    DefFun,
    DefGlobalVar,
    DefLocalVar
};

struct Definition
{
    DefKind kind;
    ValueT addr;
    ValueT params;

    Definition(DefKind kind, ValueT addr)
        : kind(kind)
        , addr(addr)
    {}

    Definition(DefKind kind, ValueT addr, ValueT params)
        : kind(kind)
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
            if (it->second.back().kind == DefLocalVar || it->second.back().kind == DefGlobalVar) --frameOffsets.back();
            it->second.pop_back();
            if (it->second.empty()) defs.erase(it);
        }
        scopeDefs.pop_back();
    }

    void defineFun(const std::string& name, ValueT addr, ValueT params, int line)
    {
        errorAssert(scopeDefs.back().find(name) == scopeDefs.back().end(), "Compile", "Multiple definitions of function", line);
        defs.emplace(name, {});
        defs[name].emplace_back(DefFun, addr, params);
        scopeDefs.back().insert(name);
    }

    void defineVar(const std::string& name, DefKind kind, int line)
    {
        errorAssert(scopeDefs.back().find(name) == scopeDefs.back().end(), "Compile", "Multiple definitions of variable in the same scope", line);
        defs.checkProgram(name, {});
        defs[name].emplace_back(kind, frameOffsets.back()++);
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

    switch (node.kind)
    {
    case GetVar:
    {
        assert(node.children.empty());
        const Definition& def = env.getDef(node.name, node.line);
        errorAssert(def.kind == DefGlobalVar || def.kind == DefLocalVar, "Compile", "Cannot get value of function", node.line);
        instrStream.instrs.emplace_back(def.kind == DefGlobalVar ? IGlobal : ILocal, def.addr);
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
        errorAssert(false, "Compile", "Unknown node kind", node.line);
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

    switch (node.kind)
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

    case FunCall:
    {
        const Definition& def = env.getDef(node.name, node.line);
        errorAssert(def.kind == DefFun, "Compile", "Cannot call variable", node.line);
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
        errorAssert(false, "Compile", "Unknown node kind", node.line);
    }
}

void compileStmt(const ASTNode& node, Env& env, InstrStream& instrStream)
{
    assert(node.isStmt() && node.isComplete());

    switch (node.kind)
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
        errorAssert(false, "Compile", "Unknown node kind", node.line);
    }
}

void compileBody(const ASTNode& node, Env& env, InstrStream& instrStream)
{
    assert(node.isBody() && node.isComplete());

    int numVars = 0;
    env.pushScope();
    for (const ASTNode& child : node.children)
    {
        assert(!child.isExpr() && child.kind != Body && child.kind != Program);
        errorAssert(child.kind != FunDef, "Compile", "Illegal nested function definition", child.line);
        compileStmt(child, env, instrStream);
        if (child.kind == VarDef)
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
        assert(!child.isExpr() && child.kind != Body && child.kind != Program);
        errorAssert(child.kind == VarDef || child.kind == FunDef, "Compile", "Illegal statement outside function body", child.line);
        if (child.kind == FunDef) env.defineFun(child.name, env.newLabel(), child.paramNames.size(), child.line);
    }

    for (const ASTNode& child : node.children)
    {
        if (child.kind == FunDef) continue;
        compileStmt(child, env, instrStream);
        if (child.kind == VarDef) env.defineVar(child.name, DefGlobalVar, child.line);
    }

    errorAssert(env.defs.find("main") != env.defs.end(), "Compile", "`main` must be defined", node.line);
    const Definition& def = env.getDef("main", node.line);
    errorAssert(def.kind == DefFun, "Compile", "`main` must be a function", node.line);
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
        if (child.kind != FunDef) continue;
        env.setLabelAddr(env.getDef(child.name, child.line).addr, instrStream.instrs.size());
        compileStmt(child, env, instrStream);
    }

    env.popScope();
    env.popFrame(0);

    for (Instr& instr : instrStream.instrs)
    {
        if (instr.kind != IAddr && instr.kind != IJmp && instr.kind != IJzr && instr.kind != IJnz) continue;
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

        switch (instr.kind)
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
                errorAssert(false, "Runtime", "Unknown binary operator", pc - pcStart - 1);
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
                errorAssert(false, "Runtime", "Unknown unary operator", pc - pcStart - 1);
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
            out << *--sp << "\n";
            break;

        case IFlush:
            out << std::flush;
            break;

        case IExit:
            out << std::flush;
            return *--sp;

        default:
            errorAssert(false, "Runtime", "Unknown instruction kind", pc - pcStart - 1);
        }
    }
}
*/

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

    Env env = checkProgram(astRoot);

    std::cerr << "Checked program.\n\n";

    /*InstrStream instrStream = compileProgram(astRoot);

    std::cerr << "Compiled program:\n\n";
    instrStream.print(std::cerr);

    std::cerr << "Program IO:\n\n";

    ValueT retCode = executeProgram(instrStream, std::cin, std::cout);

    std::cerr << "\nProgram return code:\n\n";
    std::cerr << retCode << "\n";*/

    return 0;
}
