#include <algorithm>
#include <cassert>
#include <fstream>
#include <iostream>
#include <map>
#include <memory>
#include <set>
#include <string>
#include <vector>

void errorAssert(bool cond, const std::string& kind, const std::string& descr, int line = -1)
{
    if (cond) return;
    std::cerr << kind << " error at line " << line << ": " << descr << std::endl;
    exit(1);
}

typedef long long WordT;

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

bool isname(char c)
{
    return isalpha(c) || c == '_';
}

struct Token
{
    TokenKind kind;
    std::string name;
    WordT number;
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
        else if (isname(str[0]))
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
        else if (isname(c)) cSet = WORD; 
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
    WildcardPtrT,
    PointerT,
    ArrayT,
    TupleT,
    FunctionT,
    CustomT
};

const std::map<TypeKind, std::string> baseTypeStrings = {
    {Int64T, "int"},
    {WildcardPtrT, "wildcardPtr"}
};

const std::map<std::string, TypeKind> baseTypeMap = computeInverseMap(baseTypeStrings);

struct TypeNode
{
    TypeKind kind;
    std::string name;
    std::vector<TypeNode> children;
    WordT arrLen;
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

    TypeNode(TypeKind kind)
        : kind(kind)
        , remaining(0)
    {}

    TypeNode(TypeKind kind, std::vector<TypeNode>&& children)
        : kind(kind)
        , children(std::move(children))
        , remaining(0)
    {}

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

    bool isValuePtr() const
    {
        return kind == PointerT && children[0].isValue();
    }

    bool isSubscriptable() const
    {
        return ((kind == PointerT || kind == ArrayT) && children[0].isValue()) || kind == TupleT;
    }

    bool isCopyable() const
    {
        return kind != FunctionT && kind != ArrayT && kind != TupleT;
    }

    bool structMatch(const TypeNode& other) const
    {
        if (kind != other.kind) return false;
        if (kind == CustomT && name != other.name) return false;
        if (kind == ArrayT && arrLen != other.arrLen) return false;
        if (children.size() != other.children.size()) return false;
        for (size_t i = 0; i < children.size(); ++i)
        {
            if (!children[i].structMatch(other.children[i])) return false;
        }
        return true;
    }

    bool isConvertableTo(const TypeNode& other) const
    {
        if ((kind == PointerT && other.kind == WildcardPtrT) ||
            (kind == WildcardPtrT && other.kind == PointerT))
            return true;
        return structMatch(other);
    }

    bool isComplete() const
    {
        return kind != DummyT && remaining == 0;
    }

    WordT elemOffset(size_t idx) const
    {
        assert(kind == TupleT);
        assert(idx >= 0 && idx < children.size());
        WordT offset = 0;
        for (size_t i = 0; i < idx; ++i)
        {   
            offset += children[i].typeSize();
        }
        return offset;
    }

    WordT typeSize() const
    {
        switch (kind)
        {
        case Int64T:
        case WildcardPtrT:
        case PointerT:
            return sizeof(WordT);

        case ArrayT:
            return arrLen * children[0].typeSize();

        case TupleT:
        {
            WordT size = 0;
            for (size_t i = 0; i < children.size(); ++i)
            {   
                 size += children[i].typeSize();
            }
            return size;
        }

        default:
            assert(false);
            return 0;
        }
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
            out << "dummy";
            break;

        case PointerT:
            if (children.size() > 0) children[0].print(out);
            out << "*";
            break;

        case ArrayT:
            if (children.size() > 0) children[0].print(out);
            out << "[";
            if (children[0].isComplete()) out << arrLen;
            out << "]";
            break;

        case TupleT:
            out << "(";
            for (size_t i = 0; i < children.size(); ++i)
            {
                if (i > 0) out << ", ";
                children[i].print(out);
            }
            out << ")";
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
    IntrDef,
    Program
};

struct Definition;

struct Parameter
{
    std::string name;
    TypeNode type;
    Definition* def = nullptr;

    Parameter(const std::string& name)
        : name(name)
    {}
};

struct ASTNode
{
    ASTNodeKind kind;

    std::string name;
    WordT number;
    Operator oper;
    TypeNode type;
    std::vector<Parameter> params;
    Definition* def = nullptr;
    std::vector<ASTNode> children;
    int remaining;
    bool checkDone = false;

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

    ASTNode(ASTNodeKind kind, WordT number, int remaining, int line)
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
        return kind == GetVar || kind == Subscript || kind == DerefOp;
    }

    bool isExpr() const
    {
        return isAddrExpr() || kind == ConstNumber || kind == BinOperator || kind == UnOperator || kind == FunCall || kind == AddrOfOp;
    }

    bool isStmt() const
    {
        return kind == IgnoreValue || kind == Assignment || kind == IfThenElse || kind == WhileDo || kind == ReturnVal;
    }

    bool isDef() const
    {
        return kind == VarDef || kind == FunDef || kind == IntrDef;
    }

    bool isBody() const
    {
        return kind == Body;
    }

    bool isProgram() const
    {
        return kind == Program;
    }

    bool isComplete() const
    {
        return remaining == 0;
    }

    bool needsExpr() const
    {
        return (
            ((kind == UnOperator || kind == AddrOfOp || kind == DerefOp || kind == Bracketed || kind == IgnoreValue || kind == ReturnVal || kind == VarDef) && remaining == 1)
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
 
    static void printIndent(std::ostream& out, int indent)
    {
        while (indent--) out << "  ";
    }

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
            out << "(";
            out << operatorStrings.at(oper);
            if (children.size() > 0) children[0].print(out, indent + 1);
            out << ")";
            break;

        case FunCall:
            if (children.size() > 0) children[0].print(out, indent + 1);
            out << name << "(";
            for (size_t i = 1; i < children.size(); ++i)
            {
                if (i > 1) out << ", ";
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
            out << " " << name;
            if (children.size() > 0)
            {
                out << " = ";
                children[0].print(out, indent + 1);
            }
            out << ";" << "\n";
            break;

        case FunDef:
            printIndent(out, indent);
            out << "fun ";
            type.print(out);
            out << " " << name << "(";
            for (size_t i = 0; i < params.size(); ++i)
            {
                if (i > 0) out << ", ";
                out << params[i].name;
            }
            out << ") =" << "\n";
            if (children.size() > 0) children[0].print(out, indent + 1);
            printIndent(out, indent);
            out << "end" << "\n";
            break;

        case IntrDef:
            printIndent(out, indent);
            out << "intr ";
            type.print(out);
            out << " " << name << "(";
            for (size_t i = 0; i < params.size(); ++i)
            {
                if (i > 0) out << ", ";
                out << params[i].name;
            }
            out << ");" << "\n";
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
    return right;
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
                    if (!lastT.isComplete() && lastT.remaining >= -1)
                    {
                        typeStack.emplace_back(token.name);
                    }
                    else if (lastT.isComplete())
                    {
                        assert(typeStack.size() >= 2 && typeStack.front().kind == DummyT);
                        errorAssert(typeStack.size() == 2, "Parse", "Unexpected name in type", line);
                        last.type = std::move(lastT);
                        typeStack.clear();
                        last.name = token.name;
                        if (last.kind == VarDef) defState = DefAssign;
                        else if (last.kind == FunDef) defState = DefParamList;
                        else assert(false);
                    }
                    else errorAssert(false, "Parse", "Unexpected name in type", line);
                    break;

                case Op:
                {
                    errorAssert(isPointerOperator(token.oper), "Parse", "Unexpected operator in type", line);
                    errorAssert(lastT.isComplete(), "Parse", "Unexpected asterisk in type", line);
                    TypeNode baseType = std::move(lastT);
                    typeStack.pop_back();
                    typeStack.emplace_back(PointerT, 1);
                    typeStack.back().addChild(std::move(baseType));
                    break;
                }

                case LSub:
                {
                    errorAssert(lastT.isComplete(), "Parse", "Unexpected opening bracket square in type", line);
                    TypeNode baseType = std::move(lastT);
                    typeStack.pop_back();
                    typeStack.emplace_back(ArrayT, -3);
                    typeStack.back().addChild(std::move(baseType));
                    break;
                }

                case Number:
                    errorAssert(lastT.kind == ArrayT && lastT.remaining == -3, "Parse", "Unexpected number in type", line);
                    lastT.arrLen = token.number;
                    lastT.remaining = -2;
                    break;

                case RSub:
                    errorAssert(lastT.kind == ArrayT && lastT.remaining == -2, "Parse", "Unexpected closing square bracket in type", line);
                    lastT.remaining = 0;
                    break;

                case LBrack:
                {
                    if (!lastT.isComplete())
                    {
                        typeStack.emplace_back(TupleT, -1);
                    }
                    else
                    {
                        TypeNode outType = std::move(lastT);
                        typeStack.pop_back();
                        typeStack.emplace_back(FunctionT, -1);
                        TypeNode& lastT = typeStack.back();
                        lastT.addChild(std::move(outType));
                    }
                    break;
                }

                case Comma:
                {
                    errorAssert(lastT.isComplete(), "Parse", "Unexpected comma in type", line);
                    TypeNode inType = std::move(lastT);
                    typeStack.pop_back();
                    TypeNode& lastT = typeStack.back();
                    errorAssert(!lastT.isComplete(), "Parse", "Unexpected comma in type", line);
                    lastT.addChild(std::move(inType));
                    break;
                }

                case RBrack:
                {
                    if (lastT.isComplete())
                    {
                        TypeNode inType = std::move(lastT);
                        typeStack.pop_back();
                        TypeNode& lastT = typeStack.back();
                        errorAssert(!lastT.isComplete(), "Parse", "Unexpected closing bracket in type", line);
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
                if (last.params.empty())
                    errorAssert(token.kind == Name || token.kind == RBrack, "Parse", "Expected parameter name or closing bracket in defition", line);
                else errorAssert(token.kind == Name, "Parse", "Expected parameter name in defition", line);
                if (token.kind == Name)
                {
                    last.params.emplace_back(token.name);
                    defState = DefParamSep;
                }
                else if (token.kind == RBrack) defState = DefAssign;
                break;

            case DefParamSep:
                errorAssert(token.kind == Comma || token.kind == RBrack, "Parse", "Expected comma or closing bracket in defition", line);
                if (token.kind == Comma) defState = DefParam;
                else if (token.kind == RBrack) defState = DefAssign;
                break;

            case DefAssign:
                if (token.kind == Semicol && last.kind == VarDef)
                {
                    assert(last.remaining == 1);
                    --last.remaining;
                }
                else errorAssert(token.kind == Assign, "Parse", "Expected assignment in defition", line);
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

struct Definition
{
    TypeNode type;

    Definition(const TypeNode& type)
        : type(type)
    {}
};

struct DefEnv
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

    Definition* define(const std::string& name, const TypeNode& type, bool assertUnique = false, int line = -1)
    {
        if (assertUnique) 
        {
            assert(line >= 0);
            errorAssert(
                std::find(scopeDefs.back().begin(), scopeDefs.back().end(), name) == scopeDefs.back().end(),
                "Check", "Name already defined in score where this is illegal", line);
        }
        scopeDefs.back().push_back(name);
        allDefs.push_back(std::make_unique<Definition>(type));
        Definition* def = allDefs.back().get();
        defs[name].emplace_back(def);
        return def;
    }

    Definition* getDef(const std::string& name) const
    {
        auto it = defs.find(name);
        if (it == defs.end()) return nullptr;
        return it->second.back();
    }
};

void checkValExpr(ASTNode& node, DefEnv& dEnv);

void checkAddrExpr(ASTNode& node, DefEnv& dEnv)
{
    assert(node.isAddrExpr() && node.isComplete());

    if (node.checkDone) return;

    switch (node.kind)
    {
    case GetVar:
        assert(node.children.empty());
        node.def = dEnv.getDef(node.name);
        errorAssert(node.def != nullptr, "Check", "Undefined variable or function", node.line);
        node.type = node.def->type;
        break;

    case Subscript:
    {
        assert(node.children.size() == 2);
        checkValExpr(node.children[0], dEnv);
        errorAssert(node.children[0].type.isSubscriptable(), "Check", "Subscripting non-subsriptable type", node.line);
        checkValExpr(node.children[1], dEnv);
        errorAssert(node.children[1].type.isIntegral(), "Check", "Subscripting with non-integral type", node.line);
        switch (node.children[0].type.kind)
        {
        case ArrayT:
        {
            assert(node.children[0].type.children.size() == 1);
            ASTNode addrNode(AddrOfOp, AddrOf, 1, node.line);
            addrNode.addExpr(std::move(node.children[0]));
            addrNode.type = addrNode.children[0].type;
            addrNode.type.kind = PointerT;
            addrNode.checkDone = true;
            node.children[0] = std::move(addrNode);
        }

        case PointerT:
        {
            assert(node.children[0].type.children.size() == 1);
            node.kind = BinOperator;
            node.oper = ArAdd;
            ASTNode derefNode(DerefOp, Deref, 1, node.line);
            derefNode.addExpr(std::move(node));
            node = std::move(derefNode);
            checkValExpr(node, dEnv);
            break;
        }

        case TupleT:
        {
            errorAssert(node.children[1].kind == ConstNumber, "Check", "Subscripting tuple with non-constant subscript", node.line);
            WordT idx = node.children[1].number;
            errorAssert(idx >= 0 && idx < (WordT) node.children[0].type.children.size(), "Check", "Subscripting tuple with an out of bounds subscript", node.line);
            WordT offset = node.children[0].type.elemOffset(idx);
            TypeNode type = node.children[0].type.children[idx];
            ASTNode addrNode(AddrOfOp, AddrOf, 1, node.line);
            addrNode.addExpr(std::move(node.children[0]));
            addrNode.type = TypeNode(PointerT, {type});
            addrNode.checkDone = true;
            node.children[0] = std::move(addrNode);
            node.kind = BinOperator;
            node.oper = ArAdd;
            node.children[1].number = offset;
            node.type = node.children[0].type;
            node.checkDone = true;
            ASTNode derefNode(DerefOp, Deref, 1, node.line);
            derefNode.addExpr(std::move(node));
            node = std::move(derefNode);
            checkValExpr(node, dEnv);
            break;
        }

        default:
            assert(false);
        }
        break;
    }

    case DerefOp:
        assert(node.children.size() == 1);
        checkValExpr(node.children[0], dEnv);
        errorAssert(node.children[0].type.kind == PointerT, "Check", "Dereferencing non-pointer type", node.line);
        assert(node.children[0].type.children.size() == 1);
        node.type = node.children[0].type.children[0];
        break;

    default:
        errorAssert(false, "Check", "Unknown node kind", node.line);
    }

    node.checkDone = true;
}

void checkValExpr(ASTNode& node, DefEnv& dEnv)
{
    assert(node.isExpr() && node.isComplete());

    if (node.checkDone) return;

    if (node.isAddrExpr())
    {
        checkAddrExpr(node, dEnv);
        return;
    }

    switch (node.kind)
    {
    case ConstNumber:
        assert(node.children.empty());
        node.type = TypeNode(Int64T);
        break;

    case BinOperator:
        assert(node.children.size() == 2);
        checkValExpr(node.children[0], dEnv);
        checkValExpr(node.children[1], dEnv);
        if ((node.oper == ArAdd || node.oper == ArSub) && (node.children[0].type.isValuePtr() || node.children[1].type.isValuePtr()))
        {
            errorAssert(
                node.children[0].type.isIntegral() || node.children[1].type.isIntegral(),
                "Check", "Invalid operand type in binary operator", node.line);
            if (node.children[1].type.isValuePtr()) std::swap(node.children[0], node.children[1]);
            ASTNode mulNode(BinOperator, ArMul, 2, node.line);
            mulNode.addExpr(std::move(node.children[1]));
            ASTNode valNode(ConstNumber, node.children[0].type.children[0].typeSize(), 0, node.line);
            mulNode.addExpr(std::move(valNode));
            node.children[1] = std::move(mulNode);
            checkValExpr(node.children[1], dEnv);
        }
        else
        {
            errorAssert(node.children[0].type.isIntegral(), "Check", "Invalid operand type in binary operator", node.line);
            errorAssert(node.children[1].type.isIntegral(), "Check", "Invalid operand type in binary operator", node.line);
        }
        node.type = node.children[0].type;
        break;

    case UnOperator:
        assert(node.children.size() == 1);
        checkValExpr(node.children[0], dEnv);
        errorAssert(node.children[0].type.isIntegral(), "Check", "Invalid unary type in binary operator", node.line);
        node.type = node.children[0].type;
        break;

    case AddrOfOp:
        assert(node.children.size() == 1);
        checkAddrExpr(node.children[0], dEnv);
        node.type = TypeNode(PointerT, 1);
        node.type.addChild(node.children[0].type);
        break;

    case FunCall:
    {
        assert(node.children.size() >= 1);
        checkValExpr(node.children[0], dEnv);
        const TypeNode& funType = node.children[0].type;
        errorAssert(funType.isFunction(), "Check", "Calling non-callable type", node.line);
        assert(funType.children.size() >= 1);
        errorAssert(
            node.children.size() == funType.children.size(),
            "Check", "Number of arguments does not match number of parameters in function call", node.line);
        node.type = funType.children[0];
        for (size_t i = 1; i < node.children.size(); ++i)
        {
            checkValExpr(node.children[i], dEnv);
            errorAssert(
                node.children[i].type.isConvertableTo(funType.children[i]),
                "Check", "Argument type does not match parameter type in function call", node.line);
        }
        ASTNode addrNode(AddrOfOp, AddrOf, 1, node.line);
        addrNode.addExpr(std::move(node.children[0]));
        addrNode.type = TypeNode(PointerT, {addrNode.children[0].type});
        addrNode.checkDone = true;
        node.children[0] = std::move(addrNode);
        break;
    }

    default:
        errorAssert(false, "Check", "Unknown node kind", node.line);
    }

    node.checkDone = true;
}

void checkTypeRec(const TypeNode& node, DefEnv& dEnv, int line)
{
    assert(node.isComplete());

    switch (node.kind)
    {
    case DummyT:
        assert(false);
        break;

    case PointerT:
        assert(node.children.size() == 1);
        checkTypeRec(node.children[0], dEnv, line);
        break;

    case ArrayT:
        assert(node.children.size() == 1);
        errorAssert(node.children[0].isValue(), "Check", "Array element type is not a value", line);
        errorAssert(node.arrLen >= 0, "Check", "Array length is negative", line);
        checkTypeRec(node.children[0], dEnv, line);
        break;

    case TupleT:
        for (size_t i = 0; i < node.children.size(); ++i)
        {
            errorAssert(node.children[i].isValue(), "Check", "Tuple element type is not a value", line);
            checkTypeRec(node.children[i], dEnv, line);
        }
        break;

    case FunctionT:
        assert(node.children.size() >= 1);
        errorAssert(node.children[0].isValue(), "Check", "Function return type is not a value", line);
        errorAssert(node.children[0].isCopyable(), "Check", "Function return type is not copyable", line);
        for (size_t i = 1; i < node.children.size(); ++i)
        {
            errorAssert(node.children[i].isValue(), "Check", "Function parameter type is not a value", line);
            errorAssert(node.children[i].isCopyable(), "Check", "Function parameter type is not copyable", line);
            checkTypeRec(node.children[i], dEnv, line);
        }
        break;

    case CustomT:
        assert(node.children.size() == 0);
        errorAssert(false, "Check", "Custom types not yet supported", line);
        break;

    default:
        assert(node.children.size() == 0);
    }
}

void checkDefType(ASTNode& node, DefEnv& dEnv, bool assertUnique)
{
    assert(node.isDef() && node.isComplete());

    if (node.kind == VarDef) errorAssert(node.type.isValue(), "Check", "Type in variable definition is not a value type", node.line);
    else if (node.kind == FunDef)
    {
        errorAssert(node.type.isFunction(), "Check", "Type in function definition is not a function type", node.line);
        errorAssert(node.type.children.size() == node.params.size() + 1, "Parse", "Number of parameters in function definition does not match its type", node.line);
    }
    else if (node.kind == IntrDef) assert(node.type.isFunction());
    else assert(false);

    checkTypeRec(node.type, dEnv, node.line);

    node.def = dEnv.define(node.name, node.type, assertUnique, node.line);
}

void checkBody(ASTNode& node, DefEnv& dEnv);

void checkStmt(ASTNode& node, DefEnv& dEnv)
{
    assert((node.isStmt() || node.kind == VarDef) && node.isComplete());

    if (node.checkDone) return;

    switch (node.kind)
    {
    case IgnoreValue:
        assert(node.children.size() == 1);
        checkValExpr(node.children[0], dEnv);
        break;

    case Assignment:
    {
        assert(node.children.size() == 2);
        checkAddrExpr(node.children[0], dEnv);
        checkValExpr(node.children[1], dEnv);
        errorAssert(
            node.children[1].type.isConvertableTo(node.children[0].type),
            "Check", "RHS type does not match LHS type in assignment", node.line);
        errorAssert(node.children[0].type.isValue(), "Check", "Non-value type in assignment", node.line);
        errorAssert(node.children[0].type.isCopyable(), "Check", "Non-copyable type in assignment", node.line);
        break;
    }

    case IfThenElse:
    {
        assert(node.children.size() % 2 == 1 && node.children.size() >= 3);
        for (size_t i = 0; i < node.children.size(); ++i)
        {
            ASTNode& child = node.children[i];
            if (i % 2 == 0 && i < node.children.size() - 1) checkValExpr(child, dEnv);
            else if (i % 2 == 1) checkBody(child, dEnv);
            else checkBody(child, dEnv);
        }
        break;
    }

    case WhileDo:
    {
        assert(node.children.size() == 2);
        checkValExpr(node.children[0], dEnv);
        checkBody(node.children[1], dEnv);
        break;
    }

    case ReturnVal:
        assert(node.children.size() == 1);
        checkValExpr(node.children[0], dEnv);
        errorAssert(
            node.children[0].type.isConvertableTo(dEnv.retType),
            "Check", "Return value type does not match function return type", node.line);
        break;

    case VarDef:
        assert(node.children.size() <= 1);
        if (!node.children.empty())
        {
            checkValExpr(node.children[0], dEnv);
            errorAssert(
                node.children[0].type.isConvertableTo(node.type),
                "Check", "RHS type does not match LHS type in variable definition", node.line);
        }
        break;

    default:
        errorAssert(false, "Check", "Unknown node kind", node.line);
    }

    node.checkDone = true;
}

void checkBody(ASTNode& node, DefEnv& dEnv)
{
    assert(node.isBody() && node.isComplete());

    if (node.checkDone) return;

    dEnv.pushScope();
    for (ASTNode& child : node.children)
    {
        assert(child.isStmt() || child.isDef());
        errorAssert(child.kind != FunDef, "Check", "Illegal nested function definition", child.line);
        if (child.isDef()) checkDefType(child, dEnv, false);
        checkStmt(child, dEnv);
    }
    dEnv.popScope();

    node.checkDone = true;
}

void checkParams(ASTNode& node, DefEnv& dEnv)
{
    assert(node.kind == FunDef && node.isComplete());
    assert(node.type.isFunction());
    assert(node.type.children.size() == node.params.size() + 1);

    for (size_t i = 0; i < node.params.size(); ++i)
    {
        node.params[i].def = dEnv.define(node.params[i].name, node.type.children[i + 1], true, node.line);
        node.params[i].type = node.params[i].def->type;
    }
}

void checkFunDef(ASTNode& node, DefEnv& dEnv)
{
    assert(node.kind == FunDef && node.isComplete());
    assert(node.type.isFunction());
    assert(node.children.size() == 1);
    assert(node.type.children.size() >= 1);

    if (node.checkDone) return;

    dEnv.retType = node.type.children[0];
    dEnv.pushScope();
    checkParams(node, dEnv);
    checkBody(node.children[0], dEnv);
    dEnv.popScope();

    node.checkDone = true;
}

void checkAddIntrDef(ASTNode& node, DefEnv& dEnv, const std::string& name, const TypeNode& type)
{
    assert(node.isProgram() && node.isComplete());

    node.children.emplace_back(IntrDef, name, 0, node.line);
    node.children.back().type = type;

    node.children.back().checkDone = true;
}

void checkAddIntrinsics(ASTNode& node, DefEnv& dEnv)
{
    assert(node.isProgram() && node.isComplete());

    checkAddIntrDef(node, dEnv, "nullptr", TypeNode(FunctionT, {TypeNode(WildcardPtrT)}));
    checkAddIntrDef(node, dEnv, "alloc", TypeNode(FunctionT, {TypeNode(WildcardPtrT), TypeNode(Int64T)}));
    checkAddIntrDef(node, dEnv, "free", TypeNode(FunctionT, {TypeNode(Int64T), TypeNode(WildcardPtrT)}));
    checkAddIntrDef(node, dEnv, "read", TypeNode(FunctionT, {TypeNode(Int64T)}));
    checkAddIntrDef(node, dEnv, "print", TypeNode(FunctionT, {TypeNode(Int64T), TypeNode(Int64T)}));
    checkAddIntrDef(node, dEnv, "flush", TypeNode(FunctionT, {TypeNode(Int64T)}));
    checkAddIntrDef(node, dEnv, "exit", TypeNode(FunctionT, {TypeNode(Int64T), TypeNode(Int64T)}));
}

void checkMain(ASTNode& node, DefEnv& dEnv)
{
    assert(node.isProgram() && node.isComplete());

    Definition* mainDef = dEnv.getDef("main");
    errorAssert(mainDef != nullptr, "Check", "`main` function not defined", node.line);
    errorAssert(mainDef->type.structMatch(TypeNode(FunctionT, {TypeNode(Int64T)})), "Check", "`main` function has incorrect type", node.line);
    node.def = mainDef;
}

DefEnv checkProgram(ASTNode& node)
{
    assert(node.isProgram() && node.isComplete());
    assert(!node.checkDone);

    DefEnv dEnv;

    dEnv.pushScope();

    checkAddIntrinsics(node, dEnv);

    for (ASTNode& child : node.children)
    {
        assert(child.isStmt() || child.isDef());
        errorAssert(child.isDef(), "Check", "Illegal statement outside function body", child.line);
        checkDefType(child, dEnv, true);
    }

    for (ASTNode& child : node.children)
    {
        if (child.kind == FunDef) checkFunDef(child, dEnv);
        else if (child.kind == VarDef) checkStmt(child, dEnv);
    }

    checkMain(node, dEnv);

    dEnv.popScope();

    node.checkDone = true;

    return dEnv;
}

// return address and base pointer
const WordT extraFrameSize = 2 * sizeof(WordT);

enum InstrKind
{
    IPush,
    IPop,
    ISpAdd,
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
    {ISpAdd, "spAdd"},
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

struct Instr
{
    InstrKind kind;
    WordT arg;

    Instr(InstrKind kind)
        : kind(kind)
    {
        assert(!hasArg());
    }

    Instr(InstrKind kind, WordT arg)
        : kind(kind)
        , arg(arg)
    {
        assert(hasArg());
    }

    bool hasArg() const
    {
        return kind == IPush || kind == ISpAdd || kind == IBinOp || kind == IUnOp || kind == IGlobal || kind == ILocal
            || kind == IJmp || kind == IJzr || kind == IJnz || kind == IAddr || kind == IReturn;
    }

    void print(std::ostream& out) const
    {
        out << intrStrings.at(kind);
        if (hasArg()) out << " " << arg;
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

enum LocKind
{
    LocAddr,
    LocGlobal,
    LocLocal
};

struct Location
{
    LocKind kind;
    WordT addr;

    bool isValue() const
    {
        return kind == LocGlobal || kind == LocLocal;
    }

    bool isFunction() const
    {
        return kind == LocAddr;
    }

    Instr getAddrInstr() const
    {
        switch (kind)
        {
        case LocAddr:
            return {IAddr, addr};
        case LocGlobal:
            return {IGlobal, addr};
        case LocLocal:
            return {ILocal, addr};
        default:
            assert(false);
            return {IExit};
        }
    }
};

struct Locator
{
    std::map<Definition*, Location> locMap;
    std::map<WordT, WordT> labelAddrs;

    void setLoc(Definition* def, const Location& loc)
    {
        assert(locMap.find(def) == locMap.end());
        locMap.emplace(def, loc);
    }

    const Location& getLoc(Definition* def)
    {
        auto it = locMap.find(def);
        assert(it != locMap.end());
        return it->second;
    }

    void setLabelAddr(WordT label, WordT addr)
    {
        assert(labelAddrs.find(label) == labelAddrs.end());
        labelAddrs.emplace(label, addr);
    }

    WordT getLabelAddr(WordT label)
    {
        auto it = labelAddrs.find(label);
        assert(it != labelAddrs.end());
        return it->second;
    }
};

struct LocAllocator
{
    LocKind locKind;
    WordT frameSize;
    WordT currOffset;
    WordT currLabel = 0;

    void setFrame(LocKind locKind, WordT frameSize = 0)
    {
        this->locKind = locKind;
        this->frameSize = frameSize;
        currOffset = -frameSize;
    }

    Location pushVar(WordT varSize)
    {
        WordT oldOffset = currOffset;
        currOffset += varSize;
        return {locKind, oldOffset};
    }

    WordT newLabel()
    {
        return currLabel++;
    }

    Location labelLoc(WordT label)
    {
        return {LocAddr, label};
    }
};

struct CompileEnv
{
    Locator locator;
    LocAllocator locAllocator;
};

void compileValExpr(const ASTNode& node, InstrStream& instrStream, CompileEnv& cEnv);

void compileAddrExpr(const ASTNode& node, InstrStream& instrStream, CompileEnv& cEnv)
{
    assert(node.isAddrExpr() && node.isComplete());

    switch (node.kind)
    {
    case GetVar:
    {
        assert(node.children.empty());
        const Location& loc = cEnv.locator.getLoc(node.def);
        instrStream.instrs.emplace_back(std::move(loc.getAddrInstr()));
        break;
    }

    case DerefOp:
        assert(node.children.size() == 1);
        compileValExpr(node.children[0], instrStream, cEnv);
        break;

    default:
        errorAssert(false, "Compile", "Unknown node kind", node.line);
    }
}

void compileValExpr(const ASTNode& node, InstrStream& instrStream, CompileEnv& cEnv)
{
    assert(node.isExpr() && node.isComplete());
    assert(node.type.isCopyable());

    if (node.isAddrExpr())
    {
        compileAddrExpr(node, instrStream, cEnv);
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
        if (node.oper == LogAnd || node.oper == LogOr)
        {
            WordT endOfSecond = cEnv.locAllocator.newLabel();
            instrStream.instrs.emplace_back(IPush, node.oper == LogAnd ? 0 : 1);
            compileValExpr(node.children[0], instrStream, cEnv);
            instrStream.instrs.emplace_back(node.oper == LogAnd ? IJzr : IJnz, endOfSecond);
            instrStream.instrs.emplace_back(IUnOp, LogNot);
            compileValExpr(node.children[1], instrStream, cEnv);
            cEnv.locator.setLabelAddr(endOfSecond, instrStream.instrs.size());
        }
        else
        {
            compileValExpr(node.children[0], instrStream, cEnv);
            compileValExpr(node.children[1], instrStream, cEnv);
        }
        instrStream.instrs.emplace_back(IBinOp, node.oper);
        break;

    case UnOperator:
        assert(node.children.size() == 1);
        compileValExpr(node.children[0], instrStream, cEnv);
        instrStream.instrs.emplace_back(IUnOp, node.oper);
        break;

    case AddrOfOp:
        assert(node.children.size() == 1);
        compileAddrExpr(node.children[0], instrStream, cEnv);
        break;

    case FunCall:
        assert(node.children.size() >= 1);
        for (size_t i = 1; i < node.children.size(); ++i)
        {
            compileValExpr(node.children[i], instrStream, cEnv);
        }
        compileValExpr(node.children[0], instrStream, cEnv);
        instrStream.instrs.emplace_back(ICall);
        break;

    default:
        errorAssert(false, "Compile", "Unknown node kind", node.line);
    }
}

void compileBody(const ASTNode& node, InstrStream& instrStream, CompileEnv& cEnv);

void compileVarDef(const ASTNode& node, InstrStream& instrStream, CompileEnv& cEnv)
{
    assert(node.kind == VarDef && node.isComplete());

    cEnv.locator.setLoc(node.def, cEnv.locAllocator.pushVar(node.type.typeSize()));
}

void compileStmt(const ASTNode& node, InstrStream& instrStream, CompileEnv& cEnv)
{
    assert((node.isStmt() || node.kind == VarDef) && node.isComplete());

    switch (node.kind)
    {
    case IgnoreValue:
        assert(node.children.size() == 1);
        assert(node.children[0].isExpr());
        compileValExpr(node.children[0], instrStream, cEnv);
        instrStream.instrs.emplace_back(IPop);
        break;

    case Assignment:
        assert(node.children.size() == 2);
        compileAddrExpr(node.children[0], instrStream, cEnv);
        compileValExpr(node.children[1], instrStream, cEnv);
        instrStream.instrs.emplace_back(IStore);
        break;

    case IfThenElse:
    {
        assert(node.children.size() % 2 == 1 && node.children.size() >= 3);
        WordT endOfIf = cEnv.locAllocator.newLabel();
        WordT endOfThen;
        for (size_t i = 0; i < node.children.size(); ++i)
        {
            const ASTNode& child = node.children[i];
            if (i % 2 == 0 && i < node.children.size() - 1)
            {
                endOfThen = cEnv.locAllocator.newLabel();
                compileValExpr(child, instrStream, cEnv);
                instrStream.instrs.emplace_back(IJzr, endOfThen);
            }
            else if (i % 2 == 1)
            {
                compileBody(child, instrStream, cEnv);
                instrStream.instrs.emplace_back(IJmp, endOfIf);
                cEnv.locator.setLabelAddr(endOfThen, instrStream.instrs.size());
            }
            else
            {
                compileBody(child, instrStream, cEnv);
                cEnv.locator.setLabelAddr(endOfIf, instrStream.instrs.size());
            }
        }
        break;
    }

    case WhileDo:
    {
        assert(node.children.size() == 2);
        WordT condOfWhile = cEnv.locAllocator.newLabel();
        WordT startOfWhile = cEnv.locAllocator.newLabel();
        instrStream.instrs.emplace_back(IJmp, condOfWhile);
        cEnv.locator.setLabelAddr(startOfWhile, instrStream.instrs.size());
        compileBody(node.children[1], instrStream, cEnv);
        cEnv.locator.setLabelAddr(condOfWhile, instrStream.instrs.size());
        compileValExpr(node.children[0], instrStream, cEnv);
        instrStream.instrs.emplace_back(IJnz, startOfWhile);
        break;
    }

    case ReturnVal:
        assert(node.children.size() == 1);
        compileValExpr(node.children[0], instrStream, cEnv);
        instrStream.instrs.emplace_back(IReturn, cEnv.locAllocator.frameSize);
        break;

    case VarDef:
    {
        assert(node.children.size() <= 1);
        const Location& loc = cEnv.locator.getLoc(node.def);
        if (!node.children.empty())
        {
            instrStream.instrs.emplace_back(std::move(loc.getAddrInstr()));
            compileValExpr(node.children[0], instrStream, cEnv);
            instrStream.instrs.emplace_back(IStore);
        }
        break;
    }

    default:
        errorAssert(false, "Compile", "Unknown node kind", node.line);
    }
}

void compileBody(const ASTNode& node, InstrStream& instrStream, CompileEnv& cEnv)
{
    assert(node.isBody() && node.isComplete());

    for (const ASTNode& child : node.children)
    {
        if (child.kind == VarDef) compileVarDef(child, instrStream, cEnv);
        compileStmt(child, instrStream, cEnv);
    }
}

void compileFunMakeLabel(const ASTNode& node, InstrStream& instrStream, CompileEnv& cEnv)
{
    assert((node.kind == FunDef || node.kind == IntrDef) && node.isComplete());

    WordT label = cEnv.locAllocator.newLabel();
    cEnv.locator.setLoc(node.def, cEnv.locAllocator.labelLoc(label));
}

void compileFunSetLabel(const ASTNode& node, InstrStream& instrStream, CompileEnv& cEnv)
{
    assert((node.kind == FunDef || node.kind == IntrDef) && node.isComplete());

    WordT label = cEnv.locator.getLoc(node.def).addr;
    cEnv.locator.setLabelAddr(label, instrStream.instrs.size());
}

void compileParams(const ASTNode& node, InstrStream& instrStream, CompileEnv& cEnv)
{
    assert(node.kind == FunDef && node.isComplete());

    for (size_t i = 0; i < node.params.size(); ++i)
    {
        cEnv.locator.setLoc(node.params[i].def, cEnv.locAllocator.pushVar(node.params[i].type.typeSize()));
    }
}

void compileFunDef(const ASTNode& node, InstrStream& instrStream, CompileEnv& cEnv)
{
    assert(node.kind == FunDef && node.isComplete());
    assert(node.children.size() == 1);

    WordT frameSize = extraFrameSize;
    for (size_t i = 0; i < node.params.size(); ++i)
    {
        frameSize += node.params[i].type.typeSize();
    }

    compileFunSetLabel(node, instrStream, cEnv);
    cEnv.locAllocator.setFrame(LocLocal, frameSize);
    compileParams(node, instrStream, cEnv);
    cEnv.locAllocator.pushVar(extraFrameSize);
    size_t iSpAddAddr = instrStream.instrs.size();
    instrStream.instrs.emplace_back(ISpAdd, 0);
    compileBody(node.children[0], instrStream, cEnv);
    instrStream.instrs.emplace_back(IPush, 0);
    instrStream.instrs.emplace_back(IReturn, cEnv.locAllocator.frameSize);
    instrStream.instrs[iSpAddAddr].arg = cEnv.locAllocator.currOffset;
}

void compileIntrDef(const ASTNode& node, InstrStream& instrStream, CompileEnv& cEnv)
{
    assert(node.kind == IntrDef && node.isComplete());

    WordT largeFrameSize = TypeNode(Int64T).typeSize() + extraFrameSize;

    compileFunSetLabel(node, instrStream, cEnv);
    if (node.name == "nullptr")
    {
        instrStream.instrs.emplace_back(IPush, 0);
        instrStream.instrs.emplace_back(IReturn, extraFrameSize);
    }
    else if (node.name == "alloc")
    {
        instrStream.instrs.emplace_back(ILocal, -largeFrameSize);
        instrStream.instrs.emplace_back(ILoad);
        instrStream.instrs.emplace_back(IAlloc);
        instrStream.instrs.emplace_back(IReturn, largeFrameSize);
    }
    else if (node.name == "free")
    {
        instrStream.instrs.emplace_back(ILocal, -largeFrameSize);
        instrStream.instrs.emplace_back(ILoad);
        instrStream.instrs.emplace_back(IFree);
        instrStream.instrs.emplace_back(IPush, 0);
        instrStream.instrs.emplace_back(IReturn, largeFrameSize);
    }
    else if (node.name == "read")
    {
        instrStream.instrs.emplace_back(IRead);
        instrStream.instrs.emplace_back(IReturn, extraFrameSize);
    }
    else if (node.name == "print")
    {
        instrStream.instrs.emplace_back(ILocal, -largeFrameSize);
        instrStream.instrs.emplace_back(ILoad);
        instrStream.instrs.emplace_back(IPrint);
        instrStream.instrs.emplace_back(IPush, 0);
        instrStream.instrs.emplace_back(IReturn, largeFrameSize);
    }
    else if (node.name == "flush")
    {
        instrStream.instrs.emplace_back(IFlush);
        instrStream.instrs.emplace_back(IPush, 0);
        instrStream.instrs.emplace_back(IReturn, extraFrameSize);
    }
    else if (node.name == "exit")
    {
        instrStream.instrs.emplace_back(ILocal, -largeFrameSize);
        instrStream.instrs.emplace_back(ILoad);
        instrStream.instrs.emplace_back(IExit);
        instrStream.instrs.emplace_back(IPush, 0);
        instrStream.instrs.emplace_back(IReturn, largeFrameSize);
    }
    else assert(false);
}

void compileFixLabelAddrs(const ASTNode& node, InstrStream& instrStream, CompileEnv& cEnv)
{
    assert(node.isProgram() && node.isComplete());

    for (Instr& instr : instrStream.instrs)
    {
        if (instr.kind != IAddr && instr.kind != IJmp && instr.kind != IJzr && instr.kind != IJnz) continue;
        instr.arg = cEnv.locator.getLabelAddr(instr.arg);
    }
}

InstrStream compileProgram(const ASTNode& node)
{
    assert(node.isProgram() && node.isComplete());

    CompileEnv cEnv;
    InstrStream instrStream;

    cEnv.locAllocator.setFrame(LocGlobal);

    size_t iSpAddAddr = instrStream.instrs.size();
    instrStream.instrs.emplace_back(ISpAdd, 0);

    for (const ASTNode& child : node.children)
    {
        if (child.kind == VarDef) compileVarDef(child, instrStream, cEnv);
        else compileFunMakeLabel(child, instrStream, cEnv);
    }

    for (const ASTNode& child : node.children)
    {
        if (child.kind == VarDef) compileStmt(child, instrStream, cEnv);
    }
    instrStream.instrs.emplace_back(IAddr, cEnv.locator.getLoc(node.def).addr);
    instrStream.instrs.emplace_back(ICall);
    instrStream.instrs.emplace_back(IExit);

    instrStream.instrs[iSpAddAddr].arg = cEnv.locAllocator.currOffset;

    for (const ASTNode& child : node.children)
    {
        if (child.kind == FunDef) compileFunDef(child, instrStream, cEnv);
        else if (child.kind == IntrDef) compileIntrDef(child, instrStream, cEnv);
    }

    compileFixLabelAddrs(node, instrStream, cEnv);

    return instrStream;
}

const WordT STACK_SIZE = 8 * 1024 * 1024;

WordT executeProgram(const InstrStream& instrStream, std::istream& in, std::ostream& out)
{
    static_assert(sizeof(WordT) == sizeof(WordT*));

    std::unique_ptr<WordT[]> stack = std::make_unique<WordT[]>(STACK_SIZE);
    
    const Instr* pcStart = instrStream.instrs.data();
    const Instr* pc = pcStart;

    WordT* gp = stack.get();
    WordT* bp = stack.get();
    WordT* sp = stack.get();

    // std::cerr << "pc start: " << reinterpret_cast<WordT>(pcStart) << std::endl;
    // std::cerr << "sp start: " << reinterpret_cast<WordT>(stack.get()) << std::endl;
    // std::cerr << std::endl;

    while (true)
    {
        // std::cerr << "pc: " << pc - pcStart << std::endl;
        // std::cerr << "bp: " << bp - stack.get() << std::endl;
        // std::cerr << "sp: " << sp - stack.get() << std::endl;
        // std::cerr << "stack:" << std::endl;
        // for (WordT* ptr = stack.get(); ptr != sp; ++ptr) std::cerr << *ptr << std::endl;
        // std::cerr << std::endl;
        // std::cerr << "instr: ";
        // pc->print(std::cerr);
        // std::cerr << std::endl;

        const Instr& instr = *pc++;

        switch (instr.kind)
        {
        case IPush:
            *sp++ = instr.arg;
            break;

        case IPop:
            --sp;
            break;

        case ISpAdd:
            sp += instr.arg / sizeof(WordT);
            break;

        case IBinOp:
        {
            WordT right = *--sp;
            WordT left = *--sp;
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
            WordT val = *--sp;
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
            *sp++ = reinterpret_cast<WordT>(gp + instr.arg / sizeof(WordT));
            break;

        case ILocal:
            *sp++ = reinterpret_cast<WordT>(bp + instr.arg / sizeof(WordT));
            break;

        case ILoad:
        {
            WordT ptr = *--sp;
            *sp++ = *reinterpret_cast<WordT*>(ptr);
            break;
        }

        case IStore:
        {
            WordT val = *--sp;
            WordT ptr = *--sp;
            *reinterpret_cast<WordT*>(ptr) = val;
            break;
        }

        case IJmp:
            pc = pcStart + instr.arg;
            break;

        case IJzr:
        {
            WordT val = *--sp;
            if (val == 0) pc = pcStart + instr.arg;
            break;
        }

        case IJnz:
        {
            WordT val = *--sp;
            if (val != 0) pc = pcStart + instr.arg;
            break;
        }

        case IAddr:
            *sp++ = reinterpret_cast<WordT>(pcStart + instr.arg);
            break;

        case ICall:
        {
            const Instr* newPc = reinterpret_cast<const Instr*>(*--sp);
            *sp++ = reinterpret_cast<WordT>(pc);
            *sp++ = reinterpret_cast<WordT>(bp);
            pc = newPc;
            bp = sp;
            break;
        }

        case IReturn:
        {
            WordT val = *--sp;
            WordT* oldBp = reinterpret_cast<WordT*>(bp);
            pc = reinterpret_cast<const Instr*>(oldBp[-2]);
            bp = reinterpret_cast<WordT*>(oldBp[-1]);
            sp = oldBp - instr.arg / sizeof(WordT);
            *sp++ = val;
            break;
        }

        case IAlloc:
        {
            WordT len = *--sp;
            *sp++ = reinterpret_cast<WordT>(new WordT[len]);
            break;
        }

        case IFree:
        {
            WordT ptr = *--sp;
            delete[] reinterpret_cast<WordT*>(ptr);
            break;
        }

        case IRead:
            in >> *sp++;
            break;

        case IPrint:
            out << *--sp << "\n";
            out << std::flush;
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

    DefEnv dEnv = checkProgram(astRoot);

    std::cerr << "Checked program:\n\n";
    astRoot.print(std::cerr);

    InstrStream instrStream = compileProgram(astRoot);

    std::cerr << "Compiled program:\n\n" << std::flush;
    instrStream.print(std::cerr);

    std::cerr << "Program IO:\n\n";

    WordT retCode = executeProgram(instrStream, std::cin, std::cout);

    std::cerr << "\nProgram return code:\n\n";
    std::cerr << retCode << "\n";

    return retCode;
}
