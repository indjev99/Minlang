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
    long long number;
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
            number = stoi(str);
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
    Sequence,
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
    long long number;
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

    ASTNode(ASTNodeType type, long long number, int remaining, int line)
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
            || ((type == Program || type == Sequence) && remaining == -1)
        );
    }

    void addChild(ASTNode&& child)
    {
        assert(remaining != 0);
        if (remaining > 0) --remaining;
        children.emplace_back(std::move(child));
    }

    void addExpr(ASTNode&& expr)
    {
        assert(needsExpr());
        addChild(std::move(expr));
    }

    void addBody(ASTNode&& body)
    {
        assert(needsBody());
        addChild(std::move(body));
    }

    bool isEmptyNode() const
    {
        return type == Sequence && remaining == 0 && children.size() == 0 && line == 0;
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

        case Sequence:
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
            for (const ASTNode& child : children)
            {
                child.print(out, indent);
                out << "\n";
            }
            break;
        }
    }
};

const ASTNode ASTNode::emptyNode(Sequence, 0, 0);

bool popBody(std::vector<ASTNode>& nodeStack, int line)
{
    ASTNode seq(Sequence, -1, line);
    while (nodeStack.back().isComplete() && !nodeStack.back().isExpr())
    {
        seq.addBody(std::move(nodeStack.back()));
        nodeStack.pop_back();
    }

    if (!nodeStack.back().needsBody()) return false;

    seq.remaining = 0;
    std::reverse(seq.children.begin(), seq.children.end());
    if (seq.children.size() == 1) seq = std::move(seq.children.back());
    nodeStack.back().addBody(std::move(seq));

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
                nodeStack.emplace_back(BinOperator, token.oper, 2, line);
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
                ((!last.isComplete() && (last.type == Assignment || last.type == ReturnVal || last.type == VarDef || last.type == FunDef))
                || (last.isComplete() && !last.isExpr()))
                , "Parse", "Unexpected semicolon", line
            );
            if (!last.isComplete()) last.addExpr(std::move(expr));
            else
            {
                nodeStack.emplace_back(IgnoreValue, 1, line);
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
            if (last.children.back().type == Sequence) last.children = std::move(last.children.back().children);
            last.remaining = 0;
            break;
        }
        }
    }

    return nodeStack.back();
}

int main()
{
    TokenStream tokenStream = lexProgram(std::cin);
    tokenStream.print(std::cerr);

    std::cerr << "\n";

    ASTNode astRoot = parseProgram(tokenStream);
    astRoot.print(std::cerr);

    return 0;
}
