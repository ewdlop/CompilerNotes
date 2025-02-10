using System;
using System.Collections.Generic;
using System.Reflection;
using System.Reflection.Emit;

namespace SimpleCompiler
{
    // Token types for our simple language
    public enum TokenType
    {
        Number,
        Plus,
        Minus,
        Multiply,
        Divide,
        LeftParen,
        RightParen,
        EOF
    }

    // Token class to hold lexical units
    public class Token
    {
        public TokenType Type { get; }
        public string Value { get; }
        public int Position { get; }

        public Token(TokenType type, string value, int position)
        {
            Type = type;
            Value = value;
            Position = position;
        }
    }

    // Lexer class to convert input string into tokens
    public class Lexer
    {
        private readonly string _input;
        private int _position;

        public Lexer(string input)
        {
            _input = input;
            _position = 0;
        }

        public Token GetNextToken()
        {
            while (_position < _input.Length)
            {
                char currentChar = _input[_position];

                // Skip whitespace
                if (char.IsWhiteSpace(currentChar))
                {
                    _position++;
                    continue;
                }

                // Parse numbers
                if (char.IsDigit(currentChar))
                {
                    string number = "";
                    int startPos = _position;

                    while (_position < _input.Length && char.IsDigit(_input[_position]))
                    {
                        number += _input[_position];
                        _position++;
                    }

                    return new Token(TokenType.Number, number, startPos);
                }

                // Parse operators
                switch (currentChar)
                {
                    case '+':
                        _position++;
                        return new Token(TokenType.Plus, "+", _position - 1);
                    case '-':
                        _position++;
                        return new Token(TokenType.Minus, "-", _position - 1);
                    case '*':
                        _position++;
                        return new Token(TokenType.Multiply, "*", _position - 1);
                    case '/':
                        _position++;
                        return new Token(TokenType.Divide, "/", _position - 1);
                    case '(':
                        _position++;
                        return new Token(TokenType.LeftParen, "(", _position - 1);
                    case ')':
                        _position++;
                        return new Token(TokenType.RightParen, ")", _position - 1);
                }

                throw new Exception($"Invalid character: {currentChar} at position {_position}");
            }

            return new Token(TokenType.EOF, "", _position);
        }
    }

    // Abstract class for AST nodes
    public abstract class AstNode
    {
        public abstract object Evaluate();
    }

    // Number node
    public class NumberNode : AstNode
    {
        private readonly double _value;

        public NumberNode(double value)
        {
            _value = value;
        }

        public override object Evaluate()
        {
            return _value;
        }
    }

    // Binary operation node
    public class BinaryOpNode : AstNode
    {
        private readonly AstNode _left;
        private readonly Token _op;
        private readonly AstNode _right;

        public BinaryOpNode(AstNode left, Token op, AstNode right)
        {
            _left = left;
            _op = op;
            _right = right;
        }

        public override object Evaluate()
        {
            var left = Convert.ToDouble(_left.Evaluate());
            var right = Convert.ToDouble(_right.Evaluate());

            switch (_op.Type)
            {
                case TokenType.Plus:
                    return left + right;
                case TokenType.Minus:
                    return left - right;
                case TokenType.Multiply:
                    return left * right;
                case TokenType.Divide:
                    if (right == 0)
                        throw new DivideByZeroException();
                    return left / right;
                default:
                    throw new Exception($"Invalid operator: {_op.Value}");
            }
        }
    }

    // Parser class to build AST
    public class Parser
    {
        private readonly Lexer _lexer;
        private Token _currentToken;

        public Parser(Lexer lexer)
        {
            _lexer = lexer;
            _currentToken = _lexer.GetNextToken();
        }

        private void Eat(TokenType tokenType)
        {
            if (_currentToken.Type == tokenType)
                _currentToken = _lexer.GetNextToken();
            else
                throw new Exception($"Expected {tokenType}, got {_currentToken.Type}");
        }

        public AstNode Parse()
        {
            return Expr();
        }

        private AstNode Expr()
        {
            var node = Term();

            while (_currentToken.Type == TokenType.Plus || _currentToken.Type == TokenType.Minus)
            {
                var token = _currentToken;
                if (token.Type == TokenType.Plus)
                    Eat(TokenType.Plus);
                else if (token.Type == TokenType.Minus)
                    Eat(TokenType.Minus);

                node = new BinaryOpNode(node, token, Term());
            }

            return node;
        }

        private AstNode Term()
        {
            var node = Factor();

            while (_currentToken.Type == TokenType.Multiply || _currentToken.Type == TokenType.Divide)
            {
                var token = _currentToken;
                if (token.Type == TokenType.Multiply)
                    Eat(TokenType.Multiply);
                else if (token.Type == TokenType.Divide)
                    Eat(TokenType.Divide);

                node = new BinaryOpNode(node, token, Factor());
            }

            return node;
        }

        private AstNode Factor()
        {
            var token = _currentToken;

            if (token.Type == TokenType.Number)
            {
                Eat(TokenType.Number);
                return new NumberNode(double.Parse(token.Value));
            }
            else if (token.Type == TokenType.LeftParen)
            {
                Eat(TokenType.LeftParen);
                var node = Expr();
                Eat(TokenType.RightParen);
                return node;
            }

            throw new Exception($"Invalid factor: {token.Value}");
        }
    }

    // IL Emitter class to generate and run .NET IL
    public class ILEmitter
    {
        private readonly AstNode _ast;
        private readonly DynamicMethod _dynamicMethod;
        private readonly ILGenerator _ilGenerator;

        public ILEmitter(AstNode ast)
        {
            _ast = ast;
            _dynamicMethod = new DynamicMethod(
                "Calculate",
                typeof(double),
                Type.EmptyTypes,
                typeof(ILEmitter).Module);
            _ilGenerator = _dynamicMethod.GetILGenerator();
        }

        public double EmitAndRun()
        {
            EmitExpression(_ast);
            _ilGenerator.Emit(OpCodes.Ret);

            var calculate = (Func<double>)_dynamicMethod.CreateDelegate(typeof(Func<double>));
            return calculate();
        }

        private void EmitExpression(AstNode node)
        {
            switch (node)
            {
                case NumberNode numberNode:
                    _ilGenerator.Emit(OpCodes.Ldc_R8, (double)numberNode.Evaluate());
                    break;

                case BinaryOpNode binaryOpNode:
                    EmitBinaryOperation(binaryOpNode);
                    break;

                default:
                    throw new Exception($"Unknown node type: {node.GetType()}");
            }
        }

        private void EmitBinaryOperation(BinaryOpNode node)
        {
            // Emit left and right operands
            EmitExpression(node._left);
            EmitExpression(node._right);

            // Emit the operation
            switch (node._op.Type)
            {
                case TokenType.Plus:
                    _ilGenerator.Emit(OpCodes.Add);
                    break;
                case TokenType.Minus:
                    _ilGenerator.Emit(OpCodes.Sub);
                    break;
                case TokenType.Multiply:
                    _ilGenerator.Emit(OpCodes.Mul);
                    break;
                case TokenType.Divide:
                    _ilGenerator.Emit(OpCodes.Div);
                    break;
                default:
                    throw new Exception($"Unknown operator: {node._op.Type}");
            }
        }
    }

    // Example usage
    public class Program
    {
        public static void Main()
        {
            string input = "2 + 3 * (4 - 1)";
            
            // Create lexer
            var lexer = new Lexer(input);
            
            // Create parser and parse input into AST
            var parser = new Parser(lexer);
            var ast = parser.Parse();
            
            // Create IL emitter and run the code
            var ilEmitter = new ILEmitter(ast);
            double result = ilEmitter.EmitAndRun();
            
            Console.WriteLine($"Result: {result}");
        }
    }
}
