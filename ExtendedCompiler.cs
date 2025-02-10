using System;
using System.Collections.Generic;
using System.Reflection;
using System.Reflection.Emit;
using System.Linq;

namespace ExtendedCompiler
{
    public enum TokenType
    {
        Number,
        String,
        Identifier,
        Plus,
        Minus,
        Multiply,
        Divide,
        LeftParen,
        RightParen,
        Comma,
        Dot,
        EOF
    }

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

                if (char.IsWhiteSpace(currentChar))
                {
                    _position++;
                    continue;
                }

                // Handle string literals
                if (currentChar == '"')
                {
                    _position++; // Skip opening quote
                    int startPos = _position;
                    string value = "";
                    
                    while (_position < _input.Length && _input[_position] != '"')
                    {
                        value += _input[_position];
                        _position++;
                    }
                    
                    if (_position >= _input.Length)
                        throw new Exception("Unterminated string literal");
                    
                    _position++; // Skip closing quote
                    return new Token(TokenType.String, value, startPos);
                }

                // Handle identifiers
                if (char.IsLetter(currentChar))
                {
                    string identifier = "";
                    int startPos = _position;

                    while (_position < _input.Length && 
                           (char.IsLetterOrDigit(_input[_position]) || _input[_position] == '_'))
                    {
                        identifier += _input[_position];
                        _position++;
                    }

                    return new Token(TokenType.Identifier, identifier, startPos);
                }

                // Handle numbers
                if (char.IsDigit(currentChar))
                {
                    string number = "";
                    int startPos = _position;

                    while (_position < _input.Length && 
                           (char.IsDigit(_input[_position]) || _input[_position] == '.'))
                    {
                        number += _input[_position];
                        _position++;
                    }

                    return new Token(TokenType.Number, number, startPos);
                }

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
                    case ',':
                        _position++;
                        return new Token(TokenType.Comma, ",", _position - 1);
                    case '.':
                        _position++;
                        return new Token(TokenType.Dot, ".", _position - 1);
                }

                throw new Exception($"Invalid character: {currentChar} at position {_position}");
            }

            return new Token(TokenType.EOF, "", _position);
        }
    }

    public abstract class AstNode
    {
        public abstract Type GetResultType();
        public abstract void Emit(ILGenerator il);
    }

    public class NumberNode : AstNode
    {
        private readonly double _value;

        public NumberNode(double value)
        {
            _value = value;
        }

        public override Type GetResultType() => typeof(double);

        public override void Emit(ILGenerator il)
        {
            il.Emit(OpCodes.Ldc_R8, _value);
        }
    }

    public class StringNode : AstNode
    {
        private readonly string _value;

        public StringNode(string value)
        {
            _value = value;
        }

        public override Type GetResultType() => typeof(string);

        public override void Emit(ILGenerator il)
        {
            il.Emit(OpCodes.Ldstr, _value);
        }
    }

    public class SystemCallNode : AstNode
    {
        private readonly string _typeName;
        private readonly string _methodName;
        private readonly List<AstNode> _arguments;
        private readonly MethodInfo _methodInfo;

        public SystemCallNode(string typeName, string methodName, List<AstNode> arguments)
        {
            _typeName = typeName;
            _methodName = methodName;
            _arguments = arguments;

            // Find the method in the specified type
            Type type = Type.GetType(_typeName) ?? 
                       Type.GetType($"System.{_typeName}") ??
                       throw new Exception($"Type not found: {_typeName}");

            Type[] parameterTypes = _arguments.Select(arg => arg.GetResultType()).ToArray();
            _methodInfo = type.GetMethod(_methodName, parameterTypes) ??
                         throw new Exception($"Method not found: {_methodName}");
        }

        public override Type GetResultType() => _methodInfo.ReturnType;

        public override void Emit(ILGenerator il)
        {
            // Emit arguments
            foreach (var arg in _arguments)
            {
                arg.Emit(il);
            }

            // Emit method call
            il.EmitCall(OpCodes.Call, _methodInfo, null);
        }
    }

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
            return SystemCall();
        }

        private AstNode SystemCall()
        {
            if (_currentToken.Type != TokenType.Identifier)
                throw new Exception("Expected type name");

            string typeName = _currentToken.Value;
            Eat(TokenType.Identifier);

            if (_currentToken.Type != TokenType.Dot)
                throw new Exception("Expected dot operator");
            Eat(TokenType.Dot);

            if (_currentToken.Type != TokenType.Identifier)
                throw new Exception("Expected method name");

            string methodName = _currentToken.Value;
            Eat(TokenType.Identifier);

            // Parse arguments
            Eat(TokenType.LeftParen);
            var arguments = new List<AstNode>();

            if (_currentToken.Type != TokenType.RightParen)
            {
                arguments.Add(ParseArgument());
                while (_currentToken.Type == TokenType.Comma)
                {
                    Eat(TokenType.Comma);
                    arguments.Add(ParseArgument());
                }
            }

            Eat(TokenType.RightParen);

            return new SystemCallNode(typeName, methodName, arguments);
        }

        private AstNode ParseArgument()
        {
            switch (_currentToken.Type)
            {
                case TokenType.Number:
                    var value = double.Parse(_currentToken.Value);
                    Eat(TokenType.Number);
                    return new NumberNode(value);

                case TokenType.String:
                    var strValue = _currentToken.Value;
                    Eat(TokenType.String);
                    return new StringNode(strValue);

                default:
                    throw new Exception($"Unexpected token type: {_currentToken.Type}");
            }
        }
    }

    public class Compiler
    {
        private readonly AstNode _ast;

        public Compiler(AstNode ast)
        {
            _ast = ast;
        }

        public Delegate Compile()
        {
            var dynamicMethod = new DynamicMethod(
                "Execute",
                _ast.GetResultType(),
                Type.EmptyTypes,
                typeof(Compiler).Module,
                true);

            var il = dynamicMethod.GetILGenerator();
            _ast.Emit(il);
            il.Emit(OpCodes.Ret);

            return dynamicMethod.CreateDelegate(typeof(Func<>).MakeGenericType(_ast.GetResultType()));
        }
    }

    public class Program
    {
        public static void Main()
        {
            // Example: Console.WriteLine("Hello, World!")
            string input = @"Console.WriteLine(""Hello, World!"")";
            
            var lexer = new Lexer(input);
            var parser = new Parser(lexer);
            var ast = parser.Parse();
            
            var compiler = new Compiler(ast);
            var method = compiler.Compile();
            
            // Execute the compiled code
            method.DynamicInvoke();

            // Example: Math.Pow(2, 3)
            input = "Math.Pow(2, 3)";
            
            lexer = new Lexer(input);
            parser = new Parser(lexer);
            ast = parser.Parse();
            
            compiler = new Compiler(ast);
            method = compiler.Compile();
            
            var result = method.DynamicInvoke();
            Console.WriteLine($"Math.Pow(2, 3) = {result}");
        }
    }
}
