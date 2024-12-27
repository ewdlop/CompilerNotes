# CompilerNotes

https://godbolt.org/

https://sharplab.io/

https://onecompiler.com/

https://onecompiler.com/redis

# For Reader

A compiler is a software tool that translates code written in a high-level programming language (such as C, C++, or Java) into machine language (binary code) that a computer's CPU can execute. The compilation process involves several stages, each transforming the code closer to its final executable form. Here's an overview of the typical stages in a compiler chain:

1. **Lexical Analysis (Scanning):**
   - The source code is converted into a stream of tokens. Tokens are the basic building blocks of a program, such as keywords, operators, identifiers, and literals.
   - This stage is handled by a component called the lexer or scanner.

2. **Syntax Analysis (Parsing):**
   - The stream of tokens is analyzed according to the grammatical rules of the programming language to produce a syntax tree (also known as an abstract syntax tree or AST).
   - This stage is handled by a component called the parser.

3. **Semantic Analysis:**
   - The syntax tree is checked for semantic errors, such as type mismatches, undeclared variables, and scope violations.
   - The compiler may also perform other checks, such as ensuring that function calls have the correct number of arguments.

4. **Intermediate Code Generation:**
   - The validated syntax tree is transformed into an intermediate representation (IR), which is a lower-level code that is easier to optimize and translate into machine code.
   - The IR is often platform-independent, allowing the same front-end to be used for different target architectures.

5. **Optimization:**
   - The intermediate code undergoes various optimization techniques to improve performance and reduce resource consumption.
   - Optimizations can include removing redundant code, inlining functions, and loop unrolling.

6. **Code Generation:**
   - The optimized intermediate code is translated into target-specific machine code (assembly language).
   - This stage involves mapping high-level constructs to the specific instructions of the target CPU architecture.

7. **Assembly and Linking:**
   - The generated assembly code is assembled into object code (binary format).
   - The object code is then linked with other object files and libraries to produce the final executable file.
   - The linker resolves references between different code modules and includes necessary runtime libraries.

Here's a simplified illustration of the compiler chain:

```
```

## For Visual Learner

Source Code (High-Level Language)
           |
           V
    Lexical Analysis (Scanner)
           |
           V
    Syntax Analysis (Parser)
           |
           V
   Semantic Analysis
           |
           V
Intermediate Code Generation
           |
           V
      Optimization
           |
           V
   Code Generation (Assembly)
           |
           V
     Assembly and Linking
           |
           V
   Executable (Machine Code)
```

Each stage of the compiler chain plays a crucial role in transforming human-readable code into a form that a CPU can execute efficiently. Different compilers may implement these stages in various ways, and some stages may be combined or split further depending on the compiler's design.
