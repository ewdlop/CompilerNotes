# CompilerNotes

https://godbolt.org/

https://sharplab.io/

https://onecompiler.com/

https://onecompiler.com/redis

# Minimal Viable Product

Sure, I'll break down the necessary steps to set up and run the Haskell parser code using the `parsec` library in different comment blocks:

1. **Set Up Your Haskell Project:**

   First, create a new Haskell project. You can use Stack or Cabal to manage your project.

   **Using Stack:**

   ```sh
   stack new my-law-parser
   cd my-law-parser
   ```

   **Using Cabal:**

   ```sh
   cabal init --non-interactive
   cd my-law-parser
   ```

2. **Add the `parsec` Dependency:**

   Open your project's `.cabal` or `package.yaml` file and add `parsec` to the dependencies.

   **For Cabal:**

   ```cabal
   build-depends:       base >=4.7 && <5, parsec
   ```

   **For Stack:**

   ```yaml
   dependencies:
   - base >= 4.7 && < 5
   - parsec
   ```

3. **Create Your Haskell Source File:**

   Create a new file named `Main.hs` in the `src` directory (or the project root if you're not using a specific structure).

   ```sh
   touch src/Main.hs
   ```

4. **Write the Parser Code:**

   Open `Main.hs` and add the following code to define the parser:

   ```haskell
   import Text.Parsec
   import Text.Parsec.String (Parser)
   import Text.Parsec.Char (digit, letter)
   import Control.Applicative ((<|>), many)

   -- Define the data types to represent the grammar
   data Law = Law Section [Section] deriving Show
   data Section = Section Number ClauseList deriving Show
   data ClauseList = ClauseList [Clause] deriving Show
   data Clause = Clause Number Statement deriving Show
   data Statement = Statement Subject Verb Object (Maybe Conditions) deriving Show
   data Conditions = Conditions [Condition] deriving Show
   data Condition = Condition Subject Verb Object deriving Show
   data Subject = Individual | Entity | Court | ProperNoun String deriving Show
   data Verb = Shall | Must | May | IsEntitledTo deriving Show
   data Object = ComplyWith Requirement | Action Action deriving Show
   data Requirement = TaxPayment | LegalObligations | Regulations deriving Show
   data Action = PayFine | SubmitReport | AttendHearing deriving Show
   type Number = String

   -- Parser for Digit
   digitParser :: Parser Char
   digitParser = oneOf "0123456789"

   -- Parser for Number
   numberParser :: Parser Number
   numberParser = many1 digitParser

   -- Parser for ProperNoun
   properNounParser :: Parser Subject
   properNounParser = ProperNoun <$> many1 letter

   -- Parser for Subject
   subjectParser :: Parser Subject
   subjectParser = (string "individual" >> return Individual)
               <|> (string "entity" >> return Entity)
               <|> (string "court" >> return Court)
               <|> properNounParser

   -- Parser for Verb
   verbParser :: Parser Verb
   verbParser = (string "shall" >> return Shall)
             <|> (string "must" >> return Must)
             <|> (string "may" >> return May)
             <|> (string "is entitled to" >> return IsEntitledTo)

   -- Parser for Requirement
   requirementParser :: Parser Requirement
   requirementParser = (string "tax payment" >> return TaxPayment)
                   <|> (string "legal obligations" >> return LegalObligations)
                   <|> (string "regulations" >> return Regulations)

   -- Parser for Action
   actionParser :: Parser Action
   actionParser = (string "pay fine" >> return PayFine)
              <|> (string "submit report" >> return SubmitReport)
              <|> (string "attend hearing" >> return AttendHearing)

   -- Parser for Object
   objectParser :: Parser Object
   objectParser = (string "comply with" >> spaces >> ComplyWith <$> requirementParser)
              <|> (Action <$> actionParser)

   -- Parser for Condition
   conditionParser :: Parser Condition
   conditionParser = Condition <$> (subjectParser <* spaces) <*> (verbParser <* spaces) <*> objectParser

   -- Parser for ConditionList
   conditionListParser :: Parser [Condition]
   conditionListParser = sepBy1 conditionParser (spaces >> string "and" >> spaces)

   -- Parser for Conditions
   conditionsParser :: Parser Conditions
   conditionsParser = (string "if" >> spaces >> Conditions <$> conditionListParser) <|> return (Conditions [])

   -- Parser for Statement
   statementParser :: Parser Statement
   statementParser = Statement <$> (subjectParser <* spaces) <*> (verbParser <* spaces) <*> (objectParser <* spaces) <*> optionMaybe conditionsParser

   -- Parser for Clause
   clauseParser :: Parser Clause
   clauseParser = Clause <$> (string "Clause" >> spaces >> numberParser <* string ":") <*> (spaces >> statementParser)

   -- Parser for ClauseList
   clauseListParser :: Parser ClauseList
   clauseListParser = ClauseList <$> many1 (clauseParser <* spaces)

   -- Parser for Section
   sectionParser :: Parser Section
   sectionParser = Section <$> (string "Section" >> spaces >> numberParser <* string ":") <*> (spaces >> clauseListParser)

   -- Parser for Law
   lawParser :: Parser Law
   lawParser = Law <$> sectionParser <*> many sectionParser

   -- Main function for testing the parser
   main :: IO ()
   main = do
       let input = "Section 1: Clause 1: individual shall comply with tax payment if individual must comply with legal obligations and entity may comply with regulations Section 2: Clause 2: entity may submit report"
       case parse lawParser "" input of
           Left err -> print err
           Right result -> print result
   ```

5. **Build and Run the Project:**

   **Using Stack:**

   ```sh
   stack build
   stack exec my-law-parser-exe
   ```

   **Using Cabal:**

   ```sh
   cabal build
   cabal run
   ```

These steps will set up a Haskell project, add the necessary dependencies, write the parser code, and run the project to test the parser.

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
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;V  
Lexical Analysis (Scanner)  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;V  
Syntax Analysis (Parser)  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;V  
Semantic Analysis  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;V  
Intermediate Code Generation  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;V  
Optimization  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;V  
Code Generation (Assembly)  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;V  
Assembly and Linking  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;V  
Executable (Machine Code)

```

Each stage of the compiler chain plays a crucial role in transforming human-readable code into a form that a CPU can execute efficiently. Different compilers may implement these stages in various ways, and some stages may be combined or split further depending on the compiler's design.
