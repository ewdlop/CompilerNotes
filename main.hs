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
