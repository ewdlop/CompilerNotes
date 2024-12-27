import System.Random
import Control.Monad (replicateM)

-- Define the grammar as a set of production rules
data Rule = SectionRule | ClauseRule | StatementRule | ConditionRule
          | SubjectRule | VerbRule | ObjectRule | ConditionListRule | ClauseListRule
          deriving (Show, Enum, Bounded)

-- Generate a random element from a list
randomElement :: [a] -> IO a
randomElement xs = do
    idx <- randomRIO (0, length xs - 1)
    return (xs !! idx)

-- Generate a random number as a string
randomNumber :: IO String
randomNumber = show <$> randomRIO (1, 100 :: Int)

-- Define how to generate each part of the grammar
generate :: Rule -> IO String
generate SectionRule = do
    num <- randomNumber
    clauses <- generate ClauseListRule
    return $ "Section " ++ num ++ ": " ++ clauses
generate ClauseRule = do
    num <- randomNumber
    statement <- generate StatementRule
    return $ "Clause " ++ num ++ ": " ++ statement
generate ClauseListRule = do
    n <- randomRIO (1, 3)
    clauses <- replicateM n (generate ClauseRule)
    return $ unwords clauses
generate StatementRule = do
    subject <- generate SubjectRule
    verb <- generate VerbRule
    object <- generate ObjectRule
    conditions <- generate ConditionListRule
    return $ subject ++ " " ++ verb ++ " " ++ object ++ conditions
generate ConditionRule = do
    subject <- generate SubjectRule
    verb <- generate VerbRule
    object <- generate ObjectRule
    return $ subject ++ " " ++ verb ++ " " ++ object
generate SubjectRule = randomElement ["individual", "entity", "court", "ProperNoun"]
generate VerbRule = randomElement ["shall", "must", "may", "is entitled to"]
generate ObjectRule = do
    objType <- randomElement ["comply with", "Action"]
    if objType == "comply with"
        then randomElement ["comply with tax payment", "comply with legal obligations", "comply with regulations"]
        else randomElement ["pay fine", "submit report", "attend hearing"]
generate ConditionListRule = do
    hasConditions <- randomRIO (0, 1)
    if hasConditions == 1
        then do
            n <- randomRIO (1, 2)
            conditions <- replicateM n (generate ConditionRule)
            return $ " if " ++ unwords (map (++ " and") (init conditions) ++ [last conditions])
        else return ""

-- Generate a complete law
generateLaw :: IO String
generateLaw = do
    n <- randomRIO (1, 3)
    sections <- replicateM n (generate SectionRule)
    return $ unlines sections

main :: IO ()
main = do
    law <- generateLaw
    putStrLn law
