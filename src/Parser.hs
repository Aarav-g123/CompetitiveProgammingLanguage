module Parser (parseCP) where

-- Why parseCP returns Either String AST
type AST = [String] -- Placeholder for Abstract Syntax Tree. Replace with actual data structure.

-- Dummy parser for `.cp` files
parseCP :: String -> Either String AST
parseCP content = 
    if null content 
    then Left "File is empty"
    else Right (lines content) -- Parses `.cp` lines into AST (replace)