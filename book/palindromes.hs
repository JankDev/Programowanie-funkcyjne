main = interact palindromes

palindromes :: String -> String
palindromes = unlines . map func . lines
    where func line
            | line == reverse line = "palindrome"
            | otherwise = "not a palindrome"