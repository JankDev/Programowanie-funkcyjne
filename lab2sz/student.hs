import System.FilePath

data Student = Student {firstName::String, lastName::String, age::Int} 
   deriving (Show, Read, Eq)

studentNames :: [Student] -> [String]
studentNames = map (\s -> firstName s ++ " " ++ lastName s)

toNumberedList :: [Student] -> [(Int,Student)]
toNumberedList = zipWith (\n student -> (n, student)) [1..]

toFileReport :: FilePath -> [Student] -> IO ()
toFileReport path = writeFile path . unlines . map (\(n,s)-> show n ++ ". " ++ toString s)  . toNumberedList

toString :: Student -> String
toString (Student firstName lastName age) = "student: " ++ lastName ++ " " ++ firstName ++ ". " ++ "wiek: " ++ (show age)

toHtmlReport :: FilePath -> [Student] -> IO ()
toHtmlReport path students
   | (takeExtension path) /= ".html" = error "Must be a html file!"
   | otherwise =  (writeFile path . insertIntoHtmlTemplate . insertIntoHtmlTable . unlines . map toTableRow) students
    
insertIntoHtmlTemplate :: String -> String
insertIntoHtmlTemplate text = "<html><body>" ++ text ++ "</body></html>"

insertIntoHtmlTable :: String -> String
insertIntoHtmlTable rows = "<table border='1'>" ++ rows ++ "</table>"

toTableRow :: Student -> String
toTableRow (Student firstName lastName age) = "<tr><td>" ++ firstName ++ "</td><td>" ++ lastName ++ "</td><td>" ++ show age ++ "</td></tr>"

listToProcess = [
    Student "Alicja" "Akla" 21,Student "Batrek" "Bodo" 20,
    Student "Celina" "Czyzyk" 21, Student "Damian" "Dab" 22,
    Student "Eustachy" "Elo" 20]