import System.FilePath

data Student = Student {firstName::String, lastName::String, age::Int} 
   deriving (Show, Read, Eq)

{-- Zadanie 1 --}
studentNames :: [Student] -> [String]
studentNames = map (\s -> firstName s ++ " " ++ lastName s)

type StudentRecord = (Int, Student)
{-- Zadanie 2 --}
toNumberedList :: [Student] -> [StudentRecord]
toNumberedList = zipWith (\n student -> (n, student)) [1..]

{-- Zadanie 3 --}
toReport :: [StudentRecord] -> [String]
toReport = map (\(n,s)-> show n ++ ". " ++ toString s)

toString :: Student -> String
toString (Student firstName lastName age) = "student: " ++ lastName ++ " " ++ firstName ++ ". " ++ "wiek: " ++ (show age)

{-- Zadanie 4 i 6.2 --}
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

{-- Zadanie 5 --}
data StudentsFirstNameChangeEvent = StudentsFirstNameChangeEvent String String deriving(Show)

studentsChanges :: [Student] -> [Student] -> [StudentsFirstNameChangeEvent]
studentsChanges before after = zipWith (\b a -> StudentsFirstNameChangeEvent (firstName b) (firstName a)) before after

modifiedList = [Student "AlicjaX" "Akla" 21, Student "BatrekX" "Bodo" 20, Student "Celina" "Czyzyk" 21, Student "DamianX" "Dab"  22, Student "Eustachy" "Elo" 20]
{-- Zadanie 6.1 --}
studentNamesFromFile :: FilePath -> IO()
studentNamesFromFile path = readFile path >>= putStr

{-- Zadanie 6.2 --}
toReportFile :: FilePath -> [StudentRecord] -> IO ()
toReportFile path = writeFile path . unlines . toReport

listToProcess = [
    Student "Alicja" "Akla" 21,Student "Batrek" "Bodo" 20,
    Student "Celina" "Czyzyk" 21, Student "Damian" "Dab" 22,
    Student "Eustachy" "Elo" 20]