import Data.Char (isDigit)

-- Function to extract the first digit of a string
firstDigit :: String -> Char        -- Empty string has no first digit
firstDigit (c:cs) | isDigit c = c  -- If the first character is a digit, return it
                  | otherwise = firstDigit cs  -- Otherwise, check the rest of the string

-- Function to extract the last digit of a string
lastDigit :: String -> Char           -- Empty string has no last digit
lastDigit line = firstDigit (reverse line)  -- Otherwise, return the first character


-- Function to extract the calibration value from a line
-- Example: "1abc2" -> '1' + '2' = "12"
calibrationValue :: String -> String
calibrationValue line = [firstDigit line] ++ [lastDigit line]

-- Function to convert a string to an integer
-- Example: "12" -> 12
stringToInt :: String -> Int
stringToInt s = read s :: Int

-- Function to calculate the sum of all calibration values in the document
sumCalibrationValues :: [String] -> Int
sumCalibrationValues lines = sum (map stringToInt (calibrationValues lines))

-- Function to extract all calibration values from a document
calibrationValues :: [String] -> [String]
calibrationValues lines = map calibrationValue lines

-- Function to read a file and return its contents as a list of strings
readFileLines :: FilePath -> IO [String]
readFileLines path = do
    contents <- readFile path
    return (lines contents)


main :: IO ()
main = do
    -- Your calibration document as a list of strings
    let calibrationDocument = readFileLines "input.txt"
    putStrLn $ "Document contents: " ++ show calibrationDocument

    -- Calculate and print the sum of calibration values
    let listCalibrationValues = calibrationValues calibrationDocument
    let totalSum = sumCalibrationValues calibrationDocument
    putStrLn $ "Sum of calibration values: " ++ show listCalibrationValues ++ " = " ++ show totalSum