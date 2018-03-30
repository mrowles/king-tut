import System.IO
import System.Directory
import Data.List

data FileParsingInformation = FileParsingInformation{ beforeStatement :: String,
                                                      statement :: String,
                                                      afterStatement :: String }

data ParseAndTestInformation = ParseAndTestInformation {statements :: FileParsingInformation,
                                                        testCommand :: String
                                                        }

main = do
    let fileName = "test-file.txt"
    let currDir = withCurrentDirectory
    let backupFile = (fileName ++ ".tut.backup")
    let tempFile = (fileName ++ ".tut.tmp")
    let testCommand = "test-command"

    -- Create handles for both the fileName and the backup file
    originalFileHandle <- openFile fileName ReadMode
    backupHandle <- openFile backupFile WriteMode

    -- Passes the contents of the file into contents
    contents <- hGetContents originalFileHandle

    -- Creates the backup file
    hPutStr backupHandle $ contents
    hClose backupHandle

    -- Testing parsing
    let (ParseAndTestInformation (FileParsingInformation beforeStatement statement afterStatement) testCommand) = (parseAndTestFile $ ParseAndTestInformation (FileParsingInformation "" "" contents) testCommand)
    putStrLn ("next test" ++ beforeStatement)

    -- Create the new file with the handler
    tempFileHandle <- openFile tempFile WriteMode
    hClose tempFileHandle

    -- Change the original file to remove the statement
    removeFile fileName
    renameFile tempFile fileName

    -- Run the tests

    -- Revert the file back to it's original state and delete other files
    removeFile fileName
    renameFile backupFile fileName

goThroughFile :: String -> IO ()
goThroughFile originalString = do
    putStrLn originalString

parseAndTestFile :: ParseAndTestInformation -> ParseAndTestInformation
parseAndTestFile (ParseAndTestInformation (FileParsingInformation beforeStatement statement afterStatement) testCommand) = do
    let FileParsingInformation nextBeforeStatement nextStatement nextAfterStatement = (getNextStatement beforeStatement statement afterStatement)

    -- Run Tests

    if afterStatement == ""
    then (ParseAndTestInformation (FileParsingInformation nextBeforeStatement nextStatement nextAfterStatement) testCommand)
    else parseAndTestFile (ParseAndTestInformation (FileParsingInformation (nextBeforeStatement ++ nextStatement) "" nextAfterStatement) testCommand)

--End of file
getNextStatement :: String -> String -> String -> FileParsingInformation
getNextStatement beforeStatement currentStatement "" = FileParsingInformation beforeStatement currentStatement ""

--Parse File
getNextStatement beforeStatement currentStatement afterStatement = do
    if checkEndOfStatement (afterStatement !! 0)
    then FileParsingInformation beforeStatement (currentStatement ++ [(afterStatement !! 0)]) (tail afterStatement)
    else (getNextStatement beforeStatement (currentStatement ++ [(afterStatement !! 0)]) (tail afterStatement))

checkEndOfStatement :: Char -> Bool
checkEndOfStatement character = do
    if character == ';'
    then True
    else False
