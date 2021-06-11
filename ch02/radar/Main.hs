{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

import Control.Exception (IOException, try)
import Fmt
import Radar
import System.Environment (getArgs)

deriving instance Read Direction

deriving instance Read Turn

instance Buildable Direction where
  build North = "N"
  build East = "E"
  build South = "S"
  build West = "W"

instance Buildable Turn where
  build TNone = "--"
  build TLeft = "<-"
  build TRight = "->"
  build TAround = "||"

main :: IO ()
main = do
  args <- getArgs
  cmd <- parseCommand args
  case cmd of
    Right c -> execute c
    Left err ->
      putStrLn err >> putStrLn "Usage: bla bla bla"

-- Playing around with extracting commands

data Command
  = Rotate
      { initialDirection :: Direction,
        turns :: [Turn]
      }
  | Orient {directions :: [Direction]}

execute :: Command -> IO ()
execute Rotate {initialDirection, turns} = do
  let finalDir = rotateMany initialDirection turns
      dirs = rotateManySteps initialDirection turns
  fmtLn $ "Final direction: " +|| finalDir ||+ ""
  fmt $ nameF "Intermediate directions" (unwordsF dirs)
execute Orient {directions} = do
  fmt $ nameF "All turns" (unwordsF $ orientMany directions)

type ErrorReason = String

parseCommand :: [String] -> IO (Either ErrorReason Command)
parseCommand args@(_ : filename : _) = do
  file <- readSafely filename
  case file of
    Left _ -> return $ Left "Could not read file"
    Right fileContents ->
      return $ parseCommand' fileContents args
parseCommand _ =
  return $ Left "Could not parse command: did you include a file path?"

parseCommand' :: String -> [String] -> Either ErrorReason Command
parseCommand' fileContents ["-r", _, direction] =
  Right
    ( Rotate
        { initialDirection = read direction,
          turns = map read $ lines fileContents
        }
    )
parseCommand' fileContents ["-o", _] =
  Right
    (Orient {directions = map read $ lines fileContents})
parseCommand' _ _ =
  Left "Could not parse your command"

readSafely :: FilePath -> IO (Either IOException String)
readSafely filename =
  try $ readFile filename
