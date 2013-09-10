module Main where

import Prelude hiding (putStr, putStrLn)
import Data.ByteString.Char8 (putStr, putStrLn)
import Data.ByteString.UTF8 (fromString)
import DatabaseStuff (setupDatabase)
import Control.Monad.State
import System.Random(getStdGen, next, randomRIO)
import Vragenbak(createDefaultVragenbak, Question (..), Answer (..), getRandomQuestion, verifyQuestion, Vraagbaak,createLegenBak,getAndRemoveQuestion, addVraag, getSize)



main :: IO ()
main =
  let vraagbaak = createDefaultVragenbak in
  do
      result <- evalStateT  (sequence $ take 5 $ repeat exerciseState) vraagbaak
      putStr "Score is "
      putStr $ fromString $ show $ foldr (\b r -> if b then r+1 else r) 0 result
      putStr " op "
      putStrLn $ fromString $ show $ length result
      return ()
     
    
exerciseState::StateT Vraagbaak IO Bool
exerciseState = do 
                   q <- askStateQuestion
                   a <- readStateQuestion
                   s <- get
                   juist <- liftIO $  correctionGuard $ verifyQuestion q a s 
                   return juist                   
                   
     
askStateQuestion::StateT Vraagbaak IO Question
askStateQuestion  = do
                        vraagbaakSize <- getSize
                        index <- liftIO $ randomRIO (0,vraagbaakSize -1) 
                        question <- getAndRemoveQuestion index
                        liftIO $ askQuestion question
                        return question


readStateQuestion::StateT Vraagbaak IO Answer
readStateQuestion = do
                        liftIO $ readAnswer


correctionGuard::(Bool, Answer) -> IO Bool
correctionGuard (True,_) = return True
correctionGuard (False,Answer s) = do 
                    print "This is wrong, correct answer is "
                    putStrLn s
                    return False

askQuestion::Question -> IO Question
askQuestion (Question s) = do
                            print s
                            return (Question s)

readAnswer::IO Answer
readAnswer = do
             antwoord <- getLine
             return (Answer $ fromString antwoord)


