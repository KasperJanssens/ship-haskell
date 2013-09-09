module Main where

import Prelude hiding (putStr, putStrLn)
import Data.ByteString.Char8 (putStr, putStrLn)
import Data.ByteString.UTF8 (fromString)
import DatabaseStuff (setupDatabase)
import Control.Monad.State
import Vragenbak(createDefaultVragenbak, Question (..), Answer (..), getRandomQuestion, verifyQuestion, Vraagbaak,createLegenBak,getAndRemoveQuestion, addVraag)



main :: IO ()
main =
  let vraagbaak = createDefaultVragenbak in
  do
      (a,s) <- runStateT exerciseState  vraagbaak
      return ()
     
    
exerciseState::StateT Vraagbaak IO ()
exerciseState = do 
                   q <- askStateQuestion
                   a <- readStateQuestion
                   s <- get
                   juist <- liftIO $  correctionGuard $ verifyQuestion q a s 
                   liftIO $ if juist then putStrLn "juist" else putStrLn "fout"
                   
                   
     
askStateQuestion::StateT Vraagbaak IO Question
askStateQuestion  = do
                        question <- getAndRemoveQuestion 0
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
                            print $  "De vraag is : "++s
                            return (Question s)

readAnswer::IO Answer
readAnswer = do
             antwoord <- getLine
             return (Answer $ fromString antwoord)


