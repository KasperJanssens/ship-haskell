module Main where

import Prelude hiding (putStr, putStrLn)
import Data.ByteString.Char8 (putStr, putStrLn)
import Data.ByteString.UTF8 (fromString)
import DatabaseStuff (setupDatabase)
import Control.Monad.State
import Vragenbak(createDefaultVragenbak, Question (..), Answer (..), getRandomQuestion, verifyQuestion, Vraagbaak,getAndRemoveQuestion)



main :: IO ()
main =
  let vraagbaak = createDefaultVragenbak in
  do
      b <- execStateT (return vraagbaak) $ exerciseState
                                                 
      return ()
      --if b then putStrLn "Goe" else putStrLn "nie goe"

 -- do
     
  --    askQuestion question
   --   answer <- readAnswer
   --   correctionGuard $ verifyQuestion question answer vragen
     -- setupDatabase
      --exam <- sequence $ take 20 $ map (\j -> vragen >>= exercise ) [1..]
     -- print "your result is"
     -- print (foldr (\ b i -> if b then i+1 else i) 0 exam) 
     -- print "out of "
     -- print $ length exam
    
    
exerciseState::StateT Vraagbaak IO ()
exerciseState = do 
                   q <- askStateQuestion
                   a <- readStateQuestion
                   s <- get
                   juist <- liftIO $  correctionGuard $ verifyQuestion q a s 
                   liftIO $ if juist then putStrLn "juist" else putStrLn "fout"
                   
                   
     
askStateQuestion::StateT Vraagbaak IO Question
askStateQuestion  = do
                        question <- getAndRemoveQuestion 1
                        liftIO $ askQuestion question
                        return question


readStateQuestion::StateT Vraagbaak IO Answer
readStateQuestion = do
                        liftIO $ readAnswer

exercise::Vraagbaak -> IO Bool
exercise vragen =do
  question <-getRandomQuestion vragen
  askQuestion question
  answer <- readAnswer
  correctionGuard $ verifyQuestion question answer vragen


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


