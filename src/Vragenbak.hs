module Vragenbak where

import Data.Hashable(Hashable(..))
import Data.HashMap (Map, empty, singleton, insert, lookup)
import Data.ByteString(ByteString(..))
import Data.ByteString.UTF8 (fromString)
import System.Random(getStdRandom, randomR)
import Control.Applicative((<*>),(<$>))
import Data.List(delete)
import Control.Monad.State


rollDice :: IO Int
rollDice = getStdRandom (randomR (1,6))

newtype Question = Question String deriving (Ord, Eq)

instance Hashable Question where
    hash (Question s) = hash s
    
newtype Answer = Answer ByteString deriving (Eq, Show)

addVraag::Question -> Answer -> Vraagbaak -> Vraagbaak
addVraag question answer vraagbaak = Vraagbaak { questions = (questions vraagbaak)++[question], delphi= insert question answer $ delphi vraagbaak}

deleteVraag::Question -> Vraagbaak -> Vraagbaak
deleteVraag question vraagbaak = Vraagbaak { questions = delete question $ questions vraagbaak, delphi = delphi vraagbaak}

addQuestion::Question-> Answer -> State Vraagbaak ()
addQuestion question answer = state $ \vraagbaak -> ((), addVraag question answer vraagbaak)

getAndRemoveQuestion::Int ->  StateT Vraagbaak IO Question
getAndRemoveQuestion index = state $ \vraagbaak -> let question = questions vraagbaak !! index 
                                                   in  (question, deleteVraag question vraagbaak)

getRandomQuestion::Vraagbaak -> IO Question
getRandomQuestion vraagbaak = let numberOfQuestions = length $ questions vraagbaak in
                              (\index -> questions vraagbaak !! index) <$> getStdRandom (randomR (0, numberOfQuestions-1))
                              
                              


verifyQuestion::Question -> Answer  -> Vraagbaak -> (Bool, Answer)
verifyQuestion question answerFromUser vraagbaak = let possibleAnswer = (Data.HashMap.lookup question $ delphi vraagbaak) in
                                           case possibleAnswer of
                                               Nothing -> (False, Answer $ fromString "impossible situation found I guess")
                                               Just  answerFromDelphi -> (answerFromDelphi == answerFromUser, answerFromDelphi)
                                               
getSize::StateT Vraagbaak IO Int
getSize = state $ \vraagbaak -> (length $ questions vraagbaak, vraagbaak)


data Vraagbaak = Vraagbaak{
    questions::[Question],
    delphi::Map Question Answer
}


createLegenBak = Vraagbaak { questions = [], delphi = empty} 


createDefaultVragenbak::Vraagbaak
createDefaultVragenbak =    addVraag (Question "goeienavond") (Answer $ fromString "dobrý večer") $
                            addVraag (Question "alsjeblief") (Answer $ fromString "páči sa") $
                            addVraag (Question "peer") (Answer $ fromString "hruška") $
                            addVraag (Question "geit") (Answer $ fromString "koza") $
                            addVraag (Question "ik zal") (Answer $ fromString "ja budem") $
                            addVraag (Question "jij zal") (Answer $ fromString "ty budeš") $
                            addVraag (Question "men zal") (Answer $ fromString "on bude") $
                            addVraag (Question "we zullen") (Answer $ fromString "my budeme") $
                            addVraag (Question "jullie zullen") (Answer $ fromString "vy budete") $
                            addVraag (Question "zij zullen") (Answer $ fromString "oni budú") $
                            addVraag (Question "ik ben") (Answer $ fromString "ja som") $
                            addVraag (Question "jij bent") (Answer $ fromString "ty si") $
                            addVraag (Question "men is") (Answer $ fromString "on je") $
                            addVraag (Question "wij zijn") (Answer $ fromString "my sme") $
                            addVraag (Question "jullie zijn") (Answer $ fromString "vy ste") $
                            addVraag (Question "zij zijn") (Answer $ fromString "oni sú") $
                            addVraag (Question "ik ben geweest") (Answer $ fromString "ja som bol") $
                            addVraag (Question "jij bent geweest")  (Answer $ fromString "ty si bol") $
                            addVraag (Question "men is geweest")  (Answer $ fromString "on bol") $
                            addVraag (Question "wij zijn geweest") (Answer $ fromString "my sme boli") $
                            addVraag (Question "jullie zijn geweest") (Answer $ fromString "vy ste boli") $
                            addVraag (Question "0") (Answer $ fromString "nula") $
                            addVraag (Question "1") (Answer $ fromString "jeden") $
                            addVraag (Question "2") (Answer $ fromString "dva") $
                            addVraag (Question "3") (Answer $ fromString "tri") $
                            addVraag (Question "4") (Answer $ fromString "štyri") $
                            addVraag (Question "5") (Answer $ fromString "päť") $
                            addVraag (Question "6") (Answer $ fromString "šesť") $
                            addVraag (Question "7") (Answer $ fromString "sedem") $
                            addVraag (Question "8") (Answer $ fromString "osem") $
                            addVraag (Question "9") (Answer $ fromString "deväť") $
                            addVraag (Question "10") (Answer $ fromString "desať") $
                            addVraag (Question "11") (Answer $ fromString "jedenásť") $ 
                            addVraag (Question "12") (Answer $ fromString "dvanásť") $
                            addVraag (Question "13") (Answer $ fromString "trinásť") $
                            addVraag (Question "14") (Answer $ fromString "štrinásť") $
                            addVraag (Question "15") (Answer $ fromString "päťnásť") $
                            addVraag (Question "16") (Answer $ fromString "šestnásť") $
                            addVraag (Question "17") (Answer $ fromString "sedemnásť") $
                            addVraag (Question "18") (Answer $ fromString "osemnásť") $
                            addVraag (Question "19") (Answer $ fromString "deväťnásť") $
                            addVraag (Question "20") (Answer $ fromString "dvadsať") $
                            addVraag (Question "30") (Answer $ fromString "tridsať") $
                            addVraag (Question "40") (Answer $ fromString "štyridsať")$ 
                            addVraag (Question "50") (Answer $ fromString "päťdesiat") $
                            addVraag (Question "60") (Answer $ fromString "šesťdesiat") $
                            addVraag (Question "70") (Answer $ fromString "sedemdesiat") $
                            addVraag (Question "80") (Answer $ fromString "osemdesiat") $
                            addVraag (Question "90") (Answer $ fromString "deväťdesiat") $    
                            addVraag (Question "100") (Answer $ fromString "sto") $
                            addVraag (Question "1000") (Answer $ fromString "tisíc") $
                            addVraag (Question "Hoi") (Answer $ fromString "čau") $
                            addVraag (Question "Tot ziens") (Answer $ fromString "dovidenia")     $
                            addVraag (Question "Goedenacht") (Answer $ fromString "dobrú noc") $
                            addVraag (Question "wit") (Answer $ fromString "biela")$
                            addVraag (Question "geel") (Answer $ fromString "žitá") $
                            addVraag (Question "rood") (Answer $ fromString "červená") $
                            addVraag (Question "roos") (Answer $ fromString "ružová") $
                            addVraag (Question "blauw") (Answer $ fromString "modrá")  $   
                            addVraag (Question "groen") (Answer $ fromString "zelená") $
                            addVraag (Question "bruin") (Answer $ fromString "hnedá") $
                            addVraag (Question "zwart") (Answer $ fromString "čierná") $
                            addVraag (Question "oranje") (Answer $ fromString "oranžová") $
                            addVraag (Question "paars") (Answer $ fromString "fialová") $
                            addVraag (Question "grijs") (Answer $ fromString "sivá") $
                            addVraag (Question "maandag") (Answer $ fromString "pondelok") $
                            addVraag (Question "dinsdag") (Answer $ fromString "utorok") $
                            addVraag (Question "woensdag") (Answer $ fromString "streda") $
                            addVraag (Question "donderdag") (Answer $ fromString "štvrtok") $
                            addVraag (Question "vrijdag") (Answer $ fromString "piatok") $
                            addVraag (Question "zaterdag") (Answer $ fromString "sobota") $
                            addVraag (Question "zondag") (Answer $ fromString "nedeľa")     $
                            addVraag (Question "links") (Answer $ fromString "doľava") $
                            addVraag (Question "rechts") (Answer $ fromString "doprava")     $
                            addVraag  (Question "goedemorgen") (Answer $ fromString "dobré ráno") $ Vraagbaak { questions = [], delphi = empty}



