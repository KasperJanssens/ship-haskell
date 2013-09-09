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


data Vraagbaak = Vraagbaak{
    questions::[Question],
    delphi::Map Question Answer
}



createDefaultVragenbak::State Vraagbaak ()
--createDefaultVragenbak= let legeBak =  Vraagbaak { questions = [], delphi = empty} in
createDefaultVragenbak = do
                            addQuestion (Question "goeienavond") (Answer $ fromString "dobrý večer")
                            addQuestion (Question "alsjeblief") (Answer $ fromString "páči sa")
                            addQuestion (Question "peer") (Answer $ fromString "hruška") 
                            addQuestion (Question "geit") (Answer $ fromString "koza") 
                            addQuestion (Question "ik zal") (Answer $ fromString "ja budem") 
                            addQuestion (Question "jij zal") (Answer $ fromString "ty budeš") 
                            addQuestion (Question "men zal") (Answer $ fromString "on bude") 
                            addQuestion (Question "we zullen") (Answer $ fromString "my budeme") 
                            addQuestion (Question "jullie zullen") (Answer $ fromString "vy budete") 
                            addQuestion (Question "zij zullen") (Answer $ fromString "oni budú") 
                            addQuestion (Question "ik ben") (Answer $ fromString "ja som") 
                            addQuestion (Question "jij bent") (Answer $ fromString "ty si") 
                            addQuestion (Question "men is") (Answer $ fromString "on je") 
                            addQuestion (Question "wij zijn") (Answer $ fromString "my sme") 
                            addQuestion (Question "jullie zijn") (Answer $ fromString "vy ste") 
                            addQuestion (Question "zij zijn") (Answer $ fromString "oni sú") 
                            addQuestion (Question "ik ben geweest") (Answer $ fromString "ja som bol")
                            addQuestion (Question "jij bent geweest")  (Answer $ fromString "ty si bol")
                            addQuestion (Question "men is geweest")  (Answer $ fromString "on bol")
                            addQuestion (Question "wij zijn geweest") (Answer $ fromString "my sme boli")
                            addQuestion (Question "jullie zijn geweest") (Answer $ fromString "vy ste boli")
                            addQuestion (Question "0") (Answer $ fromString "nula") 
                            addQuestion (Question "1") (Answer $ fromString "jeden") 
                            addQuestion (Question "2") (Answer $ fromString "dva") 
                            addQuestion (Question "3") (Answer $ fromString "tri") 
                            addQuestion (Question "4") (Answer $ fromString "štyri") 
                            addQuestion (Question "5") (Answer $ fromString "päť") 
                            addQuestion (Question "6") (Answer $ fromString "šesť") 
                            addQuestion (Question "7") (Answer $ fromString "sedem") 
                            addQuestion (Question "8") (Answer $ fromString "osem") 
                            addQuestion (Question "9") (Answer $ fromString "deväť") 
                            addQuestion (Question "10") (Answer $ fromString "desať") 
                            addQuestion (Question "11") (Answer $ fromString "jedenásť") 
                            addQuestion (Question "12") (Answer $ fromString "dvanásť") 
                            addQuestion (Question "13") (Answer $ fromString "trinásť") 
                            addQuestion (Question "14") (Answer $ fromString "štrinásť")
                            addQuestion (Question "15") (Answer $ fromString "päťnásť")
                            addQuestion (Question "16") (Answer $ fromString "šestnásť")
                            addQuestion (Question "17") (Answer $ fromString "sedemnásť") 
                            addQuestion (Question "18") (Answer $ fromString "osemnásť") 
                            addQuestion (Question "19") (Answer $ fromString "deväťnásť") 
                            addQuestion (Question "20") (Answer $ fromString "dvadsať") 
                            addQuestion (Question "30") (Answer $ fromString "tridsať") 
                            addQuestion (Question "40") (Answer $ fromString "štyridsať") 
                            addQuestion (Question "50") (Answer $ fromString "päťdesiat") 
                            addQuestion (Question "60") (Answer $ fromString "šesťdesiat") 
                            addQuestion (Question "70") (Answer $ fromString "sedemdesiat") 
                            addQuestion (Question "80") (Answer $ fromString "osemdesiat") 
                            addQuestion (Question "90") (Answer $ fromString "deväťdesiat")     
                            addQuestion (Question "100") (Answer $ fromString "sto") 
                            addQuestion (Question "1000") (Answer $ fromString "tisíc") 
                            addQuestion (Question "Hoi") (Answer $ fromString "čau") 
                            addQuestion (Question "Tot ziens") (Answer $ fromString "dovidenia")     
                            addQuestion (Question "Goedenacht") (Answer $ fromString "dobrú noc") 
                            addQuestion (Question "wit") (Answer $ fromString "biela")
                            addQuestion (Question "geel") (Answer $ fromString "žitá") 
                            addQuestion (Question "rood") (Answer $ fromString "červená") 
                            addQuestion (Question "roos") (Answer $ fromString "ružová") 
                            addQuestion (Question "blauw") (Answer $ fromString "modrá")     
                            addQuestion (Question "groen") (Answer $ fromString "zelená") 
                            addQuestion (Question "bruin") (Answer $ fromString "hnedá") 
                            addQuestion (Question "zwart") (Answer $ fromString "čierná") 
                            addQuestion (Question "oranje") (Answer $ fromString "oranžová") 
                            addQuestion (Question "paars") (Answer $ fromString "fialová") 
                            addQuestion (Question "grijs") (Answer $ fromString "sivá") 
                            addQuestion (Question "maandag") (Answer $ fromString "pondelok") 
                            addQuestion (Question "dinsdag") (Answer $ fromString "utorok") 
                            addQuestion (Question "woensdag") (Answer $ fromString "streda") 
                            addQuestion (Question "donderdag") (Answer $ fromString "štvrtok") 
                            addQuestion (Question "vrijdag") (Answer $ fromString "piatok") 
                            addQuestion (Question "zaterdag") (Answer $ fromString "sobota") 
                            addQuestion (Question "zondag") (Answer $ fromString "nedeľa")     
                            addQuestion (Question "links") (Answer $ fromString "doľava") 
                            addQuestion (Question "rechts") (Answer $ fromString "doprava")     
                            addQuestion  (Question "goedemorgen") (Answer $ fromString "dobré ráno")



