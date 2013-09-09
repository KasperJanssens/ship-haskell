module DatabaseStuff where

import Data.Text (Text)
import Vragenbak(Question, Answer)
import Database.Persist
import Database.Persist.Sql(insert)
import Database.Persist.Sqlite (runSqlite, runMigrationSilent)
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase,
       share, sqlSettings)

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
ExamQuestion
   question    Text
   answer      Text
   deriving Show
|]

setupDatabase:: IO ()
setupDatabase = runSqlite ":memory:" $ do 
                                            runMigrationSilent migrateTables
                                            insert $ ExamQuestion "goeienavond" "dobrý večer"
                                            insert $ ExamQuestion "alsjeblief" "páči sa"
                                            insert $ ExamQuestion "peer" "hruška"
                                            insert $ ExamQuestion "geit" "koza"
                                            insert $ ExamQuestion "ik zal" "ja budem"
                                            insert $ ExamQuestion "jij zal" "ty budeš"
                                            insert $ ExamQuestion "men zal" "on bude"
                                            insert $ ExamQuestion "we zullen" "my budeme"
                                            insert $ ExamQuestion "jullie zullen" "vy budete"
                                            insert $ ExamQuestion "zij zullen" "oni budú"
                                            insert $ ExamQuestion "ik ben" "ja som"
                                            insert $ ExamQuestion "jij bent" "ty si"
                                            insert $ ExamQuestion "men is" "on je"
                                            insert $ ExamQuestion "wij zijn" "my sme"
                                            insert $ ExamQuestion "jullie zijn" "vy ste"
                                            insert $ ExamQuestion "zij zijn" "oni sú"
                                            insert $ ExamQuestion "ik ben geweest" "ja som bol"
                                            insert $ ExamQuestion "jij bent geweest" "ty si bol"
                                            insert $ ExamQuestion "men is geweest" "on bol"
                                            insert $ ExamQuestion "wij zijn geweest" "my sme boli"
                                            insert $ ExamQuestion "jullie zijn geweest" "vy ste boli"
                                            insert $ ExamQuestion "0" "nula"
                                            insert $ ExamQuestion "1" "jeden"
                                            insert $ ExamQuestion "2" "dva"
                                            insert $ ExamQuestion "3" "tri"
                                            insert $ ExamQuestion "4" "štyri"
                                            insert $ ExamQuestion "5" "päť"
                                            insert $ ExamQuestion "6" "šesť"
                                            insert $ ExamQuestion "7" "sedem"
                                            insert $ ExamQuestion "8" "osem"
                                            insert $ ExamQuestion "9" "deväť"
                                            insert $ ExamQuestion "10" "desať"
                                            insert $ ExamQuestion "11" "jedenásť"
                                            insert $ ExamQuestion "12" "dvanásť"
                                            insert $ ExamQuestion "13" "trinásť"
                                            insert $ ExamQuestion "14" "štrnásť"
                                            insert $ ExamQuestion "15" "päťnásť"
                                            insert $ ExamQuestion "16" "šestnásť"
                                            insert $ ExamQuestion "17" "sedemnásť"
                                            insert $ ExamQuestion "18" "osemnásť"
                                            insert $ ExamQuestion "19" "deväťnásť"
                                            insert $ ExamQuestion "20" "dvadsať"
                                            insert $ ExamQuestion "30" "tridsať"
                                            insert $ ExamQuestion "40" "štyridsať"
                                            insert $ ExamQuestion "50" "päťdesiat"
                                            insert $ ExamQuestion "60" "šesťdesiat"
                                            insert $ ExamQuestion "70" "sedemdesiat"
                                            insert $ ExamQuestion "80" "osemdesiat"
                                            insert $ ExamQuestion "90" "deväťdesiat"
                                            insert $ ExamQuestion "100" "sto"
                                            insert $ ExamQuestion "1000" "tisíc"
                                            insert $ ExamQuestion "Hoi" "čau"
                                            insert $ ExamQuestion "Tot ziens" "dovidenia"
                                            insert $ ExamQuestion "Goedenacht" "dobrú noc"
                                            insert $ ExamQuestion "wit" "biela"
                                            insert $ ExamQuestion "geel" "žitá"
                                            insert $ ExamQuestion "rood" "červená"
                                            insert $ ExamQuestion "roos" "ružová"
                                            insert $ ExamQuestion "blauw" "modrá"
                                            insert $ ExamQuestion "groen" "zelená"
                                            insert $ ExamQuestion "bruin" "hnedá"
                                            insert $ ExamQuestion "zwart" "čierná"
                                            insert $ ExamQuestion "oranje" "oranžová"
                                            insert $ ExamQuestion "paars" "fialová"
                                            insert $ ExamQuestion "grijs" "sivá"
                                            insert $ ExamQuestion "maandag" "pondelok"
                                            insert $ ExamQuestion "dinsdag" "utorok"
                                            insert $ ExamQuestion "woensdag" "streda"
                                            insert $ ExamQuestion "donderdag" "štvrtok"
                                            insert $ ExamQuestion "vrijdag" "piatok"
                                            insert $ ExamQuestion "zaterdag" "sobota"
                                            insert $ ExamQuestion "zondag" "nedeľa"
                                            insert $ ExamQuestion "links" "doľava"
                                            insert $ ExamQuestion "rechts" "doprava"
                                            insert $ ExamQuestion "goedemorgen" "dobré ráno"
                                            return ()
                                            
