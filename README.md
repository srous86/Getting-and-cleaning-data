# CodeBook
Slayton  
December 16, 2015  

The goal of this project is to use collected data from wearable computing devices and create a clean data set. 

```r
setwd("~/assignment 1/UCI HAR Dataset/test")

subject_test <- read.table("./subject_test.txt")
X_test <- read.table("./X_test.txt")
y_test <- read.table("./y_test.txt")


test_data <- cbind(subject_test,y_test,X_test)


setwd("~/assignment 1/UCI HAR Dataset/train")

subject_train <- read.table("./subject_train.txt")
X_train <- read.table("./X_train.txt")
y_train <- read.table("./y_train.txt")



subject <- rbind(subject_test,subject_train)
activity <- rbind(y_test,y_train)
features <- rbind(X_test,X_train)


setwd("~/assignment 1/UCI HAR Dataset")




featuresNames <- read.table("features.txt")
names(features)<- featuresNames$V2
featuresMean_std  <- grepl("(-std\\(\\)|-mean\\(\\))",featuresNames$V2)
features <- features[, which(featuresMean_std  == TRUE)]


names(subject)<-c("subject")
names(activity)<- c("activityNum")


activityLabels  <- read.table("./activity_labels.txt")
names(activityLabels)<-c("activityNum","activityName")


subject_activity <- cbind(subject, activity)
final_data <- cbind(subject_activity,features )
newFinal_data <- merge(activityLabels,final_data,by = "activityNum",all = TRUE)

library(reshape2)

molten <- melt(newFinal_data,id.vars=c("activityNum","activityName","subject"))
tidyData <- dcast(molten,subject + activityName ~ variable,mean)
```


-1 Use the setwd function to set the working directory to the test folder
-2 Use the read.table function to read the Subject, X and Y csv test files to tables
-3 Use the cbind function which sets binds the three test tables (Subject, X and Y) by the columns


```
##   [1] "V1"   "V1"   "V1"   "V2"   "V3"   "V4"   "V5"   "V6"   "V7"   "V8"  
##  [11] "V9"   "V10"  "V11"  "V12"  "V13"  "V14"  "V15"  "V16"  "V17"  "V18" 
##  [21] "V19"  "V20"  "V21"  "V22"  "V23"  "V24"  "V25"  "V26"  "V27"  "V28" 
##  [31] "V29"  "V30"  "V31"  "V32"  "V33"  "V34"  "V35"  "V36"  "V37"  "V38" 
##  [41] "V39"  "V40"  "V41"  "V42"  "V43"  "V44"  "V45"  "V46"  "V47"  "V48" 
##  [51] "V49"  "V50"  "V51"  "V52"  "V53"  "V54"  "V55"  "V56"  "V57"  "V58" 
##  [61] "V59"  "V60"  "V61"  "V62"  "V63"  "V64"  "V65"  "V66"  "V67"  "V68" 
##  [71] "V69"  "V70"  "V71"  "V72"  "V73"  "V74"  "V75"  "V76"  "V77"  "V78" 
##  [81] "V79"  "V80"  "V81"  "V82"  "V83"  "V84"  "V85"  "V86"  "V87"  "V88" 
##  [91] "V89"  "V90"  "V91"  "V92"  "V93"  "V94"  "V95"  "V96"  "V97"  "V98" 
## [101] "V99"  "V100" "V101" "V102" "V103" "V104" "V105" "V106" "V107" "V108"
## [111] "V109" "V110" "V111" "V112" "V113" "V114" "V115" "V116" "V117" "V118"
## [121] "V119" "V120" "V121" "V122" "V123" "V124" "V125" "V126" "V127" "V128"
## [131] "V129" "V130" "V131" "V132" "V133" "V134" "V135" "V136" "V137" "V138"
## [141] "V139" "V140" "V141" "V142" "V143" "V144" "V145" "V146" "V147" "V148"
## [151] "V149" "V150" "V151" "V152" "V153" "V154" "V155" "V156" "V157" "V158"
## [161] "V159" "V160" "V161" "V162" "V163" "V164" "V165" "V166" "V167" "V168"
## [171] "V169" "V170" "V171" "V172" "V173" "V174" "V175" "V176" "V177" "V178"
## [181] "V179" "V180" "V181" "V182" "V183" "V184" "V185" "V186" "V187" "V188"
## [191] "V189" "V190" "V191" "V192" "V193" "V194" "V195" "V196" "V197" "V198"
## [201] "V199" "V200" "V201" "V202" "V203" "V204" "V205" "V206" "V207" "V208"
## [211] "V209" "V210" "V211" "V212" "V213" "V214" "V215" "V216" "V217" "V218"
## [221] "V219" "V220" "V221" "V222" "V223" "V224" "V225" "V226" "V227" "V228"
## [231] "V229" "V230" "V231" "V232" "V233" "V234" "V235" "V236" "V237" "V238"
## [241] "V239" "V240" "V241" "V242" "V243" "V244" "V245" "V246" "V247" "V248"
## [251] "V249" "V250" "V251" "V252" "V253" "V254" "V255" "V256" "V257" "V258"
## [261] "V259" "V260" "V261" "V262" "V263" "V264" "V265" "V266" "V267" "V268"
## [271] "V269" "V270" "V271" "V272" "V273" "V274" "V275" "V276" "V277" "V278"
## [281] "V279" "V280" "V281" "V282" "V283" "V284" "V285" "V286" "V287" "V288"
## [291] "V289" "V290" "V291" "V292" "V293" "V294" "V295" "V296" "V297" "V298"
## [301] "V299" "V300" "V301" "V302" "V303" "V304" "V305" "V306" "V307" "V308"
## [311] "V309" "V310" "V311" "V312" "V313" "V314" "V315" "V316" "V317" "V318"
## [321] "V319" "V320" "V321" "V322" "V323" "V324" "V325" "V326" "V327" "V328"
## [331] "V329" "V330" "V331" "V332" "V333" "V334" "V335" "V336" "V337" "V338"
## [341] "V339" "V340" "V341" "V342" "V343" "V344" "V345" "V346" "V347" "V348"
## [351] "V349" "V350" "V351" "V352" "V353" "V354" "V355" "V356" "V357" "V358"
## [361] "V359" "V360" "V361" "V362" "V363" "V364" "V365" "V366" "V367" "V368"
## [371] "V369" "V370" "V371" "V372" "V373" "V374" "V375" "V376" "V377" "V378"
## [381] "V379" "V380" "V381" "V382" "V383" "V384" "V385" "V386" "V387" "V388"
## [391] "V389" "V390" "V391" "V392" "V393" "V394" "V395" "V396" "V397" "V398"
## [401] "V399" "V400" "V401" "V402" "V403" "V404" "V405" "V406" "V407" "V408"
## [411] "V409" "V410" "V411" "V412" "V413" "V414" "V415" "V416" "V417" "V418"
## [421] "V419" "V420" "V421" "V422" "V423" "V424" "V425" "V426" "V427" "V428"
## [431] "V429" "V430" "V431" "V432" "V433" "V434" "V435" "V436" "V437" "V438"
## [441] "V439" "V440" "V441" "V442" "V443" "V444" "V445" "V446" "V447" "V448"
## [451] "V449" "V450" "V451" "V452" "V453" "V454" "V455" "V456" "V457" "V458"
## [461] "V459" "V460" "V461" "V462" "V463" "V464" "V465" "V466" "V467" "V468"
## [471] "V469" "V470" "V471" "V472" "V473" "V474" "V475" "V476" "V477" "V478"
## [481] "V479" "V480" "V481" "V482" "V483" "V484" "V485" "V486" "V487" "V488"
## [491] "V489" "V490" "V491" "V492" "V493" "V494" "V495" "V496" "V497" "V498"
## [501] "V499" "V500" "V501" "V502" "V503" "V504" "V505" "V506" "V507" "V508"
## [511] "V509" "V510" "V511" "V512" "V513" "V514" "V515" "V516" "V517" "V518"
## [521] "V519" "V520" "V521" "V522" "V523" "V524" "V525" "V526" "V527" "V528"
## [531] "V529" "V530" "V531" "V532" "V533" "V534" "V535" "V536" "V537" "V538"
## [541] "V539" "V540" "V541" "V542" "V543" "V544" "V545" "V546" "V547" "V548"
## [551] "V549" "V550" "V551" "V552" "V553" "V554" "V555" "V556" "V557" "V558"
## [561] "V559" "V560" "V561"
```


-4 Use the setwd function to set the working directory to the train folder
-5 Use the read.table function to read the Subject, X and Y csv train files to tables
-6 Use the cbind function which sets binds the three train tables (Subject, X and Y) by the columns
-7 Use the rbind function to bind the X train and X Test, Y Train and Y Test, and Subject Test and Subject Train
-8 Use the setwd function to set the working directory back to the UCI Har Dataset
-9 Use the read.table function to read the features names from the text file 
-10 Using the grepl function find the standard deviation and mean variables, read into a character vector


```
##  [1] "tBodyAcc-mean()-X"           "tBodyAcc-mean()-Y"          
##  [3] "tBodyAcc-mean()-Z"           "tBodyAcc-std()-X"           
##  [5] "tBodyAcc-std()-Y"            "tBodyAcc-std()-Z"           
##  [7] "tGravityAcc-mean()-X"        "tGravityAcc-mean()-Y"       
##  [9] "tGravityAcc-mean()-Z"        "tGravityAcc-std()-X"        
## [11] "tGravityAcc-std()-Y"         "tGravityAcc-std()-Z"        
## [13] "tBodyAccJerk-mean()-X"       "tBodyAccJerk-mean()-Y"      
## [15] "tBodyAccJerk-mean()-Z"       "tBodyAccJerk-std()-X"       
## [17] "tBodyAccJerk-std()-Y"        "tBodyAccJerk-std()-Z"       
## [19] "tBodyGyro-mean()-X"          "tBodyGyro-mean()-Y"         
## [21] "tBodyGyro-mean()-Z"          "tBodyGyro-std()-X"          
## [23] "tBodyGyro-std()-Y"           "tBodyGyro-std()-Z"          
## [25] "tBodyGyroJerk-mean()-X"      "tBodyGyroJerk-mean()-Y"     
## [27] "tBodyGyroJerk-mean()-Z"      "tBodyGyroJerk-std()-X"      
## [29] "tBodyGyroJerk-std()-Y"       "tBodyGyroJerk-std()-Z"      
## [31] "tBodyAccMag-mean()"          "tBodyAccMag-std()"          
## [33] "tGravityAccMag-mean()"       "tGravityAccMag-std()"       
## [35] "tBodyAccJerkMag-mean()"      "tBodyAccJerkMag-std()"      
## [37] "tBodyGyroMag-mean()"         "tBodyGyroMag-std()"         
## [39] "tBodyGyroJerkMag-mean()"     "tBodyGyroJerkMag-std()"     
## [41] "fBodyAcc-mean()-X"           "fBodyAcc-mean()-Y"          
## [43] "fBodyAcc-mean()-Z"           "fBodyAcc-std()-X"           
## [45] "fBodyAcc-std()-Y"            "fBodyAcc-std()-Z"           
## [47] "fBodyAccJerk-mean()-X"       "fBodyAccJerk-mean()-Y"      
## [49] "fBodyAccJerk-mean()-Z"       "fBodyAccJerk-std()-X"       
## [51] "fBodyAccJerk-std()-Y"        "fBodyAccJerk-std()-Z"       
## [53] "fBodyGyro-mean()-X"          "fBodyGyro-mean()-Y"         
## [55] "fBodyGyro-mean()-Z"          "fBodyGyro-std()-X"          
## [57] "fBodyGyro-std()-Y"           "fBodyGyro-std()-Z"          
## [59] "fBodyAccMag-mean()"          "fBodyAccMag-std()"          
## [61] "fBodyBodyAccJerkMag-mean()"  "fBodyBodyAccJerkMag-std()"  
## [63] "fBodyBodyGyroMag-mean()"     "fBodyBodyGyroMag-std()"     
## [65] "fBodyBodyGyroJerkMag-mean()" "fBodyBodyGyroJerkMag-std()"
```

-11 Extract only the measurements on the mean and standard deviation for each measurement


```
##   tBodyAcc-mean()-X tBodyAcc-mean()-Y tBodyAcc-mean()-Z tBodyAcc-std()-X
## 1         0.2571778       -0.02328523       -0.01465376       -0.9384040
## 2         0.2860267       -0.01316336       -0.11908252       -0.9754147
## 3         0.2754848       -0.02605042       -0.11815167       -0.9938190
## 4         0.2702982       -0.03261387       -0.11752018       -0.9947428
## 5         0.2748330       -0.02784779       -0.12952716       -0.9938525
## 6         0.2792199       -0.01862040       -0.11390197       -0.9944552
##   tBodyAcc-std()-Y tBodyAcc-std()-Z tGravityAcc-mean()-X
## 1       -0.9200908       -0.6676833            0.9364893
## 2       -0.9674579       -0.9449582            0.9274036
## 3       -0.9699255       -0.9627480            0.9299150
## 4       -0.9732676       -0.9670907            0.9288814
## 5       -0.9674455       -0.9782950            0.9265997
## 6       -0.9704169       -0.9653163            0.9256632
##   tGravityAcc-mean()-Y tGravityAcc-mean()-Z tGravityAcc-std()-X
## 1           -0.2827192            0.1152882          -0.9254273
## 2           -0.2892151            0.1525683          -0.9890571
## 3           -0.2875128            0.1460856          -0.9959365
## 4           -0.2933958            0.1429259          -0.9931392
## 5           -0.3029609            0.1383067          -0.9955746
## 6           -0.3089397            0.1305608          -0.9988423
##   tGravityAcc-std()-Y tGravityAcc-std()-Z tBodyAccJerk-mean()-X
## 1          -0.9370141          -0.5642884            0.07204601
## 2          -0.9838872          -0.9647811            0.07018069
## 3          -0.9882505          -0.9815796            0.06936778
## 4          -0.9704192          -0.9915917            0.07485289
## 5          -0.9709604          -0.9680853            0.07837679
## 6          -0.9907387          -0.9712319            0.07598039
##   tBodyAccJerk-mean()-Y tBodyAccJerk-mean()-Z tBodyAccJerk-std()-X
## 1           0.045754401          -0.106042660           -0.9066828
## 2          -0.017876016          -0.001720629           -0.9492040
## 3          -0.004908146          -0.013672983           -0.9910699
## 4           0.032274322           0.012141275           -0.9908016
## 5           0.022276764           0.002748499           -0.9920724
## 6           0.017519217           0.008208061           -0.9938344
##   tBodyAccJerk-std()-Y tBodyAccJerk-std()-Z tBodyGyro-mean()-X
## 1           -0.9380164           -0.9359358        0.119976160
## 2           -0.9726989           -0.9777267       -0.001552463
## 3           -0.9714041           -0.9728674       -0.048212671
## 4           -0.9728957           -0.9759253       -0.056641577
## 5           -0.9786700           -0.9866120       -0.059992128
## 6           -0.9791029           -0.9876064       -0.039697949
##   tBodyGyro-mean()-Y tBodyGyro-mean()-Z tBodyGyro-std()-X
## 1        -0.09179234         0.18962854        -0.8830891
## 2        -0.18729119         0.18070522        -0.9255665
## 3        -0.16627974         0.15417437        -0.9729882
## 4        -0.12601846         0.11833718        -0.9677905
## 5        -0.08472343         0.07865926        -0.9746560
## 6        -0.06682849         0.07054732        -0.9798784
##   tBodyGyro-std()-Y tBodyGyro-std()-Z tBodyGyroJerk-mean()-X
## 1        -0.8161636        -0.9408812            -0.20489621
## 2        -0.9295992        -0.9675810            -0.13866849
## 3        -0.9785106        -0.9756483            -0.09780967
## 4        -0.9751338        -0.9632327            -0.10223178
## 5        -0.9779511        -0.9676287            -0.09184994
## 6        -0.9764628        -0.9634617            -0.09273955
##   tBodyGyroJerk-mean()-Y tBodyGyroJerk-mean()-Z tBodyGyroJerk-std()-X
## 1            -0.17448771            -0.09338934            -0.9012242
## 2            -0.02580207            -0.07141841            -0.9623042
## 3            -0.03421327            -0.06002840            -0.9841669
## 4            -0.04471346            -0.05343650            -0.9841709
## 5            -0.02900502            -0.06123535            -0.9884738
## 6            -0.03213821            -0.07257549            -0.9894167
##   tBodyGyroJerk-std()-Y tBodyGyroJerk-std()-Z tBodyAccMag-mean()
## 1            -0.9108601            -0.9392504         -0.8669294
## 2            -0.9562894            -0.9813408         -0.9689614
## 3            -0.9879399            -0.9761876         -0.9762282
## 4            -0.9895542            -0.9807240         -0.9743245
## 5            -0.9918773            -0.9819836         -0.9758356
## 6            -0.9894940            -0.9777794         -0.9817163
##   tBodyAccMag-std() tGravityAccMag-mean() tGravityAccMag-std()
## 1        -0.7051911            -0.8669294           -0.7051911
## 2        -0.9539024            -0.9689614           -0.9539024
## 3        -0.9790834            -0.9762282           -0.9790834
## 4        -0.9770159            -0.9743245           -0.9770159
## 5        -0.9768934            -0.9758356           -0.9768934
## 6        -0.9777039            -0.9817163           -0.9777039
##   tBodyAccJerkMag-mean() tBodyAccJerkMag-std() tBodyGyroMag-mean()
## 1             -0.9297665            -0.8959942          -0.7955439
## 2             -0.9737168            -0.9410078          -0.8984331
## 3             -0.9816177            -0.9713710          -0.9391936
## 4             -0.9827121            -0.9748075          -0.9471844
## 5             -0.9869079            -0.9888539          -0.9574192
## 6             -0.9873357            -0.9919970          -0.9697442
##   tBodyGyroMag-std() tBodyGyroJerkMag-mean() tBodyGyroJerkMag-std()
## 1         -0.7620732              -0.9251949             -0.8943436
## 2         -0.9108583              -0.9733934             -0.9440900
## 3         -0.9717763              -0.9867480             -0.9844015
## 4         -0.9703682              -0.9888204             -0.9855681
## 5         -0.9694995              -0.9900621             -0.9904189
## 6         -0.9732947              -0.9878482             -0.9890428
##   fBodyAcc-mean()-X fBodyAcc-mean()-Y fBodyAcc-mean()-Z fBodyAcc-std()-X
## 1        -0.9185097        -0.9182132        -0.7890915       -0.9482903
## 2        -0.9609030        -0.9644333        -0.9566748       -0.9843500
## 3        -0.9919063        -0.9650336        -0.9668996       -0.9947537
## 4        -0.9930298        -0.9682839        -0.9669116       -0.9955936
## 5        -0.9924042        -0.9692072        -0.9796527       -0.9945037
## 6        -0.9938427        -0.9707293        -0.9755771       -0.9946471
##   fBodyAcc-std()-Y fBodyAcc-std()-Z fBodyAccJerk-mean()-X
## 1       -0.9251369       -0.6363167            -0.8996332
## 2       -0.9701748       -0.9418619            -0.9435190
## 3       -0.9736959       -0.9622866            -0.9910295
## 4       -0.9769176       -0.9689579            -0.9905362
## 5       -0.9674681       -0.9782399            -0.9914800
## 6       -0.9710160       -0.9614438            -0.9937759
##   fBodyAccJerk-mean()-Y fBodyAccJerk-mean()-Z fBodyAccJerk-std()-X
## 1            -0.9374850            -0.9235514           -0.9244291
## 2            -0.9691623            -0.9734489           -0.9616312
## 3            -0.9733689            -0.9717030           -0.9919122
## 4            -0.9724525            -0.9703374           -0.9919600
## 5            -0.9798346            -0.9834930           -0.9935709
## 6            -0.9789845            -0.9860986           -0.9944514
##   fBodyAccJerk-std()-Y fBodyAccJerk-std()-Z fBodyGyro-mean()-X
## 1           -0.9432104           -0.9478915         -0.8235579
## 2           -0.9800263           -0.9807873         -0.9225130
## 3           -0.9709857           -0.9723222         -0.9728456
## 4           -0.9754341           -0.9806211         -0.9715016
## 5           -0.9786966           -0.9885033         -0.9764045
## 6           -0.9807585           -0.9875569         -0.9797034
##   fBodyGyro-mean()-Y fBodyGyro-mean()-Z fBodyGyro-std()-X
## 1         -0.8079160         -0.9179126        -0.9032627
## 2         -0.9264957         -0.9682295        -0.9270506
## 3         -0.9808333         -0.9720602        -0.9731979
## 4         -0.9813450         -0.9667055        -0.9671856
## 5         -0.9804043         -0.9687820        -0.9743979
## 6         -0.9805261         -0.9602064        -0.9800146
##   fBodyGyro-std()-Y fBodyGyro-std()-Z fBodyAccMag-mean() fBodyAccMag-std()
## 1        -0.8226770        -0.9561651         -0.7909464        -0.7110740
## 2        -0.9320107        -0.9701434         -0.9541266        -0.9597458
## 3        -0.9771945        -0.9790953         -0.9756497        -0.9837843
## 4        -0.9719094        -0.9652755         -0.9733931        -0.9821196
## 5        -0.9765654        -0.9700165         -0.9777390        -0.9788381
## 6        -0.9742469        -0.9678163         -0.9780025        -0.9798756
##   fBodyBodyAccJerkMag-mean() fBodyBodyAccJerkMag-std()
## 1                 -0.8950612                -0.8963596
## 2                 -0.9454372                -0.9341520
## 3                 -0.9710690                -0.9703078
## 4                 -0.9716545                -0.9784844
## 5                 -0.9874888                -0.9897160
## 6                 -0.9912947                -0.9917440
##   fBodyBodyGyroMag-mean() fBodyBodyGyroMag-std()
## 1              -0.7706100             -0.7971128
## 2              -0.9244608             -0.9167741
## 3              -0.9752095             -0.9739984
## 4              -0.9762973             -0.9712482
## 5              -0.9770070             -0.9696193
## 6              -0.9770345             -0.9750513
##   fBodyBodyGyroJerkMag-mean() fBodyBodyGyroJerkMag-std()
## 1                  -0.8901655                 -0.9073076
## 2                  -0.9519774                 -0.9382124
## 3                  -0.9856888                 -0.9832727
## 4                  -0.9855619                 -0.9858429
## 5                  -0.9904980                 -0.9905719
## 6                  -0.9887326                 -0.9898050
```

-12 Using the names function label the variables in the subject and activity tables
-13 Read the activity labels text file into a data table
-14 Using the names function label the variable in the new activityLabels data table


```
## [1] "activityNum"  "activityName"
```


-15 Merge the **subject, activity and features** data tables the final data set


```
## [1] 10299    69
```

-16 Using the Melt function reshape the final data set by the variable names **activityNum, activityName, subject, variable and value**


```
##   activityNum activityName subject          variable     value
## 1           1      WALKING      26 tBodyAcc-mean()-X 0.2314146
## 2           1      WALKING      29 tBodyAcc-mean()-X 0.3312213
## 3           1      WALKING      29 tBodyAcc-mean()-X 0.3755700
## 4           1      WALKING      29 tBodyAcc-mean()-X 0.2332297
## 5           1      WALKING      29 tBodyAcc-mean()-X 0.2362494
## 6           1      WALKING      29 tBodyAcc-mean()-X 0.2645428
```

-17 Reshape using the dcast function to return the data starting by the subject variable


```
##   subject       activityName tBodyAcc-mean()-X tBodyAcc-mean()-Y
## 1       1             LAYING         0.2215982      -0.040513953
## 2       1            SITTING         0.2612376      -0.001308288
## 3       1           STANDING         0.2789176      -0.016137590
## 4       1            WALKING         0.2773308      -0.017383819
## 5       1 WALKING_DOWNSTAIRS         0.2891883      -0.009918505
## 6       1   WALKING_UPSTAIRS         0.2554617      -0.023953149
##   tBodyAcc-mean()-Z tBodyAcc-std()-X tBodyAcc-std()-Y tBodyAcc-std()-Z
## 1        -0.1132036      -0.92805647     -0.836827406      -0.82606140
## 2        -0.1045442      -0.97722901     -0.922618642      -0.93958629
## 3        -0.1106018      -0.99575990     -0.973190056      -0.97977588
## 4        -0.1111481      -0.28374026      0.114461337      -0.26002790
## 5        -0.1075662       0.03003534     -0.031935943      -0.23043421
## 6        -0.0973020      -0.35470803     -0.002320265      -0.01947924
##   tGravityAcc-mean()-X tGravityAcc-mean()-Y tGravityAcc-mean()-Z
## 1           -0.2488818            0.7055498           0.44581772
## 2            0.8315099            0.2044116           0.33204370
## 3            0.9429520           -0.2729838           0.01349058
## 4            0.9352232           -0.2821650          -0.06810286
## 5            0.9318744           -0.2666103          -0.06211996
## 6            0.8933511           -0.3621534          -0.07540294
##   tGravityAcc-std()-X tGravityAcc-std()-Y tGravityAcc-std()-Z
## 1          -0.8968300          -0.9077200          -0.8523663
## 2          -0.9684571          -0.9355171          -0.9490409
## 3          -0.9937630          -0.9812260          -0.9763241
## 4          -0.9766096          -0.9713060          -0.9477172
## 5          -0.9505598          -0.9370187          -0.8959397
## 6          -0.9563670          -0.9528492          -0.9123794
##   tBodyAccJerk-mean()-X tBodyAccJerk-mean()-Y tBodyAccJerk-mean()-Z
## 1            0.08108653          0.0038382040           0.010834236
## 2            0.07748252         -0.0006191028          -0.003367792
## 3            0.07537665          0.0079757309          -0.003685250
## 4            0.07404163          0.0282721096          -0.004168406
## 5            0.05415532          0.0296504490          -0.010971973
## 6            0.10137273          0.0194863076          -0.045562545
##   tBodyAccJerk-std()-X tBodyAccJerk-std()-Y tBodyAccJerk-std()-Z
## 1          -0.95848211           -0.9241493           -0.9548551
## 2          -0.98643071           -0.9813720           -0.9879108
## 3          -0.99460454           -0.9856487           -0.9922512
## 4          -0.11361560            0.0670025           -0.5026998
## 5          -0.01228386           -0.1016014           -0.3457350
## 6          -0.44684389           -0.3782744           -0.7065935
##   tBodyGyro-mean()-X tBodyGyro-mean()-Y tBodyGyro-mean()-Z
## 1        -0.01655309        -0.06448612         0.14868944
## 2        -0.04535006        -0.09192415         0.06293138
## 3        -0.02398773        -0.05939722         0.07480075
## 4        -0.04183096        -0.06953005         0.08494482
## 5        -0.03507819        -0.09093713         0.09008501
## 6         0.05054938        -0.16617002         0.05835955
##   tBodyGyro-std()-X tBodyGyro-std()-Y tBodyGyro-std()-Z
## 1        -0.8735439      -0.951090440        -0.9082847
## 2        -0.9772113      -0.966473895        -0.9414259
## 3        -0.9871919      -0.987734440        -0.9806456
## 4        -0.4735355      -0.054607769        -0.3442666
## 5        -0.4580305      -0.126349195        -0.1247025
## 6        -0.5448711       0.004105184        -0.5071687
##   tBodyGyroJerk-mean()-X tBodyGyroJerk-mean()-Y tBodyGyroJerk-mean()-Z
## 1            -0.10727095            -0.04151729            -0.07405012
## 2            -0.09367938            -0.04021181            -0.04670263
## 3            -0.09960921            -0.04406279            -0.04895055
## 4            -0.08999754            -0.03984287            -0.04613093
## 5            -0.07395920            -0.04399028            -0.02704611
## 6            -0.12223277            -0.04214859            -0.04071255
##   tBodyGyroJerk-std()-X tBodyGyroJerk-std()-Y tBodyGyroJerk-std()-Z
## 1            -0.9186085            -0.9679072            -0.9577902
## 2            -0.9917316            -0.9895181            -0.9879358
## 3            -0.9929451            -0.9951379            -0.9921085
## 4            -0.2074219            -0.3044685            -0.4042555
## 5            -0.4870273            -0.2388248            -0.2687615
## 6            -0.6147865            -0.6016967            -0.6063320
##   tBodyAccMag-mean() tBodyAccMag-std() tGravityAccMag-mean()
## 1        -0.84192915       -0.79514486           -0.84192915
## 2        -0.94853679       -0.92707842           -0.94853679
## 3        -0.98427821       -0.98194293           -0.98427821
## 4        -0.13697118       -0.21968865           -0.13697118
## 5         0.02718829        0.01988435            0.02718829
## 6        -0.12992763       -0.32497093           -0.12992763
##   tGravityAccMag-std() tBodyAccJerkMag-mean() tBodyAccJerkMag-std()
## 1          -0.79514486            -0.95439626           -0.92824563
## 2          -0.92707842            -0.98736420           -0.98412002
## 3          -0.98194293            -0.99236779           -0.99309621
## 4          -0.21968865            -0.14142881           -0.07447175
## 5           0.01988435            -0.08944748           -0.02578772
## 6          -0.32497093            -0.46650345           -0.47899162
##   tBodyGyroMag-mean() tBodyGyroMag-std() tBodyGyroJerkMag-mean()
## 1         -0.87475955         -0.8190102              -0.9634610
## 2         -0.93089249         -0.9345318              -0.9919763
## 3         -0.97649379         -0.9786900              -0.9949668
## 4         -0.16097955         -0.1869784              -0.2987037
## 5         -0.07574125         -0.2257244              -0.2954638
## 6         -0.12673559         -0.1486193              -0.5948829
##   tBodyGyroJerkMag-std() fBodyAcc-mean()-X fBodyAcc-mean()-Y
## 1             -0.9358410       -0.93909905      -0.867065205
## 2             -0.9883087       -0.97964124      -0.944084550
## 3             -0.9947332       -0.99524993      -0.977070848
## 4             -0.3253249       -0.20279431       0.089712726
## 5             -0.3065106        0.03822918       0.001549908
## 6             -0.6485530       -0.40432178      -0.190976721
##   fBodyAcc-mean()-Z fBodyAcc-std()-X fBodyAcc-std()-Y fBodyAcc-std()-Z
## 1        -0.8826669      -0.92443743      -0.83362556      -0.81289156
## 2        -0.9591849      -0.97641231      -0.91727501      -0.93446956
## 3        -0.9852971      -0.99602835      -0.97229310      -0.97793726
## 4        -0.3315601      -0.31913472       0.05604001      -0.27968675
## 5        -0.2255745       0.02433084      -0.11296374      -0.29792789
## 6        -0.4333497      -0.33742819       0.02176951       0.08595655
##   fBodyAccJerk-mean()-X fBodyAccJerk-mean()-Y fBodyAccJerk-mean()-Z
## 1           -0.95707388           -0.92246261            -0.9480609
## 2           -0.98659702           -0.98157947            -0.9860531
## 3           -0.99463080           -0.98541870            -0.9907522
## 4           -0.17054696           -0.03522552            -0.4689992
## 5           -0.02766387           -0.12866716            -0.2883347
## 6           -0.47987525           -0.41344459            -0.6854744
##   fBodyAccJerk-std()-X fBodyAccJerk-std()-Y fBodyAccJerk-std()-Z
## 1           -0.9641607           -0.9322179           -0.9605870
## 2           -0.9874930           -0.9825139           -0.9883392
## 3           -0.9950738           -0.9870182           -0.9923498
## 4           -0.1335866            0.1067399           -0.5347134
## 5           -0.0863279           -0.1345800           -0.4017215
## 6           -0.4619070           -0.3817771           -0.7260402
##   fBodyGyro-mean()-X fBodyGyro-mean()-Y fBodyGyro-mean()-Z
## 1         -0.8502492        -0.95219149        -0.90930272
## 2         -0.9761615        -0.97583859        -0.95131554
## 3         -0.9863868        -0.98898446        -0.98077312
## 4         -0.3390322        -0.10305942        -0.25594094
## 5         -0.3524496        -0.05570225        -0.03186943
## 6         -0.4926117        -0.31947461        -0.45359721
##   fBodyGyro-std()-X fBodyGyro-std()-Y fBodyGyro-std()-Z fBodyAccMag-mean()
## 1        -0.8822965       -0.95123205        -0.9165825        -0.86176765
## 2        -0.9779042       -0.96234504        -0.9439178        -0.94778292
## 3        -0.9874971       -0.98710773        -0.9823453        -0.98535636
## 4        -0.5166919       -0.03350816        -0.4365622        -0.12862345
## 5        -0.4954225       -0.18141473        -0.2384436         0.09658453
## 6        -0.5658925        0.15153891        -0.5717078        -0.35239594
##   fBodyAccMag-std() fBodyBodyAccJerkMag-mean() fBodyBodyAccJerkMag-std()
## 1        -0.7983009                -0.93330036                -0.9218040
## 2        -0.9284448                -0.98526213                -0.9816062
## 3        -0.9823138                -0.99254248                -0.9925360
## 4        -0.3980326                -0.05711940                -0.1034924
## 5        -0.1865303                 0.02621849                -0.1040523
## 6        -0.4162601                -0.44265216                -0.5330599
##   fBodyBodyGyroMag-mean() fBodyBodyGyroMag-std()
## 1              -0.8621902             -0.8243194
## 2              -0.9584356             -0.9321984
## 3              -0.9846176             -0.9784661
## 4              -0.1992526             -0.3210180
## 5              -0.1857203             -0.3983504
## 6              -0.3259615             -0.1829855
##   fBodyBodyGyroJerkMag-mean() fBodyBodyGyroJerkMag-std()
## 1                  -0.9423669                 -0.9326607
## 2                  -0.9897975                 -0.9870496
## 3                  -0.9948154                 -0.9946711
## 4                  -0.3193086                 -0.3816019
## 5                  -0.2819634                 -0.3919199
## 6                  -0.6346651                 -0.6939305
```








