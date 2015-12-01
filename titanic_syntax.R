#PART 1 - Booting Up R
setwd("C:/Users/orlan/Desktop/Timey Wimey Stuff/Kaggle/Titanic Challenge")

train <- read.csv("train.csv")
test <- read.csv("test.csv")

View(test)
View(train)
str(train)

table(train$Survived)
prop.table(table(train$Survived))

#mivel a trainben 61% meghalt, legyen az elsõ predikció, hogy a testben mindenki meghalt
test$Survived <- rep(0, 418)

#PassId és Surv oszlopok --> dataframe, aztán export csv-be
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "sub_theyallperish.csv", row.names = FALSE)




#PART 2 - The Gender-Class Model
summary(train$Sex)
#ahogy nézem, ^ugyanaz, mint table(x$y)

prop.table(table(train$Sex, train$Survived))

#a végén az 1 az 1. dimenziót, a row-okat jelöli (így row%-t kérünk)
prop.table(table(train$Sex, train$Survived),1)

test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1

#új: scale változó, prop table használhatatlan --> kategoriálist hozunk létre yay
summary(train$Age)
train$Child <- 0
train$Child[train$Age < 18] <- 1

aggregate(Survived ~ Child + Sex, data=train, FUN=sum)
# ^hányan élték túl
aggregate(Survived ~ Child + Sex, data=train, FUN=length)
# ^hányan voltak összesen
aggregate(Survived ~ Child + Sex, data=train, FUN=function(x) {sum(x)/length(x)})
# ^prop.table kicsit bonyolultabban

train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'

aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0

# SUBMISSION
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "sub_genderclass.csv", row.names = FALSE)




#PART 3 - Decision Trees
library(rpart)

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class")

plot(fit)
text(fit)


install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(fit)

# SUBMISSION
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "sub_myfirstdtree.csv", row.names = FALSE)


# OVERFITTING-RE PÉLDA
#fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, 
#    method="class", control=rpart.control(minsplit=2, cp=0))
#fancyRpartPlot(fit)




#PART 4 - Feature Engineering
train$Name[1]

#rbind-nál egyezõ számú oszlopnak kell lennie
test$Survived <- NA
test$Child <- NA
test$Fare2 <- NA

combi <- rbind(train, test)

#factor adatot characterré (textté) konvertálni
combi$Name <- as.character(combi$Name)
combi$Name[1]

#stringsplit
strsplit(combi$Name[1], split='[,.]')
strsplit(combi$Name[1], split='[,.]')[[1]]
strsplit(combi$Name[1], split='[,.]')[[1]][2]

#elõzõ függvény alkalmazása minden cellára
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})

#space-ek levágása a title-ök elejérõl
combi$Title <- sub(' ', '', combi$Title)

table(combi$Title)

#sok rare név --> merge
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

#karakter adatot faktorrá alakítani vissza (mivel végülis kategóriák - decision tree így tud vele dolgozni)
combi$Title <- factor(combi$Title)

#familysize meghatározása sibling/spouse és parent/children számából (+1 az adott illetõ)
combi$FamilySize <- combi$SibSp + combi$Parch + 1

#hipotézis: nagycsaládosok nehezebben találják meg pld. a gyereket és viszik a lifeboathoz (--> nagycsaládosoknál nagyobb a halálozási esély?)
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")

#kiscsalád <= 2 fõ
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'

table(combi$FamilyID)

#pár családnál gebasz van (nem egyezik a familysize és a darab?)
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)

#train és test adattáblák szétválasztása
#itt fontos a , jel (mindegyik column-t akarjuk átvinni mindkét adattáblába)
train <- combi[1:891,]
test <- combi[892:1309,]


#DECISION TREE az új változókkal
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
             data=train, method="class")

fancyRpartPlot(fit)

# SUBMISSION
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "sub_featureengineering.csv", row.names = FALSE)




#PART 5 - Random Forests

#BAGGING (sampling)
sample(1:10, replace = TRUE)

#missing value-kat kezelni kell random foresteknél
summary(combi$Age)

#NA-kra életkor predikció anovával
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=combi[!is.na(combi$Age),], method="anova")
fancyRpartPlot(Agefit)
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])

summary(combi)
# ^van 2 case, ahol az embarked üres --> a többség S-nél szállt fel --> üreseket replace with S

which(combi$Embarked == '')

combi$Embarked[c(62,830)] = "S"
combi$Embarked <- factor(combi$Embarked)


summary(combi$Fare)
# ^1 case NA --> replace with mean

which(is.na(combi$Fare))

combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)

#ADATTÁBLA NA-KTÓL MENTES

#2. limitáló faktor: A random forestek csak 32 level-es factorokat képesek kezelni (familyID)
combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)

# 22 szintes a ^ változó, split a combi táblát train és test-re
train <- combi[1:891,]
test <- combi[892:1309,]

#mehet a random forest
install.packages('randomForest')
library(randomForest)

set.seed(951)

fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize +
                      FamilyID2, data=train, importance=TRUE, ntree=2000)

#variable importance plot
varImpPlot(fit)

#SUBMISSION
Prediction <- predict(fit, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "sub_firstforest.csv", row.names = FALSE)

#elég trágya submission^

#CONDITIONAL INFERENCE TREES
install.packages('party')
library(party)

set.seed(415)

#cond inf tree-k több level-û factorokat képesek kezelni --> eredeti FamilyID-vel dolgozunk
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
               data = train, controls=cforest_unbiased(ntree=2000, mtry=3))
fancyRpartPlot(fit)

#OOB = out of basket, a többirõl fogalmam sincs
#Elég lassan fut le, 140Mb-os a fit value
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "sub_infcondtree.csv", row.names = FALSE)