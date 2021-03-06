#PART 1 - Booting Up R
setwd("C:/Users/orlan/Desktop/Timey Wimey Stuff/Kaggle/Titanic Challenge")

train <- read.csv("train.csv")
test <- read.csv("test.csv")

View(test)
View(train)
str(train)

table(train$Survived)
prop.table(table(train$Survived))

#mivel a trainben 61% meghalt, legyen az els� predikci�, hogy a testben mindenki meghalt
test$Survived <- rep(0, 418)

#PassId �s Surv oszlopok --> dataframe, azt�n export csv-be
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "sub_theyallperish.csv", row.names = FALSE)




#PART 2 - The Gender-Class Model
summary(train$Sex)
#ahogy n�zem, ^ugyanaz, mint table(x$y)

prop.table(table(train$Sex, train$Survived))

#a v�g�n az 1 az 1. dimenzi�t, a row-okat jel�li (�gy row%-t k�r�nk)
prop.table(table(train$Sex, train$Survived),1)

test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1

#�j: scale v�ltoz�, prop table haszn�lhatatlan --> kategori�list hozunk l�tre yay
summary(train$Age)
train$Child <- 0
train$Child[train$Age < 18] <- 1

aggregate(Survived ~ Child + Sex, data=train, FUN=sum)
# ^h�nyan �lt�k t�l
aggregate(Survived ~ Child + Sex, data=train, FUN=length)
# ^h�nyan voltak �sszesen
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


# OVERFITTING-RE P�LDA
#fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, 
#    method="class", control=rpart.control(minsplit=2, cp=0))
#fancyRpartPlot(fit)




#PART 4 - Feature Engineering
train$Name[1]

#rbind-n�l egyez� sz�m� oszlopnak kell lennie
test$Survived <- NA
test$Child <- NA
test$Fare2 <- NA

combi <- rbind(train, test)

#factor adatot characterr� (textt�) konvert�lni
combi$Name <- as.character(combi$Name)
combi$Name[1]

#stringsplit
strsplit(combi$Name[1], split='[,.]')
strsplit(combi$Name[1], split='[,.]')[[1]]
strsplit(combi$Name[1], split='[,.]')[[1]][2]

#el�z� f�ggv�ny alkalmaz�sa minden cell�ra
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})

#space-ek lev�g�sa a title-�k elej�r�l
combi$Title <- sub(' ', '', combi$Title)

table(combi$Title)

#sok rare n�v --> merge
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

#karakter adatot faktorr� alak�tani vissza (mivel v�g�lis kateg�ri�k - decision tree �gy tud vele dolgozni)
combi$Title <- factor(combi$Title)

#familysize meghat�roz�sa sibling/spouse �s parent/children sz�m�b�l (+1 az adott illet�)
combi$FamilySize <- combi$SibSp + combi$Parch + 1

#hipot�zis: nagycsal�dosok nehezebben tal�lj�k meg pld. a gyereket �s viszik a lifeboathoz (--> nagycsal�dosokn�l nagyobb a hal�loz�si es�ly?)
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")

#kiscsal�d <= 2 f�
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'

table(combi$FamilyID)

#p�r csal�dn�l gebasz van (nem egyezik a familysize �s a darab?)
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)

#train �s test adatt�bl�k sz�tv�laszt�sa
#itt fontos a , jel (mindegyik column-t akarjuk �tvinni mindk�t adatt�bl�ba)
train <- combi[1:891,]
test <- combi[892:1309,]


#DECISION TREE az �j v�ltoz�kkal
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

#missing value-kat kezelni kell random forestekn�l
summary(combi$Age)

#NA-kra �letkor predikci� anov�val
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=combi[!is.na(combi$Age),], method="anova")
fancyRpartPlot(Agefit)
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])

summary(combi)
# ^van 2 case, ahol az embarked �res --> a t�bbs�g S-n�l sz�llt fel --> �reseket replace with S

which(combi$Embarked == '')

combi$Embarked[c(62,830)] = "S"
combi$Embarked <- factor(combi$Embarked)


summary(combi$Fare)
# ^1 case NA --> replace with mean

which(is.na(combi$Fare))

combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)

#ADATT�BLA NA-KT�L MENTES

#2. limit�l� faktor: A random forestek csak 32 level-es factorokat k�pesek kezelni (familyID)
combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)

# 22 szintes a ^ v�ltoz�, split a combi t�bl�t train �s test-re
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

#el�g tr�gya submission^

#CONDITIONAL INFERENCE TREES
install.packages('party')
library(party)

set.seed(415)

#cond inf tree-k t�bb level-� factorokat k�pesek kezelni --> eredeti FamilyID-vel dolgozunk
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
               data = train, controls=cforest_unbiased(ntree=2000, mtry=3))
fancyRpartPlot(fit)

#OOB = out of basket, a t�bbir�l fogalmam sincs
#El�g lassan fut le, 140Mb-os a fit value
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "sub_infcondtree.csv", row.names = FALSE)