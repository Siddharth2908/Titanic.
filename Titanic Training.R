setwd("C:/Siddhartha/Personal Documents/BOK/Data Science/Tuturial/Titanic Data Set")
# Upload files into R
train.learn<-read.csv(file = "train.csv", header = TRUE)
test.learn<-read.csv(file = "test.csv", header = TRUE)

# Explore the data to understand the current state and spread
View(test.learn)
View(train.learn)
test.survived<-data.frame(Survived = rep("None",nrow(test.learn)), test.learn[,])
View(test.survived)
Learn.Combined<-rbind(train.learn,test.survived)
str(Learn.Combined)
Learn.Combined$Survived<-as.factor(Learn.Combined$Survived)
Learn.Combined$Pclass<-as.factor(Learn.Combined$Pclass)
table(Learn.Combined$Survived,Learn.Combined$Pclass)
#install.packages("ggplot2")
library(ggplot2)
train.learn$Pclass<-as.factor(train.learn$Pclass)
ggplot(train.learn,aes(x = Pclass, fill = factor(Survived)))+
  geom_bar(stat = "count")+
  xlab("pclass")+
  ylab("Total Count")+
  labs(fill = "Survived")
head(as.character(train.learn$Name))
length(unique(as.character(Learn.Combined$Name)))
duplicate.names<-as.character(Learn.Combined[which(duplicated(as.character(Learn.Combined$Name))),"name"])
Learn.Combined[which(Learn.Combined$Name %in% duplicate.names)]
library(stringr)
misses<-Learn.Combined[which(str_detect(Learn.Combined$Name, "Miss.")),]
misses[1:5,]
mrses<-Learn.Combined[which(str_detect(Learn.Combined$Name, "Mrs.")),]
mrses[1:5,]
Men<-Learn.Combined[which(train.learn$Sex == "male"),]
Men[1:5,]
#create an utility function to extract set of characters from a string - Feature Engineering
ExtractTitle <- function(Name){
  Name<-as.character(Name)
  if(length(grep("Miss.", Name))>0){
    return("Miss.")
  }else if(length(grep("Mrs.",Name))>0){
    return("Mrs.")
  }else if(length(grep("Master",Name))>0){
    return("Master.")
  }else if(length(grep("Mr.",Name))>0){
    return("Mr.")
  }else{
    return("Other")
  }
}
Titles <- NULL
for (i in 1:nrow(Learn.Combined)){
  Titles <-c(Titles, ExtractTitle(Learn.Combined[i,"Name"]))
}
Learn.Combined$Titles <- as.factor(Titles)
# Explore the data further
View(Learn.Combined)
ggplot(Learn.Combined[1:891,],aes(x = Titles, fill = Survived))+
  geom_bar(stat = "count")+
  facet_wrap(~Pclass)+
  ggtitle("Pclass")+
  xlab("pclass")+
  ylab("Total Count")+
  labs(fill = "Survived")
ggplot(Learn.Combined[1:891,],aes(x = Titles, fill = Survived))+
  geom_bar(stat = "count")+
  facet_wrap(~Embarked)+
  ggtitle("Embarked")+
  xlab("Titles")+
  ylab("Total Count")+
  labs(fill = "Survived")
table(Learn.Combined$Sex)
ggplot(Learn.Combined[1:891,],aes(x = Sex, fill = Survived))+
  geom_bar(stat = "count")+
  facet_wrap(~Pclass)+
  ggtitle("Pclass")+
  xlab("Sex")+
  ylab("Total Count")+
  labs(fill = "Survived")
Learn.Combined$Age<-as.numeric(Learn.Combined$Age)
summary(Learn.Combined$Age)
summary(Learn.Combined[1:891,"Age"])
ggplot(Learn.Combined[1:891,],aes(x = Age, fill = Survived))+
  geom_histogram(binwidth = 15)+
  facet_wrap(~Sex + Pclass)+
  ggtitle("Pclass, Age, Sex")+
  xlab("Age")+
  ylab("Total Count")+
  labs(fill = "Survived")
Boys <- Learn.Combined[which(Learn.Combined$Titles == "Master."),]
summary(Boys$Age)
Ladies <- Learn.Combined[which(Learn.Combined$Titles == "Miss."),]
summary(Ladies$Age)
ggplot(Ladies[Ladies$Survived != "None",],aes(x = Age, fill = Survived))+
  geom_histogram(binwidth = 5)+
  facet_wrap(~Pclass)+
  ggtitle("Age for Miss in PClass")+
  xlab("Age")+
  ylab("Total Count")+
  labs(fill = "Survived")
Girls <- Ladies[which(Ladies$SibSp == 0 & Ladies$Parch == 0),]
summary(Girls$Age)
length(which(Girls$Age <= 14.5))
summary(Learn.Combined$SibSp)
length(unique(Learn.Combined$SibSp))
Learn.Combined$SibSp <- as.factor(Learn.Combined$SibSp)
ggplot(Learn.Combined[1:891,],aes(x = SibSp, fill = Survived))+
  geom_bar(stat = "count")+
  facet_wrap(~Pclass + Titles)+
  ggtitle("Pclass, Titles")+
  xlab("Sibsp")+
  ylab("Total Count")+
  labs(fill = "Survived")
Learn.Combined$Parch <- as.factor(Learn.Combined$Parch)
ggplot(Learn.Combined[1:891,],aes(x = Parch, fill = Survived))+
  geom_bar(stat = "count")+
  facet_wrap(~Pclass + Titles)+
  ggtitle("Pclass, Titles")+
  xlab("parch")+
  ylab("Total Count")+
  labs(fill = "Survived")
Tempsibsp<-c(train.learn$SibSp, test.learn$SibSp)
Tempparch<-c(train.learn$Parch, test.learn$Parch)
Learn.Combined$Familysize <- as.factor(Tempsibsp+Tempparch+1)
ggplot(Learn.Combined[1:891,],aes(x = Familysize, fill = Survived))+
  geom_bar(stat = "count")+
  facet_wrap(~Pclass + Titles)+
  ggtitle("Pclass, Titles")+
  xlab("FamilySize")+
  ylab("Total Count")+
  ylim(0,300)
  labs(fill = "Survived")
str(Learn.Combined$Ticket)
Learn.Combined$Ticket<-as.character(Learn.Combined$Ticket)
Learn.Combined$Ticket[1:20]
Ticketfirstchar <- ifelse(Learn.Combined$Ticket == "", " ", substr(Learn.Combined$Ticket,1,1))
unique(Ticketfirstchar)
Learn.Combined$Ticketfirstchar<-as.factor(Ticketfirstchar)
ggplot(Learn.Combined[1:891,],aes(x = Ticketfirstchar, fill = Survived))+
  geom_bar()+
  facet_wrap(~Pclass)+
  ggtitle("Pclass")+
  xlab("Ticketfirstchar")+
  ylim(0,150)+
  ylab("Total Count")+
  labs(fill = "Survived")
ggplot(Learn.Combined[1:891,],aes(x = Ticketfirstchar, fill = Survived))+
  geom_bar()+
  facet_wrap(~Pclass + Titles)+
  ggtitle("Pclass and Titles")+
  xlab("Ticketfirstchar")+
  ylim(0,150)+
  ylab("Total Count")+
  labs(fill = "Survived")
summary(Learn.Combined$Fare)
length(unique(Learn.Combined$Fare))
str(Learn.Combined$Fare)
ggplot(Learn.Combined[1:891,],aes(x = Fare, fill = Survived))+
  geom_histogram(binwidth = 5)+
  facet_wrap(~Pclass + Titles)+
  ggtitle("Pclass and Titles")+
  xlab("Fare")+
  ylim(0,50)+
  ylab("Total Count")+
  labs(fill = "Survived")
str(Learn.Combined$Cabin)
Learn.Combined$Cabin<-as.character(Learn.Combined$Cabin)
Learn.Combined$Cabin[1:100]
Learn.Combined[which(Learn.Combined$Cabin == ""),"Cabin"] <- "U"
Learn.Combined$Cabin
Cabinfirstchar <- as.factor(substr(Learn.Combined$Cabin,1,1))
str(Cabinfirstchar)
levels(Cabinfirstchar)
Learn.Combined$Cabinfirstchar<-Cabinfirstchar
ggplot(Learn.Combined[1:891,],aes(x = Cabinfirstchar, fill = Survived))+
  geom_bar()+
  facet_wrap(~Pclass)+
  ggtitle("Pclass and Titles")+
  xlab("CabinFirstChar")+
  ylim(0,500)+
  ylab("Total Count")+
  labs(fill = "Survived")
ggplot(Learn.Combined[1:891,],aes(x = Cabinfirstchar, fill = Survived))+
  geom_bar(stat = "count")+
  facet_wrap(~Pclass + Titles)+
  ggtitle("Pclass and Titles")+
  xlab("Fare")+
  ylim(0,50)+
  ylab("Total Count")+
  labs(fill = "Survived")
Learn.Combined$Multiplecabin <- as.factor(ifelse(str_detect(Learn.Combined$Cabin," "), "Y","N"))
ggplot(Learn.Combined[1:891,],aes(x = Multiplecabin, fill = Survived))+
  geom_bar(stat = "count")+
  facet_wrap(~Pclass + Titles)+
  ggtitle("Pclass and Titles")+
  xlab("Multiplecabin")+
  ylim(0,50)+
  ylab("Total Count")+
  labs(fill = "Survived")
str(Learn.Combined$Embarked)
levels(Learn.Combined$Embarked)
ggplot(Learn.Combined[1:891,],aes(x = Embarked, fill = Survived))+
  geom_bar(stat = "count")+
  facet_wrap(~Pclass + Titles)+
  ggtitle("Pclass and Titles")+
  xlab("Embarked")+
  ylim(0,50)+
  ylab("Total Count")+
  labs(fill = "Survived")

#exploratory modelling using randomForest
library(randomForest)
Modellingdata.RF<-Learn.Combined[1:891,c("Pclass","Titles")]
Modellinglabel.RF<-as.factor(train.learn$Survived)
set.seed(1234)
Model.1.RF<-randomForest(x = Modellingdata.RF, y=Modellinglabel.RF, ntree = 1000, importance = TRUE)
Model.1.RF
varImpPlot(Model.1.RF)

Modellingdata.RF2<-Learn.Combined[1:891,c("Pclass","Titles","SibSp")]
set.seed(1234)
Model.1.RF2<-randomForest(x = Modellingdata.RF2, y=Modellinglabel.RF, ntree = 1000, importance = TRUE)
Model.1.RF2
varImpPlot(Model.1.RF2)

Modellingdata.RF3<-Learn.Combined[1:891,c("Pclass","Titles","SibSp","Parch")]
set.seed(1234)
Model.1.RF3<-randomForest(x = Modellingdata.RF3, y=Modellinglabel.RF, ntree = 1000, importance = TRUE)
Model.1.RF3
varImpPlot(Model.1.RF3)

Modellingdata.RF4<-Learn.Combined[1:891,c("Pclass","Titles","Familysize")]
set.seed(1234)
Model.1.RF4<-randomForest(x = Modellingdata.RF4, y=Modellinglabel.RF, ntree = 1000, importance = TRUE)
Model.1.RF4
varImpPlot(Model.1.RF4)

# Perform a trial run in the test data and submit

Test.submit<-Learn.Combined[892:1309,c("Pclass","Titles","Familysize")]

Perdict1.RF <- predict(Model.1.RF4,Test.submit)
table(Perdict1.RF)

Submit.1<-data.frame(PassengerID = rep(892:1309),Survived = Perdict1.RF)
write.csv(Submit.1, file = "RF_Sub_04082018_1.csv", row.names = FALSE)

# Use cross validation to improve the accuracy of prediction - 10 Fold
library(caret)
library(doSNOW)
set.seed(2348)
cv.folds<-createMultiFolds(Modellinglabel.RF,k=10,times = 10)
table(Modellinglabel.RF)
table(Modellinglabel.RF[cv.folds[[33]]])
Ctrl.1<-trainControl(method = "repeatedcv", number = 10, repeats = 10, index = cv.folds)
cl<-makeCluster(3,type="SOCK")
registerDoSNOW(cl)
set.seed(34324)
Model.RFCV.1<-train(x = Modellingdata.RF4, y = Modellinglabel.RF, method = "rf", tuneLength = 3, ntree = 1000, trControl = Ctrl.1)
stopCluster(cl)
Model.RFCV.1

# Use cross validation to improve the accuracy of prediction - 5 Fold
set.seed(2348)
cv.folds<-createMultiFolds(Modellinglabel.RF,k=5,times = 10)
table(Modellinglabel.RF)
table(Modellinglabel.RF[cv.folds[[33]]])
Ctrl.1<-trainControl(method = "repeatedcv", number = 5, repeats = 10, index = cv.folds)
cl<-makeCluster(3,type="SOCK")
registerDoSNOW(cl)
set.seed(34324)
Model.RFCV.2<-train(x = Modellingdata.RF4, y = Modellinglabel.RF, method = "rf", tuneLength = 3, ntree = 1000, trControl = Ctrl.1)
stopCluster(cl)
Model.RFCV.2

# Use cross validation to improve the accuracy of prediction - 3 Fold
set.seed(2348)
cv.folds<-createMultiFolds(Modellinglabel.RF,k=3,times = 10)
table(Modellinglabel.RF)
table(Modellinglabel.RF[cv.folds[[33]]])
Ctrl.1<-trainControl(method = "repeatedcv", number = 3, repeats = 10, index = cv.folds)
cl<-makeCluster(3,type="SOCK")
registerDoSNOW(cl)
set.seed(34324)
Model.RFCV.1<-train(x = Modellingdata.RF4, y = Modellinglabel.RF, method = "rf", tuneLength = 3, ntree = 1000, trControl = Ctrl.1)
stopCluster(cl)
Model.RFCV.1

library(rpart)
library(rpart.plot)

# Improve the model using hyper parameter tuning   
# Create a function to run rpart modelling - Decision Tree
rpart.cvfunction<-function(seed, training, labels, ctrl) {
  cl<-makeCluster(3, type = "SOCK")
  registerDoSNOW(cl)
  set.seed(seed)
  rpart.cvfunction<-train(x = training, y = labels, method = "rpart", tuneLength = 30, trControl = ctrl)
  stopCluster(cl)
  return(rpart.cvfunction)
}

features<-c("Pclass","Titles","Familysize")
Rparttrainingdata <- Learn.Combined[1:891, features]
Model.CV.Rpart <- rpart.cvfunction(94622, Rparttrainingdata, Modellinglabel.RF, Ctrl.1)
Model.CV.Rpart

prp(Model.CV.Rpart$finalModel, type = 0, extra = 1, under = TRUE)

table(Learn.Combined$Titles)

Name.Split<-str_split(Learn.Combined$Name, ",")
Last.Name<-sapply(Name.Split, "[", 1)
Learn.Combined$Lastname<-Last.Name

Name.Split<-str_split(sapply(Name.Split,"[",2)," ")
Titles<-sapply(Name.Split,"[",2)
unique(Titles)

Learn.Combined[which(Titles == "the"),]
Titles[Titles %in% c("Dona.","the")]<-"Lady."
Titles[Titles %in% c("Ms.","Mlle.")]<-"Miss."
Titles[Titles %in% c("Mme.")]<-"Mrs."
Titles[Titles %in% c("Jonkheer.","Don.")]<-"Sir."
Titles[Titles %in% c("Col.","Capt.","Major.")]<-"Officer"
unique(Titles)
table(Titles)

Learn.Combined$New.Titles<-as.factor(Titles)

ggplot(Learn.Combined[1:891,], aes(x=New.Titles, fill=Survived))+
  geom_bar()+
  facet_wrap(~Pclass)+
  ggtitle("Survival rate by new title")

indexes<-which(Learn.Combined$New.Titles == "Lady.")
Learn.Combined$New.Titles[indexes]<-"Mrs."

indexes<-which(Learn.Combined$New.Titles == "Dr."| 
               Learn.Combined$New.Titles == "Rev."|
               Learn.Combined$New.Titles == "Sir."|
               Learn.Combined$New.Titles == "Officer")
Learn.Combined$New.Titles[indexes] <- "Mr."

features<-c("Pclass","New.Titles","Familysize")
Rparttrainingdata2 <- Learn.Combined[1:891, features]
Model.CV.Rpart.2 <- rpart.cvfunction(94622, Rparttrainingdata2, Modellinglabel.RF, Ctrl.1)
Model.CV.Rpart.2
prp(Model.CV.Rpart.2$finalModel, type = 0, extra = 1, under = TRUE)

indexes.first.mr<-which(Learn.Combined$New.Titles == "Mr." & Learn.Combined$Pclass == "1")
First.Mr.df<-Learn.Combined[indexes.first.mr,]     
summary(First.Mr.df)

First.Mr.df[First.Mr.df$Sex == "female",]

indexes <- which(Learn.Combined$New.Titles == "Mr." & Learn.Combined$Sex == "female")
Learn.Combined$New.Titles[indexes] <- "Mrs."

length(which(Learn.Combined$Sex == "female" & (Learn.Combined$New.Titles == "Mr." | Learn.Combined$New.Titles == "Master.")))
indexes.first.mr<-which(Learn.Combined$New.Titles == "Mr." & Learn.Combined$Pclass == "1")
First.Mr.df<-Learn.Combined[indexes.first.mr,] 

summary(First.Mr.df[First.Mr.df$Survived == "1",])
View(First.Mr.df[First.Mr.df$Survived == "1",])

indexes <- which(Learn.Combined$Ticket == "PC 17755"|
                 Learn.Combined$Ticket == "PC 17611"|
                 Learn.Combined$Ticket == "113760")
View(Learn.Combined[indexes,])

ggplot(First.Mr.df, aes(x=Fare, fill = Survived))+
  geom_density(alpha = 0.5)+
  ggtitle("1st Class survival by fare")

# Feature engineering to improve the accuracy
Ticket.Troop <- rep(0,nrow(Learn.Combined))
Avg.Fare<-rep(0.0, nrow(Learn.Combined))
Tickets <- unique(Learn.Combined$Ticket)
for (i in 1:length(Tickets)) {
  current.ticket <- Tickets[i]
  Troop.indexes<-which(Learn.Combined$Ticket == current.ticket)
  current.avg.fare <- Learn.Combined[Troop.indexes[1],"Fare"] / length(Troop.indexes)
  
  for (k in 1:length(Troop.indexes)) {
    Ticket.Troop[Troop.indexes[k]] <- length(Troop.indexes)
    Avg.Fare[Troop.indexes[k]] <- current.avg.fare
    }
}

Learn.Combined$Ticket.Troop <- Ticket.Troop
Learn.Combined$Avg.Fare <- Avg.Fare

First.Mr.df<-Learn.Combined[indexes.first.mr,] 
summary(First.Mr.df)

ggplot(First.Mr.df[First.Mr.df$Survived!= "None",], aes(x = Ticket.Troop, fill = Survived))+
  geom_density(alpha = 0.5)+
  ggtitle("Survival rate in 1st class by troop size")

ggplot(First.Mr.df[First.Mr.df$Survived!= "None",], aes(x = Avg.Fare, fill = Survived))+
  geom_density(alpha = 0.5)+
  ggtitle("Survival rate in 1st class by Avg Fare")

summary(Learn.Combined$Avg.Fare)
Learn.Combined[is.na(Learn.Combined$Avg.Fare),]

indexes <- with(Learn.Combined, which(Pclass == "3" & Titles == "Mr." & Familysize == "1" & Ticket != "3701"))
similar.na.passenger <- Learn.Combined[indexes,]
summary(similar.na.passenger$Avg.Fare)
Learn.Combined[is.na(Avg.Fare), "Avg.Fare"] <- 7.840

preproc.Learn.combined <- Learn.Combined[,c("Ticket.Troop", "Avg.Fare")]
Pre.Proc<-preProcess(preproc.Learn.combined, method = c("center", "scale"))
Post.proc <- predict(Pre.Proc, preproc.Learn.combined)

cor(Post.proc$Ticket.Troop, Post.proc$Avg.Fare)

indexes <- which(Learn.Combined$Pclass == "1")
cor(Post.proc$Ticket.Troop[indexes], Post.proc$Avg.Fare[indexes])

features<-c("Pclass","New.Titles","Familysize", "Ticket.Troop", "Avg.Fare")
Rparttrainingdata3 <- Learn.Combined[1:891, features]
Model.CV.Rpart.3 <- rpart.cvfunction(94622, Rparttrainingdata3, Modellinglabel.RF, Ctrl.1)
Model.CV.Rpart.3
prp(Model.CV.Rpart.3$finalModel, type = 0, extra = 1, under = TRUE)
