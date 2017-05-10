# general functions
list.as.df <- function(l, col.names=NULL) {
    df <- data.frame(matrix(unlist(l), nrow=length(l), byrow=T),
                     stringsAsFactors=FALSE)
    if(!is.null(col.names))
        names(df) <- col.names
    df
}
NA_per_column <- function(df) {
    x <- sapply(df, function(x) sum(is.na(x)))
    x[x>0]
}
impute_medians <- function(df) {
    nas <- NA_per_column(df)
    #print(nas)
    for(i in names(nas)) {
        m <- median(df[,i], na.rm=T)
        #cat(i,m)
        df[!complete.cases(df[,i]),i] <- m
    }
    #print(NA_per_column(df))
    return(df)
}
# titanic2 <- impute_medians(titanic)
# summary(titanic2)
# NA_per_column(titanic2)
# i <- "Age"
# m <- median(titanic[,i], na.rm=T)
# tmp <- !complete.cases(titanic[,i])
# titanic[!complete.cases(titanic[,i]),i]
# titanic[!complete.cases(titanic[,i]),i] <- m
# titanic[tmp,i]

# preprocessing functions
titanic_preprocess <- function(data, training=T) {
    print('Titanic data preprocessing')
    print('Imputing missing data: medians')
    data <- impute_medians(data)
    # dim(titanic[,-1:-2])
    # preObj <- preProcess(titanic[,-1:-2], method='knnImpute')
    # str(preObj)
    # predict(preObj, titanic[,-1:-2])
    print('+TicketCount')
    t_Ticket <- table(data$Ticket)
    TicketCount <- as.numeric(t_Ticket[data$Ticket])
    print('+logFarePerPerson')
    logFarePerPerson <- log(data$Fare/TicketCount+1) # though Fare=0 should be a missing data?
    print('preproc: Name')
    cName <- as.character(data$Name)
    #NameLength <- sapply(cName, nchar)
    #print('name - surname')
    require(stringr)
    ns1 <- str_split(cName, ',', 2) # Split: Surname | Title + Forename
    for(i in seq_along(ns1)) {
        ns1[[i]][1] <- trimws(ns1[[i]][1])
        ns1[[i]][2] <- trimws(ns1[[i]][2])
    }
    #print('name - title & forename')
    ns2 <- sapply(ns1, function(x) str_split(x[2], '\\.', 2)) # Split: Title | Forename
    for(i in seq_along(ns2)) {
        ns2[[i]][1] <- trimws(ns2[[i]][1])
        ns2[[i]][2] <- trimws(ns2[[i]][2])
    }
    print('+Surname, Title, Forename')
    ns1df <- list.as.df(ns1, c('Surname','X'))
    ns2df <- list.as.df(ns2, c('Title','Forename'))
    NameStruct <- data.frame(ns1df, ns2df)
    NameStruct$X <- NULL # Data frame: Surname, Title, Forename
    Surname <- NameStruct$Surname
    Title <- NameStruct$Title
    Forename <- NameStruct$Forename
    print('+SurnameCount')
    t_Surname <- table(Surname)
    SurnameCount <- as.numeric(t_Surname[Surname])
    print('+FamilyCount')
    FamilyCount <- data$SibSp + data$Parch
    print('+NetworkEffect')
    NetworkEffect <- pmax(FamilyCount, TicketCount, SurnameCount)
    #cTicket <- as.character(data$Ticket)
    #df <- df[complete.cases(df),]
    if(training) {
        print('+fSurvived (factor response)')
        fSurvived <- factor(data$Survived, levels=c(1,0), labels=c('Survived', 'Died'))
        print('Creating the final data frame')
        df <- data.frame(data,
                        logFarePerPerson, Title, TicketCount, FamilyCount, NetworkEffect,
                        fSurvived,
                        row.names = NULL) # not sure why this is needed
        df <- subset(df, select=-c(PassengerId,Survived,Name,Ticket,Fare,Cabin))
    } else {
        print('Creating the final data frame')
        df <- data.frame(data,
                         logFarePerPerson, Title, TicketCount, FamilyCount, NetworkEffect,
                         row.names = NULL) # not sure why this is needed
        df <- subset(df, select=-c(PassengerId,Survived,Name,Ticket,Fare,Cabin))
    }
    return(df)
}

# Model prototyping
preTitanic <- titanic_preprocess(titanic)
dim(preTitanic)
#summary(preTitanic)
#head(preTitanic)
#head(titanic)
library(caret)
inTrain <- createDataPartition(preTitanic$fSurvived, p=0.8, list=F)
training <- preTitanic[inTrain,]
testing <- preTitanic[-inTrain,]

# Benchmarks
# 0
pred <- factor(rep('Died', nrow(testing)), levels=c('Survived', 'Died'), labels=c('Survived', 'Died'))
confusionMatrix(pred,testing$fSurvived) # accu. .6158;; 0.5915
# 1
pred <- factor(testing$Sex, levels=c('female', 'male'), labels=c('Survived', 'Died'))
confusionMatrix(pred,testing$fSurvived) # accu. .7627;; 0.7394; 0.7606; 0.7817; 0.7887

# Formulae
f0 <- fSurvived~.
f1 <- fSurvived~Pclass+Sex+Age
f2 <- fSurvived~Pclass+Sex+Age+TicketCount
f3 <- fSurvived~Pclass+Sex+Age


# GLM fit
trCtrl <- trainControl(method='cv', number=2)
m.glm <- train(f2,
               data=training, method='glm',
               trControl=trCtrl)
m.glm
summary(m.glm$finalModel)
pred <- predict(m.glm, newdata=testing)
confusionMatrix(pred, testing$fSurvived) # acc. .7628;; 0.8662; 0.8099, even 84%
# Tree fit
trCtrl <- trainControl(method='cv', number=2)
m.tree <- train(f2,
                data=training, method='rpart',
                trControl=trCtrl)
m.tree
plot(m.tree$finalModel, uniform=T, main='Titanic')
text(m.tree$finalModel, use.n=T, all=T)
pred <- predict(m.tree, newdata=testing)
confusionMatrix(pred, testing$fSurvived) # acc. .7797;; 0.7817; 0.7746; 0.8125
# RF fit
trCtrl <- trainControl(method='cv', number=2)
m.rf <- train(f2,
              data=training, method='rf',
              trControl=trCtrl)
m.rf
pred <- predict(m.rf, newdata=testing)
confusionMatrix(pred, testing$fSurvived) # acc. .7966; 0.8028 ; 0.8387; 0.871 (with Ticket)

# Conclusions:
# - deal with missing values: NA -> automated imputation
# - look again at the predictors
# - select important predictors (you use many of them, while most is irrelevant)
# - check for any correlations between predictors (e.g. Title and Gender)
# - run models with fewer predictors
# - set a strategy for variable selection
# - run some models
# - check if your preprocessing actually improves predictions comparing with raw variables
# - compare different type of models
#   - do they err for the same data?
#   - mabye sometimes trees are better and sometimes glm or some other model?
#   - if so then check how to merge models for making predictions
# - submit

