# RESULT here:
# http://rpubs.com/Ibah/110043
rm(list=ls())

getwd()
if(!file.exists("data")) dir.create("data")

resetPar <- function() {
    dev.new()
    op <- par(no.readonly = TRUE)
    dev.off()
    op
}

# dumping R objects into files
getwd()
y  <- data.frame(a = 1, b = "a") ## a (numeric) and b (factor) columns
dput(y) ## deparses y
dput(y, file = "y.R") ## deparses y into an R file
new.y <- dget("y.R")
# dump(c("x,"y"), file=...) # for many objects
# source(file)

# VARIABLE DESCRIPTIONS:
#     survival        Survival
# (0 = No; 1 = Yes)
# pclass          Passenger Class
# (1 = 1st; 2 = 2nd; 3 = 3rd)
# name            Name
# sex             Sex
# age             Age
# sibsp           Number of Siblings/Spouses Aboard
# parch           Number of Parents/Children Aboard
# ticket          Ticket Number
# fare            Passenger Fare
# cabin           Cabin
# embarked        Port of Embarkation
# (C = Cherbourg; Q = Queenstown; S = Southampton)

# downloading the data
train_url <- "http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/train.csv"
test_url <- "http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/test.csv"
train_dest <- "data/train.csv"
test_dest <- "data/test.csv"
download.file(train_url, train_dest, mode="wb")
download.file(test_url, test_dest, mode="wb")

# reading the data
train <- read.csv(train_dest)
test <- read.csv(test_dest)

ncol(train)
nrow(train)
names(train)
sapply(train, class)
str(train)
summary(train)
head(train)

ncol(test)
nrow(test)
names(test)
head(test)

length(unique(train$Cabin))
table(train$Cabin, useNA = "always")[1]
# 891 into 148 cabins only, cabin recorder for everyone, but " " is set for 687 records!
train$Cabin == ''


# APPROACH ONE: INTRODUCE NAs FOR ALL MISSING DATA


# checking missing values
sum(is.na(train)) # 177
sum(!complete.cases(train)) # 177

df <- data.frame(a=c(1,2,NA), b=c(NA,4,5))
sapply(df, function(x){sum(is.na(x))})
missing_values_per_column <- function(df) {
    x <- sapply(df, function(x){sum(is.na(x))})
    x[x>0]
}
missing_values_per_column(df)
#missing_values_per_column(train) # Age (177)
#missing_values_per_column(test)  # Age (86) and Fare (1)

sapply(train, function(x){sum(is.na(x))}) # Age (177)
sapply(test, function(x){sum(is.na(x))}) # Age (86) and Fare (1)
sapply(train, function(x){sum(x=="", na.rm=T)}) # Cabin (687) and Embarked (2) have ""/null values
sapply(test, function(x){sum(x=="", na.rm=T)}) # Cabin (327) only has ""/null values
sapply(train, function(x){sum(x==0, na.rm=T)}) # Fare (15) (SibSp and Parch also but that's OK)
sapply(test, function(x){sum(x==0, na.rm=T)}) # Fare (2) (SibSp and Parch also but that's OK)

equal <- function (a, b) {
    if (is.na(b)) {
        equal <- is.na(a)
    } else {
        equal <- !is.na(a) & a==b
    }
}
print(equal(df$a, NA))
print(equal(df$a, 1))
# Viewing some missing values
train[train$Embarked=='',] # 62, 830, but it shows bad results if NA present in the column
# 15x Fare in train, assuming 0 fare is not applicable
train[train$Fare==0,] # 180, ..., 823
train[equal(train$Fare,0),]
# 2x Fare in test, assuming 0 fare is not applicable
test[test$Fare==0,] # 267, 373
test[equal(test$Fare,0),]

# Inputting NA
train[is.na(train$Age),]$Age #OK
test[is.na(test$Age),]$Age #OK
test[is.na(test$Fare),]$Fare #OK
train[train$Cabin=="",]$Cabin <- NA
train[train$Embarked=="",]$Embarked <- NA
test[test$Cabin=="",]$Cabin <- NA
train[train$Fare==0,]$Fare <- NA
test[test$Fare==0,]$Fare <- NA # Error
test[test$Fare==0&!is.na(test$Fare),]$Fare <- NA # correct

head(train[NA,]) # see what it gives if you subset with an NA - don't do that

# now you have the NA's introduced:
sapply(train, function(x){sum(is.na(x))})
sapply(test, function(x){sum(is.na(x))})
summary(train)


# EXPLORATORY DS (EXCLUDING NAs)


# Exploration
library(ggplot2)

train$Sex=='female'
any(is.na(train[complete.cases(train),])) # no NA's

hist(train$Survived, breaks=c(0,0.5,1))
qplot(train$Survived)
par(mfrow=c(1,2))
hist(train[train$Sex=='female',]$Survived, breaks=c(0,0.5,1), main='female', xlab='Survived')
hist(train[train$Sex=='male',]$Survived, breaks=c(0,0.5,1), main='male', xlab='Survived')
par(mfrow=c(1,1))
boxplot(train$Survived) # helpless
summary(train[complete.cases(train),]$Survived)
summary(train[complete.cases(train),]$Sex)
# barplot(train[complete.cases(train),]$Survived, train[complete.cases(train),]$Sex) # senseless

drops = c("PassengerId", "Name", "Survived", "Cabin","Ticket")
names(train)[!names(train) %in% drops] # all other columns
# train[,!names(train) %in% drops]
# subset(train, select = -drops)
plot(train[,!names(train) %in% drops])


# no point in this as there's no trend in "Survived"
# you need to split the sample into Survived T/F and then look for differences between the groups

library(lattice)
xyplot(train$Fare ~ train$Age | train$Survived)
xyplot(train$Fare ~ train$Age | train$Survived)
xyplot(train$Fare ~ train$Age | train$Survived)
class(train)
sapply(train, class)

plot(train[train$Survived==0,!names(train) %in% drops])
plot(train[train$Survived==1,!names(train) %in% drops])
library(scales)
sapply(train[train$Survived==0,c('Pclass','Age','SibSp','Parch','Fare')], mean, na.rm=T)
sapply(train[train$Survived==1,c('Pclass','Age','SibSp','Parch','Fare')], mean, na.rm=T)
# 1: +pclass,+SipSp,-Parch,-Fare
lapply(lapply(train[train$Survived==0,c('Sex','Embarked')],table),function(x){x/sum(x)})
lapply(lapply(train[train$Survived==1,c('Sex','Embarked')],table),function(x){x/sum(x)})
# 1: +female, +C, -S

# ISL p. 129
# EDS
par(mfrow=c(2,2))
with(train, boxplot(Pclass~Survived, col=Survived+2, main='Pclass'))
with(train, boxplot(Age~Survived, col=Survived+2, main='Age'))
with(train, boxplot(Parch~Survived, col=Survived+2, main='Parch'))
with(train, boxplot(log(Fare)~Survived, col=Survived+2, main='log(Fare)'))
par(mfrow=c(1,1))
with(train, plot(Parch, log(Fare), col=Survived+2))
with(train, plot(Age, log(Fare), col=Survived+2))
with(train, plot(Parch, Pclass, col=Survived+2))


# LOGISTIC REGRESSION

# Pclass (as quantitative)
m <- glm(Survived ~ Pclass, data=train, family='binomial')
summary(m)
with(train, plot(Survived ~ Pclass, xlab='Pclass', ylab='Survived'))
points(train$Pclass, m$fitted, pch=19, col='blue')
p <- predict(m, data.frame(Pclass=test$Pclass))
summary(p)
# conclusion:
# make sure to always pass training and test data as data frames
# only this way the variables can be properly recognized by the predict function

# Sex (as nominal/factor)
m <- glm(Survived ~ Sex, data=train, family='binomial')
summary(m)
with(train, plot(Survived ~ Sex, xlab='Sex', ylab='Survived'))
points(train$Sex, m$fitted, pch=19, col='blue')
p <- predict(m, data.frame(Sex=test$Sex))
summary(p)


# All variables
logRegM <- glm(Survived ~ Pclass+Sex+Age+SibSp+Parch+Fare+Embarked, data=train, family='binomial')
summary(logRegM) # factors treated properly, nice
# use only: Pclass, Sex, Age, SibSp
logRegM1 <- glm(Survived ~ Pclass+Sex+Age+SibSp, data=train, family='binomial')
summary(logRegM1)
# prediction
pred <- predict(logRegM1, test, type="response")
length(pred) # OK, should be 418
summary(pred) # 86 NA's

# solution for cutoff of 0.5
solution_0.5 <- pred > 0.5 # Survived==Yes

# checking in-sample accuracy
p_hat <- predict(logRegM1, train, type="response")
y_hat <- p_hat > 0.5
mean(y_hat==train$Survived, na.rm=T) # accuracy 0.8081
length(train$Survived)
length(y_hat)
length(logRegM1$fitted.values) # shorter due to NA's!

# selecting optimal cutoff
accuracy <- function(cutoff, m, true_y) {
    return(mean((m$fitted.values > cutoff) == true_y), na.rm=T)
}
accuracy(0.5,logRegM1, train$Survived) # this doesn't work due to NA
o <- optim(0.5,
           function(cutoff) mean((p_hat>cutoff) == train$Survived, na.rm=T),
           control=list(fnscale=-1))
o$par # 0.6
o$value # 0.8137
o <- optim(0.5,
           function(cutoff) mean((p_hat>cutoff) == train$Survived, na.rm=T),
           control=list(fnscale=-1),
           method="Brent", lower=0, upper=1)
o$par # 0.5958
o$value # 0.8137

# using optimal cut-off level:
solution_0.6 <- pred > 0.6



# deal with the NA's
# submit this to Kaggle, see what you got
