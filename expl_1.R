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
train_dest <- "input/train.csv"
test_dest <- "input/test.csv"
download.file(train_url, train_dest, mode="wb")
download.file(test_url, test_dest, mode="wb")

# reading the data
titanic <- read.csv(train_dest)
titanic_submission <- read.csv(test_dest)

# The first look at the data
str(titanic) # 891 x 12
summary(titanic)
# -> note:
# Survived: 38%, keep it as 0/1 for Kaggle submission
# Age: 177 x NA
# Cabin: 687 x '' -> 'unknown'
# Cabin: probably as.character + split:
#   see what's there (why a few cabins?), separate the letter and the number
# Embarked: 2 x '' -> unknown. Skewed.
# Fare: check for outlayers (few very high values)
# Name: as.character + split, see what's there (first name, title, family name)
# Ticket: as.charcter + see what's there (why 'CA'?)


# Exploring the variables
attach(titanic)
library(ggplot2)
# qplot(Survived)
table(Survived); hist(Survived)

# Pclass
hist(Pclass) # 1 = 2 = 1/2* 3
plot(tapply(Survived, Pclass, mean)) # class 1->3 -> decreasing survival

# Sex
table(Sex) # male = 2x female
plot(tapply(Survived, Sex, mean))

# Age
hist(Age) # quasi normal, a bit longer tail towards high values, but thicker towards low values
breaks <- seq(0,max(Age,na.rm=T),5)
bins <- cut(Age, breaks)
table(bins) # the last bins are too small
breaks <- c(head(breaks,-3),80)
bins <- cut(Age, breaks)
table(bins) # now OK
AgeGroup <- (tail(breaks,-1) + head(breaks,-1)) / 2
SurvivalByAgeGroup <- tapply(Survived, bins, mean)
plot(AgeGroup, SurvivalByAgeGroup); abline(lm(SurvivalByAgeGroup~AgeGroup))
qplot(AgeGroup, SurvivalByAgeGroup) + geom_smooth(method='lm')
# -> the older the less chance to survive,
#   though the pattern isn't clear for Age < 30
# The younger, the better.
# Average age per Sex and Pclass
tapply(Age, Sex, mean, na.rm=T) # very similar
boxplot(Age~Sex)
tapply(Age, Pclass, mean, na.rm=T) # 1 -> 38, 2 -> 30, 3 -> 25
boxplot(Age~Pclass) # a bit higher variance for class 1
# -> so maybe the dip around 20-30 is due to the concentration of 3rd class that has low survival rate?
# Pclass 3
SurvivalByAgeGroup <- tapply(Survived[Pclass==3], bins[Pclass==3], mean)
qplot(AgeGroup, SurvivalByAgeGroup) + geom_smooth(method='lm')
# Pclass 2
SurvivalByAgeGroup <- tapply(Survived[Pclass==2], bins[Pclass==2], mean)
qplot(AgeGroup, SurvivalByAgeGroup) + geom_smooth(method='lm')
# Pclass 1
SurvivalByAgeGroup <- tapply(Survived[Pclass==1], bins[Pclass==1], mean)
qplot(AgeGroup, SurvivalByAgeGroup) + geom_smooth(method='lm')
# Gender
SurvivalByAgeGroup <- tapply(Survived[Sex=='male'], bins[Sex=='male'], mean)
qplot(AgeGroup, SurvivalByAgeGroup) + geom_smooth(method='lm')
# -> the younger, the better
SurvivalByAgeGroup <- tapply(Survived[Sex=='female'], bins[Sex=='female'], mean)
qplot(AgeGroup, SurvivalByAgeGroup) + geom_smooth(method='lm')
# -> the older, the better (reverse!)

# Mid-conclusions:
# Sex+Pclass+Age gives much info

# SibSp
hist(SibSp) # mostly 0
hist(SibSp[SibSp!=0]) # 0 >> 1 >> 2,3,4 > 5,8
hist(log(SibSp+1)) # somewhat better look (but not needed)
plot(tapply(Survived, SibSp, mean))
# -> misleading: it shows 'index' instead of SibSp value
tab <- tapply(Survived, SibSp, mean)
plot(dimnames(tab)[[1]], tab)
# -> looks like +1 (=2) or +2 (=3) is best...
#   but we per Surname we found that even groups (2,4) are best, if class is 2 or 3.
# Plotting:
par(mfrow=c(1,1))
plot(1, xlim=c(0,8), ylim=c(0,1), type='n', ylab='Survival rate', xlab='SibSp')
for(i in 1:3) {
    tab <- tapply(Survived[Pclass==i], SibSp[Pclass==i], mean) 
    lines(dimnames(tab)[[1]], tab, col=i+1)
}
legend(5,0.8, c("Class 1", "Class 2", "Class 3"), lty=c(1,1,1),col=c(1,2,3)+1)
# -> class 1: +2, +1, best
# -> class 2: (+3,) +1, +2, best
sum(titanic$Pclass==2 & titanic$SibSp==3) # just one person
# -> class 3: +2, +1, best

# Parch
hist(Parch) # 0 > 1,2 > 3,4,5,6
hist(log(Parch+1)) # somewhat better look
tab <- tapply(Survived, Parch, mean)
plot(dimnames(tab)[[1]], tab)
# -> looks like +1,+2,+3 is best
par(mfrow=c(1,1))
plot(1, xlim=c(0,7), ylim=c(0,1), type='n', ylab='Survival rate', xlab='Parch')
for(i in 1:3) {
    tab <- tapply(Survived[Pclass==i], Parch[Pclass==i], mean) 
    lines(dimnames(tab)[[1]], tab, col=i+1)
}
legend(5,0.8, c("Class 1", "Class 2", "Class 3"), lty=c(1,1,1),col=c(1,2,3)+1)

# SibSp + Parch
FamilyCount <- SibSp + Parch
# Plotting:
par(mfrow=c(1,1))
plot(1, xlim=c(0,10), ylim=c(0,1), type='n', ylab='Survival rate', xlab='FamilyCount')
for(i in 1:3) {
    tab <- tapply(Survived[Pclass==i], FamilyCount[Pclass==i], mean) 
    lines(dimnames(tab)[[1]], tab, col=i+1)
}
legend(6,0.8, c("Class 1", "Class 2", "Class 3"), lty=c(1,1,1),col=c(1,2,3)+1)
# -> for classes 1, 2: improvment from +0 to +4
#   for class 3: improvement from +0 to +3

# Embarked
table(Embarked) # S >> C > Q, unknown x2
tapply(Survived, Embarked, mean) # C is best

# Fare
hist(Fare) # looks like exponentially decreasy + some high outlayers
hist(log(Fare)) # this looks quite nice
titanic[Fare>300,]
# -> just 3 cases, strange value: 512.3292, the same ticket: PC 17755
nrow(titanic[Ticket=='PC 17755',])
# -> just 3 cases. It looks like an error...
head(Fare[order(Fare, decreasing = T)], 20)
# -> OK, it can be correct. It maybe the case, that one ticket
#   covers a few people, so that ticket numbers and fares repeat.
#   Relation between Fare and Ticket should be checked for this.
length(unique(Ticket))
t_Ticket <- table(Ticket)
TicketCount <- t_Ticket[Ticket]
hist(TicketCount) # 1 > 2 > 3,4 > 5,6,7
# -> there are many multiple tickets.
sum(t_Ticket==1) # 547 individual tickets
sum(t_Ticket>1) # 134 group tickets
# -> maybe a group ticket meant a common fate?
tmp <- tapply(Fare[TicketCount>1], droplevels(Ticket[TicketCount>1]), sd)
head(tmp,20) # looks like in most cases, one Ticket -> the same Fare
sum(tmp) # but not always
names(tmp[tmp>0]) # one exception: ticket 7534
titanic[Ticket == names(tmp[tmp>0]),]
# -> these two (139, 877) have the same ticket number but different Fare
#   Maybe an error.
# Relation between ticket group size and fare value
tmp <- tapply(Fare, TicketCount, mean)
plot(tmp)
# -> not an obvious relation: increases for 1,2,3,4
#   but then 5,6,7 are all around the value for 2
#   maybe because of the class? (large group = cheap class)
plot(tapply(Fare[Pclass==1], TicketCount[Pclass==1], mean)) # increases 1->4
plot(tapply(Fare[Pclass==2], TicketCount[Pclass==2], mean)) # 1,2,3 simiar, 4 higher
plot(tapply(Fare[Pclass==3], TicketCount[Pclass==3], mean)) # steady increase 1->7
# -> value of the Fare may be misleading. Expensive ticket may mean
#   a high class or a low class but travelling in a group.
#   We should split two effects:
#   * impact of Fare (per person) on survival
#   * survival of groups (does commont ticket mean common fate?)
FarePerPerson <- Fare/as.numeric(TicketCount)
summary(Fare)
summary(FarePerPerson)
# -> Fare == 0 should be treated as NA
sum(Fare==0)
titanic[Fare==0,]
# -> see the 'LINE' ticket, appears 4x here. What is it?
# Comparing Fare and FarePerPerson:
par(mfrow=c(1,2))
hist(log(Fare)) # I don't add 1, as Fare=0 I treat as NA
hist(log(FarePerPerson))
boxplot(log(Fare+1)~factor(Pclass))
boxplot(log(FarePerPerson+1)~factor(Pclass))
# -> FarePerPerson gives much better separation of the passanger classes
# Checking outlayers in Pclass 1
head(FarePerPerson[Pclass==1][order(FarePerPerson[Pclass==1])])
head(FarePerPerson[order(FarePerPerson)],20)
# Impact on Survival
logFarePerPerson <- log(FarePerPerson+1)
breaks <- seq(min(logFarePerPerson),max(logFarePerPerson,na.rm=T),0.5)
bins <- cut(logFarePerPerson, breaks)
table(bins) # start from 1.5
breaks <- seq(1.5,max(logFarePerPerson,na.rm=T),0.5)
bins <- cut(logFarePerPerson, breaks)
table(bins) # start from 1.5
FareGroup <- (tail(breaks,-1) + head(breaks,-1)) / 2
SurvivalByFareGroup <- tapply(Survived, bins, mean)
qplot(FareGroup, SurvivalByFareGroup) + geom_smooth(method='lm')
# -> strong relatingship
#   but it looks like this is strongly related to the Pclass.

# Name
Name[1:50] # initial inspection
str(Name) # 891 levels, so this is unique
# -> Obtain information:
#   Surname, Title, Forename, NameLength
cName <- as.character(Name)
# NameLength
NameLength <- sapply(cName, nchar)
head(cName[order(NameLength, decreasing=T)])
# -> long names -> some aristocrats or what ever? Check Pclass and Fare:
par(mfrow=c(1,1))
boxplot(NameLength~Pclass) # yes, but only a weak effect
plot(FarePerPerson~NameLength) # not really...
plot(log(FarePerPerson)~NameLength, col=Pclass+1) # looks like the groups should be separated
par(mfrow=c(1,3))
for(i in 1:3) plot(log(FarePerPerson[Pclass==i])~NameLength[Pclass==i],
                   ylab = paste('Class ', i),  ylim=c(1,5.5), xlim=c(10,85), col=i+1)
# -> here we see 3 groups, but no important correlation within the groups
# Correlations:
cor(FarePerPerson, NameLength) # 0.16 very weak positive
for(i in 1:3) print(cor(FarePerPerson[Pclass==i], NameLength[Pclass==i]))
# -> no correlation for 1 and 3. Very weak 0.18 for 2.
# Surname, Tile, Forename
# Prototyping
head(cName)
l1 <- strsplit(cName[1:2], ',')
length(l1[[1]][2])
sapply(l1, length)
sapply(l1, function(x) print(x[2]))
strsplit(l1[[1]][2], '\\.')
l2 <- sapply(l1, function(x) strsplit(x[2], '\\.'))
l2
# Version 1
ns1 <- strsplit(cName, ',')
tmp <- sapply(ns1, length)
summary(tmp) # nice, all Names have exactly one ',' (comma)
for(i in seq_along(ns1)) {
    ns1[[i]][1] <- trimws(ns1[[i]][1])
    ns1[[i]][2] <- trimws(ns1[[i]][2])
}
head(ns1)
ns2 <- sapply(ns1, function(x) strsplit(x[2], '\\.'))
tmp <- sapply(ns2, length)
summary(tmp) # there's at least one 3 pieces name
ns2[tmp>2] # see it here
for(i in seq_along(ns1)) {
    ns1[[i]][1] <- trimws(ns1[[i]][1])
    ns1[[i]][2] <- trimws(ns1[[i]][2])
}
# Version 2 (more robust)
require(stringr)
ns1 <- str_split(cName, ',', 2) # Split: Surname | Title + Forename
for(i in seq_along(ns1)) {
    ns1[[i]][1] <- trimws(ns1[[i]][1])
    ns1[[i]][2] <- trimws(ns1[[i]][2])
}
head(ns1)
ns2 <- sapply(ns1, function(x) str_split(x[2], '\\.', 2)) # Split: Title | Forename
for(i in seq_along(ns2)) {
    ns2[[i]][1] <- trimws(ns2[[i]][1])
    ns2[[i]][2] <- trimws(ns2[[i]][2])
}
head(ns2)
list.as.df <- function(l, col.names=NULL) {
    df <- data.frame(matrix(unlist(l), nrow=length(l), byrow=T),
                     stringsAsFactors=FALSE)
    if(!is.null(col.names))
        names(df) <- col.names
    df
}
ns1df <- list.as.df(ns1, c('Surname','X'))
ns2df <- list.as.df(ns2, c('Title','Forename'))
NameStruct <- data.frame(ns1df, ns2df)
NameStruct$X <- NULL # Data frame: Surname, Title, Forename
sapply(NameStruct, function(x) length(unique(x)))
# 17 Titles, check them:
sort(table(NameStruct$Title), decreasing=T)
# -> Mr >> Miss > Mrs >> Master >> Dr, Rev > Col, Major, Mlle, Capt
#   Don, Jonkheer (check this), Lady, Mme, Ms, Sir, the Countess
titanic[NameStruct$Title=='Jonkheer',]
# -> a Dutch equivalent to "The honourable..."
# There are just 667 surnames. Let's check the most popular ones:
head(sort(table(NameStruct$Surname), decreasing=T), 10)
# -> Most popular surnames: Andersson, Sage, Carter, Goodwin, Johnson, Panula, Skoog, Rice
titanic[NameStruct$Surname=='Andersson',]
# -> one multip. ticket (all dead), two single ticket (survived)
titanic[NameStruct$Surname=='Sage',] # one ticket, all dead
titanic[NameStruct$Surname=='Carter',]
# -> ticket class 1: survived; ticket class 2 -> dead
titanic[NameStruct$Surname=='Goodwin',] # one ticket, all dead
titanic[NameStruct$Surname=='Johnson',] # ticket 1: survived; ticket 2,3: dead
#  Panula     Skoog      Rice
t_Surname <- table(NameStruct$Surname)
SurnameCount <- t_Surname[NameStruct$Surname]
# Survival rate for large groups vs small groups vs individuals
tapply(Survived, SurnameCount, mean)
# -> looks like size=2 is best (so couples possibly?)
#   but check this for class (higher class -> smaller groups) & gender
tapply(Survived[Pclass==1], SurnameCount[Pclass==1], mean)
tapply(Survived[Pclass==2], SurnameCount[Pclass==2], mean)
tapply(Survived[Pclass==3], SurnameCount[Pclass==3], mean)
# -> interesting: for class 2 and 3, even is better than odd (2,4 > 1,3).
#   For class 1 size 2 is best.
# Plotting:
par(mfrow=c(1,1))
plot(1, xlim=c(1,7), ylim=c(0,1), type='n', ylab='Survival rate', xlab='SurnameCount')
for(i in 1:3)
    lines(tapply(Survived[Pclass==i], SurnameCount[Pclass==i], mean), col=i+1)
legend(5,0.8, c("Class 1", "Class 2", "Class 3"), lty=c(1,1,1),col=c(1,2,3)+1)
# Check sd within vs accross families/tickets
# (H: low sd within family/ticket):
tmp <- which(SurnameCount>2)
mean(tapply(Survived[tmp], NameStruct$Surname[tmp], sd))
# -> it's only 0 or 0.500, 0.577. This is due to the discretness & small groups
sd(c(0,1,1));sd(c(0,0,1,1))
sd(Survived[tmp])


# Ticket
titanic[Ticket=='LINE',] # 4 cases, Fare==0; there are other tickets with Fare==0
levels(Ticket) # 681 levels
# -> do you want to explore these? (see below)
# TicketCount vs Survival
tapply(Survived[Pclass==1], TicketCount[Pclass==1], mean) # the more people on one ticket, the better, excluding 4
tapply(Survived[Pclass==2], TicketCount[Pclass==2], mean) # the same (excluding (4-)5)
tapply(Survived[Pclass==3], TicketCount[Pclass==3], mean) # the same (excluding 4-7)
# -> in general it's best to have a ticket for 3.
par(mfrow=c(1,1))
plot(1, xlim=c(1,7), ylim=c(0,1), type='n', ylab='Survival rate', xlab='TicketCount')
for(i in 1:3)
    lines(tapply(Survived[Pclass==i], TicketCount[Pclass==i], mean), col=i+1)
legend(5,0.8, c("Class 1", "Class 2", "Class 3"), lty=c(1,1,1),col=c(1,2,3)+1)
##################
# Ticket types: number only, A..., C..., P..., F..., S..., SOTON.../STON..., W...
cTicket <- as.character(Ticket)
tic_num <- grep('^[0-9]', cTicket)
tic_A <- grep('^A', cTicket)
tic_C <- grep('^C', cTicket)
tic_P <- grep('^P', cTicket)
tic_F <- grep('^F', cTicket)
tic_S <- grep('^S', cTicket)
tic_STN <- grep('^SOTON|^STON', cTicket)
tic_W <- grep('^W', cTicket)
#tic_ <- grep('^', cTicket)
tmp <- !tic_S %in% tic_STN
tic_S <- tic_S[tmp] # excluding 'STN' tickets from the 'S...' ticket group
# Are these all tickets?
# Below is like data dredging
mean(Survived[tic_num]) # 38%
mean(Survived[tic_A]) # very low, 7%
mean(Survived[tic_C]) # 34%
mean(Survived[tic_P]) # very high, 65%
mean(Survived[tic_F]) # 57%
mean(Survived[tic_S]) # 37%
mean(Survived[tic_STN]) # 28%
mean(Survived[tic_W]) # 15%
table(titanic[tic_P,]$Pclass)
table(titanic[tic_A,]$Pclass)
table(titanic[tic_F,]$Pclass)
##################

# Cabin
str(Cabin)
levels(Cabin)
# -> versions:
#   letter+number
#   1-3 cabin ids
#   two letters + a number
#   letter alone
#   '' (missing)
head(sort(table(Cabin), decreasing = T))
# -> very bad... these are mostly missing data!
summary(titanic[Cabin=='',])
summary(titanic[Cabin!='',])


# NetworkEffect
NetworkEffect <- pmax(FamilyCount, TicketCount, SurnameCount)
NetworkEffect <- (FamilyCount + as.numeric(TicketCount) + as.numeric(SurnameCount)) / 3
summary(NetworkEffect)
# Plotting:
par(mfrow=c(1,1))
plot(1, xlim=c(1,10), ylim=c(0,1), type='n', ylab='Survival rate', xlab='NetworkEffect')
for(i in 1:3)
    lines(tapply(Survived[Pclass==i], NetworkEffect[Pclass==i], mean), col=i+1)
legend(7,0.8, c("Class 1", "Class 2", "Class 3"), lty=c(1,1,1),col=c(1,2,3)+1)

# Missing Values
# do they correlate with non-survival?
# or maybe we miss data for low class passangers, and these were also the least probable to survive?
# check this within the classes and sexes

# Look for feature engineering:
# qda  (initially glm/lda) of Pclass on FarePerPerson


