# postprediction function
post_prediction <- function(x,y) {
    require(plyr)
    f <- revalue(pred, c(Survived=1, Died=0)) # convert to a factor 1 / 0
    n <- as.numeric((levels(f))[f]) # convert to a numeric 1/0
    data.frame(PassengerId=y, Survived=n)
}

# create submission file
submit <- function(df, i) {
    if(!file.exists('subm')) {
        dir.create('subm')
    }
    i <- 1
    name <- paste0('subm/subm',i,'.csv')
    write.csv(submission, name, row.names=FALSE)
}

# final prediction
preSubmission <- titanic_preprocess(titanic_submission, training = F)
pred <- predict(m.rf, newdata=preSubmission)
submission <- post_prediction(pred, titanic_submission$PassengerId)
submit(submission, 1)

# submission 1
# lang: R
# NA: medians
# features: basic + ticket count
# model: RF
# CV: 2
