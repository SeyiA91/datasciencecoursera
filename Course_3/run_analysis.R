rm(list = ls())
packages <- c("data.table", "reshape2", "dplyr", "plyr")
sapply(packages, require, character.only = T, quietly = T)

run_analysis <- function(directory) {
    setwd(directory)
    
    train.path <- 'train'
    xTrain <- fread(file.path(train.path, 'X_train.txt'))
    yTrain <- fread(file.path(train.path, 'Y_train.txt'))
    subTrain <- fread(file.path(train.path, 'subject_train.txt'))
    
    test.path <- 'test'
    xTest <- fread(file.path(test.path, 'X_test.txt'))
    yTest <- fread(file.path(test.path, 'Y_test.txt'))
    subTest <- fread(file.path(test.path, 'subject_test.txt'))
    
    features <- fread('features.txt')
    activities <- fread('activity_labels.txt')
    
    subMerged <- rbind(subTrain, subTest)
    xMerged <- rbind(xTrain, xTest)
    yMerged <- rbind(yTrain, yTest)
    
    names(xMerged) <- features$V2
    names(yMerged) <- 'activity_label'
    names(subMerged) <- 'subject'
    
    mergedDf <- cbind(subMerged, yMerged, xMerged)
    names(mergedDf) <- tolower(names(mergedDf))
    mergedDf$activity_label <- as.factor(mergedDf$activity_label)
    levels(mergedDf$activity_label) <- activities$V2
    
    means <- unlist(lapply(mergedDf[-(1:2)], mean))
    sdevs <- unlist(lapply(mergedDf[-(1:2)], sd))

    sumDf <- aggregate(mergedDf, list(subject = mergedDf$subject, activity = mergedDf$activity), mean)
    sumDf <- sumDf[,-(3:4)]
    return(sumDf)
}

dir.path <- "~/dev/datasciencecoursera/datasciencecoursera/Course_3/UCI HAR Dataset/"
df <- run_analysis(dir.path)
write.table(df, 'aggregateDf.txt')