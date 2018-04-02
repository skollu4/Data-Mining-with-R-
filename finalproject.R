#install.packages("readr");
#install.packages("C50")


library(C50)
setwd("C:/Users/Shanthi Kollu/Documents/DB2/Final")

train <- read.csv("finalproject_training.csv");
head(train);

orderedTrain <- train[c(1, 13, 3, 4, 7, 8, 5, 6, 11, 12, 9, 14, 10, 15, 2)];
head(orderedTrain);

test <- read.csv("finalproject_test.csv");
head(test);

orderedTest <- test[c(1, 12, 3, 4, 7, 8, 5, 6, 10, 11, 9, 13, 14, 2)];
head(orderedTest);


summary(orderedTrain);

orderedTrain[, c(6)] <- sapply(orderedTrain[, c(6)], as.numeric);

summary(orderedTrain);


cols <- sapply(orderedTrain, is.logical)
orderedTrain[,cols] <- lapply(orderedTrain[,cols], as.numeric)
head(orderedTrain)

cols <- sapply(orderedTest, is.logical)
orderedTest[,cols] <- lapply(orderedTest[,cols], as.numeric)

summary(orderedTest);

# Change winner column from True to 1 and False to 0
orderedTrain$winner <- orderedTrain$winner == 'True'
orderedTrain$winner <- 1 * orderedTrain$winner

X_train <- orderedTrain[,2:14]
y_train <- orderedTrain[,15]

X_test <- orderedTest[,2:14]
y_train <- as.factor(orderedTrain$winner)

#help(C5.0)


model <- C5.0(X_train, y_train, type="prob", trails = 100)


prediction = predict.C5.0(model, X_test, type="prob")

result <- cbind(RecordID = orderedTest$RecordID, winner = prediction)

#submit <- data.frame(RecordID, winner)

write.csv(result, file = "outputFinal.csv", row.names = FALSE)

