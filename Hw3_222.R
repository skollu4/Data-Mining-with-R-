library(C50)
setwd("C:/Users/Shanthi Kollu/Documents/DB2/HW3")

training <- read.csv("csci527_hw3_training.csv", header = TRUE, nrows = 100000 )

test <- read.csv("csci527_hw3_test.csv", header = TRUE)

#pre-processing

head(training)

#converting integer values to factors for predicting models to be computed accurately
training$championId0 <- as.factor(training$championId0)
training$championId1 <- as.factor(training$championId1)
training$championId2 <- as.factor(training$championId2)
training$championId3 <- as.factor(training$championId3)
training$championId4 <- as.factor(training$championId4)


test$championId0 <- as.factor(test$championId0)
test$championId1 <- as.factor(test$championId1)
test$championId2 <- as.factor(test$championId2)
test$championId3 <- as.factor(test$championId3)
test$championId4 <- as.factor(test$championId4)


#shuffling or rearrange the rows to prevent bias

training <- training[sample(nrow(training)),]

x <- training[, 1:5]
y <- training[, 6]

testX <- test[, 2:6]

#data mining

# Constructing the model
model <- C5.0(x, y)
model <- C50::C5.0(x, y, trials = 10)
 
# apply the constructed model to do prediction
 
p_prob <- predict(model, test, type = "prob")
 
# Post-processing

winner <- p_prob[,2]

summary(model)
 
id <- test$id
 
submit <- data.frame(id, winner)

write.csv(submit, file = "HW3results_test_prob.csv", row.names = FALSE)
 
 
 