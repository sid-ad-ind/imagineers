source("lib.r")
#set input directory
inputDirectory = "True+False Floods"
#all files of the directory
filenames <- list.files(inputDirectory, full.names = TRUE)

#set up writing
logFile = "logFile_listOfTrueFloods.txt"
cat("", file=logFile, sep = "\n")

#setup directory - list of true floods
TrueFloodsDir = 'TrueFloods'
if (dir.exists(TrueFloodsDir)) {
  unlink(TrueFloodsDir, recursive=TRUE)
}
dir.create(TrueFloodsDir)

outputCsvFile = "output.csv"
headerOfoutputCsvFile <- cbind("percentOfUniqueAlarms","natureOfFlood")
write.table( headerOfoutputCsvFile,file=outputCsvFile, sep=',', row.names=F, col.names=F )
tf = 0
ff = 0
#loop over each file
for (f in filenames) {
  c <- calculationsInCsv(f,outputCsvFile)
  o <- labelFlood(f)
  if (o == 'True flood') {
    cat(f, file=logFile, append=TRUE, sep = "\n")
    file.copy(f,TrueFloodsDir)
    tf <- tf + 1
  } else {
    ff <- ff + 1
  }
}

pie_1 = c(tf,ff)
pie_2 = c("true_flood","false_flood")

#classification step
classificationDataModel <- read.csv(file = outputCsvFile)
classificationDataModel$natureOfFlood <- as.factor(classificationDataModel$natureOfFlood)
head(classificationDataModel)
#pairs.panels(classificationDataModel[-1])

#commented unwanted graphs, can be enabled
# classificationDataModel %>%
#   ggplot(aes(x=natureOfFlood, y=percentOfUniqueAlarms, fill = natureOfFlood)) +
#   geom_boxplot() +
#   ggtitle("Box Plot")
# 
# classificationDataModel %>% ggplot(aes(x=percentOfUniqueAlarms, fill = natureOfFlood)) +
#   geom_density(alpha=0.8, color= 'black') +
#   ggtitle("Density Plot")

# Data Partition
set.seed(1234)
#80 % for training and 20% for testing
ind <- sample(2, nrow(classificationDataModel), replace = T, prob = c(0.8, 0.2))
train <- classificationDataModel[ind == 1,]
test <- classificationDataModel[ind == 2,]

train

# Naive Bayes Model
model <- naive_bayes(natureOfFlood ~ ., data = train, usekernel = T)
model

train %>%
  filter(natureOfFlood == "1") %>%
  summarise(mean(percentOfUniqueAlarms), sd(percentOfUniqueAlarms))

plot(model)

# Predict
p <- predict(model, train, type = 'prob')
head(cbind(p, train))

# Confusion Matrix - train data
cm_train_data <- predict(model, train)
(tab1 <- table(cm_train_data, train$natureOfFlood))
#misclassifications
1 - sum(diag(tab1)) / sum(tab1)
#sensitivity(cm_train_data)
#confusionMatrix(cm_train_data,"1")

# Confusion Matrix - test data
cm_test_data <- predict(model, test)
(tab2 <- table(cm_test_data, test$natureOfFlood))
#misclassifications
1 - sum(diag(tab2)) / sum(tab2)

