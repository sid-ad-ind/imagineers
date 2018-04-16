
library("arules")

library("arulesViz")



#Load data set:

data("Groceries")

summary(Groceries)

#Look at data:

inspect(Groceries[1])

LIST(Groceries)[1]

rules = apriori(Groceries, parameter=list(support=0.001, confidence=0.5))

#Inspect the top 5 rules in terms of lift:

inspect(head(sort(rules, by ="lift"),5))

#Plot a frequency plot:

itemFrequencyPlot(Groceries, topN = 25)

#Scatter plot of rules:

library("RColorBrewer")

plot(rules,control=list(col=brewer.pal(11,"Spectral")),main="")

#Rules with high lift typically have low support.

#The most interesting rules reside on the support/confidence border which can be clearly seen in this plot.

#Plot graph-based visualisation:


library("arules")
library("rCBA")
data("iris")
train <- sapply(iris,as.factor)
train <- data.frame(train, check.names=FALSE)
txns <- as(train,"transactions")
rules = apriori(txns, parameter=list(support=0.03, confidence=0.03, minlen=2),
                appearance = list(rhs=c("Species=setosa", "Species=versicolor", "Species=virginica"),default="lhs"))
rulesFrame <- as(rules,"data.frame")
print(nrow(rulesFrame))
prunedRulesFrame <- rCBA::pruning(train, rulesFrame, method="m2cba")
print(nrow(prunedRulesFrame))

plot(rulesFrame)


library("arules")
library("rCBA")
library("arulesViz")

files <- list.files("C:/Users/mohit/Desktop/Floods", pattern=".csv", full.name=TRUE)

lists <- list()
clusters <- list("1","2")

for(i in 1:length(clusters)) {
  lists[clusters[[i]]] = files[i]
}
for(i in 1:length(lists)) {
  data <- read.csv(lists[i],stringsAsFactors = FALSE, header= TRUE, row.names = NULL)
  rec1[i] <- data['Alarm.id']
  
}
#rec1 <- read.csv(file="C:\\Users\\mohit\\Desktop\\New folder\\c1\\ALARM_LIST_ALARM_FLOODS_2016-12-01 00_00_00_2016-12-01 04_45_00.csv", header=TRUE, sep=",")
train <- data.frame(rec1, check.names=FALSE)
txns <- as(train,"transactions")
# rules = rCBA::fpgrowth(txns, support=0.9, confidence=0.7, maxLength=2, consequent="Alarm.Id")
# rulesFrame <- as(rules,"data.frame")
basket_rules<- apriori(txns,parameter = list(sup=0.9,conf=0.70,target='rules'))
summary(basket_rules)
as(basket_rules,train)
subrules2 <- tail(sort(basket_rules,by='lift'),10)
plot(subrules2,method="graph",engine="htmlwidget")


predictions <- rCBA::classification(train,rulesFrame)
table(predictions)
sum(train$Alarm.Id==predictions,na.rm=TRUE)/length(predictions)

prunedRulesFrame <- rCBA::pruning(train, rulesFrame, method="m2cba")
predictions <- rCBA::classification(train, prunedRulesFrame)
plot(table(predictions))
sum(train$Alarm.Id==predictions,na.rm=TRUE)/length(predictions)


# print(nrow(rulesFrame))
# prunedRulesFrame <- rCBA::pruning(train, rulesFrame, method="m2cba")
# print(nrow(prunedRulesFrame))
# plot(rulesFrame)


# rules = apriori(txns, parameter=list(support=0.03, confidence=0.03, minlen=2),
#                 appearance = list(rhs=c("rec1=Q1", "rec1=Q2", "rec1=Q3" ,"rec1=Q4" ,"rec1=Q5", "rec1=Q6"),default="lhs"))
# rulesFrame <- as(rules,"data.frame")
# fpgrowth(rec1, support = 0.01, confidence = 1, maxLength = 5,
#          consequent = NULL)


# library("rCBA")
# data("iris")
# 
# train <- sapply(iris,as.factor)
# train <- data.frame(train, check.names=FALSE)
# txns <- as(train,"transactions")
# 
# rules = rCBA::fpgrowth(txns, support=0.03, confidence=0.03, maxLength=2, consequent="Species")
# rulesFrame <- as(rules,"data.frame")
# 
# predictions <- rCBA::classification(train,rulesFrame)
# table(predictions)
# sum(train$Species==predictions,na.rm=TRUE)/length(predictions)
# 
# prunedRulesFrame <- rCBA::pruning(train, rulesFrame, method="m2cba")
# predictions <- rCBA::classification(train, prunedRulesFrame)
# plot(table(predictions))
# sum(train$Species==predictions,na.rm=TRUE)/length(predictions)
