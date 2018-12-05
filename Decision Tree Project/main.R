library(xlsx)
library(tree)
library(randomForest)
all.files <- list.files()
script.to.run <- "InitializeScript.R"
if(!any(all.files == script.to.run)){
  script.to.run <- paste("..", script.to.run, sep = "/")
  
}
source(script.to.run)
setwd(getWorkingDirectory())
data <- read.csv("creditcard.csv", skip =1)
data$SEX <- factor(data$SEX)
data$EDUCATION <- ifelse(data$EDUCATION >= 4, 4,data$EDUCATION)
data$EDUCATION <- factor(data$EDUCATION)
data$MARRIAGE <- factor(data$MARRIAGE)
pays <-2:6

pays <- pays[-2]
cols <- paste("PAY", pays, sep = "_")
for(col in cols){
  data[, col] <- factor(data[,col])
}
levels(data$PAY_5) <- levels(data$PAY_0)
levels(data$PAY_6) <- levels(data$PAY_0)
data$default.payment.next.month <- factor(data$default.payment.next.month)
set.seed(144)
half <- nrow(data)/2
group <- c(rep(0, half), rep(1, half))
group <- sample(group, length(group), replace = FALSE)
data$Group <- group
df.tr <- subset(data,Group == 0)
df.te <- subset(data, Group == 1)
df.tr$Group <- NULL
df.te$Group <- NULL
#Decision Tree
decision.tree <- tree(default.payment.next.month ~ . -ID, data = df.tr)

plot(decision.tree)
text(decision.tree)
predicts <- predict(decision.tree, newdata = df.te, type = "class")
print(table(predicts, df.te$default.payment.next.month))
missclassification.rate <- sum(predicts != df.te$default.payment.next.month)/nrow(df.te)
print(missclassification.rate)
#Cross Validation 
credit.cross.vali <- cv.tree(decision.tree,, FUN = prune.misclass, K = 10)
plot(credit.cross.vali)

#Prunned Tree
pruned.decision.tree <- prune.misclass(decision.tree, best = 2)
predict.prune <- predict(pruned.decision.tree, newdata = df.te, type = "class")
print(table(predict.prune,df.te$default.payment.next.month))
missclassification.rate.prune <- sum(predict.prune != df.te$default.payment.next.month)/nrow(df.te)

#Overall Misclassfication Rate is 17%
