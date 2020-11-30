library(RWeka)
library(partykit)
library(rJava)
library(caret)

install.packages("rJava")
Sys.setenv(JAVA_HOME="C:/Program Files/Java/jre1.8.0_271")

lympho <- read.csv("lymphography.data", header=FALSE)
head(lympho)

for(i in names(lympho)){
  lympho[,i] <- as.factor(lympho[,i])
}
str(lympho)

sampel <- sample(2, nrow(lympho), replace = T, prob=c(0.8,0.2))
trainingdata <- lympho[sampel==1,]
testingdata  <- lympho[sampel==2,]

fit1 <- J48(V1, data = trainingdata)
fit1
plot(fit1)

tree<- ctree(V1 ~., data= trainingdata)
plot(tree)

prediksi <- predict(fit1, testingdata)
confusionMatrix(table(prediksi, testingdata$V1))

prediksi2 <- predict(tree, testingdata)
confusionMatrix(table(prediksi2, testingdata$V1))



