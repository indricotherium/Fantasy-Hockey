library(ggplot2)
library(clusterGeneration)
library(mnormt)
library(corrplot)
library(randomForest)
# Read in the csv from the github repo
matchup_data.frame<-read.csv("matchup_dataframe.csv", row.names = NULL)

numeric_matchup_data<-apply(as.matrix(matchup_data.frame[,-17]),2,as.numeric)
corrplot(cor(numeric_matchup_data), order = "hclust")


fit=randomForest(factor(matchup_data.frame$Result)~., data=numeric_matchup_data,importance = TRUE,ntree = 2000)
importance(fit,type = 1)
varImpPlot(fit,type=1)

variable.importance<-as.data.frame(importance(fit,type = 1))
variable.importance$category<-rownames(variable.importance)
variable.importance$category<-factor(variable.importance$category,
                                     levels = variable.importance$category[order(variable.importance$MeanDecreaseAccuracy,decreasing = TRUE)])

# I set any negative values in the dataframe to 0 due to a ggplot bug with negative/positive values in the same barplot
variable.importance$MeanDecreaseAccuracy<-ifelse(variable.importance$MeanDecreaseAccuracy < 0, 0, variable.importance$MeanDecreaseAccuracy)

color<-sapply(variable.importance$category[order(variable.importance$MeanDecreaseAccuracy,decreasing = TRUE)], function(x){
  if(x=="SV"||x=="GAA"||x=="SO"||x=="GS"||x=="W"){
    return("forestgreen")
  }else{
    return("red")
  }
})
ggplot(data = variable.importance, aes(y = MeanDecreaseAccuracy, x = category))+
  geom_bar(stat = "identity", fill = color) +
  xlab("") +
  ggtitle("Random Forest Classifier Variable Importance")
