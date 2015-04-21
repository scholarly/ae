stocks = read.csv("StocksCluster.csv")
depvar = "PositiveDec"
indep = setdiff(colnames(stocks),depvar)

accuracy = function(conf){
  sum(diag(conf))/sum(conf)
}

accconf = function(model,data,depvar,level=0.5){
  pred = predict(model,newdata=data,type="response")
  conf = table(data[,depvar],pred>level)
  print(conf)
  sum(diag(conf))/nrow(data)
}

# 1.1
print(nrow(stocks))

# 1.2
print(mean(stocks$PositiveDec))

# 1.3
print(sort(cor(stocks),decreasing=TRUE)[13])

# 1.4
print(sort(colMeans(stocks[,indep])))


# 2.1
set.seed(144)
spl = sample.split(stocks$PositiveDec,SplitRatio=0.7)
stocksTrain = subset(stocks,spl==TRUE)
stocksTest = subset(stocks,spl!=TRUE)

stocksModel = glm(PositiveDec ~ ., stocksTrain, family=binomial)


accTrain = accconf(stocksModel,stocksTrain,depvar)
print(accTrain)

# 2.2
accTest = accconf(stocksModel,stocksTest,depvar)
print(accTest)

# 2.3
confBL = table(stocksTest$PositiveDec)
print(confBL)
accBL = confBL[2]/nrow(stocksTest)
print(accBL)


# 3.2
limitedTrain = stocksTrain[,indep]
limitedTest = stocksTest[,indep]

library(caret)
preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)
print(mean(normTrain$ReturnJan))
print(mean(normTest$ReturnJan))

# 3.4
k=3
set.seed(144)
km = kmeans(normTrain, centers = k)
print(km$size)

# 3.5
library(flexclust)
cache = "stocksKCCA.RData.xz"
if(file.exists(cache)){
  load(cache)
}else{
  km.kcca = as.kcca(km, normTrain)
  save(km.kcca,file=cache,compress="xz")
}

clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)
table(clusterTest)

# 4.1
stocksTrains = split(stocksTrain,clusterTrain)
stocksTests = split(stocksTest,clusterTest)

trainsMeans = sapply(stocksTrains,colMeans)
print(trainsMeans[depvar,])

testsMeans = sapply(stocksTests,colMeans)
print(testsMeans[depvar,])

# 4.2
stocksModels = lapply(stocksTrains,function(x){
  glm(PositiveDec~.,data=x,family=binomial)
})

signs = sapply(stocksModels,function(x){
  sign(x$coefficients)
})
print(signs)

# 4.3
accs = mapply(accconf,stocksModels,stocksTests,MoreArgs=list(depvar,0.5))
print(accs)

# 4.4
AllPredictions = unlist(mapply(function(model,data){
  predict(model,newdata=data,type="response")
},stocksModels,stocksTests))
AllOutcomes = unlist(lapply(stocksTests,function(x){x[,depvar]}))

conf = table(AllOutcomes,AllPredictions>0.5)
acc = sum(diag(conf))/length(AllOutcomes)
print(acc)