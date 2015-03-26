
baseurl = "https://courses.edx.org/c4x/MITx/15.071x_2/asset/"
files = c( "loans.csv")


get_data = function(url,local){
  if(!file.exists(local)){
    download.file(url,local,"curl")
  }
  read.csv(local)
}

data_dir = function(fname){
  paste("data",fname,sep="/")
}

getm = function(file){
  get_data(paste(baseurl,file,sep=""),data_dir(file))
}

loans=getm(files)
N = nrow(loans)

print("Problem 1.1")
print(mean(loans$not.fully.paid))

print("# 2.1")
print(summary(loans))

library(mice)
set.seed(144)
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed


library(caTools)
set.seed(144)
split = sample.split(loans$not.fully.paid,SplitRatio=.7)
train = subset(loans,split)
test = subset(loans,!split)

m1 = glm(not.fully.paid~.,data=train,family="binomial")
print(summary(m1))

test$predicted.risk = p1 = predict(m1,newdata=test,type="response")

f = function(actual,predicted,threshold){
  confusion = table(actual,predicted>threshold)
  print(confusion)
  sens = confusion[4]/(confusion[2]+confusion[4])
  spec = confusion[1]/(confusion[1]+confusion[3])
  acuracy = (confusion[1]+confusion[4])/nrow(test)
  print(c(threshold=threshold,sensitivity=sens,specificity=spec,acuracy=acuracy,fpr=1-spec,tpr=1-sens))
}

f(test$not.fully.paid,p1,.5)


rocp = prediction(p1,test$not.fully.paid)
rocperf = performance(rocp,"tpr","fpr")
plot(rocperf,colorize=T,print.cutoffs.at = seq(0,1,0.1),text.adj=c(-0.2,1.7))
auc = performance(rocp,"auc")
print(auc)

m2 = glm(not.fully.paid~int.rate,data=train,family="binomial")
print(summary(m2))
p2 = predict(m2,newdata=test,type="response")
print(summary(p2))

roc2 = prediction(p2,test$not.fully.paid)
auc = performance(roc2,"auc")
print(auc)

print("# 4.1")
print(10*exp(0.06*3))

test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1
print(max(test$profit)*10)

print(mean(test$profit)*10)

highinterest = subset(test,int.rate>=.15)
print(mean(highinterest$profit))

cutoff = sort(highinterest$predicted.risk, decreasing=FALSE)[100]
print(cutoff)
selected = subset(highinterest,highinterest$predicted.risk<=cutoff)
print(summary(selected))
print(sum(selected$profit))
