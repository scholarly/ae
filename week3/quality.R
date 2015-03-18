
baseurl = "https://courses.edx.org/c4x/MITx/15.071x_2/asset/"
files = c( "quality.csv")


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

QC=getm(files)

library(caTools)
set.seed(88)
split = sample.split(QC$PoorCare,SplitRatio=.75)
train = subset(QC,split)
mu = mean(train$PoorCare)
test = subset(QC,!split)

m1 = glm(PoorCare ~ OfficeVisits + Narcotics, data=train, family=binomial)



spec_sens = function(outcome,pred,thresh){
  tt = table(outcome,pred>thresh)
  sens = tt["1",2]/sum(tt["1",])
  spec = tt["0",1]/sum(tt["0",])
  c(sens,spec)
}




library(ROCR)
ptrain = predict(m1,type="response")
rp = performance(prediction(ptrain,train$PoorCare),"tpr","fpr")
plot(rp,colorize=T,print.cutoffs.at=seq(0,1,0.1),text.adj=c(-.3,1.7))



ptest = predict(m1,type="response",newdata=test)
auc = as.numeric(performance(prediction(ptest,test$PoorCare),"auc")@y.values)

print(auc)