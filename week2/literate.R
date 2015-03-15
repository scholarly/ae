baseurl = "https://courses.edx.org/c4x/MITx/15.071x_2/asset/"
files = c( "pisa2009train.csv","pisa2009test.csv")


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

source("lib/list.R")

list[train,test] = lapply(files,getm)

train = na.omit(train)
test = na.omit(test)
train$raceeth = relevel(train$raceeth,"White")
test$raceeth = relevel(test$raceeth,"White")

m1 = lm( readingScore ~ . ,data=train)



rmse = function(res) {sqrt(mean(res^2))}
rsq = function(pred,act,mu) {1 - (sum((act-pred)^2)/sum((act-mu)^2))}

mu = mean(train$readingScore)

p1 = predict(m1,test)
print(sum((test$readingScore-mu)^2))
print(rsq(p1,test$readingScore,mu))
