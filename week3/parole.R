
baseurl = "https://courses.edx.org/c4x/MITx/15.071x_2/asset/"
files = c( "parole.csv")


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

parole=getm(files)
N = nrow(parole)

print("Problem 1.1")
print(N)

print("Problem 1.2")
print(sum(parole$violator))

print("# 2.1: state, crime")

print("Problem 2.2")
parole$state = factor(parole$state, levels=1:4,labels=c("other","Kentucky","Louisiana","Virginia"))
parole$crime = factor(parole$crime, levels=1:4,labels=c("other","larceny","drug","driving"))
print(summary(parole))
print("table")

library(caTools)

set.seed(144)
split = sample.split(parole$violator,SplitRatio=.7)
train = subset(parole,split)
test = subset(parole,!split)

print("# 3.1: 70/30")
print("# 3.2: same,different,different")


print("# 4.1: race,state4,multple.offenses")
m1 = glm(violator~.,data=train,family="binomial")
print(summary(m1))

m2 = glm(violator~.-crime-max.sentence-time.served-male,data=train,family="binomial")
print(summary(m2))


fred = list(male=1,race=1,age=50,state="other",time.served=3,max.sentence=12,multiple.offenses=0,crime="larceny")
joe = list(male=1,race=1,age=30,state="other",time.served=3,max.sentence=12,multiple.offenses=0,crime="larceny")
predict(m1,newdata=fred,type="response")
predict(m1,newdata=joe,type="response")

p1 = predict(m1,newdata=test,type="response")
print(summary(p1))

f = function(actual,predicted,threshold){
  confusion = table(actual,predicted>threshold)
  print(confusion)
  sens = confusion[4]/(confusion[2]+confusion[4])
  spec = confusion[1]/(confusion[1]+confusion[3])
  acuracy = (confusion[1]+confusion[4])/nrow(test)
  print(c(threshold=threshold,sensitivity=sens,specificity=spec,acuracy=acuracy,fpr=1-spec,tpr=1-sens))
}

f(test$violator,p1,.2)
f(test$violator,p1,.3)
f(test$violator,p1,.4)
f(test$violator,p1,.5)
f(test$violator,p1,.6)
f(test$violator,p1,.7)

rocp = prediction(p1,test$violator)
rocperf = performance(rocp,"tpr","fpr")
plot(rocperf,colorize=T,print.cutoffs.at = seq(0,1,0.1),text.adj=c(-0.2,1.7))
