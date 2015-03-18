
baseurl = "https://courses.edx.org/c4x/MITx/15.071x_2/asset/"
files = c( "elantra.csv")


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

HE=getm(files)
HE$M = as.factor(HE$Month)

train = subset(HE,Year<=2012)
test = subset(HE,Year>2012)

m1 = lm(ElantraSales ~ Unemployment+Queries+CPI_energy+CPI_all,data=train)
m2 = lm(ElantraSales ~ Month+Unemployment+Queries+CPI_energy+CPI_all,data=train)
m3 = lm(ElantraSales ~ M+Unemployment+Queries+CPI_energy+CPI_all,data=train)
# P6.1
#m4 = step(m3)
m4 = lm(ElantraSales ~ M+Unemployment+CPI_energy+CPI_all,data=train)

p1 = predict(m4,newdata=test)
#6.2
sse = sum((test$ElantraSales - p1)^2)
#6.3
sst = sum((test$ElantraSales - mean(train$ElantraSales))^2)
#6.4
R2 = 1- sse/sst

