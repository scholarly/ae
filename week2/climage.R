baseurl = "https://courses.edx.org/c4x/MITx/15.071x_2/asset/"
files = c( "climate_change.csv")


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

gcd = getm(files)
train = subset(gcd,Year<=2006)
test = subset(gcd,Year>2006)

m1 = lm( Temp ~ MEI+CO2+CH4+N2O+CFC.11+CFC.12+TSI+Aerosols,data=train)

m2 = lm( Temp ~ MEI+N2O+TSI+Aerosols,data=train)

m3 = step(m1)

rsq = function(pred,act,mu) {1 - (sum((act-pred)^2)/sum((act-mu)^2))}

p1 = predict(m1,test)
p2 = predict(m2,test)
p3 = predict(m3,test)

mu = mean(train$Temp)

print(rsq(p1,test$Temp,mu))
print(rsq(p2,test$Temp,mu))
print(rsq(p3,test$Temp,mu))