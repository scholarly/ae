
baseurl = "https://courses.edx.org/c4x/MITx/15.071x_2/asset/"
files = c( "PollingData.csv")


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

PP=getm(files)

simple = PP[c("Rasmussen","SurveyUSA","DiffCount","PropR")]
imputed = complete(mice(simple))

PP$Rasmussen = imputed$Rasmussen
PP$SurveyUSA = imputed$SurveyUSA

latest = max(PP$Year)
train = subset(PP,Year<latest)
test = subset(PP,Year==latest)

baseline = function(dat){sign(dat$Rasmussen)}
print(table(train$Republican,baseline(train)))

mod1 = glm(Republican ~ PropR, data = train, family="binomial")
print(summary(mod1))
p1 = predict(mod1,type="response")
print(table(train$Republican,p1>.5))

mod2 = glm(Republican ~ SurveyUSA+DiffCount, data = train, family="binomial")
print(summary(mod2))
p2 = predict(mod2,type="response")
print(table(train$Republican,p2>.5))
