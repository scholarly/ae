
baseurl = "https://courses.edx.org/c4x/MITx/15.071x_2/asset/"
files = c( "songs.csv")


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

songs=getm(files)

independent = c("timesignature","timesignature_confidence",
             "loudness",
             "tempo","tempo_confidence",
             "key","key_confidence",
             "energy","pitch",
             "timbre_0_min", "timbre_0_max",
             "timbre_1_min", "timbre_1_max",
             "timbre_2_min", "timbre_2_max",
             "timbre_3_min", "timbre_3_max",
             "timbre_4_min", "timbre_4_max",
             "timbre_5_min", "timbre_5_max",
             "timbre_6_min", "timbre_6_max",
             "timbre_7_min", "timbre_7_max",
             "timbre_8_min", "timbre_8_max",
             "timbre_9_min", "timbre_9_max",
             "timbre_10_min", "timbre_10_max",
             "timbre_11_min", "timbre_11_max")

metadata = c("year","songtitle","artistname","Top10")

print("Problem 1.1")
print(sum(songs$year==2010))

print("Problem 1.2")
mj = subset(songs,artistname=="Michael Jackson")
print(nrow(mj))

print("Problem 1.3")
print(subset(mj,Top10==1)[,metadata])

print("Problem 1.4")
ts = table(songs$timesignature)
print(names(ts))
print(sort(ts,decreasing=T))

print("Problem 1.5")
print(songs[which.max(songs$tempo),c(metadata,"tempo")])


print("Problem 2.1")
split = songs$year<2010
print(sum(split))

print("Problem 2.2")
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
ss = songs[,!names(songs) %in% nonvars]
train = subset(ss,split)
test = subset(ss,!split)

model1 = glm(Top10 ~ .,data=train,family="binomial")
print(summary(model1))
print(model1$aic)

print("Problem 3.1")
print(cor(train$loudness,train$energy))

model2 = glm(Top10 ~ . - loudness, data=train, family="binomial")
print(summary(model2))
print(model2$aic)

model3 = glm(Top10 ~ . - energy, data=train, family="binomial")
print(summary(model3))
print(model3$aic)

print("Problem 4.1")
p1 = predict(model3,type="response",newdata=test)

N = nrow(test)
confusion = table(test$Top10,p1>.45)
print(confusion)
acuracy = (confusion[1]+confusion[4])/N
print(acuracy)

print("Problem 4.2")
baseline = table(test$Top10,p1>1)

print(baseline)
print(baseline[1])/N

print("Problem 4.3")
print(c(confusion[4],confusion[3]))


print("Problem 4.4")
sens = confusion[4]/(confusion[2]+confusion[4])
spec = confusion[1]/(confusion[1]+confusion[3])
print(c(sens,spec))
print(c(confusion[1],confusion[3]))
print(c(confusion[2],confusion[4]))

