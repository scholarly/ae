
whourl = "https://courses.edx.org/c4x/MITx/15.071x_2/asset/WHO.csv"
whodata = "WHO.csv"

get_data = function(url,local){
  if(!file.exists(local)){
    download.file(url,local,"curl")
  }
  read.csv(local)
}
data_dir = function(fname){
  paste("data",fname,sep="/")
}

df = get_data(whourl,data_dir(whodata))

plot(df$GNI,df$FertilityRate)

outliers = subset(df,GNI>10000 & FertilityRate > 2.5)


hist(df$CellularSubscribers)

boxplot(df$LifeExpectancy ~ df$Region 
        ,xlab="",ylab="Life Expectancy"
        ,main="Life Expectancy of Countries by Region"
        )

tapply(df$Over60,df$Region,mean)

tapply(df$LiteracyRate,df$Region,min,na.rm=T)

tapply(df$ChildMortality,df$Region,mean,na.rm=T)

