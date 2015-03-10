baseurl = "https://courses.edx.org/c4x/MITx/15.071x_2/asset/"
files = c( "wine.csv","wine_test.csv" )

library(downloader)

get_data = function(url,local){
  if(!file.exists(local)){
    download(url,local,mode="wb")
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

list[wine,winet] = lapply(files,getm)

model4 = lm(Price ~ AGST+WinterRain+HarvestRain+Age,data=wine)
print(summary(model4))
