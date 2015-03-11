baseurl = "https://courses.edx.org/c4x/MITx/15.071x_2/asset/"
files = c( "wine.csv","wine_test.csv")


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

source("http://gsubfn.googlecode.com/svn/trunk/R/list.R")

list[W,WT] = lapply(files,getm)