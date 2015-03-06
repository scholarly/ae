
baseurl = "https://courses.edx.org/c4x/MITx/15.071x_2/asset/"
files = c( "IBMStock.csv","GEStock.csv", "ProcterGambleStock.csv", "CocaColaStock.csv","BoeingStock.csv")


get_data = function(url,local){
  if(!file.exists(local)){
    download.file(url,local,"curl")
  }
  df = read.csv(local)
  df$Date = as.Date(df$Date, "%m/%d/%y")
  df
}

data_dir = function(fname){
  paste("data",fname,sep="/")
}

getm = function(file){
  get_data(paste(baseurl,file,sep=""),data_dir(file))
}

source("http://gsubfn.googlecode.com/svn/trunk/R/list.R")

list[IBM,GE,PG,KO,BA] = lapply(files,getm)

