
url = "https://courses.edx.org/c4x/MITx/15.071x_2/asset/mvtWeek1.csv"
data = "mvtWeek1.csv"


get_data = function(url,local){
  if(!file.exists(local)){
    download.file(url,local,"curl")
  }
  read.csv(local)
}
data_dir = function(fname){
  paste("data",fname,sep="/")
}

df = get_data(url,data_dir(data))

