
baseurl = "https://courses.edx.org/c4x/MITx/15.071x_2/asset/"
files = c( "CPSData.csv","MetroAreaCodes.csv", "CountryCodes.csv")


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


list[CPS,MAC,CC] = lapply(files,getm)

CPS = merge(CPS, MAC, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
CPS = merge(CPS, CC, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)


tt = tapply(CPS$Country!="United States",CPS$MetroArea,mean,na.rm=TRUE)
print(tt[["New York-Northern New Jersey-Long Island, NY-NJ-PA" ]])