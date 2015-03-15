baseurl = "https://courses.edx.org/c4x/MITx/15.071x_2/asset/"
files = c( "baseball.csv")


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

bb = getm(files)

mb = subset(bb,Year<2002)

mb$RD = mb$RS - mb$RA

WinsReg = lm(W ~ RD, data=mb)
RunsReg = lm(RS ~ OBP + SLG, data=mb)
ORunsReg = lm(RA ~ OOBP + OSLG, data=mb)

teamRank = c(1,2,3,3,4,4,4,4,5,5)
wr12 =c(SFG=94,DET=88,NYY=95,STL=88,BAL=93,OAK=94,WSN=98,CIN=97,TEX=93,ATL=94)
wr13 = c(BRS=97,STL=97,LAD=92,DET=93,TBR=92,OAK=96,PTP=94,ATL=96,CLV=92,CIN=90)

print(cor(teamRank,wr12))
print(cor(teamRank,wr13))
      