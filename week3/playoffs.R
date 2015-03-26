
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

teams=getm(files)


print("Problem 1.1")
print(nrow(teams))

print(max(table(teams$Team)))
playoffs = subset(teams,teams$Playoffs==1)
print(nrow(playoffs))

pot = table(playoffs$Year)
print(names(pot))

print(pot[c("1990","2001")])
playoffs$NumCompetitors = pot[as.character(playoffs$Year)]
print(playoffs$NumCompetitors)

print(sum(playoffs$NumCompetitors==8))

playoffs$WS = playoffs$RankPlayoffs==1
print(table(playoffs$WS))
# 
# m1 = glm(WS~Year,data=playoffs,family="binomial")
# m2 = glm(WS~RS,data=playoffs,family="binomial")
# m3 = glm(WS~RA,data=playoffs,family="binomial")
# m4 = glm(WS~W,data=playoffs,family="binomial")
# m5 = glm(WS~OBP,data=playoffs,family="binomial")
# m6 = glm(WS~SLG,data=playoffs,family="binomial")
# m7 = glm(WS~BA,data=playoffs,family="binomial")
# m8 = glm(WS~RankSeason,data=playoffs,family="binomial")
# m9 = glm(WS~OOBP,data=playoffs,family="binomial")
# ma = glm(WS~OSLG,data=playoffs,family="binomial")
# mb = glm(WS~NumCompetitors,data=playoffs,family="binomial")
# mc = glm(WS~League,data=playoffs,family="binomial")
# 
# all = list(m1,m2,m3,m4,m5,m6,m7,m8,m9,ma,mb,mc)
# sort(sapply(all,function(x) {x$coefficients[2]}))
# sapply(all,function(x) {print(summary(x))})
# 
# mm = glm(WS~Year+RA+RankSeason+NumCompetitors,data=playoffs,family="binomial")
# print(summary(mm))
# 
# sel = c("Year","RA","RankSeason","NumCompetitors")
# print(cor(playoffs[sel]))

# 
# m1 = glm(WS~Year,data=playoffs,family="binomial")
# m2 = glm(WS~RA,data=playoffs,family="binomial")
# m3 = glm(WS~RankSeason,data=playoffs,family="binomial")
# m4 = glm(WS~NumCompetitors,data=playoffs,family="binomial")
# m5 = glm(WS~Year+RA,data=playoffs,family="binomial")
# m6 = glm(WS~Year+RankSeason,data=playoffs,family="binomial")
# m7 = glm(WS~Year+NumCompetitors,data=playoffs,family="binomial")
# m8 = glm(WS~RA+RankSeason,data=playoffs,family="binomial")
# m9 = glm(WS~RA+NumCompetitors,data=playoffs,family="binomial")
# ma = glm(WS~RankSeason+NumCompetitors,data=playoffs,family="binomial")
# 
# all = list(m1,m2,m3,m4,m5,m6,m7,m8,m9,ma)
# aics = sapply(all,function(x) {x$aic})
# which.min(aics)
# 
# sapply(all,function(x) {print(summary(x))})


dumb = names(playoffs)
dumber = dumb[-c(1, 10, 12, 13, 17)]
goodvars = c("Year","RA","RankSeason","NumCompetitors")

library(gtools)
nvars = length(goodvars)
kseq= 1:nvars
nmodels = sum(choose(nvars,kseq))
df = data.frame(vars=rep(NA,nmodels),aic=rep(NA,nmodels))
j = 0
for(k in kseq){
  combi = combinations(nvars,k,goodvars)
  for (i in 1:dim(combi)[1]) { 
    vars = paste(combi[i,],collapse=" + ")
    aic = glm(formula(paste("WS ~ ",vars)) , data=playoffs, family=binomial)$aic
    j=j+1
    df[j,]$vars = vars
    df[j,]$aic = aic
  } 
}

df = df[order(df[,2]),]
smartest = glm(formula(paste("WS ~ ",df[1,1])),data=playoffs,family="binomial")

print(summary(smartest))

