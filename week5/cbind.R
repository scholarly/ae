
library(tm)
words = stopwords("english")

set.seed(42)
a = as.data.frame(matrix(sample(words,100),nrow=10))
#colnames(a)=paste("A",1:10,sep="")
rownames(a)=paste("D",1:10,sep="")
b = as.data.frame(matrix(sample(words,100),nrow=10))
#colnames(b)=paste("B",1:10,sep="")
rownames(b)=paste("D",1:10,sep="")
cbind(a,b)
rbind(a,b)