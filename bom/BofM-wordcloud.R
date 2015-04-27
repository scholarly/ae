
library(tm)

words =c(stopwords("english"),"thee","thou","thy","thine","therefore","unto","came","pass","wherefore","shall","will","things","thy","said","yea","behold","also","now")
#,"lord","god","come","even","upon","people"
#words = stopwords("english")
preproc = function(text,rm=words){
  c = Corpus(VectorSource(text))
  tm_map(c,function(x){
    removeWords(removePunctuation(PlainTextDocument(tolower(x))),rm)
  })
}

dowc = function(fname){
  
  data = read.table(fname,sep="\t",stringsAsFactors=FALSE)
  
  
  corpus = preproc(data[,2])
  
  
  dtm = as.data.frame(as.matrix(DocumentTermMatrix(corpus))) 
  
  #dtm = as.data.frame(as.matrix(removeSparseTerms(
  #  DocumentTermMatrix(corpus),sparse=.999))) 
  
  #dtm = as.data.frame(as.matrix(removeSparseTerms(
  #  DocumentTermMatrix(corpus),sparse=.99))) 
  
  library(wordcloud)
  png(oname,width=480,height=480)
  wordcloud(colnames(dtm),colSums(dtm))  
  dev.off()
}

af = list.files()

frames = lapply(af[grep(".*[.]tsv$",af)],function(fname){
  data = read.table(fname,sep="\t",stringsAsFactors=FALSE)
  data$src = fname
  data
})

data = data.frame()

for( fr in frames){
  print(nrow(fr))
  data = rbind(data,fr)
}
corpus = preproc(data[,"V2"])

dtm = as.data.frame(as.matrix(
  removeSparseTerms(DocumentTermMatrix(corpus),sparse=.997))) 

library(wordcloud)
wordcloud(colnames(dtm),colSums(dtm))  