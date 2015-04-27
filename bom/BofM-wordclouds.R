
library(tm)

words =c(stopwords("english"),"thee","thou","thy","thine","unto","came","pass","wherefore","shall","will","things","thy","said","yea","behold")
preproc = function(text,rm=words){
  c = Corpus(VectorSource(text))
  tm_map(c,function(x){
    removeWords(removePunctuation(PlainTextDocument(tolower(x))),rm)
  })
}

dowc = function(fname){
  oname = paste0(fname,".png")
  print(c(fname,oname))
  
  data = read.table(fname,sep="\t",stringsAsFactors=FALSE)
  
  
  
  corpus = preproc(data[,2])
  summary(corpus)
  
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

lapply(af[grep(".*[.]tsv$",af)],dowc)
