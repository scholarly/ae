#library(readr)
library(tm)
library(SnowballC)
library(Cairo)
library(wordcloud)
library(RColorBrewer)

baseurl = "https://courses.edx.org/c4x/MITx/15.071x_2/asset/"

getdata = function(local){
  if(!file.exists(local)){
    library(downloader)
    remote = paste0(baseurl,local)
    download(remote,local)
  }
  data = read.csv(local,stringsAsFactors=F)
}

tweets = read.csv("tweets.csv", stringsAsFactors=FALSE)

build.corpus = function(data){    
  tm_map(Corpus(VectorSource(tweets$Tweet)), function(t) {
    removeWords(
      removePunctuation(
        PlainTextDocument(
          tolower(t))),c("apple",stopwords("english")))
  })
}

tweets = getdata("tweets.csv")
corpus = build.corpus(tweets)
dtm = as.data.frame(as.matrix(DocumentTermMatrix(corpus)))
print(ncol(dtm))



pal = brewer.pal(9, "Blues")[5:9]
wordcloud(colnames(dtm),colSums(dtm),max.words=200,colors=pal)

tail(sort(colSums(dtm)))

