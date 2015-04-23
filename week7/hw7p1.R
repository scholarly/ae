baseurl = "https://courses.edx.org/c4x/MITx/15.071x_2/asset/"

getdata = function(local){
  if(!file.exists(local)){
    library(downloader)
    remote = paste0(baseurl,local)
    download(remote,local)
  }
  data = read.csv(local,stringsAsFactors=FALSE)
}

extract_features = function(data){
  data
}


statesMap = map_data("state")

# 1.1
length(table(statesMap$group))

# 1.2
ggplot(statesMap, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "white", color = "black")

# 2.1
polling = extract_features(getdata("PollingImputed.csv"))

spl = polling$Year<2009
Train = polling[spl,]
Test = polling[!spl,]

mod2 = glm(Republican~SurveyUSA+DiffCount, data=Train, family="binomial")
TestPrediction = predict(mod2, newdata=Test, type="response")
TestPredictionBinary = as.numeric(TestPrediction > 0.5)
predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, Test$State)

table(TestPredictionBinary)
mean(TestPrediction)


# 2.2
predictionDataFrame$region = tolower(predictionDataFrame$Test.State)
predictionMap = merge(statesMap, predictionDataFrame, by = "region")
predictionMap = predictionMap[order(predictionMap$order),]
nrow(predictionMap)
nrow(statesMap)

# 2.4
ggplot(predictionMap, 
  aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + 
  geom_polygon(color = "black") 
  
# 2.5
ggplot(predictionMap, 
  aes(x = long, y = lat, group = group, fill = TestPrediction))+ 
  geom_polygon(color = "black") +
  scale_fill_gradient(low = "blue", high = "red", 
    guide = "legend", # breaks= c(0,1), 
    #labels = c("Democrat", "Republican"),
    name = "Prediction 2012")

# 3.1,2
predictionDataFrame[which(Test$State=="Florida"),]


#4.1
ggplot(predictionMap, 
  aes(x = long, y = lat, group = group, fill = TestPrediction)) + 
  geom_polygon(color = "black",linetype=3) +
  scale_fill_gradientn(colours=c("blue","white","red"))

ggplot(predictionMap, 
  aes(x = long, y = lat, group = group, fill = TestPrediction)) + 
  geom_polygon(color = "black",size=3) +
  scale_fill_gradientn(colours=c("blue","white","red"))
# 4.2
ggplot(predictionMap, 
       aes(x = long, y = lat, group = group, fill = TestPrediction)) + 
  geom_polygon(color = "black",alpha=.3) +
  scale_fill_gradientn(colours=c("blue","white","red"))

# scale_fill_gradientn didn't work as expected on this dataset, 
# but combined with alpha=.3 did help show which predictions had low confidence