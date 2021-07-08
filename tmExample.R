# http://fredgibbs.net/tutorials/document-similarity-with-r.html

Sys.setenv(NOAWT=TRUE)
require("tm")
require(stopwords)
require("SnowballC")

#----- settings -----
dataFolder <- "functions/data/"
modelFolder <- "functions/model/"

dir.create(modelFolder, showWarnings = F, recursive = T)
dir.create(dataFolder, showWarnings = F, recursive = T)

gridDataFile <- paste0(dataFolder, "grid.csv")
modelTfidfFile <- paste0(modelFolder, 'modelTfidf.rds')
modelNbrsFile <- paste0(modelFolder, 'modelNbrs.rds')

DownloadGrid <- function(gridDataFile = "functions/data/grid.csv"){
  print("Download grid data")
  # """Download GRID dataset from S3 (last update: September 2016)"""
  gridUrl <- "https://s3-us-west-2.amazonaws.com/science-of-science-bucket/grid/grid.csv"
  return( download.file(gridUrl, gridDataFile, method="wget"))
}

CreateDtm_tm <- function(grid_preprocess){
  my.corpus <- Corpus(VectorSource(grid_preprocess))
  # getTransformations
  # my.corpus <- tm_map(my.corpus, removePunctuation)
  # my.corpus <- tm_map(my.corpus, removeWords, stopwords("english"))
  # my.stops <- c("history","clio", "programming")
  # my.corpus <- tm_map(my.corpus, removeWords, my.stops)
  # my.list <- unlist(read.table("PATH TO STOPWORD FILE", stringsAsFactors=FALSE))
  # my.stops <- c(my.list)
  # my.corpus <- tm_map(my.corpus, removeWords, my.stops)
  # my.corpus <- tm_map(my.corpus, stemDocument)
  
  # my.tdm <- TermDocumentMatrix(my.corpus)
  # inspect(my.tdm)
  my.dtm <- DocumentTermMatrix(my.corpus, control = list(weighting = weightTfIdf, stopwords = TRUE))
  # inspect(my.dtm)
  return(my.dtm)
}

if(!file.exists(gridDataFile)) DownloadGrid(gridDataFile)
if(!exists("gridDict")) {gridDict <- read.csv(gridDataFile)}
grid_preprocess <- paste(gridDict$Name, gridDict$Country)
if(!exists("dtm_tm")) dtm_tm <- CreateDtm_tm(grid_preprocess)
# inspect(dtm_tm)

findFreqTerms(dtm_tm, 1000)
# findAssocs(dtm_tm, 'University of Warwick',1)

mydf <- as.data.frame(inspect(dtm_tm))
mydfScale <- scale(mydf)
d <- dist(mydfScale,method="euclidean")
# fit <- hclust(d, method="ward.D2")
# plot(fit)
