# http://fredgibbs.net/tutorials/document-similarity-with-r.html

Sys.setenv(NOAWT=TRUE)
require(text2vec)
require(stopwords)

#----- settings -----
dataFolder <- "functions/data/"
modelFolder <- "functions/model/"

dir.create(modelFolder, showWarnings = F, recursive = T)
dir.create(dataFolder, showWarnings = F, recursive = T)

gridDataFile <- paste0(dataFolder, "grid.csv")
vectorizerFile <- paste0(modelFolder, "vectorizer.rds")
modelTfidfFile <- paste0(modelFolder, "modelTfidf.rds")
dtmTfidfFile <- paste0(modelFolder, "dtmTfidf.rds")

DownloadGrid <- function(gridDataFile = "functions/data/grid.csv"){
  print("Download grid data")
  # """Download GRID dataset from S3 (last update: September 2016)"""
  gridUrl <- "https://s3-us-west-2.amazonaws.com/science-of-science-bucket/grid/grid.csv"
  return( download.file(gridUrl, gridDataFile, method="wget"))
}

CreateDtm <- function(grid_preprocess){
    it <- itoken(grid_preprocess, preprocess_function = tolower, tokenizer = word_tokenizer)
    vocab <- create_vocabulary(it, ngram = c(1,1), stopwords = stopwords("en", source = "snowball"))
    prunedVocab <- prune_vocabulary(vocab, term_count_min = , doc_proportion_max = 0.8)
    vectorizer <- vocab_vectorizer(prunedVocab)
    # h_vectorizer = hash_vectorizer(hash_size = 2 ^ 15, ngram = c(1L, 1L))
    dtm <- create_dtm(it, vectorizer)
    # strip_accents='unicode',
    modelTfidf <- TfIdf$new(smooth_idf = TRUE , norm = c('l2'), sublinear_tf = TRUE)
    dtmTfidf <- modelTfidf$fit_transform(dtm)
        # dtm_h <- create_dtm(it, h_vectorizer)
        # dtmTfidf_h <- modelTfidf$fit_transform(dtm_h)
    
    saveRDS(vectorizer, vectorizerFile)
    saveRDS(modelTfidf,modelTfidfFile)
    saveRDS(dtmTfidf, dtmTfidfFile)
    
    return(list(model = modelTfidf, dtmTfidf = dtmTfidf, vectorizer = vectorizer))
}

if(!file.exists(gridDataFile)) DownloadGrid(gridDataFile)
# if(!exists("gridDict")) 
if(!exists("grid_preprocess")){
  gridDict <- read.csv(gridDataFile)
  grid_preprocess <- paste(gridDict$Name, gridDict$City, gridDict$Country)
}

if(!file.exists(vectorizerFile) | !file.exists(modelTfidfFile)|!file.exists(dtmTfidfFile) ) temp <- CreateDtm(grid_preprocess)

vectorizer <- readRDS(vectorizerFile)
modelTfidf <- readRDS(modelTfidfFile)
dtmTfidf <- readRDS(dtmTfidfFile)

test <- c("University of Warwick UK", "University of Edinburgh UK", "MPI Research, 54943 North Main Street, Mattawan, Michigan USA"
, "INGBIO Innovative Enterprise, Moscow, Russia")

dtm_test <- create_dtm(itoken(test, preprocess_function = tolower, tokenizer = word_tokenizer), vectorizer)
dtmTfidf_test <- modelTfidf$transform(dtm_test)

similarities <- sim2(dtmTfidf_test, dtmTfidf_train,  method = c("cosine"), norm = c("l2"))

get_similar_institutue <- function(similarity, n_recommendations =1 ){
  sort(similarity, decreasing = T)[1:n_recommendations]
}

# get_similar_institutue <- function(similarities, inputIndex, n_recommendations =1 ){
#   sort(similarities[inputIndex,], decreasing = T)[1:n_recommendations]
# }

dist <- get_similar_institutue(similarities[4,],  1)
print(dist)
index <- as.numeric(names(dist))
print(gridDict[index,])

# sparse_matrix <- dtmTfidf %>%
#   cast_sparse()

