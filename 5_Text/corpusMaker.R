

corpusMaker<- function(df,feature){
  corpus <- Corpus(VectorSource(df[,feature]))
  corpus[[1]]$content
  
  corpus = tm_map(corpus, content_transformer(tolower))
  
  corpus[[1]]$content
  
  # Remove punctuation
  
  corpus = tm_map(corpus, removePunctuation)
  
  
  
  
  # Remove stopwords and apple
  corpus = tm_map(corpus, removeWords, stopwords("english"))
  
  corpus[[1]]$content
  
  # Stem document 
  
  corpus = tm_map(corpus, stemDocument)
  
  dtm <- DocumentTermMatrix(corpus)
  
  dtm<- removeSparseTerms(dtm,sparse = .95)
  
  dtm<-as.data.frame(as.matrix(dtm))
  colnames(dtm)<- make.names(colnames(dtm))
  
  return( dtm)
  
}