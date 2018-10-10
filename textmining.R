library(tm)
library(proxy)
library(dplyr)

doc <- c( "For a corpus contains N document, find all the terms.", 
          "Calculate the term frequency of each term in each document and establish the TF matrix.",
          "Calculate the IDF of terms and TF-IDF matrix, in which the columns are the terms and the rows are documents. Each number in the matrix will be the TF-IDF of the a term in a document.", 
          "Each row will be a TF-IDF vector of a document.",
          "Considering the different length of the document, each vector should be divided by its document length.",
          "use the cosine similarity to measure how close are two given documents")


# create term frequency matrix using functions from tm library
doc_corpus <- Corpus( VectorSource(doc) )
control_list <- list(removePunctuation = TRUE, stopwords = TRUE, tolower = TRUE)
tdm <- TermDocumentMatrix(doc_corpus, control = control_list)

# print
( tf <- as.matrix(tdm) )


( idf <- log( ncol(tf) / ( 1 + rowSums(tf != 0) ) ) )

# diagonal matrix
( idf <- diag(idf) )

tf_idf <- crossprod(tf, idf)
colnames(tf_idf) <- rownames(tf)
tf_idf


# Note that normalization is computed "row-wise"
tf_idf / sqrt( rowSums( tf_idf^2 ) )

doc


Cossim <- function(x, y) {
  similarity <- sum(x * y) / ( sqrt( sum(y ^ 2) ) * sqrt( sum(x ^ 2) ) )
  
  # given the cosine value, use acos to convert back to degrees
  # acos returns the radian, multiply it by 180 and divide by pi to obtain degrees
  return( acos(similarity) * 180 / pi )
}

pr_DB$set_entry( FUN = Cossim, names = c("Cosine") )
d <- dist(tf_idf, method = "Cosine")
pr_DB$delete_entry("Cosine")


cluster1 <- hclust(d, method = "ward.D")
plot(cluster1)
rect.hclust(cluster1, 5)

