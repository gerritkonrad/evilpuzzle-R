
evilPuzzel <- function(word = 'mein', nrow=10, ncolumn=10, includeWord=TRUE){
  # Convert word in vector
  word <- strsplit(word, split = "")[[1]] # Returns a list
  
  sampleSize <- nrow * ncolumn
  matrix <- matrix(sample(word, size=sampleSize, replace=TRUE), 
                   nrow=nrow, 
                   ncol=ncolumn
            )
}

evilPuzzel()