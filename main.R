
evilPuzzel <- function(word = 'mein', nrow = 10, ncolumn = 10, includeWord = TRUE){
  # Convert word in vector
  letters <- strsplit(word, split = "")[[1]] # Returns a list
  
  sampleSize <- nrow * ncolumn
  
  matrix <- matrix(sample(letters, size = sampleSize, replace = TRUE), 
                   nrow=nrow, 
                   ncol=ncolumn
            )
}

evilPuzzel()


findWordInVector <- function(word, vector) {
  j <- 1
  for (i in 1:length(vector)) {
    if (vector[i] == word[j]) {
      j <- j + 1
      if (j > length(word)) {
        # Word found!
        return(TRUE)
      }
    }else{
      # Reset because letter does not match letter of word
      j <- 1 
    }
  }
  return(FALSE)
}




