evilPuzzel <- function(word = 'mein', nrow = 10, ncol = 10, includeWord = TRUE){
  # Convert word in vector
  letters <- strsplit(word, split = "")[[1]] # Returns a list
  
  if (length(unique(letters)) < 3) {
    stop("Word must contain at least 3 unique characters!")
  }
  
  sampleSize <- nrow * ncol
  
  matrix <- matrix(sample(letters, size = sampleSize, replace = TRUE), 
                   nrow=nrow, 
                   ncol=ncol
            )
  
  wordFound <- TRUE
  
  # Check until the word is no longer in the matrix
  while(wordFound == TRUE){
    wordFound <- FALSE
    
    # Check rows 
    for (i in 1:nrow) {
      # Check in row
      if (findWordInVector(letters, matrix[i, ])) {
        matrix[i, ] <- sample(letters, ncol, replace = TRUE)
        wordFound <- TRUE
      }
      # Check in reverse
      if (findWordInVector(letters, rev(matrix[i, ]))) {
        matrix[i, ] <- sample(letters, ncol, replace = TRUE)
        wordFound <- TRUE
      }
    }
    
    # Check columns
    for (i in 1:ncol) {
      # Check in column
      if (findWordInVector(letters, matrix[, i])) {
        matrix[, i] <- sample(letters, nrow, replace = TRUE)
        wordFound <- TRUE
      }
      # Check in reverse
      if (findWordInVector(letters, rev(matrix[, i]))) {
        matrix[, i] <- sample(letters, nrow, replace = TRUE)
        wordFound <- TRUE
      }
    }
    
    # Check diagonal 
    
    # Check reverse diagonal
  }
  
  matrix
}

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


evilPuzzel()



