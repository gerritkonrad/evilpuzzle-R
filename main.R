evilPuzzel <- function(word = 'mein', nrow = 10, ncol = 10, includeWord = TRUE){
  letters <- strsplit(word, split = "")[[1]]
  wordLength <- length(letters)
  
  if (length(unique(letters)) < 3) {
    stop("Word must contain at least 3 unique characters!")
  }
  
  matx <- matrix(sample(letters, size = nrow * ncol, replace = TRUE), 
                 nrow=nrow, 
                 ncol=ncol)
  
  wordCount <- 0
  wordFound <- TRUE
  
  while (wordFound) {
    
    wordFound <- FALSE
    
    # Check rows
    for (i in 1:nrow(matx)) {
      for (j in 1:(ncol(matx) - wordLength + 1)) {
        endIndex = (j-1 + wordLength)
        segment <- matx[i, j:endIndex]
        if (all(segment == letters) || all(rev(segment) == letters)) {
          matx[i, j:endIndex] = sample(letters, wordLength, replace = TRUE)
          wordFound <- TRUE
          wordCount <- wordCount + 1
        }
      }
    }
    
    # Check columns
    for (i in 1:ncol(matx)) {
      for (j in 1:(nrow(matx) - wordLength + 1)) {
        endIndex = (j-1 + wordLength)
        segment <- matx[j:endIndex, i]
        if (all(segment == letters) || all(rev(segment) == letters)) {
          matx[j:endIndex, i] = sample(letters, wordLength, replace = TRUE)
          wordFound <- TRUE
          wordCount <- wordCount + 1
        }
      }
    }
    
    # Check diagonals from top left to bottom right
    for (i in 1:(nrow(matx) - wordLength + 1)) {
      for (j in 1:(ncol(matx) - wordLength + 1)) {
        segment <- sapply(0:(wordLength - 1), function(k) matx[i + k, j + k])
        if (all(segment == letters) || all(rev(segment) == letters)) {
          for(k in 0:(wordLength - 1)){
            matx[i + k, j + k] = sample(letters, 1)
          }
          wordFound <- TRUE
          wordCount <- wordCount + 1
        }
      }
    }
    
    # Check diagonals from top right to bottom left
    for (i in 1:(nrow(matx) - wordLength + 1)) {
      for (j in wordLength:ncol(matx)) {
        segment <- sapply(0:(wordLength - 1), function(k) matx[i + k, j - k])
        if (all(segment == letters) || all(rev(segment) == letters)) {
          for(k in 0:(wordLength - 1)){
            matx[i + k, j - k] = sample(letters, 1)
          }
          wordFound <- TRUE
          wordCount <- wordCount + 1
        }
      }
    }
  }
  
  print(wordCount)
  return(matx)
}

evilPuzzel()