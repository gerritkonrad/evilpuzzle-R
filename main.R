
evilPuzzel <- function(word = 'mein', nrow = 10, ncol = 10, includeWord = TRUE){
  # Convert word in vector
  letters <- strsplit(word, split = "")[[1]] # Returns a list
  
  sampleSize <- nrow * ncol
  
  matrix <- matrix(sample(letters, size = sampleSize, replace = TRUE), 
                   nrow=nrow, 
                   ncol=ncol
            )
  
  # Check rows 
  for (i in 1:nrow) {
    if (findWordInVector(letters, matrix[i, ])) {
      # Resample the row
      matrix[i, ] <- sample(letters, ncol, replace = TRUE)
    }
  }
  
  # Check columns
  for (i in 1:ncol) {
    if (findWordInVector(letters, matrix[, i])) {
      # Resample the column
      matrix[i, ] <- sample(letters, nrow, replace = TRUE)
    }
  }
  
  # Check reverse rows
  for (i in 1:nrow) {
    if (findWordInVector(letters, rev(matrix[i, ]))) {
      # Resample the row
      matrix[i, ] <- sample(letters, ncol, replace = TRUE)
    }
  }
  
  # Check reverse columns
  for (i in 1:ncol) {
    if (findWordInVector(letters, rev(matrix[, i]))) {
      # Resample the column
      matrix[i, ] <- sample(letters, nrow, replace = TRUE)
    }
  }
  
  # Check diagonal 
  
  # Check reverse diagonal
  
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



