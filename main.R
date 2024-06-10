# Function to replace a segment in the matrix
replaceSegment <- function(mat_env, start_row, start_col, wordLength, direction) {
  for (k in 0:(wordLength - 1)) {
    if (direction == "row") {
      mat_env$mat[start_row, start_col + k] <- sample(letters, 1)
    } else if (direction == "col") {
      mat_env$mat[start_row + k, start_col] <- sample(letters, 1)
    } else if (direction == "diag1") {
      mat_env$mat[start_row + k, start_col + k] <- sample(letters, 1)
    } else if (direction == "diag2") {
      mat_env$mat[start_row + k, start_col - k] <- sample(letters, 1)
    }
  }
}

# Function to check and replace a word in the matrix
checkAndReplace <- function(mat_env, letters) {
  wordLength <- length(letters)
  wordCount <- 0
  
  # Check rows
  for (i in 1:nrow(mat_env$mat)) {
    for (j in 1:(ncol(mat_env$mat) - wordLength + 1)) {
      endIndex = (j-1 + wordLength)
      segment <- mat_env$mat[i, j:endIndex]
      if (all(segment == letters) || all(rev(segment) == letters)) {
        replaceSegment(mat_env, i, j, wordLength, "row")
        wordCount <- wordCount + 1
      }
    }
  }
  
  # Check columns
  for (i in 1:ncol(mat_env$mat)) {
    for (j in 1:(nrow(mat_env$mat) - wordLength + 1)) {
      endIndex = (j-1 + wordLength)
      segment <- mat_env$mat[j:endIndex, i]
      if (all(segment == letters) || all(rev(segment) == letters)) {
        replaceSegment(mat_env, j, i, wordLength, "col")
        wordCount <- wordCount + 1
      }
    }
  }
  
  # Check diagonals from top left to bottom right
  for (i in 1:(nrow(mat_env$mat) - wordLength + 1)) {
    for (j in 1:(ncol(mat_env$mat) - wordLength + 1)) {
      segment <- sapply(0:(wordLength - 1), function(k) mat_env$mat[i + k, j + k])
      if (all(segment == letters) || all(rev(segment) == letters)) {
        replaceSegment(mat_env, i, j, wordLength, "diag1")
        wordCount <- wordCount + 1
      }
    }
  }
  
  # Check diagonals from top right to bottom left
  for (i in 1:(nrow(mat_env$mat) - wordLength + 1)) {
    for (j in wordLength:ncol(mat_env$mat)) {
      segment <- sapply(0:(wordLength - 1), function(k) mat_env$mat[i + k, j - k])
      if (all(segment == letters) || all(rev(segment) == letters)) {
        replaceSegment(mat_env, i, j, wordLength, "diag2")
        wordCount <- wordCount + 1
      }
    }
  }
  
  return(wordCount)
}


evilPuzzel <- function(word = 'mein', nrow = 10, ncol = 10, includeWord = TRUE){
  letters <- strsplit(word, split = "")[[1]] 
  if (length(unique(letters)) < 3) {
    stop("Word must contain at least 3 unique characters!")
  }

  mat_env <- new.env()
  mat_env$mat <- matrix(sample(letters, size = nrow * ncol, replace = TRUE), 
                        nrow=nrow, 
                        ncol=ncol)
  
  word_count <- 0
  while (TRUE) {
    wordsFound <- checkAndReplace(mat_env, letters)
    if (wordsFound == 0) break
    word_count <- word_count + wordsFound
  }
  
  print(word_count)
  print(mat_env$mat)
}

evilPuzzel()