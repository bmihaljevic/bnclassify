# Common testing functions

# Smooth is always 0 so that grain does not produce 0 probabilities

nbvote <- function() {
  # data(voting, envir = parent.frame())
  lp(nb('Class', voting), voting, smooth = 1)
}

nbvotecomp <- function() {
#   data(voting, envir = parent.frame())
#   v <- na.omit(voting)
  # assign('v', v, envir = parent.frame())
  lp(nb('Class', v), v, smooth = 1)
}

nbcar <- function() {
  # data(car, envir = parent.frame())
  lp(nb('class', car), car, smooth = 1)
}

bnc <- function(class, dataset, smooth = 1) {
  lp(nb(class, dataset), dataset, smooth = smooth)
}

random_letters_db <- function() {
  df <- replicate(6, sample(letters, 100, replace = TRUE))
  df <- as.data.frame(df)
  colnames(df) <- letters[1:6]
  df
}

random_letters_vector <- function(nletters, n) {
  sample(letters[1:nletters], n, replace = TRUE)
}

# Load data

data(car)
data(voting)
v <- na.omit(voting)
alphadb <- random_letters_db()