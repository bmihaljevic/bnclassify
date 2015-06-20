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

nbcarp <- function(cardata) {
  lp(nb('class', cardata), cardata, smooth = 1)
}

nbcarclass <- function() {
  lp(nb('class', car[, 'class', drop = FALSE]), car, smooth = 1)
}

bnc <- function(class, dataset, smooth = 1) {
  lp(nb(class, dataset), dataset, smooth = smooth)
}

random_letters_db <- function(nlet = 6, nrow = 100) {
  df <- replicate(nlet, random_letters_vector(nlet, nrow))
  df <- as.data.frame(df)
  colnames(df) <- letters[seq_len(nlet)]
  df
}

random_letters_vector <- function(nletters, n) {
  sample(letters[1:nletters], n, replace = TRUE)
}

# Creates a random augmented NB with class as class. 
random_aug_nb_dag <- function(class, V, maxpar, wgt) {
  dg <- gRbase::random_dag(V = V, maxpar = maxpar, wgt = wgt)
  superimpose_node(dag = dg, class)
}

# Load data

data(car)
data(voting)
v <- na.omit(voting)
alphadb <- random_letters_db()