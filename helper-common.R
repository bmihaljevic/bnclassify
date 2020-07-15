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

random_letters_db <- function(nlet = 6, nrow = 100) {
  df <- replicate(nlet, random_letters_vector(nlet, nrow))
  df <- as.data.frame(df, stringsAsFactors = TRUE)
  colnames(df) <- letters[seq_len(nlet)]
  df
}

random_letters_vector <- function(nletters, n) {
  sample(letters[1:nletters], n, replace = TRUE)
}

# Creates a random augmented NB with class as class. 
random_aug_nb_dag <- function(class, V, maxpar, wgt) {
  dg <- gRbase::random_dag(V = V, maxpar = maxpar, wgt = wgt)
  dg <- graphNEL2_graph_internal(dg)
  superimpose_node(dag = dg, class)
}

identical_non_call <- function(x, y) {
  x$.call_struct <- y$.call_struct <- NULL
  x$.call_bn <- y$.call_bn <- NULL 
  expect_identical(x, y)
}

test_dag <- function() {
  edges <- graph_from_to_to_edges('A', 'B')
  graph_internal(nodes = LETTERS[1:2], edges,  weights = NULL, edgemode = "directed") 
} 

# Load data

data(car, envir = environment())
data(voting, envir = environment())
v <- na.omit(voting)
alphadb <- random_letters_db()