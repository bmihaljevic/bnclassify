context("bnc dag")

test_that("check_dag_class", {
#  Nominal
  e <- list(A='B', B=NULL)
  g <- graph::graphNEL(nodes = LETTERS[1:2], edgeL = e, edgemode = "directed")
  check_dag_class(dag = g, class = 'A')
# class not in dag   
  expect_error(check_dag_class(dag = g, class = 'C'), 'nodes')
# class length > 1
  expect_error(check_dag_class(dag = g, class = LETTERS[1:2]), 
               '1 is not TRUE')
# Undirected graph
  e <- list(A='B', B='A')
  g <- graph::graphNEL(nodes = LETTERS[1:2], edgeL = e, edgemode = "undirected")
  expect_error(check_dag_class(dag = g, class = LETTERS[1]), 'DAG')
})

test_that("bnc_dag", {
# Nominal
  e <- list(A='B', B=NULL)
  g <- graph::graphNEL(nodes = LETTERS[1:2], edgeL = e, edgemode = "directed")
  bd <- bnc_dag(dag = g, class = 'A', call = NULL)
  expect_is(bd, 'bnc_dag')
  expect_identical(bd$.class, 'A')
  expect_identical(bd$.dag, g)
  expect_identical(bd$.features, 'B')
  expect_identical(bd$.vars, setNames(nm = LETTERS[2:1]))
  expect_identical(bd$.families, list(B=LETTERS[2:1], A='A'))
# Just class
  g <- graph::graphNEL(nodes = LETTERS[1], edgemode = "directed")
  bd <- bnc_dag(dag = g, class = 'A', call = NULL)
  expect_is(bd, 'bnc_dag')
  expect_identical(bd$.class, 'A')
  expect_identical(bd$.dag, g)
  expect_identical(bd$.features, character())
  expect_identical(bd$.vars, setNames(nm = 'A'))
  expect_identical(bd$.families, list(A='A'))
# Not an augmented NB 
  e <- list(A='B', B=NULL)
  g <- graph::graphNEL(nodes = LETTERS[1:3], edgeL = e, edgemode = "directed")
  expect_error(bnc_dag(dag = g, class = 'A', call = NULL), "families")
})

test_that("Accessors", {
# Nominal  
  set.seed(0)
  ran <- random_aug_nb_dag('z', letters[-26], maxpar = 5, wgt = 0.8)
  dg <- bnc_dag(ran, class = 'z', call = NULL)
  expect_identical(unname(sort(bnc_vars(dg))), letters)  
})

test_that("check vars", {
  # Nominal
  tvars <- setNames(nm = letters[1:6])
  check_vars(tvars, letters[1:5], 'f')
  # class at beginning
  expect_error(check_vars(tvars, letters[2:6], 'a'), 'setNames')
})

test_that("check families", {
  # Nominal
  tvars <- setNames(nm = letters[1:6])
  tfams <- lapply(tvars[-6], function(x) c(x, 'f'))
  tfams <- append(tfams, list(f='f'))
  check_families(tfams, tvars, 'f')
  # Class not in all families
  tvars <- setNames(nm = letters[1:6])
  tfams <- lapply(tvars[-6], function(x) c(x, 'f'))
  tfams <- append(tfams, list(f='f'))
  tfams$b <- 'b'
  expect_error(check_families(tfams, tvars, 'f'), 'last')
  # Family not in vars order
  tvars <- setNames(nm = letters[1:6])
  tfams <- lapply(tvars[-6], function(x) c(x, 'f'))
  tfams <- append(tfams, list(f='f'))
  tfams <- tfams[6:1]
  expect_error(check_families(tfams, tvars, 'f'), 'vars')
})