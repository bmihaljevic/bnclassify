context("bnc dag")

test_that("bnc_dag", {
# Nominal 
  g <- test_dag() 
  bd <- bnc_dag(dag = g, class = 'A')
  expect_is(bd, 'bnc_dag')
  expect_identical(bd$.class, 'A')
  expect_identical(bd$.dag, g)
  expect_identical(features(bd), 'B')
  expect_identical(vars(bd), setNames(nm = LETTERS[2:1]))
  expect_identical(bd$.families, list(B = LETTERS[2:1], A = 'A'))
  
# Just class 
  g <- graph_internal(nodes = LETTERS[1], edgemode = "directed")
  bd <- bnc_dag(dag = g, class = 'A')
  expect_is(bd, 'bnc_dag')
  expect_identical(bd$.class, 'A')
  expect_identical(bd$.dag, g)
  expect_identical(features(bd), character())
  expect_identical(vars(bd), setNames(nm = 'A'))
  expect_identical(bd$.families, list(A='A'))
  
# Class not parent of all other nodes
  e <- list(A = 'B',B = NULL)
  edges <- graph_from_to_to_edges('A', 'B')
  g <- graph_internal(nodes = LETTERS[1:3], edges = edges, edgemode = "directed")
  # if (!skip_assert()) expect_error(bnc_dag(dag = g, class = 'A'), "fams_ok")
  # never use skip_assert here so that i always set it to false before publishing
  expect_error(bnc_dag(dag = g, class = 'A'), "fams_ok")
})

test_that("Accessors", {
# Nominal  
  skip_if_not_installed('gRbase') 
  suppressWarnings(RNGversion("3.5.0"))
  set.seed(0)
  ran <- random_aug_nb_dag('z', letters[-26], maxpar = 5, wgt = 0.8)
  dg <- bnc_dag(ran, class = 'z')
  expect_identical(unname(sort(vars(dg))), letters)  
})

test_that("feature families nominal", {
  n <- nbcar()
  a <- feature_families(n)
  expect_equal(length(a), 6)
  expect_equal(a[[1]], c('buying', 'class'))
})