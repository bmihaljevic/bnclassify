context("Aug nb families")


test_that("graph 2 families nominal", {
  g <- test_dag()
  f <- graphNEL2families(dag = g, class = 'A')
  expect_equal(names(f), c('B', 'A'))
})  
  
test_that("graph 2 families class not in dag   ", {   
  g <- test_dag()
  expect_error(graphNEL2families(dag = g, class = 'C'), 'last not found')
})

test_that("graph 2 families class length > 1   ", {     
  g <- test_dag()
  expect_error(graphNEL2families(dag = g, class = LETTERS[1:2]), 
               'string')
})

test_that("graph 2 families  Undirected graph" , { 
  e <- list(A = 'B', B = 'A') 
  edges <- graph_from_to_to_edges(c('A', 'B'), c('B', 'A')) 
  g <- graph_internal(nodes = LETTERS[1:2], edges,  weights = NULL, edgemode = "directed") 
  if (!skip_testing()) expect_error(graphNEL2families(dag = g, class = LETTERS[1]), 'is_dag_graph') 
  
  g <- graph_internal(nodes = LETTERS[1:2], edges,  weights = NULL, edgemode = "undirected") 
  if (!skip_testing()) expect_error(graphNEL2families(dag = g, class = LETTERS[1]), 'is_dag_graph')
})

test_that("check families", {
  # Nominal
  tvars <- setNames(nm = letters[1:6])
  tfams <- lapply(tvars[-6], function(x) c(x, 'f'))
  tfams <- append(tfams, list(f = 'f'))
  check_anb_families(tfams, 'f')
  # Class not in all families
  tvars <- setNames(nm = letters[1:6])
  tfams <- lapply(tvars[-6], function(x) c(x, 'f'))
  tfams <- append(tfams, list(f = 'f'))
  tfams$b <- 'b'
  if (!skip_assert()) expect_error(check_anb_families(tfams, 'f'), 'fams_ok')
  # Family not in vars order
  tvars <- setNames(nm = letters[1:6])
  tfams <- lapply(tvars[-6], function(x) c(x, 'f'))
  tfams <- append(tfams, list(f='f'))
  tfams <- tfams[6:1]
  if (!skip_assert()) expect_error(check_anb_families(tfams, 'f'), 'last')
})

test_that("is is family nominal", {
  f <- letters[1:6]
  expect_true(is_anb_family(f, 'a', 'f'))
})

test_that("is is family wrong var", {
  f <- letters[1:6]
  expect_true(!is_anb_family(f, 'b', 'f'))
})

test_that("is is family wrong class", {
  f <- letters[1:6]
  expect_true(!is_anb_family(f, 'a', 'e'))
})

test_that("is is family missing values", {
  f <- c(letters[1:6], NA, 'g')
  expect_true(!is_anb_family(f, 'a', 'g'))
})

test_that("Unique families some in common", {
  a <- families(nbcar())
  b <- families(nbcarp(car[, 4:7]))
  fams <- unique_families(list(a, b))
  expect_equal(length(fams), 7)
  expect_equivalent(fams, a)
})

test_that("Unique families none in common", {
  cr <- families(nbcar())
  vt <- families(nbvote())
  fams <- unique_families(list(cr, vt))
  expect_equal(length(fams), 7 + 17)
})

# test_that("Unique families single dag", {
#   
# })

test_that("Tag families nominal", {
  cr <- families(nbcar())
  fms <- make_families_ids(cr)  
  expect_equal(length(fms), 7)
  expect_equal(fms[['persons']], "personsclass")
})

test_that("Acyclic order nominal", {
 n <- nbcar()
 o <- order_acyclic(families(n))  
 expect_equal(o, c('class', colnames(car)[1:6]))
})

test_that("Acyclic order a cycle", {
  n <- nbcar()
  n <- add_feature_parents('safety', 'lug_boot', n)
  n <- add_feature_parents('lug_boot', 'doors', n)
  f <- families(n)
  f[['safety']] <- c('safety', 'doors', 'class')
  o <- order_acyclic(f)  
  expect_null(o)
})

test_that("Acyclic order 0 node is a DAG", {
  o <- order_acyclic(list())  
  # expect_equal(o, get_family_node(character()))
  # Not sure what should happen here...
  expect_equal(o, character())
})

test_that("Find ancestors not in graph nominal", {
  a <- tan_cl('class', car)
  b <- get_ancestors('doors', families(a))
  expect_true(is_perm(b, c('lug_boot', 'safety', 'buying', 'class')))
  b <- get_ancestors('safety', families(a))
  expect_true(is_perm(b, c('buying', 'class')))
  b <- get_ancestors('class', families(a))
  expect_equal(b, character())
})

test_that("Find ancestors", {
  a <- nbcarclass()
  b <- get_ancestors('class', families(a))
  expect_equal(b, character())
})

test_that("Find ancestors not in graph", {
  a <- nbcarclass()
  expect_error(get_ancestors('p', families(a)), "families")
})