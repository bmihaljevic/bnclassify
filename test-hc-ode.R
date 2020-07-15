context("HC ODE")

test_that("augmenting ODE arcs nominal", {  
  a <- arcs_to_orphans(letters[1:4], letters[6:10])
  expect_equal(nrow(a), 6 * 2 + 20)
  expect_equal(a, unique(a))
})

test_that("augmenting ODE arcs no orphans", {  
  a <- arcs_to_orphans(NULL, letters[6:10])
  expect_null(a)
})

test_that("augmenting ODE arcs overlapping", {  
  expect_error(arcs_to_orphans(letters[1:6], letters[6:10]), 'disjoint')
})

test_that("augmenting ODE arcs no non-orphans", {  
  a <- arcs_to_orphans(letters[1:6], NULL)
  expect_equal(nrow(a), 15 * 2)
  expect_equal(a, unique(a))
})

test_that("Remove cycles single cycle", {
  nb <- nbcarp(car[, 5:7])
  nb <- add_feature_parents('lug_boot', 'safety', nb) 
  a <- arcs_to_orphans('lug_boot', 'safety')
  d <- discard_cycles(a, nb)
  expect_equal(nrow(d), 0)
})

test_that("Remove cycles two cycles", {
  nb <- nbcarp(car[, 4:7])
  nb <- add_feature_parents('lug_boot', 'safety', nb)
  nb <- add_feature_parents('lug_boot', 'persons', nb) 
  a <- arcs_to_orphans('lug_boot', c('safety', 'persons'))
  d <- discard_cycles(a, nb)
  expect_equal(nrow(d), 0)
})

test_that("Remove cycles: cycles and non-cycle", {
  nb <- nbcarp(car[, 4:7])
  nb <- add_feature_parents('lug_boot', 'safety', nb)
  a <- arcs_to_orphans(c('lug_boot', 'persons'), 'safety')
  d <- discard_cycles(a, nb) 
  a <- as.matrix(a)
  expect_equivalent(d, a[-1, ])
  expect_equivalent(d[1, ], c('safety', 'persons'))
})

test_that("Discard perms two-column mat", {
  m <- matrix(c('a', 'b', 'b', 'a'), ncol = 2)
  n <- discard_reversed(m)  
  expect_equal(n, m[2, , drop = FALSE])
})

test_that("Discard perms empty mat", {
  m <- matrix(character(), ncol = 2)
  n <- discard_reversed(m)  
  expect_equal(ncol(n), 2)
  expect_equal(nrow(n), 0)
})

test_that("augment arcs nominal", {
  nb <- nbcar()
  a <- augment_ode_arcs(nb)
  expect_equal(nrow(a), 15)
})

test_that("augment arcs no features", {
  nb <- nbcarclass()
  expect_error(augment_ode_arcs(nb), "orphans")
})

test_that("augment arcs no arcs", {
  nb <- nbcarp(car[, 6:7])
  a <- augment_ode_arcs(nb)
  expect_equal(nrow(a), 0)
})

test_that("augment arcs one arc nominal", {
  nb <- nbcarp(car[, 5:7])
  a <- augment_ode_arcs(nb)
  expect_equal(nrow(a), 1)
  expect_equal(a[1, ], c(from = 'safety', to = 'lug_boot'))
})

test_that("augment arcs one arc nominal", {
  nb <- nbcarp(car[, -(3:5)])
  a <- augment_ode_arcs(nb)
  expect_equal(nrow(a), 3)
})

test_that("augment arcs", {
  nb <- nbcarp(car[, 4:7])
  nb <- add_feature_parents('lug_boot', 'safety', nb)
  a <- augment_ode_arcs(nb)
  expect_equal(nrow(a), 2)
  expect_equal(a[1, ], c(from = 'safety', to = 'persons'))
  expect_equal(a[2, ], c(from = 'lug_boot', to = 'persons'))
})

test_that("augment ode nominal", {
  nb <- nbcar()
  anbs <- augment_ode(nb)
  expect_equal(length(anbs), 15)
})

test_that("augment ode iterate", {
  nb <- nbcarp(car[ , 4:7])
  anbs <- augment_ode(nb)
  expect_equal(length(anbs), 3)
  
  nb <- anbs[[1]]
  anbs <- augment_ode(nb)
  expect_equal(length(anbs), 2)
  
  nb <- anbs[[1]]
  anbs <- augment_ode(nb)
  expect_equal(length(anbs), 0)
})

test_that("Superparents nominal", {  
  nb <- nbcar()
  a <- superparent_children(nb)
  expect_equal(length(a), 6)
  expect_equal(a[[1]], setdiff(features(nb), 'buying'))
  expect_equal(a[[6]], setdiff(features(nb), 'safety'))
})

test_that("Superparents one feature", {  
  nb <- nbcarp(car[, 6:7])
  a <- superparent_children(nb)
  expect_null(a)
})

test_that("Superparents no orphans", {  
  nb <- nbcarp(car[, 5:7])
  nb <- add_feature_parents('safety', 'lug_boot', nb)
  a <- superparent_children(nb)
  expect_null(a)
})

test_that("augment ode hc nominal", {
  nb <- nbcar()
  mem <- make_cpts_cache(car, smooth = 0.01)
  a <- augment_ode_sp(nb, NULL, train = list(mem, mem), test = list(car, car))
  expect_equal(length(a), 5)  
})
  
test_that("Discard existing", {
  nb <- nbcarp(car) 
  arcs <- arcs_to_orphans(features(nb), character()) 
  d <- discard_existing(arcs, nb)
  expect_equal(nrow(d), nrow(arcs))
  
  nb <- add_feature_parents('lug_boot', 'safety', nb)  
  d <- discard_existing(arcs, nb)
  expect_equal(nrow(d), nrow(arcs) - 1) 
}) 