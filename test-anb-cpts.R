context("CPTs")

test_that("subset_cpt single value", {
  cp <- extract_cpt(colnames(car)[5:7], car, smooth = 1)
  p <- subset_cpt(cp, list(lug_boot=1, safety=2, class=1))
  expect_null(dim(p))
})  

test_that("subset_cpt 1D cpt", {
  cp <- extract_cpt(colnames(car)[7], car, smooth = 1)
  obs <- vapply(car[, 5:6], as.integer, FUN.VALUE = integer(nrow(car)))
  expect_error(p <- subset_cpt(cp, obs), "vars")
})

test_that("cpt cache nominal", {
  cache <- make_cpts_cache(car, smooth = 1)
  expect_identical(cache('class'), extract_cpt('class', car, smooth = 1))
  expect_identical(cache(c('buying', 'class')),
                   extract_cpt(c('buying', 'class'), car, smooth = 1))
})

test_that("cpt cache forget", {
  cache <- make_cpts_cache(car, smooth = 1)
  expect_identical(cache('class'), extract_cpt('class', car, smooth = 1))
  expect_identical(cache(c('buying', 'class')),
                   extract_cpt(c('buying', 'class'), car, smooth = 1))
  expect_true(forget(cache))
  expect_identical(cache('class'), extract_cpt('class', car, smooth = 1))
  expect_identical(cache(c('buying', 'class')),
                   extract_cpt(c('buying', 'class'), car, smooth = 1))
})
