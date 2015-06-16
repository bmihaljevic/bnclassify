context("Aug nb families")

test_that("Unique families some in common", {
  a <- nbcar()
  b <- nbcarp(car[, 4:7])
  fams <- unique_families(list(a, b))
  expect_equal(length(fams), 7)
  expect_equivalent(fams, bnc_families(a))
})

test_that("Unique families none in common", {
  cr <- nbcar()
  vt <- nbvote()
  fams <- unique_families(list(cr, vt))
  expect_equal(length(fams), 7 + 17)
})

test_that("Unique families single dag", {
  
})

test_that("Ided families", {
  cr <- nbcar()
  fms <- bnc_idd_families(cr)  
  expect_equal(length(fms), 7)
  expect_equal(fms$personsclass, c("persons", "class"))
  
})