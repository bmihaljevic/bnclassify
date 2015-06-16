context("Aug nb families")

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
  expect_error(check_anb_families(tfams, 'f'), 'fams_ok')
  # Family not in vars order
  tvars <- setNames(nm = letters[1:6])
  tfams <- lapply(tvars[-6], function(x) c(x, 'f'))
  tfams <- append(tfams, list(f='f'))
  tfams <- tfams[6:1]
  expect_error(check_anb_families(tfams, 'f'), 'last')
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
  a <- bnc_families(nbcar())
  b <- bnc_families(nbcarp(car[, 4:7]))
  fams <- unique_families(list(a, b))
  expect_equal(length(fams), 7)
  expect_equivalent(fams, a)
})

test_that("Unique families none in common", {
  cr <- bnc_families(nbcar())
  vt <- bnc_families(nbvote())
  fams <- unique_families(list(cr, vt))
  expect_equal(length(fams), 7 + 17)
})

# test_that("Unique families single dag", {
#   
# })

test_that("Tag families nominal", {
  cr <- bnc_families(nbcar())
  fms <- tag_families(cr)  
  expect_equal(length(fms), 7)
  expect_equal(fms$personsclass, c("persons", "class"))
})