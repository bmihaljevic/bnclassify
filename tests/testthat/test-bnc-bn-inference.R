context("bnc_bn inference")

check_cp <- function(x, nrow, colnames) {
  expect_true(is.numeric(x))
  expect_equal(dimnames(x), list(NULL, colnames))
  expect_equal(dim(x), c(nrow, length(colnames)))
  expect_true(are_pdists(x))
}

test_that("Complete data set ", {
  t <- nbcar()
  a <- compute_cp(x = t, car)
  check_cp(a, nrow(car), levels(car$class))
})

test_that("Incomplete data set", {
  t <- nbvote()
  a <- compute_cp(x = t, voting)
  check_cp(a, nrow(voting), levels(voting$Class))
})

test_that("Just the class with incomplete data set", {
  nb <- bnc_bn(nb('Class', voting[, 17, drop = FALSE]), voting,smooth = 0, NULL)
  a <- compute_cp(x = nb, voting)
  check_cp(a, nrow(voting), levels(voting$Class))
  cp <- as.vector(params(nb)[[class_var(nb)]])
  expect_true(all(apply(a, 1, equivalent_num, cp)  ))
})

test_that("Single feature with complete data", {
  nb <- lp(nb(class='Class', v[, c('crime', 'Class'), drop=FALSE]), v, 
           smooth=0.01)
  p <- compute_cp(x=nb, v)
  check_cp(p, nrow(v), levels(v$Class))
})


test_that("No rows returns empty matrix", {
  nb <- nbvote()
  a <- compute_cp(x=nb, voting[FALSE, ])
  check_cp(a, 0L, levels(voting$Class))
})

test_that("Missing features in the dataset", {
  tn <- nbcar()
  expect_error(compute_cp(tn, car[, 1:2]), "undefined")
})

test_that("Complete with incomplete data", {
  a <- nbvote()
  expect_error(compute_ulcp_complete(a, voting), "anyNA")
})

 
test_that("All incomplete rows", {
  a <- nbvote()
  vna <- voting[!complete.cases(voting), -17]
  cp <- compute_ulcp_incomplete(a, vna)
  cp <- log_normalize(cp)
  check_cp(cp, nrow(vna), levels(voting$Class))
})
 
test_that("Incomplete with complete data", {
  a <- nbcar()
  expect_error(compute_ulcp_incomplete(a, car), "complete")
})

test_that("Uniform for rows with 0 probabilities ", {
  # some rows have 0 prob
  nb <- bnc('class', car[c(1, 700), ], smooth=0)
  p <- compute_cp(x=nb, car[1000:1001, ])
  check_cp(p, 2, levels(car$class))
  expect_equivalent(rep(0.25, 4), p[1, ])
  expect_equivalent(rep(0.25, 4), p[2, ])
  # Could be equal to class prior, too.
})