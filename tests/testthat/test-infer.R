context("inference")  

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
  # gRain implementation change
  # skip_if_not_installed('gRain')
  # t <- nbvote()
  # a <- compute_cp(x = t, voting)
  # check_cp(a, nrow(voting), levels(voting$Class))
  # gRain implementation change
})

test_that("Just the class with incomplete data set", {
  skip_if_not_installed('gRain')
  nb <- lp(nb('Class', voting[, 17, drop = FALSE]), voting,smooth = 0)
  a <- compute_cp(x = nb, voting)
  check_cp(a, nrow(voting), levels(voting$Class))
  cp <- as.vector(params(nb)[[class_var(nb)]])
  expect_true(all(apply(a, 1, equivalent_num, cp)  ))
})

test_that("Single feature with incomplete data", {
  skip_if_not_installed('gRain')
  nb <- lp(nb(class = 'Class', v[, c('crime', 'Class'), drop = FALSE]), v, 
           smooth = 0.01)
  p <- compute_cp(x=nb, v)
  check_cp(p, nrow(v), levels(v$Class))
})


test_that("No rows returns empty matrix", {
  skip_if_not_installed('gRain')
  nb <- nbvote()
  a <- compute_cp(x=nb, voting[FALSE, ])
  check_cp(a, 0L, levels(voting$Class))
})

test_that("Missing features in the dataset", {
  tn <- nbcar()
  expect_error(compute_cp(tn, car[, 1:2]), "Some features missing from data set.")
})

test_that("Complete with incomplete data", {
  a <- nbvote()
  expect_error(compute_log_joint_complete(a, voting), "NA entries in data set.")
})

 
test_that("All incomplete rows", { 
  # gRain implementation change
  # skip_if_not_installed('gRain')
  # a <- nbvote()
  # vna <- voting[!complete.cases(voting), -17]
  # cp <- compute_log_joint_incomplete(a, vna)
  # cp <- log_normalize(cp)
  # cp <- exponentiate_probs(cp)
  # check_cp(cp, nrow(vna), levels(voting$Class))
  # gRain implementation change
})
 
test_that("Incomplete with complete data", {
  a <- nbcar()
  expect_error(compute_log_joint_incomplete(a, car), "complete")
})

test_that("Uniform for rows with 0 probabilities ", {
  # some rows have 0 prob
  nb <- bnc('nb', 'class', car[c(1, 700), ], smooth = 0)
  p <- compute_cp(x=nb, car[1000:1001, ])
  check_cp(p, 2, levels(car$class))
  expect_equivalent(rep(0.25, 4), p[1, ])
  expect_equivalent(rep(0.25, 4), p[2, ])
  # Could be equal to class prior, too.
})

test_that("Nominal log-likelihood two vars", {
  cb <- car[1, c(1, 7), drop = FALSE]
  nb <- nbcarp(cb)
  lik <- params(nb)$class['unacc'] * params(nb)$buying['vhigh', 'unacc']
  ll <- compute_ll(nb, cb)
  expect_true(equivalent_num(ll, log(lik)))
})

test_that("Nominal conditional log-likelihood two vars", {
  cb <- car[1, c(1, 7), drop = FALSE]
  nb <- nbcarp(cb)
  p <- params(nb)$class * params(nb)$buying['vhigh', ]
  clik = (p / sum(p) )['unacc']
  cll <- compute_cll(nb, cb)
  expect_true(equivalent_num(cll, log(clik)))
})

test_that("log-likelihood with incomplete data", { 
  # gRain implementation change
  # skip_if_not_installed('gRain')
  # cb <- car[1, c(1, 7), drop = FALSE]
  # nb <- nbcarp(cb)
  # cb$buying[] <- NA_integer_
  # ll <- compute_ll(nb, cb)
  # expect_true(equivalent_num(ll, log(0.4))) 
  # gRain implementation change
})

test_that("Nominal log-likelihood 7 vars", {
  nb <- lp(nb('class', car), car, smooth = 0)
  ll <- compute_ll(nb, car)
  expect_equal(ll, -13503.69, tolerance = 1e-6)
})


