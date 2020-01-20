context("bnc bn")

test_that("nominal", {
  tbn <- bnc('nb', 'class', car, smooth = 0)
  texp <- prop.table(table(car$class, dnn = 'class') )
  tout <- params(tbn)$class 
  expect_equal(tout, texp)
  tvalues <- values(tbn)
  levs <- lapply(car, levels)
  expect_true(all(mapply(identical, levs, tvalues, SIMPLIFY = TRUE)))
  expect_identical(values(tbn)$buying, levels(car$buying))
})

test_that("nominal as grain", {
  skip_if_not_installed('gRain')
  tbn <- bnc('nb', 'class', car, smooth = 0)
  # gRain implementation change
  # expect_is(as_grain(tbn), 'grain')
  # gRain implementation change
  expect_error(as_grain(tbn))
})


test_that("bnc_bn no class in dataset ", {     
  tbdag <- nb_dag('class', 'buying')
  tb <- bnc_dag(tbdag, class = 'class')
  expect_error(lp(tb, car[ , 1, drop = FALSE], smooth = 0),
               "not found")
})

test_that("Just the class in dataset", {
  tbdag <- nb_dag('class', character())
  tbdag <- bnc_dag(tbdag, class = 'class')
  tbn <- lp(tbdag, car, smooth = 0)    
  expect_equal(class_var(tbn), 'class')
})  

test_that(" Wrong data set", {
  tbdag <- nb_dag('class', colnames(car)[-7])
  tbdag <- bnc_dag(tbdag, class = 'class')
  expect_error(lp(tbdag, voting, smooth = 0)   , "not found")
})

test_that(" Classes", {
  a <- lp(nb('class', car), car, smooth = 0)   
  expect_equal(classes(a), levels(car$class))
})

test_that("nominal wanbia", {
  a <- lp(nb('Class', v), v, smooth = 0, wanbia = TRUE)   
  expect_equal(a$.weights[['handicapped_infants']], 0.00000000) 
  expect_equal(a$.weights[['immigration']], 1.00000000) 
})