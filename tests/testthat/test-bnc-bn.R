context("bnc bn")

test_that("nominal", {
  tbn <- bnc('class', car, smooth = 0)
  texp <- prop.table(table(car$class, dnn = 'class') )
  tout <- params(tbn)$class 
  expect_equal(tout, texp)
  tvalues <- values(tbn)
  levs <- lapply(car, levels)
  expect_true(all(mapply(identical, levs, tvalues)))
  expect_identical(values(tbn)$buying, levels(car$buying))
  expect_is(to_grain(tbn), 'grain')
})

test_that("bnc_bn no class in dataset ", {     
  tbdag <- nb_dag('class', 'buying')
  tb <- bnc_dag(tbdag, class = 'class', call = NULL)
  expect_error(bnc_bn(tb, car[ , 1, drop = FALSE], smooth = 0, call = NULL),
               "subset")
})

test_that("Just the class in dataset", {
  tbdag <- nb_dag('class', character())
  tbdag <- bnc_dag(tbdag, class = 'class', call = NULL)
  tbn <- bnc_bn(tbdag, car, smooth = 0, call = NULL)    
  expect_equal(class_var(tbn), 'class')
})  

test_that(" Wrong data set", {
  tbdag <- nb_dag('class', colnames(car)[-7])
  tbdag <- bnc_dag(tbdag, class = 'class', call = NULL)
  expect_error(bnc_bn(tbdag, voting, smooth = 0, call = NULL)   , "colnames")
})