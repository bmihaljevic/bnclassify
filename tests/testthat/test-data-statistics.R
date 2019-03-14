context("Statistics")

test_that("Conditional mutual information", {  
# With Incomplete data.   
  a <- cmi('physician_fee_freeze', 'Class', 
               dataset = voting, z = 'water_project_cost_sharing')  
  b <- cmi('physician_fee_freeze', 'Class', dataset = voting)
  expect_equal(a, b, tolerance = 0.001)
  # Mutual information
  a <- cmi('physician_fee_freeze', 'Class', 
           dataset = voting, z = 'water_project_cost_sharing')  
  b <- cmi('physician_fee_freeze', 'Class', voting)
  expect_equal(a, b, tolerance = 0.001)
  c <- cmi('buying', 'maint', dataset = car)
  expect_equal(c, 0)
# the same as when removing missing directly.
  a <- cmi('physician_fee_freeze', 'Class', 
           dataset = voting, z = 'water_project_cost_sharing')  
  v <- voting[,c('physician_fee_freeze', 'water_project_cost_sharing', 'Class')]
  b <- cmi('physician_fee_freeze', 'Class', v, z = 'water_project_cost_sharing')
  expect_equal(a, b, tolerance = 1e-10)
# Log base
  a <- cmi('physician_fee_freeze', 'Class', dataset = voting, unit = "log2")
  b <- cmi('physician_fee_freeze', 'Class', dataset = voting)
  expect_true(abs(a - b) > 0.1)
})

test_that("Contingency table", {
	t <- extract_ctgt('doors', car)
	expect_identical(dim(t), 4L) 
	t <- extract_ctgt(c('buying', c('doors', 'maint')), car)
	expect_identical(dim(t), c(4L, 4L ,4L)) 
	d <- dimnames(t)
	vars <- c('buying', 'doors', 'maint')
	expect_identical(names(d), vars)
	levs <- lapply(car[, vars], levels)
	expect_true(all(mapply(identical, levs, d, SIMPLIFY = TRUE)))
	t <- extract_ctgt('crime', voting)
	expect_identical(sum(t), 418L) 
})

test_that("Degrees freedom", {
  tbl <- extract_ctgt(c('persons', 'doors', 'class'), car)
  df <- cmi_degrees_freedom(freqs_table = tbl)
  expect_equal(df, 24L)

  tbl <- extract_ctgt(c('doors', 'class', 'persons'), car)
  df <- cmi_degrees_freedom(freqs_table = tbl)
  expect_equal(df, 27L)
})

test_that("Contingency table to CPT", {  
  # Nominal
  suppressWarnings(RNGversion("3.5.0"))
  set.seed(0) 
  tc <- table(random_letters_vector(3, 20), random_letters_vector(4, 20))
  tcpt <- ctgt2cpt(tc, 1)
  expect_equal(colnames(tcpt), letters[1:4])
  expect_equal(tcpt[, 1], setNames(c(1, 3, 4) / 8, letters[1:3]))
  # No smooth  
  suppressWarnings(RNGversion("3.5.0"))
  set.seed(0) 
  tc <- table(random_letters_vector(3, 20), random_letters_vector(4, 20))
  tcpt <- ctgt2cpt(tc, 0)
  expect_equal(colnames(tcpt), letters[1:4])
  expect_equal(tcpt[, 1], setNames(c(0, 2, 3) / 5, letters[1:3]))
  # 1D table 
  suppressWarnings(RNGversion("3.5.0"))
  set.seed(0)
  tc <- table(random_letters_vector(4, 200))
  tcpt <- ctgt2cpt(tc, 0)
  expect_equal(names(tcpt), letters[1:4])
  expect_equal(tcpt, tc / 200)
})

