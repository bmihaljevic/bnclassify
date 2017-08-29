context("Data input")

test_that("Check class in dataset", {
  expect_error(check_class_in_dataset(class = 2, voting))  
  expect_error(check_class_in_dataset(class = 'class', voting))
  check_class_in_dataset(class = 'Class', voting)
  expect_error(check_class_in_dataset(class = c('Class', 'Class'), voting), 
               "string")
})

test_that("Check dataset", {
  check_dataset(car)
  expect_error(check_dataset(as.matrix(car)))
  tm <- cbind(car, class=car$class)
  expect_error(check_dataset(tm), "unique")
  tm <- car; colnames(tm)[1] <- NA
  expect_error(check_dataset(tm),
               "is_non_empty_complete(cnames) is not TRUE", fixed = TRUE)
  tm <- car; tm[[1]] <- as.numeric(tm[[1]])
  expect_error(check_dataset(tm), "factors")
})

test_that("Get features", {
  expect_error(get_features('class', voting), "disjoint")
  ft <- get_features('Class', voting)  
  expect_identical(ft, colnames(voting)[-17])
  ft <- get_features('class', car)  
  expect_identical(ft, colnames(car)[-7])
  tm <- car; tm[[1]] <- as.numeric(tm[[1]])
  expect_error(get_features('class', tm), "factors")
})

test_that("check features", {
  # Nominal 
  check_features(letters[1:5], 'f')
  check_features(NULL, 'f')
  check_features(character(), 'f')
  # Class in features
  expect_error(check_features(letters[1:5], 'e'), 'class')
  # Empty class
  expect_error(check_features(character(), NULL), 'string')
})

test_that("trim dataset", {
  # Nominal
  a <- trim_dataset('buying', car)
  expect_identical(dim(a), c(nrow(car), 1L))
  # Empty vars set 
  expect_error(trim_dataset(character(), car), "complete")
  # Integer vars
  expect_error(trim_dataset(1, car), "character")
})