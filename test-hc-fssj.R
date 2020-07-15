context("HC fssj") 

test_that("Expand supernode nominal", {
  a <- nbcarp(car[, 2:7])
  s <- not_cci(a)
  ex <- augment_supernodes(new_node = 'buying', supernodes = s, bnc_dag = a)
  expect_equal(length(ex), 5)
  maint <- ex[[1]]
  expect_equal(graph_num_arcs(dag(maint)), 7)
  expect_equal(families(maint)[['buying']], c('buying', 'maint', 'class'))
  
  a <- nbcar()
  expect_error(augment_supernodes(new_node = 'safety', 
                                 supernodes = s, bnc_dag = a), 'already')
})

test_that("includes_by_joins nominal", {
  a <- nbcarp(car[, 5:7])
  # All possible including models
  # debugonce(includes_by_joins)
  cands <- includes_in_supernodes(a, colnames(car)[-7])
  expect_equal(length(cands), 4 * 2)
  buying <- cands[[1]]
  expect_equal(families(buying)[['buying']], 
               c('buying', 'lug_boot', 'class'))
})

test_that("includes", {
  nb <- nb('Class')
  a <- includes(nb, c('crime','immigration'))
  expect_equal(length(a), 2)
  expect_equal(features(a[[1]]), 'crime')  
  expect_equal(features(a[[2]]), 'immigration')  
})

test_that("includes no features", {  
  nb <- nbcar()
  a <- includes(nb, character())
  expect_equal(length(a), 0)
})

test_that("augment supernodes", {
  nb <- nb('class', NULL)
  e <- includes_in_supernodes(nb, NULL)
  expect_equal(length(e), 0)
  
  nb <- nb('class', NULL)
  e <- includes_in_supernodes(nb, 'doors')
  expect_equal(length(e), 0)  
  
  nb <- nb('class', NULL)
  e <- includes_in_supernodes(nb, c('doors', 'safety'))
  expect_equal(length(e), 0)
  
  nb <- nb('class', features = 'maint')
  e <- includes_in_supernodes(nb, c('doors', 'safety'))
  expect_equal(length(e), 2)
  
  nb <- nb('class', features = c('maint', 'buying'))
  e <- includes_in_supernodes(nb, c('doors', 'safety'))
  expect_equal(length(e), 4)
  
  nb <- nb('class', features = c('maint', 'buying'))
  e <- includes_in_supernodes(nb, c('doors', 'safety', 'lug_boot'))
  expect_equal(length(e), 6)  
  
  nb <- nb('class', features = c('maint', 'buying', 'persons'))
  e <- includes_in_supernodes(nb, c('doors', 'safety', 'lug_boot'))
  expect_equal(length(e), 9)  
  
  f <- includes_in_supernodes(e[[1]], c('doors', 'safety', 'lug_boot'))
  expect_equal(length(f), 6)
})