context("HC operators") 

test_that("Expand supernode nominal", {
  a <- nbcarp(car[, 2:7])
  s <- not_cci(a)
  ex <- expand_supernodes(new_node = 'buying', supernodes = s, bnc_dag = a)
  expect_equal(length(ex), 5)
  maint <- ex[[1]]
  expect_equal(graph::numEdges(to_graphNEL(maint)), 7)
  expect_equal(families(maint)[['buying']], c('buying', 'maint', 'class'))
  
  a <- nbcar()
  expect_error(expand_supernodes(new_node = 'safety', 
                                 supernodes = s, bnc_dag = a), 'already')
})

test_that("includes_by_joins nominal", {
  a <- nbcarp(car[, 5:7])
  # All possible including models
  # debugonce(includes_by_joins)
  cands <- includes_by_joins(a, colnames(car)[-7])
  expect_equal(length(cands), 4 * 2)
  buying <- cands[[1]]
  expect_equal(families(buying)[['buying']], 
               c('buying', 'lug_boot', 'class'))
})

