test_that("spode", {
  u <- spode(sp='D', features=LETTERS[5:9], class = 'C')
  expect_equal(graph::numEdges(bnc_dag(u)), 11)
  expect_equal(length(graph::adj(bnc_dag(u), 'D')$D), 5)
  
  u <- spode(sp='E', features=LETTERS[c(4, 6:9)], class = 'C')
  expect_equal(graph::numEdges(bnc_dag(u)), 11)
  expect_equal(length(graph::adj(bnc_dag(u), 'E')$E), 5)
})