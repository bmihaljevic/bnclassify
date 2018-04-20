context("Chow-Liu")

check_cl <- function(x, nedges, class, features) {
  expect_is(x, "bnc_dag")
  expect_equal(narcs(x), nedges)
  expect_equal(class_var(x), class)
  expect_true(is_perm(features(x), features))
}

test_that("chowliu nominal car", {
  cl <- chowliu(class = 'class', dataset = car)
  check_cl(cl, 6 + 5, 'class', colnames(car)[-7])
})  

test_that("chowliu nominal voting", {  
  cl <- chowliu(class = 'Class', dataset = voting)
  check_cl(cl, 16 + 15, 'Class', colnames(voting)[-17])
})

test_that("chowliu No features", {
  cl <- chowliu(class = 'Class', dataset = voting[ , 17, drop=F])
  check_cl(cl, 0, 'Class', character()) 
})

test_that("chowliu bic car", {
  t <- chowliu(class = 'class', dataset = car, score = "bic")
  check_cl(t, 6, 'class', colnames(car)[-7])   
})

test_that("chowliu bic voting", {
  t <- chowliu(class = 'Class', dataset = voting, score = "bic")
  check_cl(t, 30, 'Class', colnames(voting)[-17])   
  expect_equal(families(t)[['immigration']], c('immigration', 'Class'))
})

#   t <- chowliu(class = 'Class', dataset = dbreast, score = "bic")
#   expect_equal(narcs(t), 13) 

test_that("chowliu single-row dataset", {
  t <- chowliu(class = 'class', dataset = car[1, ], score = "loglik")
  expect_equal(narcs(t), 11)
})

test_that("pairwise local scores nominal", {  
  a <- pairwise_ode_score_contribs(class = 'class', dataset = car, score= "loglik")  
  w <- graph_get_named_weights(a) 
  b <- subset(w, from == 'maint' & to == 'safety')['w']
  cmi <- cmi("maint", "safety", car, 'class')
  expect_equal(unname(unlist(b)), cmi, tolerance = 0.0001)
  expect_true(all(unlist(w) > 0))  
})

test_that("pairwise local scores No features", {  
  a <- pairwise_ode_score_contribs(class = 'class', dataset = car[,7, drop=F], 
                           score = "loglik")
  expect_equal(a, graph_internal(edgemode = "undirected"))
# No weights kept. 
  a <- pairwise_ode_score_contribs(class = 'class', dataset = car, score = "bic")  
  w <- a$weights
  expect_equal(graph_nodes(a), colnames(car)[-7])
  expect_equal(length(unlist(w)), 0L)
})

test_that("pairwise local scores bic", {  
  t <- pairwise_ode_score_contribs(class = 'class', dataset = car, score = "bic")    
  expect_equal(graph_num_arcs(t), 0)
})

test_that("local scores correctness", {  
  a <- local_ode_score_contrib(x = 'buying', y = 'maint', class = 'class', 
                               dataset = car)
  expect_equal(unname(a['loglik']), 0.07199921, tolerance = 0.0001)
  expect_true(a['bic'] < a['aic'])    
  
  a <- local_ode_score_contrib(x = 'water_project_cost_sharing', y = 'crime', 
                     class = 'Class', dataset = voting)  
  expect_equal(unname(a['loglik']), 0.003924715, tolerance = 0.0001)
  expect_true(a['bic'] < a['aic'])      
})

test_that("local scores bic correctness", {
  v <- na.omit(voting)
  scores <- local_ode_score_contrib(x = 'handicapped_infants', 
                            y = 'water_project_cost_sharing', class = 'Class',
                            dataset = v)
  # I know the correct value; see correctness-checks.R
  expect_equal(scores[['bic']], -4.280159, tolerance = 1e-5)
})

test_that("Max weight forest", {   
  g <- pairwise_ode_score_contribs(class = 'Class', voting, score = 'loglik')
  u <- max_weight_forest(g)
  expect_equal(graph_num_arcs(u), 15)  
  
  g <- pairwise_ode_score_contribs(class = 'class', dataset = car, 
                                   score = "loglik")
  u <- max_weight_forest(g)
  expect_equal(graph_num_arcs(u), 5)  
  
# Forest
  g <- pairwise_ode_score_contribs(class = 'class', dataset = car, 
                                   score = "aic")
  u <- max_weight_forest(g)
  expect_equal(graph_num_arcs(u), 3)  
})