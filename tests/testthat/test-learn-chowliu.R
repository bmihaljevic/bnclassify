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

test_that("continuous chowliu iris", {
  data(car)
 cl <- chowliu(class = 'Species', dataset = iris)
 check_cl(cl, 4 + 3, 'Species', colnames(iris)[-5])
}) 

test_that("continuous chowliu mammographic", {
  data(mammographic)
  cl <- chowliu(class = 'class', dataset = mammographic)
  check_cl(cl, 4 + 5, 'class', colnames(mammographic)[-6])
}) 

test_that("continuous chowliu No features", {
 cl <- chowliu(class = 'Species', dataset = iris[ , 5, drop=F])
 check_cl(cl, 0, 'Species', character()) 
})

test_that('loglik,aic,bic test with continuous variables',{
  data("mammographic")
  x_iyes<-subset(mammographic$X5, mammographic$class == 'yes')
  x_ino<-subset(mammographic$X5, mammographic$class == 'no')
  x_jno<-subset(mammographic$X67, mammographic$class == 'no')
  x_jyes<-subset(mammographic$X67, mammographic$class == 'yes')
  
  first_class <- log(1-cor(x_iyes,x_jyes)^2)*0.5
  second_class <- log(1-cor(x_ino,x_jno)^2)*0.5
  cmi <- -(first_class+second_class)/2
  aic <- 2*28-(2*log(cmi))
  bic <- 28*log(884) - 2*log(cmi)
  
  
  scores <- local_ode_score_contrib_cont(x = 'X5', 
                                         y = 'X67', class = 'class',
                                         dataset = mammographic, param = 28)
  expect_equal(scores[['loglik']], cmi, tolerance = 1e-3)
  expect_equal(scores[['aic']], aic,tolerance = 1e-1)
  expect_equal(scores[['bic']], bic, tolerance = 1e-1)
  
  })

test_that("local scores bic correctness with continuous variables", {
  cl <- bnc("tan_cl",'class',mammographic,dag_args = list('loglik'))
  acclog <- accuracy(predict(cl,mammographic),mammographic$class)
  
  aic <- bnc("tan_cl",'class',mammographic,dag_args = list('aic'))
  accaic <- accuracy(predict(aic,mammographic),mammographic$class)
  
  expect_gt(accaic,acclog)
  
  })

