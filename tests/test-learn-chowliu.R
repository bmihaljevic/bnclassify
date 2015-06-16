# context("Chow-Liu")
# 
# check_cl <- function(x, nedges, class, features) {
#   expect_is(x, "bnc_dag")
#   expect_equal(graph::numEdges(to_graphNEL(x)), nedges)
#   expect_equal(bnc_class(x), class)
#   expect_equal(bnc_features(x), features)
# }
# 
# test_that("chowliu nominal car", {
#   cl <- chowliu(class = 'class', dataset = car)
#   check_cl(cl, 6 + 5, 'class', colnames(car)[-7])
# })  
# 
# test_that("chowliu nominal voting", {  
#   cl <- chowliu(class = 'Class', dataset = voting)
#   check_cl(cl, 16 + 15, 'Class', colnames(voting)[-17])
# })
# 
# test_that("chowliu No features", {
#   cl <- chowliu(class = 'Class', dataset = voting[ , 17, drop=F])
#   check_cl(cl, 0, 'Class', character()) 
# })
# 
# test_that("pairwise local scores", {  
# # ?   
#   a <- pairwise_ode_scores(class = 'class', dataset = car, score="loglik")  
#   w <- graph::edgeWeights(a)
#   b <- w[['maint']][['safety']]
#   cmi <- cmi("maint", "safety", car, 'class')
#   expect_equal(unname(unlist(b)), cmi, tolerance = 0.0001)
#   d <- w[['safety']][['maint']] # Its symmetric
#   expect_identical(unname(b), unname(d))   #   All weights non-negative 0
#   expect_true(all(unlist(w) > 0))  
# #  No features
#   a <- pairwise_ode_scores(class = 'class', dataset = car[,7, drop=F], 
#                            score="loglik")
#   expect_equal(a, graph::graphNEL())
# # No weights kept. 
#   a <- pairwise_ode_scores(class = 'class', dataset = car, score="bic")  
#   w <- graph::edgeWeights(a)  
#   expect_equal(graph::nodes(a), colnames(car)[-7])
#   expect_equal(length(unlist(w)), 0L)
# 
#   t <- pairwise_ode_scores(class = 'class', dataset = car, score = "bic")    
#   expect_equal(graph::numEdges(t), 6 + 5) 
#   
#   t <- chowliu(class = 'class', features = colnames(car)[1:6], 
#                blacklist = list(score="bic"), dataset = car)
#   expect_equal(ncol(blacklisted(t)), 15)  
#   expect_equal(graph::numEdges(t$dag), 6) 
#   
#   t <- chowliu(class = 'Class', features = colnames(v)[1:16], 
#                blacklist = list(score="bic"), dataset = voting)
#   expect_equal(graph::numEdges(bnc_dag(t)), 30)   
#   expect_equal(gRbase::parents('immigration', bnc_dag(t)), 'Class')
#   expect_equal(ncol(blacklisted(t)), 38)  
#   
#   t <- chowliu(class = 'Class', features = colnames(dbreast)[1:9], 
#                blacklist = list(score="bic"), dataset = dbreast)
#   expect_equal(graph::numEdges(bnc_dag(t)), 13) 
#   expect_equal(ncol(blacklisted(t)), 32)  
# })
# 
# test_that("pairwise ode scores", {  
#   a <- local_ode_score(x = 'buying', y = 'maint', class = 'class', 
#                                dataset = car)
#   expect_equal(unname(a['loglik']), 0.07199921, tolerance = 0.0001)
#   expect_true(a['bic'] < a['aic'])    
#   
#   a <- local_ode_score(x = 'water_project_cost_sharing', y = 'crime', 
#                      class = 'Class', dataset = voting)  
#   expect_equal(unname(a['loglik']), 0.003924715, tolerance = 0.0001)
#   expect_true(a['bic'] < a['aic'])      
# })
# 
# test_that("pairwise ode scores correctness", {
#   # Compare edge BIC score to that assigned by bnlearn:  
#   v <- v[, c(1,2,17)]
#   colnames(v) <- letters[1:3]
#   # Fit NB and get BIC
#   nb <- gRbase::dag(~a:c,~b:c)
#   nb <- gRain:::grain.graphNEL(nb, v, smooth = 0)
#   nb <- bnlearn::as.bn.fit(nb)
#   bic_nb <- bnlearn:::BIC.bn.fit(nb, data = v)
#   # fit TAN and get BIC 
#   tn <- gRbase::dag(~a:b:c,~b:c)
#   tn <- gRain:::grain.graphNEL(tn, v, smooth = 0)
#   tn <- bnlearn::as.bn.fit(tn)
#   bic_tn <- bnlearn:::BIC.bn.fit(tn, data = v)
#   #  BIC of the edge = BIC_TAN - BIC_NB  
#   bic_ab_bnlearn <- bic_tn - bic_nb
#   # Compute 
#   bic_ab <- local_ode_score(x = 'a', y = 'b', class = 'c', dataset = v)
#   bic_ab <- bic_ab['bic']
#   # Check equal 
#   expect_equal(unname(bic_ab), unname(bic_ab_bnlearn))  
# })