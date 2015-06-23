## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ------------------------------------------------------------------------
library(bnclassify)
data(car)
summary(car)
a <- nb('class', car)
a

## ------------------------------------------------------------------------
features(a)
is_nb(a)

## ------------------------------------------------------------------------
plot(a)

## ------------------------------------------------------------------------
b <- lp(a, car, smooth = 1)
params(b)

## ------------------------------------------------------------------------
p <- predict(b, car)
head(p)

## ------------------------------------------------------------------------
cv(b, car, k = 10, dag = FALSE)

## ------------------------------------------------------------------------
t <- tan_cl(class = 'class', dataset = car)
tb <- tan_cl(class = 'class', dataset = car, score = 'bic')
plot(t)
plot(tb)

## ------------------------------------------------------------------------
is_ode(t)
is_nb(t)
is_ode(tb)
is_nb(tb)

## ------------------------------------------------------------------------
set.seed(0)
a <- tan_hc('class', car, k = 10, epsilon = 0, smooth = 1)
b <- tan_hcsp('class', car, k = 10, epsilon = 0, smooth = 1)
c <- bsej('class', car, k = 10, epsilon = 0, smooth = 1)
d <- fssj('class', car, k = 10, epsilon = 0, smooth = 1)

## ------------------------------------------------------------------------
is_ode(a)
is_ode(b)
is_ode(c)
is_ode(d)
is_semi_naive(a)
is_semi_naive(b)
is_semi_naive(c)
is_semi_naive(b)
plot(a)
plot(b)
plot(c)
plot(d)

## ------------------------------------------------------------------------
a <- tan_cl('class', car, score = 'aic')
a <- lp(a, car, smooth = 1)
b <- bnc('tan_cl', 'class', car, smooth = 1, dag_args = list(score = 'aic'))
identical(a, b)

## ------------------------------------------------------------------------
a <- nb('class', car)
b <- lp(a, car, smooth = 1)
c <- lpawnb(a, car, smooth = 1, trees = 20, bootstrap_size = 0.5)
sum(abs(params(b)$safety - params(c)$safety))

## ------------------------------------------------------------------------
t <- tan_cl('class', car)
t <- lp(t, dataset = car, smooth = 1)
ta <- lpawnb(t, car, smooth = 1, trees = 10, bootstrap_size = 0.5)
params(t)$buying
params(ta)$buying

## ------------------------------------------------------------------------
nb <- nb('class', car)
nb <- lp(nb, car[c(1, 700), ], smooth = 0)
predict(object = nb, newdata = car[1000:1001, ], prob = TRUE)

## ------------------------------------------------------------------------
library(microbenchmark)
nb <- nb('class', car)
nb <- lp(nb, car, smooth = 0)
gr <- as_grain(nb)
microbenchmark(predict(object = nb, newdata = car, prob = TRUE))
microbenchmark(gRain::predict.grain(gr, 'class', newdata = car),
                               times = 1)

## ------------------------------------------------------------------------
a <- bnc('nb', 'class', car, smooth = 1)
car_cv <- car[1:300, ]
microbenchmark::microbenchmark(cv(a, car_cv, k = 2, dag = FALSE), times = 3e1)

car_cv[1, 4] <- NA
microbenchmark::microbenchmark(cv(a, car_cv, k = 2, dag = FALSE), times = 3e1)

## ------------------------------------------------------------------------
data(voting)
dag <- nb('Class', voting)
a <- lp(dag, voting, smooth = 1)
b <- lpawnb(dag, voting, smooth = 1, trees = 40, bootstrap_size = 0.5)
c <- bnc('tan_cl', 'Class', voting,  smooth = 1)
r <- cv(list(a, b, c), voting, k = 3, dag = FALSE)
r

## ------------------------------------------------------------------------
a <- lp(nb('class', car), car, smooth = 1)	
b <- lp(nb('class', car[, 'class', drop = FALSE]), car, smooth = 1)
d <- lp(nb('class', car[, c(sample(1:6, 4), 7), drop = FALSE]), car, smooth = 1)	
set.seed(0)
microbenchmark(r <- cv(a, car, k = 10, dag = FALSE, smooth = 1))
r
set.seed(0)
microbenchmark(r <- cv(list(a, b, d), car, k = 10, dag = FALSE, smooth = 1))
r

## ------------------------------------------------------------------------
library(mlr)
ctrl = makeFeatSelControlSequential(alpha = 0, method = "sfs")
rdesc = makeResampleDesc(method = "Holdout")
ct <- mlr::makeClassifTask(id = "compare", data = car, target = 'class', 
                        fixup.data = "no", check.data = FALSE)  
nf <- lp(nb('class', car), car, 1)
bnl <- as_mlr(nf, dag = TRUE)
sfeats = selectFeatures(learner = bnl, task = ct, resampling = rdesc,
                      control = ctrl, show.info = FALSE)
sfeats$x
detach('package:mlr')

## ------------------------------------------------------------------------
if (!requireNamespace("gRain", quietly = TRUE)) {
    stop("gRain needed ", call. = FALSE)
  }
a <- lp(nb('class', car), car, smooth = 1)
g <- as_grain(a)
gRain::querygrain.grain(g)

