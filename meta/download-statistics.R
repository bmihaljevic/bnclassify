df <- cranlogs::cran_downloads('bnclassify', from = "2014-01-01", to = "2018-01-01")
sum(df$count) 


df <- cranlogs::cran_downloads('bnclassify', from = "2014-01-01", to = "2019-09-25") 
sum(df$count) 

# install.packages('adjustedcranlogs')
library(adjustedcranlogs)
df <- adjustedcranlogs::adj_cran_downloads('bnclassify', from = "2014-01-01", to = "2019-09-25")
head(df)
tail(df)
plot(df$total_downloads)
plot(df$adjusted_total_downloads)
