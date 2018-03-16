library(UsingR)
data(father.son)
x <- father.son$sheight
n <- length(x)
B <- 10000
resamples <- matrix(sample(x, n*B, replace=TRUE),B,n)
resampledMedians <-apply(resamples, 1, median)


summary(resampledMedians)
