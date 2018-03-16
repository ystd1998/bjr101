
n1 <- 10; n2 <- 10
sdg1 <- sqrt(0.6); sdg2 <- sqrt(0.68)
mg1 <- 3; mg2 <- 5
md <- mg1-mg2
sp <- sqrt( ((n1 - 1) * sdg1^2 + (n2-1) * sdg2^2) / (n1 + n2-2))
sp
qt1 <- qt(0.975,n1+n2-2)
qt1
semd <- sp * sqrt(1 / n1 + 1/n2)
md + c(-1, 1) * qt1 * semd

