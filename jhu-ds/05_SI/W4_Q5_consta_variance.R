
n1 <- 9; n2 <- 9
sdg1 <- 1.5; sdg2 <- 1.8
mg1 <- -3; mg2 <- 1
md <- mg1-mg2
md
sp <- sqrt( ((n1 - 1) * sdg1^2 + (n2-1) * sdg2^2) / (n1 + n2-2))
semd <- sp * sqrt(1 / n1 + 1/n2)
semd
#qt1 <- qt(0.975,n1+n2-2)
qt1 <- pt(md/semd, n1+n2-2)
qt1
md + c(-1, 1) * qt1 * semd

