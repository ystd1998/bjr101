
data(sleep)
head(sleep)
g1 <- sleep$extra[1 : 10]; g2 <- sleep$extra[11 : 20]

n1 <- length(g1); n2 <- length(g2)
sp <- sqrt( ((n1 - 1) * sd(g1)^2 + (n2-1) * sd(g2)^2) / (n1 + n2-2))
md <- mean(g2) - mean(g1)
semd <- sp * sqrt(1 / n1 + 1/n2)
rbind(
    md + c(-1, 1) * qt(.975, n1 + n2 - 2) * semd,
    t.test(g2, g1, paired = FALSE, var.equal = TRUE)$conf,
    t.test(g2, g1, paired = TRUE)$conf
)
