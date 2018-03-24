library(MASS)
names(shuttle)
str(shuttle)

# q1
fit <- glm( use ~ wind, family='binomial' , data=shuttle)
exp(fit$coeff)

# q2
fit <- glm(use ~ wind + as.factor(magn), family='binomial', shuttle)
exp(fit$coeff)

# q4
data(InsectSprays)
outp <- exp(coef(glm(count ~ as.factor(spray) - 1, family="poisson", InsectSprays)))
outp
outp[1]/outp[2]

# Question 5
?offset
log(10)

# Question 6
x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)

z <- (x > 0) * x
fit <- lm(y ~ x + z)
sum(coef(fit)[2:3])




plot(y ~ x)
d1 <- c(0, 0 ,0, 0, 0,  0,  1 , 1,  1 , 1 , 1)
d2 <- c(1, 1 ,1, 1, 1,  1,  0 , 0,  0, 0 , 0) 
# y = c + d1 * x + d2 * x 
summary(lm(y ~ d1*x))
