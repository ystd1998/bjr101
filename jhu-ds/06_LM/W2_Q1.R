x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)

q1lm <- lm(y~x)
summary(q1lm)

# q1
summary(lm(y ~ x))$coef

# q2
summary(lm(y ~ x))$sigma

# q3
q3lm <- lm(mpg ~ wt, data=mtcars)
summary(q3lm)

# q3
predict(q3lm, newdata=data.frame(wt=mean(mtcars$wt)), interval = "confidence")


# q5
predict(q3lm, newdata=data.frame(wt=3), interval="predict") 

# q6
confint(q3lm)[2,]*2

# q9
fit1 <- lm(mpg ~ wt, data = mtcars)
fit2 <- lm(mpg ~ 1, data = mtcars)
1 - summary(fit1)$r.squared

# or
sse1 <- sum((predict(fit1) - mtcars$mpg)^2)
sse2 <- sum((predict(fit2) - mtcars$mpg)^2)
sse1/sse2


