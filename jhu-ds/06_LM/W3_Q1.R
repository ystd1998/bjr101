q1lm <- lm(mpg~factor(cyl)+wt, data=mtcars)
summary(q1lm)

q1lm$coefficients

q1lm <- lm(mpg ~ factor(cyl) + wt, data = mtcars)
summary(q1lm)$coef

q2lm <- lm(mpg~factor(cyl), data=mtcars)
q2lm$coefficients

q3lm <- lm(mpg~factor(cyl)+wt+factor(cyl)*wt, data=mtcars)
summary(q3lm)

x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)

q5lm <- lm(y~x)
hatvalues(q5lm)
influence(lm(y ~ x))$hat

dfbeta(q5lm)
influence.measures(lm(y ~ x))
