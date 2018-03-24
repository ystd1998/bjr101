# 10: Variance Inflation Factors

makelms <- function(x1, x2, x3){
  # Simulate a dependent variable, y, as x1
  # plus a normally distributed error of mean 0 and 
  # standard deviation .3.
  y <- x1 + rnorm(length(x1), sd = .3)
  # Find the coefficient of x1 in 3 nested linear
  # models, the first including only the predictor x1,
  # the second x1 and x2, the third x1, x2, and x3.
  c(coef(lm(y ~ x1))[2], 
    coef(lm(y ~ x1 + x2))[2], 
    coef(lm(y ~ x1 + x2 + x3))[2])
}

# Regressor generation process 1.
rgp1 <- function(){
  print("Processing. Please wait.")
  # number of samples per simulation
  n <- 100
  # number of simulations
  nosim <- 1000
  # set seed for reproducibility
  set.seed(4321)
  # Point A
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  x3 <- rnorm(n)
  # Point B
  betas <- sapply(1 : nosim, function(i)makelms(x1, x2, x3))
  round(apply(betas, 1, var), 5)
}

# Regressor generation process 2.
rgp2 <- function(){
  print("Processing. Please wait.")
  # number of samples per simulation
  n <- 100
  # number of simulations
  nosim <- 1000
  # set seed for reproducibility
  set.seed(4321)
  # Point C
  x1 <- rnorm(n)
  x2 <- x1/sqrt(2) + rnorm(n) /sqrt(2)
  x3 <- x1 * 0.95 + rnorm(n) * sqrt(1 - 0.95^2)
  # Point D
  betas <- sapply(1 : nosim, function(i)makelms(x1, x2, x3))
  round(apply(betas, 1, var), 5)
}

> library(car)
> mdl <- lm(Fertility ~ ., data=swiss)
> vif(mdl)
>  mdl2 <- lm(Fertility ~ . - Examination, data=swiss)
> vif(mdl2)

# 11: Overfitting and Underfitting  

> fit1 <- lm(Fertility ~ Agriculture, data=swiss)

> fit3 <- lm(Fertility ~ Agriculture + Examination + Education, data=swiss) 

# significance test for adding regressor
> anova(fit1, fit3)

> deviance(fit3) ## RSS
> d <- deviance(fit3)/43
> n <- (deviance(fit1)-deviance(fit3))/2
> n/d ==> F-Statistical used by anova
>  pf(n/d, 2, 43, lower.tail=FALSE) ==> p-vaulue used by anova

## need to test residual's noraml distribution
> shapiro.test(fit3$residuals)


# 12: Binary Outcomes

> View(ravenData)

> mdl <- glm( ravenWinNum ~ ravenScore, family='binomial' , data=ravenData)

> lodds <- predict(mdl, data.frame(ravenScore=c(0, 3, 6)))

> summary(mdl)
> confint(mdl)

> exp(confint(mdl))

> anova(mdl)

> qchisq(0.95, 1)

# 13: Count Outcomes                            

> var(rpois(1000, 50))

> class(hits[,'date'])

> as.integer(head(hits[,'date']))

> mdl <- glm(visits ~ date, poisson, hits)

> confint(mdl, 'date')

> which.max(hits[,'visits'])

> hits[704,]

> mdl2 <- glm(formula = simplystats ~ date, family = poisson, data = hits, offset = log(visits + 1))

> qpois(.95, mdl2$fitted.values[704])



