# W4_Q3 , p-value from proportion test -- index.pdf
pbinom(2, size=4, prob=0.5, lower.tail = FALSE)

# W4_Q4, p-value from proportion test -- index.pdf
ppois(10, 17.87, lower.tail=TRUE)

# W4_Q7, power.t.test
power.t.test(n=100, delta=0.01, sd=0.04, type="one.sample",  alternative = "one.sided")$power

# W4_Q8. power.t.test
power.t.test(power=0.9, delta=0.01, sd=0.04, type="one.sample",  alternative = "one.sided")$n
