

x <- rnorm(30, 1,1)
y <- rnorm(30, 3,2)

f1 <- lm(y ~ x)

ci <- confint(f1)

str(ci)
ci[2,]
