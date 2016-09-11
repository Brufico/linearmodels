
# Book chapt 2
# ============

# general matrix ops examples
#
a <- matrix(c(1,2, 3, 4), ncol=2)
b <- matrix(c(10, 20, 30, 40), ncol=2)

t(a %*% b)

t(b) %*% t(a)


# Least squares regressions matrix solutions

data("mtcars")

y = mtcars$mpg
x = cbind(1, mtcars$hp, mtcars$wt)
solve(t(x) %*% x , t(x) %*% y)

# compare with lm

lm(mpg ~ hp + wt, data = mtcars)


# centering
n <- nrow(x)
I <- diag(rep(1, n))
H = matrix(1, n, n) / n
xt = (I - H) %*% x

apply(xt, 2, mean)

# [1] 0.000000e+00 0.000000e+00 2.168404e-16

## Doing it using sweep
xt2 = sweep(x, 2, apply(x, 2, mean))

apply(xt2, 2, mean)

# [1] 0.000000e+00 0.000000e+00 3.469447e-17

# scaling ?

xt3 = sweep(xt2, 2, apply(x, 2, sd), FUN = "/")
xt3
apply(xt3, 2, sd)
