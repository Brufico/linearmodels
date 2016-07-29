
Linear model with two variables

# population
n = 100; x = rnorm(n); x2 = rnorm(n); x3 = rnorm(n)
y = 1 + x + x2 + x3 + rnorm(n, sd = .1)

# calculs

ey = resid(lm(y ~ x2 + x3))
ex = resid(lm(x ~ x2 + x3))

sum(ey * ex) / sum(ex ^ 2)

coef(lm(ey ~ ex - 1))
coef(lm(y ~ x + x2 + x3))



#' Examples multiple variables
#' ===========================
#'
#'
#' swiss
#' -----


require(datasets); data(swiss); require(GGally); require(ggplot2)
g = ggpairs(swiss, lower = list(continuous = "smooth") #, params = c(method = "loess")
            )
g




#' Calling lm

summary(lm(Fertility ~ . , data = swiss))
summary(lm(Fertility ~ . , data = swiss))$coefficients

# by contrast:

summary(lm(Fertility ~ Agriculture, data = swiss))$coefficients # == > Sign change in the coefficient


# How can adjustment reverse the sign of an effect? Let's try a simulation.

n <- 100
x2 <- 1 : n
x1 <- .01 * x2 + runif(n, -.1, .1)
y = -x1 + x2 + rnorm(n, sd = .01)


summary(lm(y ~ x1))$coef
summary(lm(y ~ x1 + x2))$coef


dat = data.frame(y = y, x1 = x1, x2 = x2, ey = resid(lm(y ~ x2)), ex1 = resid(lm(x1 ~ x2)))
library(ggplot2)
g = ggplot(dat, aes(y = y, x = x1, colour = x2))
g = g + geom_point(colour="grey50", size = 5) + geom_smooth(method = lm, se = FALSE, colour = "black")
g = g + geom_point(size = 4)
g

g2 = ggplot(dat, aes(y = ey, x = ex1, colour = x2))
g2 = g2 + geom_point(colour="grey50", size = 5) + geom_smooth(method = lm, se = FALSE, colour = "black") + geom_point(size = 4)
g2




#' What if we include an unnecessary variable?
#' z adds no new linear information, since it's a linear combination of variables already included. R just drops terms that are linear combinations of other terms.

z <- swiss$Agriculture + swiss$Education
lm(Fertility ~ . + z, data = swiss)
summary(lm(Fertility ~ . + z, data = swiss))


