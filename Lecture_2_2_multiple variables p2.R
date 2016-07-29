
#' ---
#' title = "Examples multiple variables"
#' ---
#'
#'
#' 1 swiss
#' ==========

# ggally must be loaded !!!!!!

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

#'
#' 2 Dummy Variables
#'================
#'

#  Graph
require(datasets)
data("InsectSprays")
require(stats)
g <- ggplot(InsectSprays, aes(spray, count, fill = spray)) +
        geom_violin(color="black", size = 2)
g + xlab("Type of spray") + ylab("Insect count")


#'
#' Linear Model
#' ------------
summary(lm(count ~ spray, data = InsectSprays))$coef

# ==> A is missing from the output (reference level) ==> R chooses automatically
# the reference level as the first level

#'
#' Hardcoding the dummy variables
#' ------------------------------

summary(lm(count ~
                   I(1* (spray == 'B')) + I(1* (spray == 'C')) +
                   I(1* (spray == 'D')) + I(1* (spray == 'E')) +
                   I(1* (spray == 'F'))
           , data = InsectSprays))$coef

# REM 1 * (spray == 'B')  the multiplication forces the
# result to numeric


# if we (mistakenly) also include spray A

summary(lm(count ~
                   I(1* (spray == 'B')) + I(1* (spray == 'C')) +
                   I(1* (spray == 'D')) + I(1* (spray == 'E')) +
                   I(1* (spray == 'F')) + I(1* (spray == 'A')),
           data = InsectSprays))


#' ###If we omit the intercept ?

summary(lm(count ~ spray - 1, data = InsectSprays))$coef

# ==> All the sprays have a coefficient


#' ### Changing the reference level
#'

spray2 <- relevel(InsectSprays$spray, "C")
summary(lm(count ~ spray2, data = InsectSprays))$coef
# verif
levels(spray2)



#' ANCOVA ??
#' =========

# get swiss
require(datasets)
require(ggplot2)
data(swiss)
head(swiss)

hist(swiss$Catholic)
# Create a binary variable
# dplyr
library(dplyr)
swiss <- mutate(swiss, CatholicBin = 1 * (Catholic >= 50))

g <-  ggplot(swiss, aes(Agriculture, Fertility)) +
        geom_point(size=3, color = "black") +
        geom_point(aes(color=CatholicBin), size = 2)
g

# fitting one line and plotting

fit <- lm(Fertility ~ Agriculture, data = swiss)

coef(fit)
g1 <- g + geom_abline(intercept = coef(fit)[1] ,
                      slope = coef(fit)[2])
g1



# fitting parallel line and plotting

fit <- lm(Fertility ~ Agriculture + factor(CatholicBin),
          data = swiss)

coef(fit)

g2 <- g + geom_abline(intercept = coef(fit)[1] ,
                      slope = coef(fit)[2]) +
        geom_abline(intercept = coef(fit)[1] + coef(fit)[3],
                    slope = coef(fit)[2], color = "blue")
g2 + facet_grid(. ~ CatholicBin)


# fitting multiple line and plotting


fit <- lm(Fertility ~ Agriculture * factor(CatholicBin),
          data = swiss)

coef(fit)

g2 <- g + geom_abline(intercept = coef(fit)[1] ,
                      slope = coef(fit)[2]) +
        geom_abline(intercept = coef(fit)[1] + coef(fit)[3],
                    slope = coef(fit)[2] + coef(fit)[4]
                    , color = "blue")
g2 + facet_grid(. ~ CatholicBin)
