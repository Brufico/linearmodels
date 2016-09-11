#' ---
#' title: 'Quiz4'
#' author: 'Bruno Fischer Colonimos'
#' output:
#'     html_document:
#'         number_sections: yes
#'     pdf_document:
#'         number_sections: yes
#' urlcolor: blue
#' fontsize: 11pt
#' geometry: top=.9in, left=1in, right=1in, bottom = 1in, footskip = 0.3in
#'
#' date: "`r format(Sys.Date(), '%d %B %Y')`
#' ---


#'+ libraries, include = FALSE
library(knitr)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(boot)
library(MASS)

data("shuttle")


#' Space Shuttle Autolander Problem (help)
#' ======================================= Description
#'
#' The shuttle data frame has 256 rows and 7 columns. The
#' first six columns are categorical variables giving
#' example conditions; the seventh is the decision. The
#' first 253 rows are the training set, the last 3 the test
#' conditions.
#'
#' Usage:
#' shuttle
#'
#' Format
#' This data frame contains the following factor columns:
#'
#' stability: stable positioning or not (stab / xstab).\
#' error: size of error (MM / SS / LX / XL).\
#' sign: sign of error, positive or negative (pp / nn).\
#' wind: wind sign (head / tail).\
#' magn: wind strength (Light / Medium / Strong / Out of Range).\
#' vis: visibility (yes / no).\
#' use: use the autolander or not. (auto / noauto.)\
#'
#' Source\
#' D. Michie (1989) Problems of computer-aided concept
#' formation. In Applications of Expert Systems 2, ed. J. R.
#' Quinlan, Turing Institute Press / Addison-Wesley, pp.
#' 310–333.
#'
#' References\
#' Venables, W. N. and Ripley, B. D. (2002) Modern Applied
#' Statistics with S. Fourth edition. Springer.
#'


#' Let's take a look
#' =================
head(shuttle)
summary(shuttle)
ggplot(data=shuttle, aes(wind, use))+ geom_jitter()



#' Q1
#' ======
#'
shuttle$use
shuttle$use2


# 1st : recode use ==> auto = 1, noauto = 0
shuttle$use2 <- ifelse(shuttle$use == "auto", 1, 0)

mdl <- glm(formula = use2 ~ wind, family = "binomial" , data = shuttle)
summary(mdl)


cf <- coefficients(mdl)
# result =
# (Intercept)    windtail
# 0.25131443  0.03181183

ecf <- exp(cf)

(exp(cf))[1]*(exp(cf))[2]

#' 2nd try:
#'
1/ (exp(cf))[2]

# ==> 0.9686888

# correction:
library(MASS)
data(shuttle)
## Make our own variables just for illustration
shuttle$auto <- 1 * (shuttle$use == "auto")
shuttle$headwind <- 1 * (shuttle$wind == "head")
fit <- glm(auto ~ headwind, data = shuttle, family = binomial)
exp(coef(fit))
## (Intercept)    headwind
##      1.3273      0.9687


## Another way without redifing variables
fit <- glm(relevel(use, "noauto") ~ relevel(wind, "tail"), data = shuttle, family = binomial)
exp(coef(fit))

##               (Intercept) relevel(wind, "tail")head
##                    1.3273                    0.9687




#' Q2
#' ======
#'

mdl2 <- glm(formula = use2 ~ wind + magn, family = "binomial" , data = shuttle)
summary(mdl2)

cf2 <- coefficients(mdl2)
1/ (exp(cf2))[2]

# Réponse correcte
# The estimate doesn't change with the inclusion of wind strength

shuttle$auto <- 1 * (shuttle$use == "auto")
shuttle$headwind <- 1 * (shuttle$wind == "head")
fit <- glm(auto ~ headwind + magn, data = shuttle, family = binomial)
exp(coef(fit))

## (Intercept)    headwind  magnMedium     magnOut  magnStrong
##      1.4852      0.9685      1.0000      0.6842      0.9376

## Another way without redifing variables
fit <- glm(relevel(use, "noauto") ~ relevel(wind, "tail") + magn, data = shuttle,
           family = binomial)
exp(coef(fit))

##               (Intercept) relevel(wind, "tail")head
##                    1.4852                    0.9685
##                magnMedium                   magnOut
##                    1.0000                    0.6842
##                magnStrong
##                    0.9376



#'Q3
#'======
#'
#'If you fit a logistic regression model to a binary variable, for example use
#'of the autolander, then fit a logistic regression model for one minus the
#'outcome (not using the autolander) what happens to the coefficients?

#' ==> The coefficients reverse their signs.
#'


#'Q4
#'======
#'

data("InsectSprays")
head(InsectSprays  )

mdsp <- glm(count ~ spray, data = InsectSprays, family = "poisson")
summary(mdsp)
coef(mdsp)
1/exp(coef(mdsp)[2])
##   sprayB
## 0.9456522


# Réponse correcte ==============

fit <- glm(count ~ relevel(spray, "B"), data = InsectSprays, family = poisson)
exp(coef(fit))[2]

## relevel(spray, "B")A
##               0.9457


#'
#' Q5
#' ===

x <- rnorm(100, 5, 1)
y <- sapply(x, function(x) rpois(1, 2*x))

t <- 2 * x + rnorm(100, 2, 2)
t2 <- t + log(10)

plot(x,y)


summary(glm(formula = y ~ x, offset= t, family = "poisson" ))
summary(glm(formula = y ~ x, offset= t + log(10), family = "poisson" ))


# Réponse correcte ==> the coefficient estimate is unchanged

# Note, the coefficients are unchanged, except the intercept, which is shifted
# by log(10). Recall that, except the intercept, all of the coefficients are
# interpretted as log relative rates when holding the other variables or offset
# constant. Thus, a unit change in the offset would cancel out. This is not true
# of the intercept, which is interperted as the log rate (not relative rate)
# with all of the covariates set to 0.








#'
#' Q6
#' ===


# scatter.smoothing


n <- 500; x <- seq(0, 4 * pi, length = n); y <- sin(x) + rnorm(n, sd = .3)
knots <- seq(0, 8 * pi, length = 20);
splineTerms <- sapply(knots, function(knot) (x > knot) * (x - knot))
xMat <- cbind(1, x, splineTerms)
yhat <- predict(lm(y ~ xMat - 1))
plot(x, y, frame = FALSE, pch = 21, bg = "lightblue", cex = 2)
lines(x, yhat, col = "red", lwd = 2)




x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)

plot(x, y, frame = FALSE, pch = 21, bg = "lightblue", cex = 2)

knots <- c(-5, 0, 5)
splineTerms <- sapply(knots, function(knot) (x > knot) * (x - knot))
xMat <- cbind(1, x, splineTerms)
fit <- lm(y ~ xMat - 1)
yhat <- predict(lm(y ~ xMat - 1))
plot(x, y, frame = FALSE, pch = 21, bg = "lightblue", cex = 2)
lines(x, yhat, col = "red", lwd = 2)

coef(fit)

(yhat[11] - yhat[6])/5
(yhat[11] - yhat[7])/4

# ==> 1.013067




# Réponse correcte

z <- (x > 0) * x
fit <- lm(y ~ x + z)
sum(coef(fit)[2:3])

## [1] 1.013


