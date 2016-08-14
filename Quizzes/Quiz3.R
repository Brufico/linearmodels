
#  Q1
# Consider the mtcars data set. Fit a model with mpg as the
# outcome that includes number of cylinders as a factor
# variable and weight as confounder. Give the adjusted
# estimate for the expected change in mpg comparing 8
# cylinders to 4.

library(dplyr)
unique(mtcars$cyl)
mtcars <- mutate(mtcars, fcyl= factor(cyl))
str(mtcars$fcyl)

fit <- lm(mpg ~ fcyl + wt, data = mtcars)
summary(fit)

# Coefficients:
#         Estimate Std. Error t value Pr(>|t|)
# (Intercept)  33.9908     1.8878  18.006  < 2e-16 ***
#         fcyl6        -4.2556     1.3861  -3.070 0.004718 **
#         fcyl8        -6.0709     1.6523  -3.674 0.000999 ***
#         wt           -3.2056     0.7539  -4.252 0.000213 ***


# Ans = -6.0709 ===============> correct



# Q2
#
# Consider the mtcars data set. Fit a model with mpg as
# the outcome that includes number of cylinders as a factor
# variable and weight as a possible confounding variable.
# Compare the effect of 8 versus 4 cylinders on mpg for the
# adjusted and unadjusted by weight models. Here, adjusted
# means including the weight variable as a term in the
# regression model and unadjusted means the model without
# weight included. What can be said about the effect
# comparing 8 and 4 cylinders after looking at models with
# and without weight included?.

fit_noadjust <- lm(mpg ~ fcyl, data = mtcars)
summary(fit_noadjust)$coef

# Ans : Holding weight constant, cylinder appears to have
# more of an impact on mpg than if weight is disregarded.


# Ans2: Holding weight constant, cylinder appears to have
# less of an impact on mpg than if weight is disregarded.
#  =====================> correct

# Q3

# Consider the mtcars data set. Fit a model with mpg as the
# outcome that considers number of cylinders as a factor
# variable and weight as confounder. Now fit a second model
# with mpg as the outcome model that considers the
# interaction between number of cylinders (as a factor
# variable) and weight. Give the P-value for the likelihood
# ratio test comparing the two models and suggest a model
# using 0.05 as a type I error rate significance benchmark.

fit <- lm(mpg ~ fcyl + wt, data = mtcars)
summary(fit)
fit2 <- lm(mpg ~ fcyl*wt, data = mtcars)
summary(fit2)

# likelihood ratio test ??????

anova(fit, fit2)


# ANs: p-value =  0.1239

# The P-value is larger than 0.05. So, according to our
# criterion, we would fail to reject, which suggests that
# the interaction terms may not be necessary.
# ==============> correct


#  Q4

# Consider the mtcars data set. Fit a model with mpg as the
# outcome that includes number of cylinders as a factor
# variable and weight inlcuded in the model as :
# lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)

# How is the wt coefficient interpreted?

fit4 <- lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)
summary(fit4)$coef


fit4B <- lm(mpg ~ wt + factor(cyl), data = mtcars)
summary(fit4B)$coef


# Ans: The estimated expected change in MPG per half ton
# increase in weight for for a specific number of cylinders
# (4, 6, 8). ==> WRONG

# Ans2 :The estimated expected change in MPG per half ton increase in weight.
===> WRONG

# Ans 3 The estimated expected change in MPG per one ton
# increase in weight for a specific number of cylinders (4,
# 6, 8).


# Q5.

# Consider the following data set;

x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)


# Give the hat diagonal for the most influential point


fit5 <- lm(y ~ x)
hatvalues(fit5)

# 1         2         3         4         5
# 0.2286650 0.2438146 0.2525027 0.2804443 0.9945734


# ANs: 0.9946

# solution:

influence(lm(y ~ x))$hat

## showing how it's actually calculated
xm <- cbind(1, x)
diag(xm %*% solve(t(xm) %*% xm) %*% t(xm))



influence(lm(y ~ x))



# Q6.

# Consider the following data set

x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)

# Give the slope dfbeta for the point with the highest hat value.


fit6 <- lm(y ~ x)
hatvalues(fit6)

dfbeta(fit6)

influence(fit6)


#  Ans = ? -0.931 ?? not in the answer's list
#
#  -.378 is incorrect
#  0.673 is incorrect
#  no answer is incorrect
#  =====> should try ???




# Q7.

# Consider a regression relationship between Y and X with
# and without adjustment for a third variable Z. Which of
# the following is true about comparing the regression
# coefficient between Y and X with and without adjustment
# for Z.


# Ans: It is possible for the coefficient to reverse sign
# after adjustment. For example, it can be strongly
# significant and positive before adjustment and strongly
# significant and negative after adjustment.

