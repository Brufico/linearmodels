#'---
#'title: "Linear models"
#'subtitle: "Code: Mdel selection ; Lecture 2_5"
#'author: "Bruno Fischer Colonimos"
#'date: '`r Sys.Date()`'
#'output:
#'      html_document:
#'          number_sections: yes
#'          theme: readable
#'          toc: yes
#'      word_document:
#'          default
#'      pdf_document:
#'          number_sections: yes
#'          theme: readable
#'          toc: yes
#' ---




#' Variance inflation
#' ===================


#'
#' Simulation 1
#' ------------

# 3 regressors, independents
n <- 100; nosim <- 1000
x1 <- rnorm(n); x2 <- rnorm(n); x3 <- rnorm(n);
betas <- sapply(1:nosim, function(i){
        y <- x1 + rnorm(n, sd = .3)
        c(coef(lm(y ~ x1))[2],
          coef(lm(y ~ x1 + x2))[2],
          coef(lm(y ~ x1 + x2 + x3))[2])
})
round(apply(betas, 1, sd), 5)

# results
# xl      xl      xl
# 0.02887 0.02892 0.02920
#
# ==> not a lot of change



#'
#' Simulation 2
#' ------------

# 3 regressors, correlated
n <- 100; nosim <- 1000
x1 <- rnorm(n); x2 <- x1/sqrt(2) + rnorm(n) /sqrt(2)
x3 <- x1 * 0.95 + rnorm(n) * sqrt(1 - 0.95^2);

betas <- sapply(1:nosim, function(i){
        y <- x1 + rnorm(n, sd = .3)
        c(coef(lm(y ~ x1))[2],
          coef(lm(y ~ x1 + x2))[2],
          coef(lm(y ~ x1 + x2 + x3))[2])
})
round(apply(betas, 1, sd), 5)

results
# x1      x1      x1
# 0.02808 0.03737 0.10609

# ==> Huge increase in the variance of the simulated betas
# happens when the regressors are correlated
#

#'
#' Revisiting   our previous simulation
#' ====================================

##doesn't depend on which y you use,
y <- xl + rnorm(n, sd = .3)
a <- summary(lm(y ~ x1))$cov.unscaled[2,2]
c(summary(lm(y ~ x1 + x2))$cov.unscaled[2,2],
  summary(lm(y ~ x1 + x2 + x3))$cov.unsealed[2,2]) / a



temp <- apply(betas, 1, var); temp[2 : 3] / temp[1]


#'
#' Swiss again
#' ============

data(swiss);
fit1 <- lm(Fertility ~ Agriculture, data = swiss)
a <- summary(fit1)$cov.unscaled[2,2]
fit2 <- lm(Fertility ~ Agriculture + Examination, data = swiss)
fit3 <- lm(Fertility ~ Agriculture + Examination + Education,
               data = swiss)
c(summary(fit2)$cov.unscaled[2,2],
  summary(fit3)$cov.unscaled[2,2]) / a



#'
#' Swiss VIF's
#' ============

library(car)






#'
#'
#' Variance Inflation factors
#' ==========================
#'
# Swiss data
#




#' How to do nested model testing in R
#' ====================================
#'

fit1 <- lm(Fertility ~ Agriculture, data = swiss)
fit3 <- update(fit1, Fertility ~ Agriculture + Examination + Education)
fit5 <- update(fit1, Fertility ~ Agriculture + Examination + Education + Catholic + Infant.Mortality)
anova(fit1, fit3, fit5)

