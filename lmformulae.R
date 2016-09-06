#' ---
#' title: 'Regression matrix formulae, verification'
#' subtitle: "lmformulae.R"
#' author: "Bruno Fischer Colonimos"
#' date: '`r format(Sys.Date(), "%d %B %Y")`'
#' output:
#'         html_document:
#'                 theme: readable
#'                 number_sections: yes
#'         pdf_document:
#'         number_sections: yes
#' ---


#' Make some data
#' ==============

set.seed(6)
n <- 30
x1 = rnorm(n, 10, 1)
x2 = rnorm(n, 8, 2)
y = 3 + 2 * x1 - x2 + rnorm(n, 0, .5)


#'
#' regression using lm
#' ===================
#'
fit0 <- lm(y ~ x1 + x2)
summary(fit0)

# Coefficients:
#         Estimate Std. Error t value Pr(>|t|)
# (Intercept)  2.31307    1.21996   1.896   0.0687 .
# x1           2.05180    0.09365  21.910  < 2e-16 ***
# x2          -0.98140    0.05825 -16.848 7.49e-16 ***


# coef(fit0)
# str(fit0)
# summary(fit0)$coef[ , 2]




#'
#' Regression using Matrices
#' =========================
#'


# construction of matrices
X <- cbind(Inter = rep(1, 30), x1, x2)
# str(X)
Y = matrix(y, ncol = 1)



#'
#' coefficient estimates
#' ----------------------
#'
Z <- solve(t(X) %*% X)
Cestimates <- Z %*% (t(X) %*% Y)

#        [,1]
# Inter  2.313067
# x1     2.051797
# x2    -0.981396


#'
#' Estimates of Y
#' --------------
Yest <- X %*% Cestimates

#' Residuals
#' ---------
Res <- Y - Yest

#' Standard error of the coeffs
#' ----------------------------
#'
#'$$\sigma_j = \sqrt{z_{j,j} \cdot \hat \sigma_{resid}}$$
#' with $$\hat \sigma_{resid} = \frac { \sum{resid ^2} } {n - p - 1}$$
#'
#' or with numbering:
#'
#'(@foo) $\hat \sigma_{resid} = \frac { \sum{resid ^2} } {n - p - 1}$
#'
#' and we quote (@foo)
#'
#'
# diagonal(Z)
Zd <- numeric(3)
for(i in 1:3) { Zd[i] <- Z[i,i]}
# ecart-type des résidus
s2=sum(Res ^ 2)/ (n - 3) # dl = n - (nvars + 1)
sigmacoeff <- sqrt(Zd * s2)
sigmacoeff

# [1] 1.21995790 0.09364535 0.05824959



