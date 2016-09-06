
# Bootstrap Package

library(bootstrap)

# To jackknife functions of more  complex data structures,
# write theta so that its argument x is the set of observation numbers
# and simply  pass as data to jackknife the vector 1,2,..n. For example, to jackknife
# the correlation coefficient from a set of 15 data pairs:
# xdata <- matrix(rnorm(30),ncol=2)
# n <- 15
# theta <- function(x,xdata){ cor(xdata[x,1],xdata[x,2]) }
# results <- jackknife(1:n,theta,xdata)


# here we have a sample data mtcars2E

n <- nrow(mtcars2E)
theta <- function(x,xdata){
        fit <- lm(mpg ~ wt + am*wt + qsec, xdata[x, ])
        swiar <- summary(fit)
        sfc <- swiar$coefficients
        nwt <- - (sfc[3,1] / sfc[5,1])
        nwt
}

results <- jackknife(1:n,theta,mtcars2E)

wt0bias <- results$jack.bias

hist(results$jack.values, nclass = nclass.FD(results$jack.values))

# Bootstrap now...
theta <- function(x,xdata){
        fit <- lm(mpg ~ wt + am*wt + qsec, xdata[x, ])
        swiar <- summary(fit)
        sfc <- swiar$coefficients
        nwt <- - (sfc[3,1] / sfc[5,1]) - wt0bias # bias correction?
        nwt
}
results <- bootstrap(1:n,100,theta,mtcars2E, func = mean)
bcalim <- bcanon(1:n,100,theta, xdata=mtcars2E)


mean(results$thetastar, trim = 0.025, na.rm = TRUE)
sd(results$thetastar, na.rm = TRUE)
