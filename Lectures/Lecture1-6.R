#' ---
#' title: "Linear Models"
#' author: "Bruno Fischer Colonimos"
#' date: '`r format(Sys.Date(), "%d %B %Y")`'
#' output:
#'         html_document:
#'               number_sections: yes
#'               theme: readable
#'               toc: yes
#'         pdf_document:
#'              number_sections: yes
#'              toc: yes
#' subtitle: "lecture 1.6"
#' ---


#' Week 2
#' ======

#' first
#' ---------
#'
library(UsingR)
data(diamond)
library(ggplot2)
g <- ggplot(diamond, aes(x = carat, y = price)) +
        xlab("Mass (carats)") +
        ylab("Price (SIN $)") +
        geom_smooth(method = "lm", colour = "black") +
        geom_point(size = 7, colour = "black", alpha=0.5) +
        geom_point(size = 5, colour = "blue", alpha=0.2)
g

#' Second
#' ---------
#'
data(diamond)
# easy access names
y <- diamond$price
x <- diamond$carat
n <- length(y)

#  fit a model
fit <- lm(y ~ x)
e <- resid(fit) # get the residuals
yhat <- predict(fit) # get the predicted values

# verifications
sum(e)
all.equal(y - yhat, e)
sum(e * x)

max(abs(e -(y - yhat)))
max(abs(e - (y - coef(fit)[1] - coef(fit)[2] * x)))


# new plot (Base R)
plot(diamond$carat, diamond$price,
     xlab = "Mass (carats)",
     ylab = "Price (SIN $)",
     bg = "lightblue",
     col  =  "black", cex = 1.1, pch = 21,frame = FALSE)
abline(fit, lwd = 2)
for (i in 1 : n)
        lines(c(x[i], x[i]), c(y[i], yhat[i]), col = "red", lwd=2)

# plotting the residuals
plot(x, e,
     xlab = "Mass (carats)",
     ylab = "Residuals (SIN $)",
     bg = "lightblue",
     col = "black", cex = 2, pch = 21,frame = FALSE)
abline(h = 0, lwd = 2)
for (i in 1 : n)
        lines(c(x[i], x[i]), c(e[i], 0), col = "red" , lwd = 2)




#' Pathological residual plots
#'  ---------------------------

# Non-linear data -----------------------

# data
x = runif(100, -3, 3); y = x + sin(x) + rnorm(100, sd = .2);

# plot
library(ggplot2)
g = ggplot(data.frame(x = x, y = y), aes(x = x, y = y))
g = g + geom_smooth(method = "lm", colour = "black")
g = g + geom_point(size = 7, colour = "black", alpha = 0.4)
g = g + geom_point(size = 5, colour = "red", alpha = 0.4)
g


# Residual plot

g = ggplot(data.frame(x = x, y = resid(lm(y ~ x))),
           aes(x = x, y = y))
g = g + geom_hline(yintercept = 0, size = 2);
g = g + geom_point(size = 7, colour = "black", alpha = 0.4)
g = g + geom_point(size = 5, colour = "red", alpha = 0.4)
g = g + xlab("X") + ylab("Residual")
g




# Heteroskedasticity ---------------

x <- runif(100, 0, 6); y <- x + rnorm(100,  mean = 0, sd = .001 * x);
g = ggplot(data.frame(x = x, y = y), aes(x = x, y = y))
g = g + geom_smooth(method = "lm", colour = "black")
g = g + geom_point(size = 7, colour = "black", alpha = 0.4)
g = g + geom_point(size = 5, colour = "red", alpha = 0.4)
g

# Getting rid of the blank space can be helpful -----------------------

g = ggplot(data.frame(x = x, y = resid(lm(y ~ x))),
           aes(x = x, y = y))
g = g + geom_hline(yintercept = 0, size = 2);
g = g + geom_point(size = 7, colour = "black", alpha = 0.4)
g = g + geom_point(size = 5, colour = "red", alpha = 0.4)
g = g + xlab("X") + ylab("Residual")
g


# Diamond data residual plot ---------------------------------------

# generation of the residuals
diamond$e <- resid(lm(price ~ carat, data = diamond))
# plot
g = ggplot(diamond, aes(x = carat, y = e))
g = g + xlab("Mass (carats)")
g = g + ylab("Residual price (SIN $)")
g = g + geom_hline(yintercept = 0, size = 2)
g = g + geom_point(size = 7, colour = "black", alpha=0.5)
g = g + geom_point(size = 5, colour = "blue", alpha=0.2)
g

# Diamond data residual plot (new) ------------------------------------

e = c(resid(lm(price ~ 1, data = diamond)), # variation around the avg price
      resid(lm(price ~ carat, data = diamond))) # variation around the reg line

fit = factor(c(rep("Itc", nrow(diamond)),
               rep("Itc, slope", nrow(diamond))))
# plot
# g = ggplot(data.frame(e = e, fit = fit), aes(y = e, x = fit, fill = fit))
# g = g + geom_dotplot(binaxis = "y" , #size = 2,
#                      stackdir = "center", binwidth = 20)
# g = g + xlab("Fitting approach")
# g = g + ylab("Residual price")
# g

g = ggplot(data.frame(e = e, fit = fit),
           aes(y = e, x = fit, fill = fit)) +
        geom_violin() +
        geom_jitter(width = .5, alpha = .5) +
        xlab("Fitting approach")+ ylab("Residual price")
g






#' Estimating residual variation
#' -----------------------------

#' ### diamond data

data("diamond")
y <- diamond$price; x <- diamond$carat; n <- length(y)
fit <- lm(y ~ x)
summary(fit)$sigma

sqrt(sum(resid(fit)^2) / (n - 2))




#'
#' ### Anscombe data


require(stats); require(graphics); data(anscombe)
# see example

## Not run
## example(anscombe)


ff <- y ~ x
mods <- setNames(as.list(1:4), paste0("lm", 1:4))
for(i in 1:4) {
        ff[2:3] <- lapply(paste0(c("y","x"), i), as.name)
        ## or   ff[[2]] <- as.name(paste0("y", i))
        ##      ff[[3]] <- as.name(paste0("x", i))
        mods[[i]] <- lmi <- lm(ff, data = anscombe)
        #print(anova(lmi))
}

# mods is the list of all the models for the 4 data sets ==> same slope, same intercept, same R

mods[[1]]$coefficients

lapply(mods, function(x ) x$coefficients)



## Now, do what you should have done in the first place: PLOTS
op <- par(mfrow = c(2, 2), mar = 0.1+c(4,4,1,1), oma =  c(0, 0, 2, 0))
for(i in 1:4) {
        ff[2:3] <- lapply(paste0(c("y","x"), i), as.name)
        plot(ff, data = anscombe, col = "red", pch = 21, bg = "orange", cex = 1.2,
             xlim = c(3, 19), ylim = c(3, 13))
        abline(mods[[i]], col = "blue")
}

mtext("Anscombe's 4 Regression data sets", outer = TRUE, cex = 1.5)
par(op)


#' Inference in regression
#' ------------------------
#'
#' ### verifications correspondence theory/ R

library(UsingR); data(diamond)
y <- diamond$price; x <- diamond$carat; n <- length(y)
# coeffs
beta1 <- cor(y, x) * sd(y) / sd(x)
beta0 <- mean(y) - beta1 * mean(x)
#residuals
e <- y - beta0 - beta1 * x
# sd of residuals
sigma <- sqrt(sum(e^2) / (n-2))
# sum of squares
ssx <- sum((x - mean(x))^2)
#stderror Beta1
seBeta0 <- (1 / n + mean(x) ^ 2 / ssx) ^ .5 * sigma
#stderror Beta1
seBeta1 <- sigma / sqrt(ssx)
# t values
tBeta0 <- beta0 / seBeta0; tBeta1 <- beta1 / seBeta1

# p-values (2-sided)
pBeta0 <- 2 * pt(abs(tBeta0), df = n - 2, lower.tail = FALSE)
pBeta1 <- 2 * pt(abs(tBeta1), df = n - 2, lower.tail = FALSE)

# coeffs table
coefTable <- rbind(c(beta0, seBeta0, tBeta0, pBeta0), c(beta1, seBeta1, tBeta1, pBeta1))
colnames(coefTable) <- c("Estimate", "Std. Error", "t value", "P(>|t|)")
rownames(coefTable) <- c("(Intercept)", "x")
# printout
coefTable




# The same The easy way with lm
coefTable
fit <- lm(y ~ x);
summary(fit)$coefficients


summary(fit)


#' Getting a confidence interval for a coefficient

# table with coeffs + sd + t + p
sumCoef <- summary(fit)$coefficients
# conf intrv for the intercept
sumCoef[1,1] + c(-1, 1) * qt(.975, df = fit$df) * sumCoef[1, 2]
# conf intrv for the slope, modified for an increase of 0.1 carat
(sumCoef[2,1] + c(-1, 1) * qt(.975, df = fit$df) * sumCoef[2, 2]) / 10




#'
#' Plotting the prediction intervals
#' ----------
#'


library(ggplot2)
newx = data.frame(x = seq(min(x), max(x), length = 100))
p1 = data.frame(predict(fit, newdata= newx,interval = ("confidence")))
p2 = data.frame(predict(fit, newdata = newx,interval = ("prediction")))
p1$interval = "confidence"
p2$interval = "prediction"
p1$x = newx$x
p2$x = newx$x
dat = rbind(p1, p2)
names(dat)[1] = "y"

g <-  ggplot(dat, aes(x = x, y = y))
g  <-  g + geom_ribbon(aes(ymin = lwr, ymax = upr, fill = interval), alpha = 0.2)
g <- g + geom_line()
g <- g + geom_point(data = data.frame(x = x, y=y), aes(x = x, y = y), size = 4)
g



