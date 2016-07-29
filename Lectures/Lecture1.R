#'
#'
#' Regression models Lecture 1
#' ===========================


#' Regression to the mean
#' ----------------------

# Data
library(UsingR)
library(reshape2)
library(dplyr)


data(galton)

head(galton)

long <- melt(galton)

head(long)

g <- ggplot(long, aes(x = value, fill = variable)) +
        geom_histogram(colour = "black", binwidth= 1) +
        facet_grid( variable ~ . )
g




# middle = least square estimation ==> mean mu

library(manipulate)
myHist <- function(mu){
        mse <- mean((galton$child - mu)^2)
        g <- ggplot(galton, aes(x = child)) +
                geom_histogram(fill = "salmon", colour = "black", binwidth =1) +
                geom_vline(xintercept = mu, size = 3) +
                labs(title=paste("mu = ", mu, ", MSE = ", round(mse, 2), sep = ""))
        g
}

manipulate(myHist(mu), mu = slider(62, 74, step = 0.5))




g <- ggplot(galton, aes(x = child)) +
        geom_histogram(fill = "salmon", colour = "black", binwidth=1) +
        geom_vline(xintercept = mean(galton$child), size = 3)
g



# Scatterplot + regression
#
#
str(galton)

mean(galton$child)
mean(galton$child)

y <- galton$child - mean(galton$child)
x <- galton$parent - mean(galton$parent)
freqData <- as.data.frame(table(x, y), stringsAsFactors = FALSE)

names(freqData) <- c("child", "parent", "freq")

str(freqData)

freqData$child <- as.numeric(as.character(freqData$child))
freqData$parent <- as.numeric(as.character(freqData$parent))


#scatterplot first


g <- ggplot(filter(freqData, freq > 0), aes(x = parent, y = child)) +
        scale_size(range = c(2, 20), guide = "none" ) +
        geom_point(colour="grey50", aes(size = freq + 20, show_guide= FALSE)) +
        geom_point(aes(colour=freq, size = freq)) +
        scale_colour_gradient(low = "lightblue", high="white")

g




# with manipulate reg line
#
myPlot <- function(beta){
        g <- ggplot(filter(freqData, freq > 0), aes(x = parent, y = child))
        g <- g  + scale_size(range = c(2, 20), guide = "none" )
        g <- g + geom_point(colour="grey50", aes(size = freq+20, show_guide= FALSE))
        g <- g + geom_point(aes(colour=freq, size = freq))
        g <- g + scale_colour_gradient(low = "lightblue", high="white")
        g <- g + geom_abline(intercept = 0, slope = beta, size = 3)
        mse <- mean( (y - beta * x)^2 )
        g <- g + ggtitle(paste("beta = ", beta, "mse = ", round(mse, 3)))
        g
}

manipulate(myPlot(beta), beta = slider(0.5, 1.2, step = 0.02))



# using lm

lm(I(child - mean(child)) ~ I(parent - mean(parent)) - 1, data = galton)




# or
y <- galton$child - mean(galton$child)
x <- galton$parent - mean(galton$parent)
newgal <- data.frame(x,y)
head(newgal)
str(newgal)

lm(y ~ x - 1, data=newgal)



# Double check our calculations using R
#
y <- galton$child
x <- galton$parent
beta1 <- cor(y, x) *  sd(y) / sd(x)
beta0 <- mean(y) - beta1 * mean(x)

rbind(c(beta0, beta1), coef(lm(y ~ x)))


# Fathers-Sons
#
library(UsingR)
data(father.son)
head(father.son)

y <- (father.son$sheight - mean(father.son$sheight)) / sd(father.son$sheight)
x <- (father.son$fheight - mean(father.son$fheight)) / sd(father.son$fheight)
rho <- cor(x, y)

library(ggplot2)

g = ggplot(data.frame(x = x, y = y), aes(x = x, y = y))
g = g + geom_point(size = 6, colour = "black", alpha = 0.2)
g = g + geom_point(size = 4, colour = "salmon", alpha = 0.2)
g = g + xlim(-4, 4) + ylim(-4, 4)
g = g + geom_abline(intercept = 0, slope = 1)
g = g + geom_vline(xintercept = 0)
g = g + geom_hline(yintercept = 0)
g = g + geom_abline(intercept = 0, slope = rho, size = 2) #father -> sons
g = g + geom_abline(intercept = 0, slope = 1 / rho, size = 2) #father <- sons
g
# =======
g = ggplot(data.frame(x, y), aes(x = x, y = y))
g = g + geom_point(size = 5, alpha = .2, colour = "black")
g = g + geom_point(size = 4, alpha = .2, colour = "red")
g = g + geom_vline(xintercept = 0)
g = g + geom_hline(yintercept = 0)
g = g + geom_abline(position = "identity")
g = g + geom_abline(intercept = 0, slope = rho, size = 2)
g = g + geom_abline(intercept = 0, slope = 1 / rho, size = 2)
g = g + xlab("Father's height, normalized")
g = g + ylab("Son's height, normalized")
g
