#'---
#'title: "Linear models"
#'subtitle: "Code: Adjustment ; Lecture 2_4"
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
#'


#'
#' First Data
#' ==========

# data generation

#parameters
beta0 <- 0
beta1 <- 2
tau <- 1
sigma <- .2

set.seed(5)
n <- 100
# predictors x and t. 2 groups: t= 0 and t = 1
t <- rep(c(0,1), c(n/2,n/2)) # 0,0,...0, 1, 1, ..., 1
x <- c(runif(n/2), runif(n/2)) # uniform
# response
y <- beta0 +  x* beta1 + t * tau + rnorm(n, sd = sigma)

# graphing function
graphxy <- function(x, y) { # using base graphics
        plot(x, y, type = "n", frame = FALSE)
        abline(lm(y ~ x), lwd = 2)
        abline(h = mean(y[1 : (n/2)]), lwd = 3,
               col = "lightblue")
        abline(h = mean(y[(n/2 + 1) : n]), lwd = 3,
               col = "salmon")
        fit <- lm(y ~  x+ t)
        abline(coef(fit)[1], coef(fit)[2],
               lwd = 3, col="lightblue")
        abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2],
               lwd = 3, col="salmon")
        points(x[1 : (n/2)], y[1 : (n/2)],
               pch = 21, col = "black", bg = "lightblue", cex= 2)
        points(x[(n/2 + 1):n], y[(n/2 + 1):n],
               pch = 21, col = "black", bg = "salmon", cex= 2)
}

# Graph the data
graphxy(x, y)


#'
#' Second simulation
#' =================

# data generation
set.seed(5)
n <- 100
# predictors x and t. 2 groups: t= 0 and t = 1
t <- rep(c(0,1), c(n/2,n/2)) # 0,0,...0, 1, 1, ..., 1
x <- c(runif(n/2), runif(n/2) + 1.5) # uniform
# response
y <- beta0 +  x* beta1 + t * tau + rnorm(n, sd = sigma)
y[1:n/2] <- y[1:n/2] + 1.5
# Graph the data
graphxy(x, y)



#'
#' Third simulation
#' =================

# data generation
set.seed(5)
n <- 100
# predictors x and t. 2 groups: t= 0 and t = 1
t <- rep(c(0,1), c(n/2,n/2)) # 0,0,...0, 1, 1, ..., 1
x <- c(runif(n/2), runif(n/2) + .5) # uniform
# response
y <- beta0 +  x* beta1 + t * tau + rnorm(n, sd = sigma)
y[1:n/2] <- y[1:n/2] + 2
# Graph the data
graphxy(x, y)


#'
#' Fourth simulation
#' =================


# data generation
set.seed(5)
n <- 100
# predictors x and t. 2 groups: t= 0 and t = 1
t <- rep(c(0,1), c(n/2,n/2)) # 0,0,...0, 1, 1, ..., 1
x <- c(runif(n/2), runif(n/2) + .5) # uniform
# response
y <- beta0 +  x* beta1 - t * tau + rnorm(n, sd = sigma)
y[1:n/2] <- y[1:n/2] + 2
# Graph the data
graphxy(x, y)



#' 5th simulation
#' ==============

# graphing function (2 slopes)

graphxy2 <- function(x, y) { # using base graphics
        plot(x, y, type = "n", frame = FALSE)
        abline(lm(y ~ x), lwd = 2)
        abline(h = mean(y[1 : (n/2)]), lwd = 3,
               col = "lightblue")
        abline(h = mean(y[(n/2 + 1) : n]), lwd = 3,
               col = "salmon")
        fit <- lm(y ~  x*t)
        abline(coef(fit)[1], coef(fit)[2],
               lwd = 3, col="lightblue")
        abline(coef(fit)[1] + coef(fit)[3],
               coef(fit)[2] + coef(fit)[4],
               lwd = 3, col="salmon")
        points(x[1 : (n/2)], y[1 : (n/2)],
               pch = 21, col = "black", bg = "lightblue", cex= 2)
        points(x[(n/2 + 1):n], y[(n/2 + 1):n],
               pch = 21, col = "black", bg = "salmon", cex= 2)
}



# data generation
set.seed(5)
n <- 100
# predictors x and t. 2 groups: t= 0 and t = 1
t <- rep(c(0,1), c(n/2,n/2)) # 0,0,...0, 1, 1, ..., 1
x <- c(runif(n/2), runif(n/2)) # uniform (2*in the same range)

# response
y <- beta0 + 3 * (.8 - t)* x* beta1 + 4* t * tau + rnorm(n, sd = sigma)


# Graph the data
graphxy2(x, y)

