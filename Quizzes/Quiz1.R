# Quiz1
#
#
# Q1
# ====
x <- c(0.18, -1.54, 0.42, 0.95)
w <- c(2, 1, 3, 1)

m <- sum(w*x) / sum(w)
m

# Ans = 0.1471



#Q2
# ===

x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)

# Regression Trough the origin

lm(y ~ x - 1)

# Ans = 0.8263




# Q3
# ===

data(mtcars)

lm(mpg ~ wt, mtcars)

#  Ans = -5.344



# Q4 :
# ====

cor <- .5

cor* 1 / .5




# Q5 :
# ====

0.4 * 1.5

#  Ans = 0.6







# Q6
# ====
x <- c(8.58, 10.46, 9.01, 9.64, 8.86)

(x - mean(x))/ sd(x)

# Ans = -0.9718658




# Q7
# ===

x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)

lm( y ~ x)


# Ans = 1.567


# Q8.
# ====

# You know that both the predictor and response have mean 0. What
# can be said about the intercept when you fit a linear regression?
#
# ==> It must be identically 0.



# Q9.
# ====

x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)

mean(x)

# Ans= 0.573






