#' ---
#' title: "Linear Models"
#' subtitle: "bootstrapping the difference of 2 regression coeffs"
#' author: "Bruno Fischer Colonimos"
#' date: '`r format(Sys.Date(), "%d %B %Y")`'
#' output:
#'         html_document:
#'                 theme: readable
#'                 number_sections: yes
#'         word_document:
#'              fig_caption: yes
#'              fig_width: 9
#'              toc: no
#'              number_sections: yes
#'              theme: readable
#' ---




#' (@) Original bootstrapping example from the little inference book
#' =================================================================
#'


library(UsingR)
data(father.son)
x <- father.son$sheight
n <- length(x)
B <- 10000
set.seed(1254)
resamples <- matrix(sample(x,
                           n * B,
                           replace = TRUE),
                    B, n)
resampledMedians <- apply(resamples, 1, median)




quantile(resampledMedians, c(.025, .975))

g = ggplot(data.frame(medians = resampledMedians), aes(x = medians)) +
        geom_histogram(color = "black", fill = "lightblue", binwidth = 0.05)
g


g = ggplot(data.frame(medians = resampledMedians), aes(x = medians)) +
        geom_density(color = "black", fill = "lightblue", bw=.02)
g




#'
#' (@) Bootstrapping the difference of coefficients
#' ====================================================
#'

data(mtcars)

# first: estimate the model from lm
fit <- lm(mpg ~  qsec + wt*am, mtcars)
sc <- summary(fit)$coefficients

# conf int for coeff(wt:am)
sc[5,1] + c(-1,1) * qt(df = nrow(mtcars) - 2, p = .975) * sc[5,2]



n <- nrow(mtcars)
B <- 10000

# resampling
set.seed(1254)


# do not storethe samples
#
# generate samples
x <- sample(1:n, n, replace = TRUE)

lsamp <- lapply(1:B, function(i) sample(1:n, n, replace = TRUE))

getcoeffs <- function(x){
        # sample data
        sdata <- mtcars[x,]
        # compute model
        fit <- lm(mpg ~  hp + wt*am, sdata)
        sc <- summary(fit)$coef

        bam <- sc[4,1]
        bamwt <- sc[5,1]
        wt0 <- (- bam / bamwt)
        c(wt0, bamwt)
}


lsamp <- sapply(lsamp, getcoeffs)
lsamp <- t(lsamp)
lsamp <- as.data.frame(lsamp, stringsAsFactors = FALSE)
colnames(lsamp) <- c("neutral", "diff_slope")

# conclusions about "neutral"
quantile(lsamp$neutral, c(.025, .975))
mean(lsamp[[1]]) # surprising
median(lsamp$neutral)
head(lsamp)
min(lsamp$neutral)
max(lsamp$neutral)

g = ggplot(lsamp, aes(x = neutral)) +
        geom_density(color = "black", fill = "lightblue") +
        scale_x_continuous(limits = c(1,5))
g


# About "diff_slope"

quantile(lsamp$diff_slope, c(.025, .975))
mean(lsamp$diff_slope) # surprising
median(lsamp$diff_slope)
