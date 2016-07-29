#' ---
#' title: "Exploration de 'mtcars"
#' subtitle: "pour le module linear models"
#' author: "Bruno Fischer"
#' date: '`r format(Sys.Date(), "%B %d, %Y")`'
#' output:
#'         html_document:
#'                 theme: readable
#'         word_document:
#'              fig_caption: yes
#'              fig_width: 9
#'              toc: no
#' number_sections: yes
#' theme: readable
#' ---


# libraries
library(dplyr)
require(graphics)
library(GGally)
library(ggplot2)

# load C:\Users\Bruno\SkyDrive\Documents\R\linearmodels\morecode\multiplot

source(file.path("morecode", "multiplot", "multiplot.R"))



#' The data
#' ========

# Help file : data frame with 32 observations on 11 variables.

# [, 1]	 mpg	 Miles/(US) gallon
# [, 2]	 cyl	 Number of cylinders
# [, 3]	 disp	 Displacement (cu.in.)
# [, 4]	 hp	 Gross horsepower
# [, 5]	 drat	 Rear axle ratio
# [, 6]	 wt	 Weight (1000 lbs)
# [, 7]	 qsec	 1/4 mile time
# [, 8]	 vs	 V/S
# [, 9]	 am	 Transmission (0 = automatic, 1 = manual)
# [,10]	 gear	 Number of forward gears
# [,11]	 carb	 Number of carburetors


data("mtcars")
# head(mtcars)


#' Data exploration
#' ----------------

# pairs(mtcars, main = "mtcars data")
# coplot(mpg ~ disp | as.factor(am), data = mtcars,
#        panel = panel.smooth, rows = 1)
#
# coplot(mpg ~ wt | as.factor(am), data = mtcars,
#        panel = panel.smooth, rows = 1)
#
# coplot(mpg ~ hp | as.factor(am), data = mtcars,
#        panel = panel.smooth, rows = 1)
#
# coplot(mpg ~ drat | as.factor(am), data = mtcars,
#        panel = panel.smooth, rows = 1)
#


# help(package="GGally")
# windows()
# ggpairs(mtcars,
#         lower = list(continuous = "smooth"))
# dev.off()

# colnames(mtcars)

# plotting function
plotam <- function(varname) {
        ggplot(mtcars,
               aes_(x = as.name(varname),
                    y = quote(mpg),
                    color = quote(factor(am)))) +
                geom_jitter(width= .3, height = .2) +
                geom_smooth(data = filter(mtcars, am ==0),
                            method="lm", se= FALSE) +
                geom_smooth(data = filter(mtcars, am ==1),
                            method="lm", se= FALSE) +
                scale_color_discrete(guide_legend("am"))
}

# test

plotam("wt")


# plot all variables except mpg, against am
lpl <- lapply(X = setdiff(colnames(mtcars), c("am", "mpg")),
              plotam)


windows(width = 10, height = 6)
multiplot(plotlist = lpl, cols = 3)
dev.off




#' correlations
#' ------------

cor(mtcars)

heatmap(cor(mtcars),
        col=colorRampPalette(c("blue","white", "red"))( 20 ))



#' backwards elimination
#' ---------------------

# first iteration
fit <- lm(mpg ~ carb + hp + disp + cyl + wt + gear + am + drat + vs + qsec, mtcars)
sfc <- summary(fit)$coef
rnames <- dimnames(sfc)[[1]]
ix <- match(max(sfc[,4] ), sfc[,4] )

print("Least significant variable : ")
print(paste("variable =", rnames[ix], "; p-value =", sfc[ix, 4]))


# removing cyl

fit <- lm(mpg ~ carb + hp + disp + wt + gear + am + drat + vs + qsec, mtcars)
summary(fit)

# removing vs

fit <- lm(mpg ~ carb + hp + disp + wt + gear + am + drat + qsec, mtcars)
summary(fit)
# exit gear

fit <- lm(mpg ~ carb + hp + disp + wt + am + drat + qsec, mtcars)
summary(fit)
# exit carb

fit <- lm(mpg ~ hp + disp + wt + am + drat + qsec, mtcars)
summary(fit)
# exit drat

fit <- lm(mpg ~ hp + disp + wt + am + qsec, mtcars)
summary(fit)
# exit disp

fit <- lm(mpg ~ hp + wt + am + qsec, mtcars)
summary(fit)
summary(fit)$adj.r.squared
# exit hp


# final step
fit <- lm(mpg ~ wt + am + qsec, mtcars)
s <- summary(fit)
s
# All significants
s$adj.r.squared



# correlations
names <- c("wt", "am", "qsec") # variables in the final model

select(mtcars, one_of(names))
cor(mtcars[names], select(mtcars, -one_of(names)))



redv <- mtcars[c("mpg", names)]

heatmap(cor(mtcars[c("mpg", names)]),
        col=colorRampPalette(c("blue","white", "red"))( 20 ))

cor(mtcars[c(names)])
windows()
heatmap(cor(mtcars[c(names)])
        , col=colorRampPalette(c("blue","white", "red"))( 50 )
        #, zlim=c(-1.05, 1.05),
        , symm = TRUE
        )
dev.off()



# Correlations between variables inside the models and
# variables left out
cor(mtcars[names],
    select(mtcars, -one_of(c(names, "mpg"))))
# corresponding heatmap (blue = negative, red = positive)
heatmap(cor(mtcars[names],
            select(mtcars, -one_of(c(names, "mpg")))),
        col=colorRampPalette(c("blue","white", "red"))( 100 )
        #, zlim=c(-1, 1)
        )


# nested models

fit1 <- lm(mpg ~ am, mtcars)
fit2 <- lm(mpg ~ am + wt , mtcars)
fit3 <- lm(mpg ~ am + wt + qsec, mtcars)
anova(fit1,fit2,fit3)

fit4 <- lm(mpg ~ am + wt + qsec + hp, mtcars)
anova(fit3, fit4)
# ==> the addition of hp is not justified

fit5 <- lm(mpg ~ am + wt + qsec + disp, mtcars)
anova(fit3, fit5)
# ==> the addition of disp is not justified

fit3B <- lm(mpg ~ am + wt + qsec + wt*am, mtcars)
anova(fit3, fit3B)

summary(fit3)
summary(fit3B)



# nested models B

fitB1 <- lm(mpg ~ am, mtcars)
fitB2 <- lm(mpg ~ am + wt , mtcars)
fitB2B <- lm(mpg ~ am + wt*am , mtcars)

anova(fitB1,fitB2, fitB2B) # justified
fitB3B <- lm(mpg ~ am + wt*am + qsec, mtcars)
anova(fitB2B, fitB3B) # justified
summary(fitB3B)


fitB4 <- lm(mpg ~ am + wt + qsec + hp, mtcars)
anova(fit3, fit4)
# ==> the addition of hp is not justified


fit5 <- lm(mpg ~ am + wt + qsec + disp, mtcars)
anova(fit3, fit5)
# ==> the addition of disp is not justified

fit3B <- lm(mpg ~ am + wt + qsec + wt*am, mtcars)
anova(fit3, fit3B)

summary(fit3)
summary(fit3B)


# conclusion:
# 2 equations, automatic vs manual
# automatic: mpg = 9.723 - 2.937 wt + 1.017 qs
# manual : mpg = (9.723 + 14.079) - (2.937 + 7.078) wt + 1.017 qs

# ==> for small wt, manual is better, for hign wt auto is better.
# balance point, w = 3.4 (thousands of lb)
# The manufactureres seem aware of it, as the overlap zone goes from circa

mt24 <- filter(mtcars, wt > 2.5 & wt < 4.5)
mt24a <- filter(mt24, am==0)
mt24m <- filter(mt24, am==1)
# ggplot(mt24, aes(wt, am))+ geom_point()
min(mt24a$wt) # 3.15
max(mt24m$wt) # 3.57
