#' ---
#' title: "Linear Models"
#' subtitle: "Project1.R"
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

# plot histograms
k <- nclass.FD(mtcars$mpg)

g <- ggplot(mtcars, aes(mpg, fill = factor(am))) +
        geom_histogram(bins = k) +
        scale_fill_discrete("am") +
        facet_grid(am ~.)
g


# plotting the effect of every variable on mpg, contrasting
# autimatic and manual transmissions

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
# plotam("wt")


# plot all variables except mpg, against am
lpl <- lapply(X = setdiff(colnames(mtcars), c("am", "mpg")),
              plotam)

# display plots
windows(width = 10, height = 6)
multiplot(plotlist = lpl, cols = 3)
dev.off




#' correlations
#' ------------

# cor(mtcars)

# heatmap(cor(mtcars),
#        col=colorRampPalette(c("blue","white", "red"))( 20 ))



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
s$sigma
s$r.squared
s$adj.r.squared


#' diagnostics
#' ------------

pr <- par("mfrow")
par(mfrow=c(2,2))
plot(fit)
par(mfrow=pr)


# residuals plots

vnames <- c("wt", "am", "qsec") # variables in the final model

# rstudent(fit) # try


# Store residuals
mtcars <- mutate(mtcars, resid1 = rstudent(fit))


resvarplot <- function(varname) {
        ggplot(mtcars, aes_(as.name(varname) , quote(resid1))) +
                geom_point()
}
# resvarplot("wt")
# resvarplot("am")
# resvarplot("qsec")

lpres <- lapply(vnames, FUN=resvarplot)
multiplot(plotlist = lpres,
          #ncols=3
          layout = matrix(c(1,2,3), nrow=1)
          )


#' what about a different model with hp instead of qsec
#' --------------------------------------------------------


fit <- lm(mpg ~ wt + am + hp, mtcars)
s <- summary(fit)
s
# All significants
s$sigma
s$r.squared
s$adj.r.squared

# with hp and interaction

fit <- lm(mpg ~  am + hp + wt*am, mtcars)
s <- summary(fit)
s
# All significants
s$sigma
s$r.squared
s$adj.r.squared

# diagnostics
windows()
pr <- par("mfrow")
par(mfrow=c(2,2))
plot(fit)

par(mfrow = pr)
dev.off()

# SAME WITHOUR MASERATI bORA

rownames(mtcars)
mtcars <- mutate(mtcars, modname = rownames(mtcars))

mtcars2 <- filter(mtcars, modname != "Maserati Bora")


fit <- lm(mpg ~  am + hp + wt*am, mtcars2)
s <- summary(fit)
s
# All significants
s$sigma
s$r.squared
s$adj.r.squared

# diagnostics
windows()
pr <- par("mfrow")
par(mfrow=c(2,2))
plot(fit)

par(mfrow=pr)
dev.off()




vnames2 <- c("wt", "am", "hp") # variables in the final model
# Store residuals2
mtcars <- mutate(mtcars, resid2 = rstudent(fit))


resvarplot2 <- function(varname) {
        ggplot(mtcars, aes_(as.name(varname) , quote(resid2))) +
                geom_point()
}


lpres2 <- lapply(vnames2, FUN=resvarplot2)
multiplot(plotlist = lpres, ncols=3)
windows(width = 10)
multiplot(plotlist = lpres2,
          layout=matrix(c(1,2,3), nrow=1, byrow=TRUE))
dev.off()


# qqnorm(mtcars$resid1)
# qqnorm(mtcars$resid2)

ggplot(mtcars) + geom_qq(aes(sample = resid1))
ggplot(mtcars) + geom_qq(aes(sample = resid2))

ggplot(mtcars, aes(sample = resid1)) + stat_qq()
ggplot(mtcars, aes(sample = resid2)) + stat_qq()



ggplot(mtcars, aes(x= resid1)) +
        geom_histogram(bins=nclass.FD(mtcars$resid1))


ggplot(mtcars, aes(x= resid2)) +
        geom_histogram(bins=nclass.FD(mtcars$resid2))




#' correlations
#' -------------


# select(mtcars, one_of(vnames))
# cor(mtcars[vnames], select(mtcars, -one_of(names)))



# redv <- mtcars[c("mpg", vnames)]

heatmap(cor(mtcars[c("mpg", vnames)]),
        col=colorRampPalette(c("blue","white", "red"))( 20 ))

cor(mtcars[vnames])

heatmap(cor(mtcars[vnames])
        , col=colorRampPalette(c("blue","white", "red"))( 50 )
        #, zlim=c(-1.05, 1.05),
        , symm = TRUE
        )


heatmap(cor(mtcars[vnames2])
        , col=colorRampPalette(c("blue","white", "red"))( 50 )
        #, zlim=c(-1.05, 1.05),
        , symm = TRUE
)


# Correlations between variables inside the models and
# variables left out
cor(select(mtcars, one_of(vnames)),
    select(mtcars, - one_of(c(vnames, "mpg", 'modname'))))
# corresponding heatmap (blue = negative, red = positive)
heatmap(cor(mtcars[vnames],
            select(mtcars, -one_of(c(vnames, "mpg", "modname")))),
        col=colorRampPalette(c("blue","white", "red"))( 100 )
        #, zlim=c(-1, 1)
        )


cor(mtcars[vnames2],
    select(mtcars, -one_of(c(vnames2, "mpg", "resid1", "resid2", "modname"))))
heatmap(cor(mtcars[vnames2],
            select(mtcars, -one_of(c(vnames2, "mpg", "resid1", "resid2", "modname")))),
        col=colorRampPalette(c("blue","white", "red"))( 100 )
        # , zlim=c(-.9, .9)
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


#' nested models C
#' ---------------


fitC1 <- lm(mpg ~ am, mtcars)
fitC2 <- lm(mpg ~ am + wt , mtcars)
fitC2B <- lm(mpg ~ am + wt*am , mtcars)

anova(fitC1,fitC2, fitC2B) # justified
fitC3B <- lm(mpg ~ am + wt*am + hp, mtcars)
anova(fitC2B, fitC3B) # justified
summary(fitC3B)


fitC4B <- lm(mpg ~ am + wt*am + hp + qsec , mtcars)
anova(fitC3B, fitC4B)
# ==> the addition of hp is not justified
summary(fitC4B)



#' conclusion:
#' -----------

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
