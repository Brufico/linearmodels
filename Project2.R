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


#+ options
knitr::opts_chunk$set(echo = FALSE)


#' libraries + external code
#' =========================

#+ libraries, warnings = FALSE

library(dplyr)
# require(graphics)
# library(GGally)
library(ggplot2)
library(grid)
library(gridExtra)

# source(file.path("morecode", "multiplot", "multiplot.R"))



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



#' Data exploration
#' ----------------


# freq polygons
gpoly <- ggplot(mtcars, aes(mpg, y = ..density..,
                            color = factor(am))) +
        geom_freqpoly(bins = 16) +
        scale_color_discrete("am") +
        scale_x_continuous(limits = c(5, 40)) +
        xlab(NULL)
# gpoly


# boxplots
gbox <- ggplot(mtcars, aes(factor(am), mpg, color=factor(am))) +
        geom_boxplot() +
        # labs(x= "am\ - ") +
        scale_x_discrete("am",labels = c("0.00", "1.00")) +
        scale_y_continuous(limits = c(5, 40)) +
        scale_color_discrete("am") +
        ylab(NULL)+
        coord_flip()

# show freq polygons and boxplot

# remark: with the bold title, this is very slow
# bold title
title1 <- textGrob("Automatic vs manual gearbox mpg distributions",
                gp=gpar(fontface="bold"))

gr <- arrangeGrob(grobs=list(gpoly, gbox),
                  layout_matrix = matrix(c(1,2), nrow = 2) ,
                  top = title1,
                  bottom = "mpg")
# stores in gr use then grid.graw(gr)
grid.draw(gr)


# plotting the effect of every variable on mpg, conmparing
# automatic and manual transmissions

# plotting function
plotam <- function(varname, withlegend = FALSE) {
        g <- ggplot(mtcars,
               aes_(x = as.name(varname),
                    y = quote(mpg),
                    color = quote(factor(am)))) +
                geom_jitter(width= .3, height = .2) +
                geom_smooth(data = filter(mtcars, am ==0),
                            method="lm", se= FALSE) +
                geom_smooth(data = filter(mtcars, am ==1),
                            method="lm", se= FALSE) +
                ylab(NULL) +
                scale_color_discrete(guide_legend("am")) +
                theme(legend.position="bottom")
        if (withlegend) {
                g
        } else {
                g + theme(legend.position = "none")
        }
}

# test
# plotam("wt")
# plotam("wt" , withlegend=TRUE)

# plot all variables except mpg, against am
lpl <- lapply(X = setdiff(colnames(mtcars), c("am", "mpg")),
              plotam)


# display plots (with a common legend):

# get the legend from any one plot (the plot must have a legend)

# extract legend
# https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
        tmp <- ggplot_gtable(ggplot_build(a.gplot))
        leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
        legend <- tmp$grobs[[leg]]
        return(legend)}

mylegend <- g_legend(plotam("wt", withlegend = TRUE))


# draw the plot
# windows(width = 10, height = 6)
title2 <- textGrob("Automatic vs manual:  mpg simple linear regressions",
                   gp = gpar(fontface="bold"))

gg <- arrangeGrob(grobs = lpl,
                  ncol = 3,
                  top = title2,
                  left = "mpg")

g3 <- arrangeGrob(gg,
                   mylegend, nrow=2, heights=c(10, 1))

grid.draw(g3)
# dev.off()







#' model selection
#' ===============

#' backwards elimination (without interaction)
#' -------------------------------------------

#' principle : start with a model with all variables, then
#' remove one by one the variable whith a coefficient not
#' significantly different from zero, starting with the
#' highest p-values.
#'
#' Here we get: mpg ~ am + wt + qsec
#'

# start

# first iteration
fit <- lm(mpg ~ carb + hp + disp + cyl + wt + gear + am + drat + vs + qsec, mtcars)

# testing function, not-really-so-good
nextstep <- function(fit) {
        sf <-  summary(fit)
        sfc <- summary(fit)$coef[-1,]
        rnames <- dimnames(sfc)[[1]]
        ix <- match(max(sfc[,4] ), sfc[,4] )
        # formula
        fitfla <- formula(fit)
        cfla <- as.character(fitfla)
        cfla <- paste(cfla[2], cfla[1], cfla[3])

        # print(ix)
        # result
        if (sfc[ix,4] > 0.05) {
                data.frame(nonsignif.var = rnames[ix],
                           p.value =  round(sfc[ix, 4], 3),
                           sigma = sf$sigma,
                           Rsquared =  sf$r.squared,
                           Adj.Rsquared = sf$adj.r.squared)
        } else {
                data.frame(nonsignif.var = "all significant",
                           formula = cfla,
                           sigma = sf$sigma,
                           Rsquared =  sf$r.squared,
                           Adj.Rsquared = sf$adj.r.squared)
        }
}




nextstep(fit)

# removing cyl
fit <- lm(mpg ~ carb + hp + disp + wt + gear + am + drat + vs + qsec, mtcars)
nextstep(fit)

# removing vs
fit <- lm(mpg ~ carb + hp + disp + wt + gear + am + drat + qsec, mtcars)
nextstep(fit)

# removing carb
fit <- lm(mpg ~ hp + disp + wt + gear + am + drat + qsec, mtcars)
nextstep(fit)

# removing gear
fit <- lm(mpg ~ hp + disp + wt + am + drat + qsec, mtcars)
nextstep(fit)

# removing drat
fit <- lm(mpg ~ hp + disp + wt + am + qsec, mtcars)
nextstep(fit)

# removing disp
fit <- lm(mpg ~ hp + wt + am + qsec, mtcars)
nextstep(fit)

# removing hp final step
fit <- lm(mpg ~ wt + am + qsec, mtcars)
nextstep(fit)
s <- summary(fit)
s
# All significants
#   nonsignif.var              formula    sigma  Rsquared Adj.Rsquared
# all significant mpg ~ wt + am + qsec 2.458846 0.8496636    0.8335561



#' mpg ~ wt + am + qsec diagnostics
#' --------------------------------

pr <- par("mfrow")
par(mfrow=c(2,2))
plot(fit)
par(mfrow=pr)
dev.off()

# residuals plots
vnames <- c("wt", "am", "qsec") # variables in the final model

# Store residuals
mtcars1 <- mutate(mtcars, resid1 = rstudent(fit))

resvarplot <- function(varname) {
        ggplot(mtcars1, aes_(as.name(varname) , quote(resid1))) +
                geom_point()+
                geom_smooth()
}

lpres <- lapply(vnames, FUN=resvarplot)


windows(width = 10)
gr <- arrangeGrob(grobs = lpres,
          #ncols=3
          layout_matrix = matrix(c(1,2,3), nrow=1)
)
grid.draw(gr)
dev.off()



#' Adding interaction am:wt
#' ------------------------

fit2 <- lm(mpg ~ wt + am*wt + qsec, mtcars)
s <- summary(fit2)
s

windows()
pr <- par("mfrow")
par(mfrow=c(2,2))
plot(fit)
par(mfrow=pr)

dev.off()



#' Try Adding interaction am:qsec ==> no
#' -------------------------------------

fit3 <- lm(mpg ~ wt + am*wt + qsec + am*qsec, mtcars)
s <- summary(fit3)
s

an <- anova(fit, fit2, fit3)
an


# Analysis of Variance Table
#
# Model 1: mpg ~ wt + am + qsec
# Model 2: mpg ~ wt + am * wt + qsec
# Model 3: mpg ~ wt + am * wt + qsec + am * qsec
# Res.Df    RSS Df Sum of Sq       F   Pr(>F)
# 1     28 169.29
# 2     27 117.28  1    52.010 11.6099 0.002145 **
# 3     26 116.47  1     0.802  0.1791 0.675630
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# p-values
pv <- an$`Pr(>F)`

#  NA 0.002144762 0.675630240

# get individual p-values
# pv[2]


#' Conclusion: select : mpg ~ wt + am * wt + qsec
#' ------------------------


# diagnostics2

pr <- par("mfrow")
par(mfrow=c(2,2))
plot(fit2)
par(mfrow=pr)



hatvalues(fit2)
dfb <- dfbetas(fit2)

str(dfb)

dfb[ , c("am", "wt:am")]


# ==> Try without Chrysler Imperial , Maserati Bora,

mtcars2 <- mtcars
mtcars2$model <- rownames(mtcars)
mtcars2 <- filter(mtcars2, model != "Chrysler Imperial", model != "Maserati Bora")
rownames(mtcars2) <- mtcars2$model

fit2B <- lm(mpg ~ wt + am*wt + qsec, mtcars2)
s <- summary(fit2B)
s




pr <- par("mfrow")
par(mfrow=c(2,2))
plot(fit2B)
par(mfrow=pr)



hatvalues(fit2B)
dfb <- dfbetas(fit2B)
dfb[ , c("am", "wt:am")]








# ======================================================================


#' correlations
#' ------------

# cor(mtcars)

# heatmap(cor(mtcars),
#        col=colorRampPalette(c("blue","white", "red"))( 20 ))




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


resvarplot <- function(varname, residvar = "resid1") {
        ggplot(mtcars, aes_(as.name(varname) , as.name(residvar))) +
                geom_point()+
                geom_smooth()
}

# resvarplot("wt")
# resvarplot("am")
# resvarplot("qsec")

lpres <- lapply(vnames, FUN=resvarplot)
windows(width=10)
multiplot(plotlist = lpres,
          #ncols=3
          layout = matrix(c(1,2,3), nrow=1)
          )
dev.off()




#' what about a different model with hp instead of qsec
#' -----------------------------------------------------


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
