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


#' Global options
#' --------------
vsfigheight <-  2.5
vsfigwidth <-  2.5
sfigheight <-  3
sfigwidth <-  3
nfigheight <-  4
nfigwidth <-  4
lfigheight <-  5
lfigwidth <-  7
vlfigheight <-  7
vlfigwidth <-  10

defdevice <- NULL # NULL (print on current device), or 'win' (but anything goes)
defsleep <- 10 # default waiting time in seconds

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

#' compute means for  am = 0 / 1
means <- mtcars %>%
        group_by(am) %>%
        summarise(meanmpg=mean(mpg))

meanauto <- means[1,"meanmpg"]
meanmanual <- means[2,"meanmpg"]

#' add to data mtcars factor 'gerbox' = factor(am) ,
#' but explicitly (auto / manual)

# mtcars <- mutate(mtcars, gearbox = factor(ifelse(am == 0, "auto", "manual")))
mtcars <- mutate(mtcars,
                 gearbox =
                         factor(mtcars$am,
                                labels = c("auto", "manual")))
# verification
# levels(mtcars$gearbox)



#' Compare distributions

# violin + box + points (jitter)
gviolin <- ggplot(mtcars, aes(gearbox, mpg, color=gearbox)) +
        geom_violin(mapping=aes(fill=gearbox),
                    width=1.2, show.legend = FALSE, alpha = .2) +
        geom_boxplot(width=.3) +
        geom_jitter(width=.3, height=.3,
                    alpha =.5, size = 2, color="black") +
        scale_y_continuous(limits = c(5, 40)) +
        guides(col = guide_legend(reverse = TRUE))+
        labs(title = "Automatic vs manual gearbox : mpg distributions") +
        coord_flip()

# display

displaygraph <- function(one.graph,
                         windevice = defdevice,
                         width = nfigwidth,
                         height = nfigheight ) {
        if (is.null(windevice)) {
                grid.newpage()
                grid.draw(one.graph)
        } else {
                windows(width = width, height = height)
                grid.newpage()
                grid.draw(one.graph)
                Sys.sleep(defsleep)
                dev.off()
        }
}

# display
displaygraph(gviolin)
# displaygraph(gviolin, "win",
#              width = lfigwidth,
#              height = vsfigheight)


#' Plotting the effect of every variable on mpg, conmparing
#' automatic and manual transmissions

# plotting function
plotam <- function(varname, withlegend = FALSE) {
        g <- ggplot(mtcars,
               aes_(x = as.name(varname),
                    y = quote(mpg),
                    color = quote(gearbox))) +
                geom_jitter(width= .3, height = .2) +
                geom_smooth(data = filter(mtcars, am ==0),
                            method="lm", se= FALSE) +
                geom_smooth(data = filter(mtcars, am ==1),
                            method="lm", se= FALSE) +
                ylab(NULL) +
                # scale_color_discrete(guide_legend("am")) +
                theme(legend.position = "right")
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
lpl <- lapply(X = setdiff(colnames(mtcars), c("am", "gearbox", "mpg")),
              plotam)


# put pplots together with a common legend):

# get the legend from any one plot (the plot must have a legend)
# extract legend
# https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend <- function(a.gplot){
        tmp <- ggplot_gtable(ggplot_build(a.gplot))
        leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
        legend <- tmp$grobs[[leg]]
        return(legend)}

mylegend <- g_legend(plotam("wt", withlegend = TRUE))


# draw the plot
# windows(width = 10, height = 6)
title2 <- textGrob("Automatic vs manual gearbox:  mpg simple linear regressions",
                   gp = gpar(fontface="bold"))

gg <- arrangeGrob(grobs = lpl,
                  ncol = 3,
                  top = title2,
                  left = "mpg")

slrg <- arrangeGrob(gg,
                   mylegend, nrow=1, widths=c(10, 1))
# to display

displaygraph(slrg, "win",
             width = vlfigwidth,
             height = lfigheight)




#' model selection
#' ===============

#' backwards elimination (without interaction)
#' -------------------------------------------
# start


# testing function, not-really-so-great
nextstep <- function(fit) {
        sf <-  summary(fit)
        sfc <- summary(fit)$coef[-1,]
        rnames <- dimnames(sfc)[[1]]
        ix <- match(max(sfc[,4] ), sfc[,4] )
        # formula
        fitfla <- formula(fit)
        cfla <- as.character(fitfla)
        cfla <- paste(cfla[2], cfla[1], cfla[3])
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


# init ialize the removel process and create functions
initrem <- function() {
        i <- 1
        elim <- character(0)
        step = NULL
        list(remvar = function() {
                step <<- nextstep(fit)
                elim[i] <<- paste0(step$nonsignif.var,
                                  " (p.value:",
                                  round(step$p.value, 2) , ")" )
                i <<- i + 1
                step },
             whatstep = function() {step},
             stephist = function() {elim}
        )
}

temp <- initrem()
remvar <- temp[["remvar"]]
whatstep <- temp[["whatstep"]]
stephist <- temp[["stephist"]]

# remove var function
# remvar <- function() {
#         step <<- nextstep(fit)
#         elim[i] <<- paste(step$nonsignif.var, "(p.value =", round(step$p.value, 2) , ")" )
#         i <<- i + 1
# }




# first iteration
fit <- lm(mpg ~ carb + hp + disp + cyl + wt + gear + am + drat + vs + qsec, mtcars)
# show essentials
summary(fit)
remvar( )
stephist()


# 2 removing cyl
fit <- lm(mpg ~ carb + hp + disp + wt + gear + am + drat + vs + qsec, mtcars)
# show essentials
summary(fit)
remvar( )
stephist()


# removing vs
fit <- lm(mpg ~ carb + hp + disp + wt + gear + am + drat + qsec, mtcars)
# show essentials
summary(fit)
remvar( )
stephist()


# removing carb
fit <- lm(mpg ~ hp + disp + wt + gear + am + drat + qsec, mtcars)
# show essentials
summary(fit)
remvar( )
stephist()


# removing gear
fit <- lm(mpg ~ hp + disp + wt + am + drat + qsec, mtcars)
# show essentials
summary(fit)
remvar( )
stephist()


# removing drat
# show essentials
summary(fit)
remvar( )
stephist()


# removing disp
fit <- lm(mpg ~ hp + wt + am + qsec, mtcars)
# show essentials
summary(fit)
remvar( )
stephist()

# removing hp final step
fit <- lm(mpg ~ wt + am + qsec, mtcars)
# show essentials
summary(fit)
try(remvar( ))
sh <- stephist() # store removal history

# test list of removals (char)
remlist <- paste(sh, collapse = ", ")


# All significants
#   nonsignif.var              formula    sigma  Rsquared Adj.Rsquared
# all significant mpg ~ wt + am + qsec 2.458846 0.8496636    0.8335561




#' mpg ~ wt + am + qsec diagnostics
#' --------------------------------

# plot(fit, which = 1)


# residuals plots
# vnames <- c("wt", "am", "qsec") # variables in the final model

# Store residuals and fitted values
mtcars1 <- mutate(mtcars, resid1 = rstudent(fit),
                  fitted.values = fitted.values(fit))


# residuals plots
# ---------------
# variables in the final model
vnames <- c("fitted.values", "wt", "am", "qsec")


resvarplot <- function(varname) {
        ggplot(mtcars1, aes_(as.name(varname) , quote(resid1))) +
                geom_point() +
                geom_smooth() +
                scale_y_continuous(name = NULL)
}

# test
# resvarplot(varname = "wt")

lpres1 <- lapply(vnames, FUN=resvarplot)


gr1 <- arrangeGrob(grobs = lpres1,
          #ncols=3
          layout_matrix = matrix(c(1,2,3,4), nrow=1),
          left = "residuals",
          top = "model: mpg ~ wt + am + qsec (without interaction)"
          )

# to display
displaygraph(gr1, "win", width = vlfigwidth,
             height = sfigheight)




#' Adding interaction am:wt
#' ------------------------

fit2 <- lm(mpg ~ wt + am*wt + qsec, mtcars)
s <- summary(fit2)
# s


# pr <- par("mfrow")
# par(mfrow=c(2,2))
# plot(fit2, which = 4)
# par(mfrow=pr)



# Store residuals and fitted values
mtcars2 <- mutate(mtcars, resid1 = rstudent(fit2),
                  fitted.values = fitted.values(fit2))



# residuals plots
# ---------------
# variables in the final model
vnames <- c("fitted.values", "wt", "am", "qsec")



resvarplot2 <- function(varname) {
        ggplot(mtcars2, aes_(as.name(varname) , quote(resid1))) +
                geom_point() +
                geom_smooth() +
                scale_y_continuous(name = NULL)
}

lpres2 <- lapply(vnames, FUN=resvarplot2)


gr2 <- arrangeGrob(grobs = lpres2,
                  #ncols=3
                  layout_matrix = matrix(c(1,2,3,4), nrow=1),
                  left = "residuals",
                  top = "model: mpg ~ wt*am + qsec (with interaction)"
)

# to display
displaygraph(gr2, "win", width = vlfigwidth,
             height = sfigheight)





#' Try Adding interaction am:qsec + anova test ==> no
#' --------------------------------------------------

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
anpv <- an$`Pr(>F)`

#  NA 0.002144762 0.675630240

# get individual p-values
# pv[2]





#' We select : mpg ~ wt + am * wt + qsec
#' -------------------------------------




#' diagnostics2: influential outliers
#' ----------------------------------

data(mtcars)
mtcars2 <- mtcars
mtcars2$model <- rownames(mtcars)
rownames(mtcars2) <- mtcars2$model


fit2A <- lm(mpg ~ wt + am*wt + qsec, mtcars2)
s <- summary(fit2A)
# s

windows(width = vlfigwidth, height = sfigheight)
pr <- c(1,1)
pr <- par("mfrow")
par(mfrow = c(1,3))
plot(fit2A, which = 4) # , ylim = c(0, .25) #???
plot(fit2A, which = 5)
plot(fit2A, which = 6)
par(mfrow = pr)

dev.off()



# hatvalues and dfbetas
hatvalues(fit2A)
dfb <- dfbetas(fit2A)


dfb[ , c("wt"  , "am", "wt:am")]

dfb[c("Fiat 128", "Maserati Bora" , "Chrysler Imperial" ) , c("wt"  , "am", "wt:am")]



# ==> Try without Chrysler Imperial , Maserati Bora

# data(mtcars)
# mtcars2 <- mtcars
# mtcars2$model <- rownames(mtcars)
#

mtcars2B <- filter(mtcars2, model != "Chrysler Imperial", model != "Maserati Bora")
rownames(mtcars2B) <- mtcars2B$model

fit2B <- lm(mpg ~ am*wt + qsec, mtcars2B)
s <- summary(fit2B)
s


windows(width = vlfigwidth, height = sfigheight)

pr <- par("mfrow")
par(mfrow=c(1,3))
plot(fit2B, which = 4)
plot(fit2B, which = 5)
plot(fit2B, which = 6)
par(mfrow=pr)

dev.off()

# fixing ylim
# plotlm <- stats:::plot.lm
#
# fix("plotlm")
#  ===> to be done ??


hatvalues(fit2B)
dfb <- dfbetas(fit2B)
dfb[ , c("am", "wt:am")]



#'
#' conclusion:
#' -----------

s <- summary(fit2B)
sfc <- s$coefficients
sfc

# Point estimation
# neutral weight

nwt <- - (sfc[2,1] / sfc[5,1])
deltaslope <- sfc[5,1]


# 2 equations, automatic vs manual
# automatic: mpg = 9.723 - 2.937 wt + 1.017 qs
# manual : mpg = (9.723 + 14.079) - (2.937 + 7.078) wt + 1.017 qs

# ==> for small wt, manual is better, for high wt auto is better.
# balance point, w = 3.4 (thousands of lb)
# The manufactureres seem aware of it, as the overlap zone goes from circa

mt24 <- filter(mtcars, wt > 2.5 & wt < 4.5)
mt24a <- filter(mt24, am==0)
mt24m <- filter(mt24, am==1)
# ggplot(mt24, aes(wt, am))+ geom_point()
min(mt24a$wt) # 3.15
max(mt24m$wt) # 3.57




#'
#' Bootstrapping the difference of coefficients
#' ------------------------------------------------
#'

data(mtcars2B)

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
        sdata <- mtcars2B[x,]
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
















# ===================================================================================================
# ===================================================================================================


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

