#' ---
#' title: "Linear Models"
#' subtitle: "Project5.R"
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
library(ggplot2)
library(grid)
library(gridExtra)
library(boot)

# source(file.path("morecode", "multiplot", "multiplot.R"))


#' Global options and defaults
#' ---------------------------
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

bootstriter <-  10000 # bootstrap iterations


#' More external code
#' ------------------
# modified plotting function for plotting linear models diag plots
source("plotlm.R")


#' Helper functions
#' ------------------
#' vlookup (as in Excel). each column may be described by its rank or its name
#'
vlookup <- function(value, searchtable, searchcol = 1, returncol= 2){
        searchtable[match(value, searchtable[[searchcol]]), returncol]
}


#' reordering factors
#' -------------------------------------------------
# from 'Standard Functions'
orderfact <- function(dataf, nomfact, orderfreq = TRUE, orderdesc = TRUE,
                      ordervar = "c..nt", orderval = NA, orderfun = sum,
                      nlevels = NULL) {
        if (is.null(nlevels)) {
                direction <- ifelse(orderdesc,-1, 1)

                if (orderfreq & ordervar == "c..nt") {
                        dataf$c..nt <- c(1)
                }
                if (is.na(orderval) & ordervar == "c..nt") {
                        dataf$c..nt <- c(1)
                } else if (is.na(orderval) & ordervar != "c..nt") {
                        # dataf$c..nt <- ifelse(is.na(dataf[, ordervar]), 0, 1)
                        #
                        # ordervar <- "c..nt"
                        # ne rien faire ??
                } else {
                        dataf$c..nt <-
                                ifelse(is.na(dataf[[ordervar]]),
                                       0 ,
                                       ifelse(dataf[[ordervar]] == orderval,
                                              1, 0))
                        ordervar <- "c..nt"
                }
                # reordonner le facteur
                if (orderfreq) {
                        xx <- dataf[[nomfact]]
                        xxx <- direction * dataf[[ordervar]]
                        resfact <- reorder(xx, xxx, orderfun, na.rm = TRUE)
                } else {
                        resfact <- dataf[[nomfact]]
                }
        } else {
                resfact <- factor(dataf[[nomfact]], levels = nlevels) ### modfié ????????????
        }
        # retour
        resfact
}








#' The data
#' ========

# From help file : data frame with 32 observations on 11 variables.

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

# comment
# :(http://stackoverflow.com/questions/18617174/r-mtcars-dataset-meaning-of-vs-variable:
# car has a V engine or a Straight engine)

# get the data
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
# preserve rownames
rownames0 <- rownames(mtcars)
# mtcars <- mutate(mtcars, gearbox = factor(ifelse(am == 0, "auto", "manual")))
mtcars <- mutate(mtcars,
                 gearbox =
                         factor(mtcars$am,
                                labels = c("auto", "manual")))
# verification
# levels(mtcars$gearbox)
# restore rwnames
rownames(mtcars) <- rownames0

#' Compare distributions
# violin + box + points (jitter)
gviolin <- ggplot(mtcars, aes(gearbox, mpg, color=gearbox)) +
        geom_violin(mapping=aes(fill=gearbox),
                    width=1.2, show.legend = FALSE, alpha = .2) +
        geom_boxplot(width=.3) +
        geom_jitter(width=.3, height=.0,
                    alpha =.5, size = 2, color="black") +
        scale_y_continuous(limits = c(5, 40)) +
        guides(col = guide_legend(reverse = TRUE))+
        labs(title = "Automatic vs manual gearbox : mpg distributions") +
        coord_flip()

# display graphs, works with ggplot2 and gtable graphs. sends graphs to default
# or windows device (set the default in defdevice)
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
# displaygraph(gviolin)




#' Plotting the effect of every variable on mpg, conmparing automatic and manual transmissions
#' -------------------------------------------------------------------------------------------

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
# displaygraph(slrg)



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


# initialize the removel process and create functions
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
rownames(mtcars1) <- rownames0

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

# title
title <- textGrob("Model: mpg ~ wt + am + qsec (without interaction)",
                   gp = gpar(fontface="bold"))

gr1 <- arrangeGrob(grobs = lpres1,
          #ncols=3
          layout_matrix = matrix(c(1,2,3,4), nrow=1),
          left = "residuals",
          top = title
          )

# to display

displaygraph(gr1)
# displaygraph(gr1, "win", width = vlfigwidth,
#              height = sfigheight)




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
rownames(mtcars2) <- rownames0


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

title <- textGrob("Model: mpg ~ wt*am + qsec (with interaction)",
                  gp = gpar(fontface="bold"))

gr2 <- arrangeGrob(grobs = lpres2,
                  #ncols=3
                  layout_matrix = matrix(c(1,2,3,4), nrow=1),
                  left = "residuals",
                  top = title
)

# to display
displaygraph(gr2)
# displaygraph(gr2, "win", width = vlfigwidth,
#              height = sfigheight)





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
# anpv[2]
# anpv[3]



#' We thus select : mpg ~ wt + am * wt + qsec
#' -------------------------------------------



#' diagnostics2: checking influential points and outliers
#' ------------------------------------------------------

# get 'mtcars' again, in order to reestablish row names
# data(mtcars)
# mtcars2 <- mtcars
mtcars2$model <- rownames(mtcars) #reestablish row names
# rownames(mtcars2) <- mtcars2$model


# stepA
# -----

# fit the model
fit2A <- lm(mpg ~ wt + am*wt + qsec, mtcars2)
s <- summary(fit2A)
sc <- data.frame(s$coefficients)
sc$coeff <- rownames(sc)


# Plotting
# pr <- par("mfrow")
# par(mfrow = c(1,3))
# plot(fit2A, which = 4 ) # to recreate
# plot(fit2A, which = 5)
# plot(fit2A, which = 6)
# par(mfrow = pr)



influencegraph <- function(fitmodel, labsize = 3, labnumber = 5,
                           ylim1 = c(NA, NA), ylim2 = c(NA, NA),
                           title = "Cook's distance and |dfbetas|") {
        dfb <- dfbetas(fitmodel)
        # important dfbetas
        idfb <- dfb[ , c("am", "wt:am")]
        idfb <- as.data.frame(idfb)
        idfb$model <- rownames(idfb)
        idfb$cook <- cooks.distance(fitmodel)
        colnames(idfb) <- c("am","wt_am", "model", "cook" )

        idfb <- idfb %>%
                mutate(absam = abs(am),
                       abswtam =abs(wt_am),
                       maxeff=ifelse (absam > abswtam, absam, abswtam),
                       mineff=ifelse (absam > abswtam, abswtam, absam)) %>%
                arrange(desc(maxeff))

        idfb$model <- orderfact(dataf = idfb, nomfact = "model", ordervar = "maxeff")
        idfb$labels <- rep("", nrow(idfb))
        for (i in 1:labnumber) {idfb$labels[i] <- as.character(idfb$model[i])}
        # idfb$ylab <- idfb$mineff

        gdfb <- ggplot(idfb, aes(as.numeric(model))) +
                geom_point(aes(y = absam, color= "am")) +
                geom_point(aes(y = abswtam, color= "wt_am")) +
                scale_color_discrete("coefficient") +
                labs(x="Car model", y = "Abs(dfbeta)") +
                scale_y_continuous(limits = ylim1) +
                scale_x_continuous(breaks = NULL) +
                # theme(axis.text.x = element_text(angle=90, hjust = .5, vjust = .5)) +
                geom_text(aes(y = mineff, label=labels),
                          size = labsize, angle=90, hjust = 1, vjust = .5, nudge_y=-.01)

        gcd <- ggplot(idfb, aes(as.numeric(model), cook)) +
                geom_point() +
                scale_y_continuous(limits = ylim2) +
                scale_x_continuous(breaks = NULL) +
                labs(x="Car model", y = "Cook's distance") +
                geom_text(aes(y = cook, label=labels),
                          size = labsize, angle=90, hjust = 1, vjust = .5, nudge_y=-.005)
        grtitle <- textGrob(title, gp = gpar(fontface="bold"))

        # return combined graph
        grid.arrange(grobs =list(gdfb,gcd), layout_matrix=matrix(c(1,2), nrow = 1), top = grtitle)
}

cbA <- influencegraph(fit2A, labnumber = 7, ylim1 = c(0,.65), ylim2 = c(0,.27), title = "Initial situation")
# displaygraph(cbA)

# initialize removal vector
pointremovals <- character(0)


# Step B: Removing Chrysler Imperial
# ---------------------------------

mtcars2B <- filter(mtcars2, model != "Chrysler Imperial")
rownames(mtcars2B) <- mtcars2B$model

# fit2B <- lm(mpg ~ am*wt + qsec, mtcars2B)
# s <- summary(fit2B)
# s
fit2B <- lm(mpg ~ wt + am*wt + qsec, mtcars2B)
s <- summary(fit2B)
sc <- data.frame(s$coefficients)
sc$coeff <- rownames(sc)



# # fixing ylim in plot.lm
# #  see:  http://stackoverflow.com/questions/29484167/setting-ylim-in-rs-plot-lm-residual-plot
#
# pr <- par("mfrow")
# par(mfrow=c(1,3))
# plotlm(fit2B, which = 4, ylim = .30)
# plotlm(fit2B, which = 5, ylim = c(-2.5, 2.5) )
# plotlm(fit2B, which = 6, ylim = .30)
# par(mfrow=pr)



# check Cook's D + dfbetas
cbB <- influencegraph(fit2B, labnumber = 6, ylim1 = c(0,.65), ylim2 = c(0,.27),
                      title = "After removing 'Chrysler Imperial'")
# displaygraph(cbB)


# register removal
pointremovals <- c(pointremovals, "Chrysler Imperial")


# Step C: Removing  Maserati Bora,
# -------------------------------

mtcars2C <- filter(mtcars2B, model != "Chrysler Imperial", model != "Maserati Bora" )
rownames(mtcars2C) <- mtcars2C$model

fit2C <- lm(mpg ~ wt + am*wt + qsec, mtcars2C)
s <- summary(fit2B)
sc <- data.frame(s$coefficients)
sc$coeff <- rownames(sc)

# register removal
pointremovals <- c(pointremovals, "Maserati Bora")

# # fixing ylim in plot.lm
# #  see:  http://stackoverflow.com/questions/29484167/setting-ylim-in-rs-plot-lm-residual-plot
#
# pr <- par("mfrow")
# par(mfrow=c(1,3))
# plotlm(fit2C, which = 4, ylim = .25)
# plotlm(fit2C, which = 5, ylim = c(-2.5, 2.5) )
# plotlm(fit2C, which = 6, ylim = .25)
# par(mfrow=pr)


cbC <- influencegraph(fit2C, labnumber = 5, ylim1 = c(0,.65), ylim2 = c(0,.27),
                      title = "After removing 'Chrysler Imperial'+'Maserati Bora'")
# displaygraph(cbC)




# Step D: Removing  "Toyota Corolla",
# -------------------------------

mtcars2D <- filter(mtcars2C, model != "Toyota Corolla")
rownames(mtcars2D) <- mtcars2D$model

fit2D <- lm(mpg ~ wt + am*wt + qsec, mtcars2D)
s <- summary(fit2D)
sc <- data.frame(s$coefficients)
sc$coeff <- rownames(sc)


# register removal
pointremovals <- c(pointremovals, "Toyota Corolla")


# pr <- par("mfrow")
# par(mfrow=c(1,3))
# plotlm(fit2D, which = 4, ylim = .3)
# plotlm(fit2D, which = 5, ylim = c(-2.5, 2.5) )
# plotlm(fit2D, which = 6, ylim = .3)
# par(mfrow=pr)



cbD <- influencegraph(fit2D, labnumber = 3, ylim1 = c(0,.65), ylim2 = c(0,.27),
                      title = "After removing 'Chrysler Imperial'+'Maserati Bora'+'Toyota Corolla'")
# displaygraph(cbD)



# Step E: Removing  "Fiat 128",
# -----------------------------

mtcars2E <- filter(mtcars2D, model != "Fiat 128")
rownames(mtcars2E) <- mtcars2E$model
fit2E <- lm(mpg ~ wt + am*wt + qsec, mtcars2E)
s <- summary(fit2E)
sc <- data.frame(s$coefficients)
sc$coeff <- rownames(sc)


# register removal
pointremovals <- c(pointremovals, "Fiat 128")
#
# pr <- par("mfrow")
# par(mfrow=c(1,3))
# plotlm(fit2E, which = 4, ylim = .3)
# plotlm(fit2E, which = 5, ylim = c(-2.5, 2.5) )
# plotlm(fit2E, which = 6, ylim = .3)
# par(mfrow=pr)



cbE <- influencegraph(fit2E, labnumber = 3, ylim1 = c(0,.65), ylim2 = c(0,.27),
                      title = "After removing 'Chrysler Imperial'+'Maserati Bora'+'Toyota Corolla'+'Fiat 128'" )
# displaygraph(cbE)



#try putting these graphs together

displaygraph(grid.arrange(grobs=list(cbA, cbB, cbC),  ncol = 1))
displaygraph(grid.arrange(grobs=list(cbD, cbE),  ncol = 1))



# Diagnostics again (checking basic assumptions)

pr <- par("mfrow")
par(mfrow=c(1,3))
for (i in 1:3) {plot(fit2E, which = i)}
par(mfrow=pr)


# ==> normality assumption believable,
# uncorrelated residuals, homoscedasticity


#'
#' conclusion:
#' -----------

s <- summary(fit2E)
sfc <- s$coefficients
sfc
s$call
# Point estimation
# neutral weight

nwt <- - (sfc[3,1] / sfc[5,1])
deltaslope <- sfc[5,1]



# ==> for small wt, manual is better, for high wt auto is better.
# balance point, w = 3(thousands of lb)
# The manufactureres might share a similar view, as the overlap zone goes from circa

mta <- filter(mtcars, am==0)
mtm <- filter(mtcars, am==1)
min(mta$wt) # 2.465
max(mtm$wt) # 3.57


annotslines <- data.frame(intc = c(min(mta$wt), max(mtm$wt)))


# graphing this:
mtcars2$gearbox <- factor(mtcars2$am, labels = c("auto", "manual"))

carsxcluded <- filter(mtcars2, model == "Chrysler Imperial" |
                              model == "Maserati Bora" |
                              model == "Toyota Corolla" | model == "Fiat 128")

carsxcluded$adj <- c(0,0,0,0)

set.seed(5)

gviolinwt <- ggplot(mtcars2, aes(gearbox, wt, color=gearbox)) +
        geom_violin(mapping=aes(fill=gearbox),
                    width=1.2, show.legend = FALSE, alpha = .2) +
        geom_boxplot(width=.4, outlier.size = 0) +
        geom_jitter(width=.15, height= 0,
                    alpha =.5, size = 2, color="black") +
        geom_hline(data=annotslines,aes(yintercept = intc), linetype = 2) +
        geom_text(data = carsxcluded,
                  aes(gearbox, wt, label = model, hjust=adj),
                  nudge_x = 0.00, nudge_y = 0.02, color = "black", angle = 30) +
        guides(col = guide_legend(reverse = TRUE)) +
        scale_y_continuous(limits=c(NA, 6)) +
        labs(title = "Automatic vs manual gearbox : car weight distributions") +
        coord_flip()

# displaygraph(gviolinwt)




# confidence interval for the slope coeff(wt:am)

Int <- sc[5,1] + c(-1,1) * qt(df = nrow(mtcars) - 2, p = .975) * sc[5,2]
# interval notation
Intslope <- paste("[", format( Int[1] ,digits =3), ";", format( Int[2] ,digits =3), "]")





#'
#' Bootstrapping (with package 'boot' and custom plotting function)
#' ----------------------------------------------------------------
#'


# function to obtain stats = c(beta(am), beta(am_wt), wt_0)
bs <- function(formula, data, indices) {
        d <- data[indices,] # allows boot to select sample
        fit <- lm(formula, data=d)
        cf <- coef(fit)
        # ret <- c("am" = cf["am"], "wt_am" = cf["wt:am"], "wt_0" = cf["am"] / cf["wt:am"])
        ret <- c(cf["am"], cf["wt:am"], "wt_0" = - cf["am"] / cf["wt:am"])
        names(ret) <- c("am", "wt_am", "wt_0")
        return(ret)
}

# initial results (on the original sample)
orig <- bs(formula = mpg ~  qsec + wt*am, data=mtcars2E, indices= 1:nrow(mtcars2E))


# bootstrapping with 10000 replications
set.seed(20)
results <- boot(data=mtcars2E, statistic=bs,
                R = bootstriter, formula = mpg ~ qsec + wt*am)



# customized plotting function
ggplotboot <- function(boot.out, index, cnames, title, trim = 0.015, conflevel = .95, digits = 3) {
        if (missing(cnames)) {cnames= paste0(rep("c", ncol(boot.out$t)),
                                            as.character(1:ncol(boot.out$t)))}
        if (missing(title)){title <- paste("Bootstrap distribution of ", cnames[index]) }

        #prepare data
        dfres <- as.data.frame(boot.out$t) # bootstrap dataframe of results
        colnames(dfres) <- cnames
        origres <- boot.out$t0 # original result
        names(origres) <- cnames

        lim <- quantile(dfres[[index]], probs = c(trim, 1 - trim), na.rm = TRUE)
        varname <- cnames[index]
        cfint <- boot.ci(results, type="bca", index= index, conf = conflevel)
        # confidence limits
        cflim <- cfint$bca[4:5]
        # data for vertical lines
        vlines <- data.frame(x = c(cflim[1],  origres[[index]], c(cflim[2])),
                             lt = factor(c(2,1,2)) )
        # data for text annots
        clabs <- data.frame(lab = paste( c("bca conf.limit =" , "point est. =", "bca conf.limit ="),
                                         round(vlines[["x"]], digits)),
                            x = vlines[["x"]],
                            y = rep(.05,3),
                            angl = c(90,0,90),
                            hjust= c(0,.5,0),
                            col = vlines[["lt"]],
                            nudge_x = c(-(lim[2] - lim[1]) / 100, 0, (lim[2] - lim[1]) / 100))


        ggplot(dfres, aes_(as.name(varname))) +
                geom_density(fill = "lightblue") +
                geom_vline(data=vlines, aes(xintercept=x, linetype = lt, color = lt)) +
                geom_text(data = clabs, aes(x + nudge_x, y, label= lab, angle = angl,
                                            hjust = hjust,  color = col)) +
                scale_x_continuous(limits = lim) +
                scale_color_manual(limits = c(1,2), breaks = c(1,2),
                                   values = c("blue", "red")) +
                labs(title = title) +
                theme(legend.position = "none")

}



# get the graphs
ggplotboot(results, index=2, trim=0.01, cnames = c("am", "wt_am", "wt_0"))
ggplotboot(results, index=3, trim=0.01, cnames =c("am", "wt_am", "wt_0"))


# confidence interval
bcaint <- boot.ci(results, type = "bca", index = 3)
bcaint <- bcaint$bca[4:5]



