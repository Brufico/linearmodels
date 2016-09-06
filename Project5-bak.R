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
displaygraph(gviolin)




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

displaygraph(slrg)



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



#' diagnostics2: influential outliers
#' ----------------------------------

# get 'mtcars' again, in order to reestablish row names
# data(mtcars)
# mtcars2 <- mtcars
mtcars2$model <- rownames(mtcars)
# rownames(mtcars2) <- mtcars2$model

# fit the model
fit2A <- lm(mpg ~ wt + am*wt + qsec, mtcars2)
s <- summary(fit2A)
sc <- data.frame(s$coefficients)
sc$coeff <- rownames(sc)


# Plotting
pr <- par("mfrow")
par(mfrow = c(1,3))
plot(fit2A, which = 4 )
plot(fit2A, which = 5)
plot(fit2A, which = 6)
par(mfrow = pr)




# dfbetas
# hatvalues(fit2A) # useful ?
dfb <- dfbetas(fit2A)
# important dfbetas
idfb <- dfb[ , c("am", "wt:am")]
idfb <- as.data.frame(idfb)
idfb$model <- rownames(idfb)
colnames(idfb) <- c("am",    "wt_am", "model")
# idfb <- idfb[order(abs(idfb[["wt_am"]]), decreasing = TRUE), ]

idfb <- idfb %>%
        mutate(absam = abs(am),
               abswtam =abs(wt_am),
               maxeff=ifelse (absam > abswtam, absam, abswtam)) %>%
        arrange(desc(maxeff))

idfb$model <- orderfact(dataf = idfb, nomfact = "model", ordervar = "maxeff")

gdfb <- ggplot(idfb, aes(model)) +
        geom_point(aes(y = absam, color= "am")) +
        geom_point(aes(y = abswtam, color= "wt_am")) +
        scale_color_discrete("coefficient") +
        scale_y_continuous("|dfbeta| ") +
        theme(axis.text.x = element_text(angle=90, hjust=1))

displaygraph(gdfb)




# # hatvalues and dfbetas, new try ======================================================
#
# # measuring uncertainty due to the car
# idfb$eslope <- vlookup("am:wt", sc, searchcol = "coeff", returncol= "Estimate")
# idfb$dslope <- abs(idfb[["wt_am"]] / idfb$eslope)
#
# idfb$esam <- vlookup("am", sc, searchcol = "coeff", returncol= "Estimate")
# idfb$dsam <- ( (abs(idfb$esam) + abs(idfb[["am"]])) / (abs(idfb$esam) - abs(idfb[["am"]]))) -
#         ( (abs(idfb$esam) - abs(idfb[["am"]])) / (abs(idfb$esam) + abs(idfb[["am"]])))
#
# idfb$model <- orderfact(dataf = idfb, nomfact = "model", ordervar = "dsam")
#
# gdfb2 <- ggplot(idfb, aes(model)) +
#         geom_point(aes(y = dsam, color= "dsam")) +
#         geom_point(aes(y = dslope, color= "dslope")) +
#         scale_color_discrete("coefficient") +
#         scale_y_continuous("dfbeta") +
#         theme(axis.text.x = element_text(angle=90, hjust=1))
#
# displaygraph(gdfb2)
#
#
#
#
#
#
#
#
#
#
#
#
# maindfb <- idfb[idfb$model %in% c("Fiat 128", "Maserati Bora" , "Chrysler Imperial" ) , c("wt"  , "am", "wt_am")]
#
# maindfb["Fiat 128", "wt"]
# maindfb["Fiat 128", "wt_am"]






# Removing Chrysler Imperial , (Maserati Bora, (Cadillac Fleetwood, Toyota Corolla, Fiat 128)
# -------------------------------------------


mtcars2B <- filter(mtcars2, model != "Chrysler Imperial") #,
                   # model != "Maserati Bora" )#,

rownames(mtcars2B) <- mtcars2B$model

fit2B <- lm(mpg ~ am*wt + qsec, mtcars2B)
s <- summary(fit2B)
s




# fixing ylim in plot.lm
#  see:  http://stackoverflow.com/questions/29484167/setting-ylim-in-rs-plot-lm-residual-plot

pr <- par("mfrow")
par(mfrow=c(1,3))
plotlm(fit2B, which = 4, ylim = .25)
plotlm(fit2B, which = 5, ylim = c(-2.5, 2.5) )
plotlm(fit2B, which = 6, ylim = .25)
par(mfrow=pr)




# verification lookup hatvalues and dfbetas
hatvalues(fit2B)
dfb <- dfbetas(fit2B)
dfb[ , c("am", "am:wt")]
dfb <- as.data.frame(dfb)


# iteration
# important dfbetas
idfb <- dfb[ , c("am", "am:wt")]
idfb <- as.data.frame(idfb)
idfb$model <- rownames(idfb)
colnames(idfb) <- c("am",    "wt_am", "model")
# idfb <- idfb[order(abs(idfb[["wt_am"]]), decreasing = TRUE), ]

idfb <- idfb %>%
        mutate(absam = abs(am),
               abswtam =abs(wt_am),
               maxeff=ifelse (absam > abswtam, absam, abswtam)) %>%
        arrange(desc(maxeff))

idfb$model <- orderfact(dataf = idfb, nomfact = "model", ordervar = "maxeff")


gdfb1 <- ggplot(idfb, aes(model)) +
        geom_point(aes(y = absam, color= "am")) +
        geom_point(aes(y = abswtam, color= "wt_am")) +
        scale_color_discrete("coefficient") +
        scale_y_continuous("|dfbeta| ") +
        theme(axis.text.x = element_text(angle=90, hjust=1))

displaygraph(gdfb1)




# Removing Chrysler Imperial + Maserati Bora, (Cadillac Fleetwood, Toyota Corolla, Fiat 128)
# -------------------------------------------


mtcars2C <- filter(mtcars2, model != "Chrysler Imperial", model != "Maserati Bora" )

rownames(mtcars2C) <- mtcars2C$model

fit2C <- lm(mpg ~ am*wt + qsec, mtcars2C)
s <- summary(fit2C)
s




# fixing ylim in plot.lm
#  see:  http://stackoverflow.com/questions/29484167/setting-ylim-in-rs-plot-lm-residual-plot

pr <- par("mfrow")
par(mfrow=c(1,3))
plotlm(fit2C, which = 4, ylim = .25)
plotlm(fit2C, which = 5, ylim = c(-2.5, 2.5) )
plotlm(fit2C, which = 6, ylim = .25)
par(mfrow=pr)




# verification lookup hatvalues and dfbetas


dfb <- dfbetas(fit2C)
dfb[ , c("am", "am:wt")]
dfb <- as.data.frame(dfb)


# iteration
# important dfbetas
idfb <- dfb[ , c("am", "am:wt")]
idfb <- as.data.frame(idfb)
idfb$model <- rownames(idfb)
colnames(idfb) <- c("am",    "wt_am", "model")
# idfb <- idfb[order(abs(idfb[["wt_am"]]), decreasing = TRUE), ]

idfb <- idfb %>%
        mutate(absam = abs(am),
               abswtam =abs(wt_am),
               maxeff=ifelse (absam > abswtam, absam, abswtam)) %>%
        arrange(desc(maxeff))

idfb$model <- orderfact(dataf = idfb, nomfact = "model", ordervar = "maxeff")


gdfb2 <- ggplot(idfb, aes(model)) +
        geom_point(aes(y = absam, color= "am")) +
        geom_point(aes(y = abswtam, color= "wt_am")) +
        scale_color_discrete("coefficient") +
        scale_y_continuous("|dfbeta| ") +
        theme(axis.text.x = element_text(angle=90, hjust=1))

displaygraph(gdfb2)






# more outliers removal, "Toyota Corolla"
mtcars2D <- filter(mtcars2C, model != "Toyota Corolla")
rownames(mtcars2D) <- mtcars2D$model
fit2D <- lm(mpg ~ am*wt + qsec, mtcars2D)
s <- summary(fit2D)
s



pr <- par("mfrow")
par(mfrow=c(1,3))
plotlm(fit2D, which = 4, ylim = .3)
plotlm(fit2D, which = 5, ylim = c(-2.5, 2.5) )
plotlm(fit2D, which = 6, ylim = .3)
par(mfrow=pr)



# verification lookup hatvalues and dfbetas
# hatvalues(fit2C)
dfb <- dfbetas(fit2D)
dfb[ , c("am", "am:wt")]
dfb <- as.data.frame(dfb)



# iteration
# important dfbetas
idfb <- dfb[ , c("am", "am:wt")]
idfb <- as.data.frame(idfb)
idfb$model <- rownames(idfb)
colnames(idfb) <- c("am",    "wt_am", "model")
# idfb <- idfb[order(abs(idfb[["wt_am"]]), decreasing = TRUE), ]

idfb <- idfb %>%
        mutate(absam = abs(am),
               abswtam =abs(wt_am),
               maxeff=ifelse (absam > abswtam, absam, abswtam)) %>%
        arrange(desc(maxeff))

idfb$model <- orderfact(dataf = idfb, nomfact = "model", ordervar = "maxeff")



gdfb3 <- ggplot(idfb, aes(model)) +
        geom_point(aes(y = absam, color= "am")) +
        geom_point(aes(y = abswtam, color= "wt:am")) +
        scale_color_discrete("coefficient") +
        scale_y_continuous("dfbeta") +
        theme(axis.text.x = element_text(angle=90, hjust=1))

displaygraph(gdfb3)




# more outliers removal, "Fiat 128"
mtcars2E <- filter(mtcars2D, model != "Fiat 128")
rownames(mtcars2E) <- mtcars2E$model
fit2E <- lm(mpg ~ am*wt + qsec, mtcars2E)
s <- summary(fit2E)
s



pr <- par("mfrow")
par(mfrow=c(1,3))
plotlm(fit2E, which = 4, ylim = .3)
plotlm(fit2E, which = 5, ylim = c(-2.5, 2.5) )
plotlm(fit2E, which = 6, ylim = .3)
par(mfrow=pr)



# verification lookup hatvalues and dfbetas
# hatvalues(fit2C)
dfb <- dfbetas(fit2E)
dfb[ , c("am", "am:wt")]
dfb <- as.data.frame(dfb)



# iteration
# important dfbetas
idfb <- dfb[ , c("am", "am:wt")]
idfb <- as.data.frame(idfb)
idfb$model <- rownames(idfb)
colnames(idfb) <- c("am",    "wt_am", "model")
# idfb <- idfb[order(abs(idfb[["wt_am"]]), decreasing = TRUE), ]

idfb <- idfb %>%
        mutate(absam = abs(am),
               abswtam =abs(wt_am),
               maxeff=ifelse (absam > abswtam, absam, abswtam)) %>%
        arrange(desc(maxeff))

idfb$model <- orderfact(dataf = idfb, nomfact = "model", ordervar = "maxeff")



gdfb4 <- ggplot(idfb, aes(model)) +
        geom_point(aes(y = absam, color= "am")) +
        geom_point(aes(y = abswtam, color= "wt:am")) +
        scale_color_discrete("coefficient") +
        scale_y_continuous("dfbeta") +
        theme(axis.text.x = element_text(angle=90, hjust=1))

displaygraph(gdfb4)







# Diagnostics again (checking basic assumptions)

pr <- par("mfrow")
par(mfrow=c(2,3))
for (i in 1:6) {plot(fit2E, which = i)}
par(mfrow=pr)






# ==> normality assumption believable


#'
#' conclusion:
#' -----------

s <- summary(fit2E)
sfc <- s$coefficients
sfc
s$call
# Point estimation
# neutral weight

nwt <- - (sfc[2,1] / sfc[5,1])
deltaslope <- sfc[5,1]



# ==> for small wt, manual is better, for high wt auto is better.
# balance point, w = 3.4 (thousands of lb)
# The manufactureres seem to share the same opinion, as the overlap zone goes from circa

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
                      )
carsxcluded$adj <- c(0, 0, 0, 0)


gviolinwt <- ggplot(mtcars2, aes(gearbox, wt, color=gearbox)) +
        geom_violin(mapping=aes(fill=gearbox),
                    width=1.2, show.legend = FALSE, alpha = .2) +
        geom_boxplot(width=.4, outlier.size = 0) +
        geom_jitter(width=.15, height= 0,
                    alpha =.5, size = 2, color="black") +
        geom_hline(data=annotslines,aes(yintercept = intc), linetype = 2) +
        geom_text(data = carsxcluded,
                  aes(gearbox, wt, label = model, hjust=adj),
                  nudge_x = 0.00, nudge_y = 0, color = "black", angle = 30) +
        guides(col = guide_legend(reverse = TRUE)) +
        scale_y_continuous(limits=c(NA, 6)) +
        labs(title = "Automatic vs manual gearbox : car weight distributions") +
        coord_flip()

displaygraph(gviolinwt)






#'
#' Bootstrapping (manually)
#' --------------
#'

# data(mtcars2B)

# first: estimate the model from lm
fit <- lm(mpg ~  qsec + wt*am, mtcars2E)
sc <- summary(fit)$coefficients

# conf int for coeff(wt:am)
Int <- sc[5,1] + c(-1,1) * qt(df = nrow(mtcars) - 2, p = .975) * sc[5,2]
# interval notation
Intslope <- paste("[", format( Int[1] ,digits =3), ";", format( Int[2] ,digits =3), "]")



n <- nrow(mtcars2E) # size of each sample
B <- 10000 # number of samples to generate

# resampling
set.seed(1254)


# do not store the samples
#
# generate samples
# x <- sample(1:n, n, replace = TRUE) # x is a vector of row indexes

lsamp <- lapply(1:B, function(i) {sample(1:n, n, replace = TRUE)}) # list of B samples

getcoeffs <- function(x){
        # sample data
        sdata <- mtcars2E[x,]
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
neutralquantile <- quantile(lsamp$neutral, c(.025, 0.50, .975))
mean(lsamp[[1]]) # surprising
neutralmedian <- median(lsamp$neutral)
# head(lsamp)
# min(lsamp$neutral)
# max(lsamp$neutral)

labdf <- data.frame(quant = neutralquantile + c(-.05, 0, 0.05),
                    y = c(.1, .1, .1),
                    lab = paste(c("q[.025] == ", "median ==" ,"q[.975] =="), round(neutralquantile, 3)),
                    just = c(0, 0.5, 0),
                    angl = c(90,0,90))

vldf <- data.frame(quant = neutralquantile,
                   lt=factor(c(2,1,2)))



gdens = ggplot(lsamp, aes(x = neutral)) +
        geom_density(color = "black", fill = "lightblue" ) +
        scale_x_continuous(limits = c(1.5 , 4.5)) +
        geom_vline(data=vldf, aes(xintercept=quant, linetype = lt), color = "red") +
        geom_text(data = labdf, aes(quant, y, label= lab, angle = angl, hjust = just), parse = TRUE) +
        labs(x = "neutral weight",
             title = expression(Resampling~distribution~of~wt[0])) +
        theme(legend.position = "none")
gdens



# confidence interval

# conf int for wt0
Int <- quantile(lsamp$neutral, c(.025, .975))
# interval notation
Intwt0 <- paste("[", format( Int[1] ,digits =3), ";", format( Int[2], digits =3), "]")




# About "diff_slope", approach through bootstrap (not used in the report)
quantile(lsamp$diff_slope, c(.025, .975))
mean(lsamp$diff_slope)
median(lsamp$diff_slope)

# conf int for slope (2)
Int <- quantile(lsamp$diff_slope, c(.025, .975))
# interval notation
Intdfslope <- paste("[", format( Int[1] ,digits =3), ";", format( Int[2] ,digits =3), "]")




