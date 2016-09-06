




# Example Baltimore Ravens win/loss
# Ravens Data

# download.file("https://dl.dropboxusercontent.com/u/7710864/data/ravensData.rda"
#               , destfile="./data/ravensData.rda",method="curl")
load("./data/ravensData.rda")
head(ravensData)






library(manipulate)


x <- seq(-10, 10, length = 1000)
manipulate(
        plot(x, exp(beta0 + beta1 * x) / (1 + exp(beta0 + beta1 * x)),
             type = "l", lwd = 3, frame = FALSE),
        beta1 = slider(-2, 2, step = .1, initial = 2),
        beta0 = slider(-2, 2, step = .1, initial = 0)
)


# Ravens logistic regression

logRegRavens <- glm(ravensData$ravenWinNum ~ ravensData$ravenScore,family="binomial")
summary(logRegRavens)


# plot

plot(ravensData$ravenScore,logRegRavens$fitted, pch=19,col="blue",xlab="Score",ylab="Prob Ravens Win")


#coeffs

exp(logRegRavens$coeff)


exp(confint(logRegRavens))


anova(logRegRavens,test="Chisq")





