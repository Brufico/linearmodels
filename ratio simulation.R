


mnum <- 10.562
snum <- 3.245
mdenom <- -3.534
sdenom <- 1.149

df = 28

resnaive <- - mnum/mdenom


# population:
B <- 1000000
set.seed(25)
num <- mnum + c(-1,1)* rt(n=B, df=df) * snum
denom <- mdenom + c(-1,1)* rt(n=B, df=df) * sdenom
res <- num / denom

summary(res)
mean(res,trim=0.025)


