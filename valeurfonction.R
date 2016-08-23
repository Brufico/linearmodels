
makefun <- function() {
        xx <- 1
        aa <- function(...) {
                xx <<- xx + 1
                paste("x =", xx )
        }
        bb <- function() { xx }
        list(aa=aa, bb=bb)
}

tmp <- makefun()

tmp$aa()
tmp$aa("d")
tmp$bb()

aa <- tmp[[1]]

str(aa)
str(tmp$aa)

sapply(list("NULL","gg"), tmp$aa)
sapply(list("NULL","gg"), aa)




assign("xa", mode="function", aaa)
get("aaa", mode = "variable")
aaa(1)

sapply(c(1), function(x) { print("hello, ")})
sapply(c(1), aaa)
sapply(c(1), eval(aaa))

sapply(c(1), get("aaa"))

mean
