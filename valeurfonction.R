
makefun <- function() {
        xx <- 1
        aa <- function() {
                xx <<- xx + 1
                paste("x =", xx )
        }
        bb <- function() { xx }
        list(aa=aa, bb=bb)
}

tmp <- makefun()

tmp$aa()
tmp$bb()

aa <- tmp[1]

aa()

do.call(quote("aa"), list())
aa()

get("aaa")
get("aaa", mode = "function")



assign("xa", mode="function", aaa)
get("aaa", mode = "variable")
aaa(1)

sapply(c(1), function(x) { print("hello, ")})
sapply(c(1), aaa)
sapply(c(1), eval(aaa))

sapply(c(1), get("aaa"))

mean
