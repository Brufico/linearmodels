

# Counter
# =======

Initcount <- function(value = 0){
        assign("count", value, parent.env(env = environment()))
               function(refname = ".last", ref = FALSE){
                       if (ref) {
                               get(refname, parent.env(env = environment()))
                       } else {
                               count <<- count + 1
                               assign(refname,count,
                                      parent.env(env = environment()))
                               count
                       }
               }
}


counter <- Initcount()

counter()
counter()
counter(".last", ref = TRUE)

counter( "token", FALSE)
counter( "ziz", FALSE)


counter( "ziz", TRUE)
counter( "token", TRUE)

counter()
counter(".last", ref = TRUE)

