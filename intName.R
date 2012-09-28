library(RJSONIO)
a <- list(l = c(name = 1L))  # list(l = as.vector(list(n=1), mode="integer"))
toJSON(a)

b <- list(l = c(c(name = 1L, other = 2L)))
toJSON(b)
