reduce.line <- paste(
                         '["reduce",["function(k, v) { return sum(v); }"],',
                         '[[[1,"699b524273605d5d3e9d4fd0ff2cb272"],10],',
                         '[[2,"c081d0f69c13d2ce2050d684c7ba2843"],20],',
                         '[[null,"foobar"],3]]]\n',sep='')
reduce.line

# this is correct
library(rjson)
reduce.trans <- fromJSON(reduce.line)
class(reduce.trans)
x <- reduce.trans[[3]]
class(x)
length(x)
x

# this is incorrect
detach()
library(RJSONIO)
reduce.iotrans <- RJSONIO:::fromJSON(reduce.line)
class(reduce.iotrans)
xio <- reduce.iotrans[[3]]
class(xio)
length(xio)
dim(xio)
xio 
