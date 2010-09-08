dQuote =
function(x)
  paste('"', x, '"', sep = "")

setGeneric("toJSON",
function(x, container = length(x) > 1  || length(names(x)) > 0, ...)
  standardGeneric("toJSON"))

setMethod("toJSON", "NULL",
           function(x, container = length(x) > 1  || length(names(x)) > 0, ...) {
             "null"
           })

setMethod("toJSON", "ANY",
           function(x, container = length(x) > 1  || length(names(x)) > 0, ...) {

             if(isS4(x)) {
               paste("{", paste(dQuote(slotNames(x)), sapply(slotNames(x), function(id) toJSON(slot(x, id), ...)), sep = ": "),
                     "}", collapse = "\n")
             } else {
               stop("No method for converting ", class(x), " to JSON")
             }
             
           })


setMethod("toJSON", "integer",
           function(x, container = length(x) > 1 || length(names(x)) > 1, ...) {

             if(container) {
                if(length(names(x)))
                   paste("{\n", paste(dQuote(names(x)), x, sep = ": ", collapse = ",\n   "), "\n}")
                else
                   paste("[", paste(x, collapse = ", "), "]")
             }
             else
                as.character(x)               
           })

setOldClass("hexmode")

setMethod("toJSON", "hexmode",
           function(x, container = length(x) > 1 || length(names(x)) > 0, ...) {
             tmp = paste("0x", format(x), sep = "")
             if(container) {
                if(length(names(x)))
                   paste("{\n", paste(dQuote(names(x)), tmp, sep = ": ", collapse = ",\n   "), "\n}")
                else               
                paste("[", paste(tmp, collapse = ", "), "]")
             } else
                tmp
           })


setMethod("toJSON", "factor",
           function(x, container = length(x) > 1 || length(names(x)) > 0, ...) {
             toJSON(as.character(x), container, ...)
           })

setMethod("toJSON", "logical",
           function(x, container = length(x) > 1 || length(names(x)) > 0, ...) {
             tmp = ifelse(x, "true", "false")
             if(container) {
                if(length(names(x)))
                   paste("{\n", paste(dQuote(names(x)), tmp, sep = ": ", collapse = ",\n   "), "\n}")
                else               
                   paste("[", paste(tmp, collapse = ", "), "]")
             } else
                tmp
           })

setMethod("toJSON", "numeric",
           function(x, container = length(x) > 1 || length(names(x)) > 0, digits = 5, ...) {
             tmp = formatC(x, digits = digits)
             if(container) {
                if(length(names(x)))
                   paste("{\n", paste(dQuote(names(x)), tmp, sep = ": ", collapse = ",\n   "), "\n}")
                else
                   paste("[", paste(tmp, collapse = ", "), "]")
             } else
               tmp
           })


setMethod("toJSON", "character",
           function(x, container = length(x) > 1 || length(names(x)) > 0, digits = 5, ...) {
             tmp = gsub("\\\n", "\\\\n", x)
             tmp = gsub('"', '\\\\"', tmp)             
             tmp = dQuote(tmp)
             if(container) {
                if(length(names(x)))
                   paste("{\n", paste(dQuote(names(x)), tmp, sep = ": ", collapse = ",\n   "), "\n}")
                else               
                   paste("[", paste(tmp, collapse = ", "), "]")
             } else
                tmp
           })



# Symbols.
setMethod("toJSON", "name",
           function(x, container = length(x) > 1 || length(names(x)) > 0, ...) {
             as.character(x)
           })

setMethod("toJSON", "matrix",
           function(x, container = length(x) > 1 || length(names(x)) > 0, ...) {
             tmp = paste(apply(x, 1, toJSON), collapse = ",\n")
             if(!container)
               return(tmp)

              if(length(names(x)))
                paste("{", paste(dQuote(names(x)), tmp, sep = ": "), "}")                
              else
                paste("[", tmp, "]")
           })

setMethod("toJSON", "list",
           function(x, container = length(x) > 1 || length(names(x)) > 0, ...) {
             els = sapply(x, toJSON, ...)

             if(all(sapply(els, is.name)))
               names(els) = NULL

             if(!container)
               return(els)
             
             if(length(names(x)))
                paste("{\n", paste(dQuote(names(x)), els, sep = ": ", collapse = ",\n   "), "\n}")
             else
                 paste("[\n", paste(els, collapse = ",\n"), "\n]")
           })
