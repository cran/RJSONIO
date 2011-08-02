emptyNamedList = structure(list(), names = character())

trim =
function (x) 
  gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)

dQuote =
function(x)
  paste('"', x, '"', sep = "")

setGeneric("toJSON",
function(x, container = .level == 1L || length(x) > 1  || length(names(x)) > 0, collapse = "\n", ..., .level = 1L, .withNames = length(x) > 0 && length(names(x)) > 0)  
  standardGeneric("toJSON"))

setMethod("toJSON", "NULL",
           function(x, container = .level == 1L || length(x) > 1  || length(names(x)) > 0, collapse = "\n", ..., .level = 1L, .withNames = length(x) > 0 && length(names(x)) > 0) {
             if(container) "[ null ] " else "null"
           })

setMethod("toJSON", "ANY",
           function(x, container = .level == 1L || length(x) > 1  || length(names(x)) > 0, collapse = "\n", ..., .level = 1L, .withNames = length(x) > 0 && length(names(x)) > 0) {

             if(isS4(x)) {
               paste("{", paste(dQuote(slotNames(x)), sapply(slotNames(x), function(id) toJSON(slot(x, id), ...)), sep = ": "),
                     "}", collapse = collapse)
             } else {
               stop("No method for converting ", class(x), " to JSON")
             }
             
           })


setMethod("toJSON", "integer",
           function(x, container = .level == 1L || length(x) > 1 || length(names(x)) > 1, collapse = "\n  ", ..., .level = 1L, .withNames = length(x) > 0 && length(names(x)) > 0) {

             if(container) {
                if(.withNames)
                   paste(sprintf("{%s", collapse), paste(dQuote(names(x)), x, sep = ": ", collapse = sprintf(",%s", collapse)), sprintf("%s}", collapse))
                else
                   paste("[", paste(x, collapse = ", "), "]")
             }
             else
                as.character(x)               
           })

setOldClass("hexmode")

setMethod("toJSON", "hexmode",
           function(x, container = .level == 1L || length(x) > 1 || length(names(x)) > 0, collapse = "\n   ", ..., .level = 1L, .withNames = length(x) > 0 && length(names(x)) > 0) {
             tmp = paste("0x", format(x), sep = "")
             if(container) {
                if(.withNames)
                   paste(sprintf("{%s", collapse), paste(dQuote(names(x)), tmp, sep = ": ", collapse = sprintf(",%s", collapse)), sprintf("%s}", collapse))
                else               
                paste("[", paste(tmp, collapse = ", "), "]")
             } else
                tmp
           })


setMethod("toJSON", "factor",
           function(x, container = .level == 1L || length(x) > 1 || length(names(x)) > 0, collapse = "\n", ..., .level = 1L, .withNames = length(x) > 0 && length(names(x)) > 0) {
             toJSON(as.character(x), container, collapse, ..., .level = .level)
           })

setMethod("toJSON", "logical",
           function(x, container = .level == 1L || length(x) > 1 || length(names(x)) > 0, collapse = "\n", ..., .level = 1L) {
             tmp = ifelse(x, "true", "false")
             if(container) {
                if(.withNames)
                   paste(sprintf("{%s", collapse), paste(dQuote(names(x)), tmp, sep = ": ", collapse = sprintf(",%s", collapse)), sprintf("%s}", collapse))
                else               
                   paste("[", paste(tmp, collapse = ", "), "]")
             } else
                tmp
           })

setMethod("toJSON", "numeric",
           function(x, container = .level == 1L || length(x) > 1 || length(names(x)) > 0, collapse = "\n", digits = 5, ..., .level = 1L, .withNames = length(x) > 0 && length(names(x)) > 0) {
             tmp = formatC(x, digits = digits)
             if(container) {
                if(.withNames)
                   paste(sprintf("{%s", collapse), paste(dQuote(names(x)), tmp, sep = ": ", collapse = sprintf(",%s", collapse)),
                                   sprintf("%s}", collapse))
                else
                   paste("[", paste(tmp, collapse = ", "), "]")
             } else
               tmp
           })


setMethod("toJSON", "character",
           function(x, container = .level == 1L || length(x) > 1 || length(names(x)) > 0, collapse = "\n", digits = 5, ..., .level = 1L, .withNames = length(x) > 0 && length(names(x)) > 0) {
# Don't do this: !             tmp = gsub("\\\n", "\\\\n", x)
             tmp = x
             tmp = gsub('"', '\\\\"', tmp)
             tmp = gsub('(\\\\)', '\\1\\1', tmp)                          
             tmp = dQuote(tmp)
             if(container) {
                if(.withNames)
                   paste(sprintf("{%s", collapse),
                          paste(dQuote(names(x)), tmp, sep = ": ", collapse = sprintf(",%s", collapse)),
                         sprintf("%s}", collapse))
                else               
                   paste("[", paste(tmp, collapse = ", "), "]")
             } else
                tmp
           })



# Symbols.
setMethod("toJSON", "name",
           function(x, container = .level == 1L || length(x) > 1 || length(names(x)) > 0, collapse = "\n", ..., .level = 1L, .withNames = length(x) > 0 && length(names(x)) > 0) {
             as.character(x)
           })

setMethod("toJSON", "AsIs",
           function(x, container = length(x) > 1 || length(names(x)) > 0, collapse = "\n", ..., .level=1L, .withNames = length(x) > 0 && length(names(x)) > 0) {
              toJSON(structure(x, class = class(x)[-1]), container = TRUE, collapse = collapse, ...)
           })



setMethod("toJSON", "matrix",
           function(x, container = length(x) > 1 || length(names(x)) > 0, collapse = "\n", ...,
                    .level = 1L, .withNames = length(x) > 0 && length(names(x)) > 0) {
             tmp = paste(apply(x, 1, toJSON), collapse = sprintf(",%s", collapse))
             if(!container)
               return(tmp)

              if(.withNames)
                paste("{", paste(dQuote(names(x)), tmp, sep = ": "), "}")                
              else
                paste("[", tmp, "]")
           })

setMethod("toJSON", "list",
           function(x, container = .level == 1L || length(x) > 1 || length(names(x)) > 0, collapse = "\n", ..., .level = 1L, .withNames = length(x) > 0 && length(names(x)) > 0) {
                # Degenerate case.
             if(length(x) == 0) {
                          # x = structure(list(), names = character()) gives {}
                return(if(is.null(names(x))) "[]" else "{}")
             }
             
             els = sapply(x, toJSON, ..., .level = .level + 1L)

             if(all(sapply(els, is.name)))
               names(els) = NULL

             if(!container)
               return(els)
             
             if(.withNames)
                paste(sprintf("{%s", collapse),
                      paste(dQuote(names(x)), els, sep = ": ", collapse = sprintf(",%s", collapse)),
                      sprintf("%s}", collapse))
             else
                 paste(sprintf("[%s", collapse), paste(els, collapse = sprintf(",%s", collapse)), sprintf("%s]", collapse))
           })
