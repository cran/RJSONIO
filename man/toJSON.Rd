\name{toJSON}
\alias{toJSON}
\alias{toJSON,list-method}
\alias{toJSON,ANY-method}
\alias{toJSON,numeric-method}
\alias{toJSON,integer-method}
\alias{toJSON,integer,missing-method}
\alias{toJSON,character-method}
\alias{toJSON,logical-method}
\alias{toJSON,hexmode-method}
\alias{toJSON,matrix-method}
\alias{toJSON,ANY-method}
\alias{toJSON,name-method}
\alias{toJSON,list-method}
\alias{toJSON,NULL-method}
\alias{toJSON,factor-method}

\title{Convert an R object to a string in Javascript Object Notation}
\description{
  This function and its methods convert an R object into a string
   that represents the object in Javascript Object Notation (JSON).
}
\usage{
toJSON(x, container = length(x) > 1 || length(names(x)) > 0, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the R object to be converted to JSON format}
  \item{\dots}{additional arguments controlling the formatting of the
    JSON. 
  }
  \item{container}{a logical value indicating whether to treat the
    object as a vector/container or a scalar and so represent it as an
    array or primitive in JavaScript.}
}

\value{
 A string containing the JSON content.
}
\references{
\url{http://www.json.org}  
}
\author{Duncan Temple Lang <duncan@wald.ucdavis.edu>}

\seealso{
  \code{\link{fromJSON}}
}
\examples{
 toJSON(1:10)
 toJSON(rnorm(3))
 toJSON(rnorm(3), digits = 4)

 toJSON(c("Duncan", "Temple Lang"))

 toJSON(c(FALSE, FALSE, TRUE))

   # List of elements
 toJSON(list(1L, c("a", "b"), c(FALSE, FALSE, TRUE), rnorm(3)))
   # with digits controlling formatting of sub-elements
 toJSON(list(1L, c("a", "b"), c(FALSE, FALSE, TRUE), rnorm(3)),
          digits = 10)

   # nested lists
 toJSON(list(1L, c("a", "b"), list(c(FALSE, FALSE, TRUE), rnorm(3))))

   # with names
 toJSON(list(a = 1L, c("a", "b"), c(FALSE, FALSE, TRUE), rnorm(3)))

 setClass("TEMP", representation(a = "integer", xyz = "logical"))
 setClass("TEMP1", representation(one = "integer", two = "TEMP"))

 new("TEMP1", one = 1:10, two = new("TEMP", a = 4L, xyz = c(TRUE, FALSE)))
}

\keyword{IO}
\keyword{programming}
