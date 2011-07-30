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
\alias{toJSON,AsIs-method}

\alias{emptyNamedList}

\title{Convert an R object to a string in Javascript Object Notation}
\description{
  This function and its methods convert an R object into a string
  that represents the object in Javascript Object Notation (JSON).

  The different methods try to map R's vectors to JSON arrays and
  associative arrays. There is ambiguity here as an R vector of length 1
  can be a JSON scalar or an array with one element. When there are
  names on the R vector, the descision is clearer.
  We have introduced the \code{emptyNamedList} variable to identify
  an empty list that has an empty names character vector and so
  maps to an associative array in JSON, albeit an empty one.

  Objects of class \code{AsIs} in R, i.e. that are enclosed in a call to
  \code{I()}  are treated  as containers even if they are of length 1.
  This allows callers to indicate the desired representation of an R "scalar"
  as an array of length 1 in JSON
}
\usage{
toJSON(x, container = .level == 1L || length(x) > 1 || length(names(x)) > 0, 
        collapse = "\n", ..., .level = 1L,
         .withNames = length(x) > 0 && length(names(x)) > 0)
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
  \item{collapse}{a string that is used as the separator when combining the individual lines of the 
    generated JSON content}
 \item{.level}{an integer value. This is not a parameter the caller is supposed to supply. It is a
  value that is passed in recursive calls to identify the top-level and sub-level serialization to JSON
  and so help to identify when a scalar needs to be in a container and when it is legitimate to 
  output a  scalar value directly.}
\item{.withNames}{a logical value. If we are dealing with a named
  vector/list, we typically generate a JSON associative
  array/dictionary. If there are no names, we create a simple array.
  This argument allows us to explicitly control whether we use a
  dictionary  or to ignore the names and use an array.
}
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


 toJSON(list())
 toJSON(emptyNamedList)
 toJSON(I(list("hi")))
 toJSON(I("hi"))


 x = list(list(),
          emptyNamedList,
          I(list("hi")),
          "hi",
          I("hi"))
 toJSON(x)

  # examples of specifying .withNames
 toJSON(structure(1:3, names = letters[1:3]))
 toJSON(structure(1:3, names = letters[1:3]), .withNames = FALSE)
}

\keyword{IO}
\keyword{programming}
