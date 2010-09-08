\name{fromJSON}
\alias{fromJSON}
\alias{fromJSON,character-method}
\alias{fromJSON,connection-method}
\alias{fromJSON,AsIs-method}
\title{Convert JSON content to R objects}
\description{
  This function and its methods read content in JSON format
  and de-serializes it into R objects.
  JSON content is made up of logicals, integers, real numbers, strings,
  arrays of these and associative arrays/hash tables using \code{key:
    value} pairs.
  These map very naturally to R data types (logical, integer, numeric,
  character,  and named lists).
}
\usage{
fromJSON(content, handler = basicJSONHandler(default.size),
          default.size = 100, depth = 150L, allowComments = TRUE,
           asText = isContent(content), data = NULL, ...)
}

\arguments{
  \item{content}{the JSON content. This can be the name of a file
    or the content itself as a character string.
     We will add support for connections in the near future.
   }
   \item{handler}{an R object that is responsible for processing
     each individual token/element within the JSON content.
     This can be an R function, a list of functions with class
     \code{"JSONParserHandler"} having  \code{update} and \code{value}
     elements, or the address of a native (C) routine.
     In the case of the latter, the \code{data} parameter can be used
     to specify an object that is passed to the C routine each time it
     is called.  This will commonly be an \code{externalptr} object.
   }
  \item{default.size}{a number giving the default buffer size to use for
    arrays and objects in an effort to avoid reallocating each time we
    add a new element.}
  \item{depth}{the maximum number of nested JSON levels, i.e. arrays and
      objects within arrays and objects. }
    \item{allowComments}{a logical value indicating whether to allow
      C-style comments within the JSON content or to raise an error if
     they are encountered.}
   \item{asText}{a logical value indicating whether the value of the \code{content}
     argument should be treated as the JSON content, i.e. read directly
     rather than considered the name of a file.}
   \item{data}{a value that is only used when the value of
     \code{handler} is a native (C) routine.
     In this case, the value is passed in each call to that C routine
     by the JSON tokenizer.
   }
   \item{\dots}{additional parameters for methods.}
}

\value{
  An R object created by mapping the JSON content
  to its R equivalent.
}
\references{
  \url{http://www.json.org}
}
\author{Duncan Temple Lang <duncan@wald.ucdavis.edu}

\seealso{
  \code{\link{toJSON}}
  the non-exported collector function
  \code{{RJSONIO:::basicJSONHandler}}.  
}
\examples{

  fromJSON(I(toJSON(1:10)))

  fromJSON(I(toJSON(1:10 + .5)))

  fromJSON(I(toJSON(c(TRUE, FALSE, FALSE, TRUE))))

  x = fromJSON('{"ok":true,"id":"x123","rev":"1-1794908527"}')


   # Reading from a connection. It is a text connection so we could
   # just read the text directly, but this could be a dynamic connection.
  m = matrix(1:27, 9, 3)
  txt = toJSON(m)
  con = textConnection(txt)
  identical(m, fromJSON(con))

    # Use a connection and move the cursor ahead to skip over some lines.
  f = system.file("sampleData", "obj1.json", package = "RJSONIO")
  con = file(f, "r")
  readLines(con, 1)
  fromJSON(con)
  close(con)


  f = system.file("sampleData", "embedded.json", package = "RJSONIO")
  con = file(f, "r")
  readLines(con, 1)  # eat the first line
  fromJSON(con, maxNumLines = 4)
  close(con)

\dontrun{
if(require(rjson)) {
    # We see an approximately a factor of 3.9 speed up when we use
    # this approach that mixes C-level tokenization and an R callback
    # function to gather the results into objects.
    
  f = system.file("sampleData", "usaPolygons.as", package = "RJSONIO")
  t1 = system.time(a <- RJSONIO:::fromJSON(f))
  t2 = system.time(b <- fromJSON(paste(readLines(f), collapse = "\n")))
}
}
    # Use a C routine
  fromJSON(I("[1, 2, 3, 4]"),
           getNativeSymbolInfo("R_json_testNativeCallback", "RJSONIO"))

    # Use a C routine that populates an R integer vector with the
    # elements read from the JSON array. Note that we must ensure
    # that the array is big enough.
  fromJSON(I("[1, 2, 3, 4]"),
           getNativeSymbolInfo("R_json_IntegerArrayCallback", PACKAGE = "RJSONIO"),
           data = rep(1L, 5))

  x = fromJSON(I("[1.1, 2.2, 3.3, 4.4]"),
               getNativeSymbolInfo("R_json_RealArrayCallback", PACKAGE = "RJSONIO"),
                data = rep(1, 5))
  length(x) = 4


    # This illustrates a "specialized" handler which knows what it is
    #  expecting and pre-allocates the answer
    # This then populates the answer with the values.
    # The speed improvement is 1.8 versus "infinity"!

  x = rnorm(1000000)
  str = toJSON(x, digits = 6)
  
  fromJSON(I(str),
           getNativeSymbolInfo("R_json_RealArrayCallback", PACKAGE = "RJSONIO"),
           data = numeric(length(x)))


    # This is another example of very fast reading of specific JSON.
  x = matrix(rnorm(1000000), 1000, 1000)
  str = toJSON(x, digits = 6)
  
  v = fromJSON(I(str),
           getNativeSymbolInfo("R_json_RealArrayCallback", PACKAGE = "RJSONIO"),
           data = matrix(0, 1000, 1000))
}
\keyword{IO}
\keyword{programming}

