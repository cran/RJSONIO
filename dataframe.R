require(RJSONIO)
DF <- head(cars)
cat(toJSON(DF))
is.na(DF[1,1]) <- TRUE
DF[1,1] <- NA
cat(toJSON(DF))

outDF = toJSON(DF)
x <- fromJSON(content = outDF, asText = TRUE, simplify = TRUE, nullValue = NA)
as.data.frame(x)

#origDF <- as.data.frame(lapply(reconstruction, "unlist"))
