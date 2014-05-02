\name{mallet.read.dir}
\alias{mallet.read.dir}
\title{Import documents from a directory into Mallet format}
\description{
 This function takes a directory path as its only argument
 and returns a \code{data.frame()} with two columns: <id> & <text>,
 which can be passed to the \code{mallet.import} function.
 This \code{data.frame()} has as many rows as there are files in the \code{Dir}.
}
\usage{
mallet.read.dir(Dir)
}
\arguments{
\item{Dir}{The path to a directory containing one document per file.}
}
\note{This function was contributed to RMallet by Dan Bowen.}
\seealso{\code{\link{mallet.import}}}
\examples{
\dontrun{
documents <- mallet.read.dir(Dir)
mallet.instances <- mallet.import(documents$id, documents$text, "en.txt",
		    		token.regexp = "\\\\p{L}[\\\\p{L}\\\\p{P}]+\\\\p{L}")
}
}