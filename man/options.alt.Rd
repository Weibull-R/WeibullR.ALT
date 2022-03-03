\name{options.alt}
\alias{options.alt}

\title{
    Options list for \code{alt} Objects
}
\description{
    This function handles the various plotting options for alt objects.
}
\usage{options.alt(\dots)}
\arguments{
    \item{\dots}{
        Options for plotting \code{alt} objects using the \pkg{WeibullR}
        package.
    }
}

\details{
    As a function, options.alt, borrows its internal structure from
    the \code{\link[graphics]{par}} function of package \pkg{graphics}.

	Unlike its corresponding function in WeibullR \code{options.wblr} this function
	is intended to only be called within \code{plot.alt}.

}

\value{
   Executing \code{options.alt} without arguments returns a named list
   containing the default options for plotting a \code{alt} object.\cr
   Should arguments be supplied, these are returned in a named list.
}
