\name{alt.fit}
\alias{alt.fit}

\title{
    Modify an \code{"alt"} Object Establishing a Accelerated Life Relationship Fit
}
\description{
    This function generates  a linear fit to the median or characteristic life estimates of the provided data sets. 
	}
\usage{alt.fit(x, omit_set=0)
	}

\arguments{
    \item{x}{
        An \code{"alt"} object as modified by \code{"alt.parallel"}.
	}
	\item{omit_set}{
		An optional vector listing any data set(s) that judged to be excluded from the accelerated life relationship determination. 
	}
}

\details{
	A linear model fit is made of the median or characteristic life estimates of applicable data sets weighted by number of failure points per each.
	
	The returned object can be plotted using S3 function \code{"plot.alt"} placing the failure points (taking intervals 
	as mean time points) in accordance with the specified alt.model relationship. The fitted relationship line will be displayed 
	as well as the default percentile \code{"c(10,90)"} lines.
	
}

\value{
	The	input object will be returned with the addition of a \code{"$alt_coef"} element.	
}

\references{

  Wayne Nelson, (1990) "Accelerated Testing", Wiley-Interscience, New York
  
  William Q. Meeker and Luis A. Escobar, (1998) "Statistical Methods for Reliability Data", Wiley-Interscience, New York
  
  Pascual F., Meeker W., Escobar L. (2006) Accelerated Life Test Models and Data Analysis. In: Pham H. (eds) Springer Handbook of Engineering Statistics. Springer Handbooks. Springer, London. https://doi.org/10.1007/978-1-84628-288-1_22
}

