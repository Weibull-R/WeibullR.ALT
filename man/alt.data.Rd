\name{alt.data}
\alias{alt.data}

\title{
    Create a \code{alt.data} Object for Accelerated Life (Failure) Test Data
}
\description{
    This function reads life test data for a single stress level using the flexible input format
	also provided for \code{"WeibullR::mleframe"} to create an \code{"alt.data"} object.
	}
\usage{alt.data(x, s=NULL, interval=NULL, stress) }		

\arguments{
    \item{x}{
        Either a dataframe containing at least \code{$time} and \code{$event}
        columns and optionally a \code{$qty} column, or a vector of class \code{"numeric"} or \code{"integer"} with
        (life-)time observations.
	}
	\item{s}{
		An optional vector of right-censored data, or suspensions. 
	}
	\item{interval}{
		An optional dataframe of interval data having columns specifically named "left" and "right". 
		Left values are the last time at which no failure was evident and may be zero for discovery.
		Right values are the earliest time at which failure was observed.
	}
	\item{stress}{
		A scalar value representing the accelerating stress applied during a life test.
	}
}

\details{
    There are several methods to passing arguments for building an \code{alt.data}
    object.
    \itemize{
        \item If argument \code{x} is of class \code{"data.frame"},
            then it must contain \code{$time} and \code{$event}
            columns. Additional columns in the dataframe will be ignored.
        \item When a single unnamed vector of class \code{"numeric"}
            or \code{"integer"} is supplied, it is treated as a vector
            of (life-)time observations.
        \item If argument \code{time} or \code{fail} is provided, it is treated as
            a vector of (life-)time observations. Take care NOT to supply both
            \code{time} and \code{fail} in the same function call.
        \item If argument \code{event} is provided, it is treated as
            a vector of event indicators with possible values of
            \code{0} and \code{1}. See section "Value" for more details on
            event vectors.
        \item If the \code{x} argument is not provided as a dataframe and \code{susp} is provided, 
			it is treated as a vector of right-censored (life-)time observations 
            (also called suspended observations or suspensions).
    }
	While \code{"WeibullR::mleframe"} is used for data sets with 3 or more failures (including intervals)
	alt.data will also prepare conforming dataframe for sets with less failuers or even no failures (ie. all censored items)
}

\value{
    A named list of class \code{"alt.data"}. The first list
    item (\code{$data}) is a list with up to least three items:
    \describe{
        \item{\code{$stress}}{
            A scalar value representing the accelerating stress applied during a life test.
        }
        \item{\code{$data}}{
            A dataframe containing the provided data formatted with \code{"left"}, \code{"right"}, and \code{"qty"} columns.
			This conforms with the output of function \code{"WeibullR::mleframe"}.
        }
    }
}

\references{

  Wayne Nelson, (1990) "Accelerated Testing", Wiley-Interscience, New York
  
  William Q. Meeker and Luis A. Escobar, (1998) "Statistical Methods for Reliability Data", Wiley-Interscience, New York
}

\examples{
  table2.2<-NelsonData("table2.2")
  alt_da190<-alt.data(table2.2$C190, stress=190)
  
  ClassH_goal<-alt.data(20000, stress=180)
}
