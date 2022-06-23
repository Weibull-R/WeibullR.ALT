\name{MeekerData}
\alias{MeekerData}

\title{
   Generate example data tables as presented in "Statistical Methods for Reliability Data" 
   by Bill Meeker and Luis Escobar
}
\description{
    This function generates life test data tables suitable for use in example scripts.
}
\usage{
	MeekerData(x)
}

\arguments{
    \item{x}{
        Either a character string ending with "3", "10", "13", "15" or "16" or
		a numeric of the table digits. In the book these tables were all prefixed with a "C."
	}
}

\details{
 
 Table C.3  Presents degeneration data on some carbon-film resistors.
 
 Table C.10 "Temperature-Accelerated Life Test Data for Device-A". This data is used 
 for the first (perhaps simplest) example of the analysis steps for accelerated test data. 
 Pages 494-500 in Meeker text.

 Table C.13 "Minutes to Failure of Mylar-Polyurethane Laminated DC HV Insulating Structure.
 This data is exampled by Meeker on pages 504-507. It is also discussed in earlier chapters.
 
 Table C.15 "Accelerated Life Test Data on a New-Technology Integrated Circuit Device".
 This data is exampled by Meeker on pages 508-511.
 
  Table C.16 "Temperature and Voltage-Accelerated Life Test Data for Tantalum Electrolytic Capacitors".
  This data is the basis for multiple variable acceleration exampled by Meeker on pages 513-515.
}

\value{
  The returned object is either a dataframe or a list that can be accessed by the names of the olumns or elments.
 }
 
\references{
  William Q. Meeker and Luis A. Escobar, (1998) "Statistical Methods for Reliability Data", Wiley-Interscience, New York
}

\examples{
  t10<-MeekerData("C.10")
}
