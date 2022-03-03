\name{NelsonData}
\alias{NelsonData}

\title{
   Generate example data tables as presented in "Accelerated Testing" by Wayne Nelson
}
\description{
    This function generates life test data tables suitable for use in example scripts.
}
\usage{
	NelsonDaata(x)
}

\arguments{
    \item{x}{
        Either a character string defining one of "table2.2", "table3.1", "table4.1" or "table5.1 or
		a numeric of the table digits.
	}
}

\details{
 This method of data provision has been found necessary since inclusion of table3.1 and table5.1
 created a problem with a "Warning: object '.Random.seed' is created by more than one data call"
 which prevented lazyData=yes to be in the package definition.  Without lazyData I could never seem
 to make \data files avialable to example scripts within the package.
 
 Table 2.2 is presented on page115 of Wayne Nelson's book "Accelerated Testing".
 It purports to represent a series tests on  a Class H insulation at elevated temperatures (in deg C)
 
 Table 3.1 is presented on page129 of Wayne Nelson's book "Accelerated Testing".
 It purports to represent a series tests on an insulating fluid at elevated voltages.
 
 Table 4.1 is presented on page135 of Wayne Nelson's book "Accelerated Testing".
 It purports to represent a series tests on  a Class B insulation at elevated temperatures (in deg C)
 
 Table 5.1 is presented on page 393 of Wayne Nelson's book "Accelerated Testing".
 It is interesting to note that items $T190, $T220, $T240, and $T260 represent
 a Table 5.1 presented  on page 140. In Chapter 7 this data forms an example of 
 competing modes analysis covering modes of failure on motorette turns, phase, and ground.
}

\value{
  The returned object is either a dataframe or a list that can be accessed by the names of the olumns or elments.
 }
 
\references{
  Wayne Nelson, (1990) "Accelerated Testing", Wiley-Interscience, New York
}

\examples{
  table4.1<-NelsonData("table4.1")
}
