\name{plot.alt}
\alias{plot.alt}

\title{
    S3 \code{alt} Object Plotting on pretty canvax
}
\description{
    This function adds the \code{.alt} method to \code{\link[graphics]{plot.default}}
    from the \pkg{graphics} package.

    Currently, the function plots the failure time observations at designated stress levels
    from the $data list item a alt object on a relationship plot based on the $alt.model list item.

}
\usage{\method{plot}{alt}(x, \dots)}
\arguments{
    \item{x}{
        Object of class \code{"alt"}.
    }
    \item{\dots}{
        Options for plotting the \code{alt} object; see section "Details".
    }
}
\details{
    The \code{\dots} argument can be any graphical parameter that can be
    supplied to \code{\link[graphics]{plot.default}}, and any option that can be
    set by the function \code{\link{options.alt}}. The options set
    in this way are applied to all graphical elements of the plot, overriding
    any of the following default options:\cr

		dev.width=5,
		dev.height=7,
		xlab= "Stress",  ## expression(paste("Temperature ",degree,"C"))    ## "Voltage in kV"
		ylab= "Time To Failure",    ##"HOURS", "MINUTES"
		
		is.plot.grid=TRUE,
		col.grid="gray",
		
		## unknown effect of these controls
		coordinate.text.size=.7,
		mar=c(5.1,4.1,5.1,2.1),
		
		
		points.col="black",
		points.pch=21,
		points.lwd=2,
		points.cex=1,
		points.jf= .003,
		
		interval.col="black",
		interval.lty="dashed",
		interval.lwd=1,
		
		fit.type = "l",
		fit.lwd=2,
		fit.col="red",
		is.plot.fit_points=TRUE,
		
		parallel_params.pch=4,
		parallel_params.cex=2,
		parallel_params.col="red",
		is.plot.parallel_params=FALSE,
		
		is.plot.fit=TRUE,
		fit.lty="solid",
		fit.col="red",
		fit.lwd=2,
		
		is.plot.goal=TRUE,
		goal.pch=8,
		goal.cex=2,
		goal.col="orange",
		goal.lwd=2,
		
		
		percentiles = c(10,90), ## percentiles are no longer an arguement to alt.fit
		is.plot.percentiles=TRUE,
		percentile.col="blue",
		percentile.lty="solid",
		percentile.lwd=2,
		
		# something about percent label?
		
		persistent=TRUE	
}
	
\value{
    Currently, the function returns no value.
}