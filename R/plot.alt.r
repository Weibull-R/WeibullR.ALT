## plot.alt.R
## refactored from plot.abrem code originally authored by Jurgen Symynck, April 2014
## Copyright 2014-2022 OpenReliability.org

# plot.alt provides S3 object functionality for plotting any single alt object
# with just the plot function.  
#
# For more info, visit http://www.openreliability.org/
#
# For the latest version of this file, check the Subversion repository at
# http://r-forge.r-project.org/projects/Weibull-R/
##-------------------------------------------------------------------------------
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

plot.alt <- function(x,...){

    # +---------------------------------------+
    # | unlike plot.wblr this fumction        |
    # | will only address a single alt object |
    # +---------------------------------------+
    if(!identical(class(x),"alt")) stop ("x must be an alt object")

    # +------------------------------------+
    # |  create default options arguments  |
    # +------------------------------------+

## depreciate use of log option and validate log or canvas entry	
	arg <- list(...)
		if(!is.null(arg$log)) {
		stop("The log chart option is not to be set directly, this is a function of the alt.model") 
		}
		
## currently I only handling "arrhenius" or "power"
		if(x$alt.model == "arrhenius")  {
			log="y"
		}else{
			log="xy"
		}
	
    opa <- options.alt()
    opa <- modifyList(opa, arg)
## use this label to replace ... where it appears below, remembering that this is a list now
##	dotargs<-arg	
	if(is.null(arg$main)) {		
		main<-paste0(toupper(x$alt.model)," - ", toupper(x$dist), "\n RELATIONSHIP PLOT")	
	}else{		
		main<-opa$main	
	}		

    
    # +--------------------------+
    # |  create new plot canvas  |
    # +--------------------------+
    ra <- findrange(x)	
	ylimits <- range(ra$yrange,na.rm=TRUE)					
	ylim <- c(10^(floor(log10(ylimits[1])-0.5)),					
	            10^(ceiling(log10(ylimits[2])+.5)))					
	xlimits <- range(ra$xrange,na.rm=TRUE)					
	xlim<- c(10*floor(xlimits[1]*.9/10),					
	10*ceiling(xlimits[2]*1.1/10))					
	
	if(!is.null(opa$xlim)) xlim<-opa$xlim
	if(!is.null(opa$ylim)) ylim<-opa$ylim
					
	 plotargs <- c(list(x=NA,axes=FALSE),					
		list(xlim=xlim, ylim=ylim, log=log, 				
		xlab=opa$xlab, ylab=opa$ylab, main=main))				
	#dev.new(width=5, height=7)					
	do.call(plot.default,plotargs)					
	# draw the gridlines					
	abline(					
		v=pretty(plotargs$xlim,10),				
		h=seq.log(plotargs$ylim[1]/10,plotargs$ylim[2]*10,seq(0,10,1)),				
		col="grey"				
	)					
						
	if(opa$is.plot.grid == TRUE) {					
	# left side axis tickmarks and labels					
		 r <- seq.log(plotargs$ylim[1]/10,plotargs$ylim[2]*10,c(1,5))				
		left<-2				
		axis(left,at=seq.log(plotargs$ylim[1]/10,plotargs$ylim[2]*10,seq(0,10,0.2)),				
		            labels=NA,tcl=-0.25)				
		axis(left,at=r,labels=r,tcl=-0.75)				
	# bottom axis tickmarks and labels					
		r<-pretty(plotargs$xlim,10)				
		bottom<-1				
		axis(bottom,at=r,				
		            labels=NA,tcl=-0.25)				
		axis(bottom,at=r,labels=r,tcl=-0.75)				
	}					
						
	# plot the data points, jittering horizontally as needed					
	p2p<-points2plot(x, opa$points.jf)					
	points(p2p$stress, p2p$time)					
#browser()						
	# plot the goal, if it exists					
	if(!is.null(x$goal) && opa$is.plot.goal==TRUE) {					
		points(x$goal$stress, x$goal$data$left,  				
			pch=opa$goal.pch,			
			cex=opa$goal.cex,			
			col=opa$goal.col,			
			lwd=opa$goal.lwd)			
	}					

if(!is.null(x$parallel_par)) {				
	# plot the parallel parameter points					
	if(opa$is.plot.parallel_params==TRUE)  {
		if(x$dist=="lognormal") P1times<-exp(x$parallel_par$P1)
		if(x$dist=="weibull") P1times<-x$parallel_par$P1		
	points(x$parallel_par$stress, P1times, 					
		pch=opa$parallel_params.pch, 				
		cex=opa$parallel_params.cex, 				
		col=opa$parallel_params.col )				
	}					

if(!is.null(x$alt_coef)) {						
	# plot the fit line					
	if(opa$is.plot.fit==TRUE)  {					
		if(log=="y")  {				
			y<-exp(x$alt_coef[2]*xlim+x$alt_coef[1])			
		}				
		if(log=="xy")  {				
			y<-exp(x$alt_coef[2]*log(xlim)+x$alt_coef[1])			
		}				
		points(xlim, y, type=opa$fit.type,				
			lwd = opa$fit.lwd,			
			 col=opa$fit.col)			
	}					
						
	if(opa$is.plot.percentiles==TRUE)  {			
			## log offsets for the percentiles are simply based on the parallelP2	
			## which is always stored in the alt object as parallel_par[1]	
			if(x$dist=="lognormal")  {	
				quants<-qlnorm(opa$percentiles/100, 0, x$parallel_par$P2[1])
			}	
			if(x$dist=="weibull")  {	
				quants<-qweibull(opa$percentiles/100, x$parallel_par$P2[1], 1)
			}	
			log_offsets<-log(quants)	
				
		for(percentile in 1:length(opa$percentiles))  {		
			yvec<-exp(log(y) + log_offsets[percentile])	
			points(xlim, yvec, type="l", 	
				lty=opa$percentile.lty,
				lwd=opa$percentile.lwd,
				col=opa$percentile.col)
		}

	le<-NULL; col<-NULL; lty<-NULL; cex<-NULL; lwd<-NULL		
			
	#sort percentiles decreasing		
	percentiles<-sort(opa$percentiles, decreasing=TRUE)		
	high_percentiles<-percentiles[which(percentiles>50)]	
	low_percentiles<-percentiles[which(percentiles<50)]		
			
			
	for(percent in 1:length(high_percentiles)) {		
		le<-c(le, paste0( "percentile ",high_percentiles[percent]))	
		col<-c(col,"blue")	
		lty<-c(lty, 1)	
		lwd<-c(lwd,2)
		cex<-c(cex, .8)
	}		
	if(x$dist == "weibull") {		
		le<-c(le, "percentile 63.2")	
		col<-c(col,"red")	
		lty<-c(lty, 1)	
		lwd<-c(lwd,2)
		cex<-c(cex, .8)	
	}		
	if(x$dist == "lognormal") {		
		le<-c(le, " percentile 50")	
		col<-c(col,"red")	
		lty<-c(lty, 1)	
		lwd<-c(lwd,2)
		cex<-c(cex, .8)
	}		
	for(percent in 1:length(low_percentiles)) {		
		le<-c(le, paste0( "percentile ",low_percentiles[percent]))	
		col<-c(col,"blue")	
		lty<-c(lty, 1)	
		lwd<-c(lwd,2)
		cex<-c(cex, .8)	
	}		
			
	legend("topright", inset=0.05, legend=le,		
	       col=col, lty=lty, lwd=lwd, cex=cex, bg="white")		
		
	} #Close is.plot,percentiles			


} #close fit test
} #close parallel test
						
}						
		
		
		
		
			
		
findrange<-function(alt)  {			
	alltimes<-NULL		
	 if(!is.null(alt$data)){		
		for(set in 1:length(alt$data)) {	
			alltimes<-c(alltimes, alt$data[[set]]$data$left, alt$data[[set]]$data$right[alt$data[[set]]$data$right>0])
		}	
		if(!is.null(alt$goal)) {	
			alltimes<-c(alltimes, alt$goal$data$right)
		}	
			
	allstress<-NULL		
		for(set in 1:length(alt$data)) {	
			allstress<-c(allstress, alt$data[[set]]$stress)
		}	
		if(!is.null(alt$goal)) {	
			allstress<-c(allstress, alt$goal$stress)
		}
		zero_positions<-which(alltimes==0)	
		if(length(zero_positions>0)) {	
			alltimes<-alltimes[-which(alltimes==0)]
			alltimes<-c(alltimes, min(alltimes)/2)
		}
		
	}		
			
	if(!is.null(alltimes)){		
		ret <- data.frame(yrange=range(alltimes))	
	            }else{		
	                stop("no time data, cannot create plot canvas.")		
	            }		
			
	if(!is.null(allstress)){		
		 ret <- cbind(ret,xrange=range(allstress))	
	            }else{		
		 stop("no stress data, cannot create plot canvas.")	
	            }			
	ret		
}			
			
			
seq.log <- function(from,to,base=c(1,2,5)){			
   r <- NULL			
   for(i in floor(log10(from)):floor(log10(to)))r <- c(r,base*10^i)			
   r[r >= from & r<=to]}			

points2plot<-function(x, jf=.001)  {				
	stress<-NULL			
	time<-NULL			
	for(set in 1:length(x$data) )  {			
		for(li in 1:nrow(x$data[[set]]$data))  {				
			if(x$data[[set]]$data$qty[li] ==1)  {			
				## excluding suspensions		
				if(  x$data[[set]]$data$right[li] - x$data[[set]]$data$left[li] > 0 )  {		
					time<-c(time, (x$data[[set]]$data$left[li] + x$data[[set]]$data$right[li])/2)	
					stress<-c(stress, x$data[[set]]$stress)	
				}		
				if(  x$data[[set]]$data$right[li] - x$data[[set]]$data$left[li] == 0 )  {		
					time<-c(time, x$data[[set]]$data$left[li])	
					stress<-c(stress, x$data[[set]]$stress)	
				}		
						
			}			
			# excluding lines with zero qty			
			if(x$data[[set]]$data$qty[li] >1)  {			
				## excluding suspensions		
				if(  x$data[[set]]$data$right[li] - x$data[[set]]$data$left[li] > 0 )  {		
					time<-c(time, rep((x$data[[set]]$data$left[li] + x$data[[set]]$data$right[li])/2, x$data[[set]]$data$qty[li]))	
					stress<-c(stress, qjitter(x$data[[set]]$stress,  x$data[[set]]$data$qty[li], jf))	
				}		
				if(  x$data[[set]]$data$right[li] - x$data[[set]]$data$left[li] == 0 )  {		
					time<-c(time, rep(x$data[[set]]$data$left[li], x$data[[set]]$data$qty[li]))	
					stress<-c(stress, qjitter(x$data[[set]]$stress,  x$data[[set]]$data$qty[li], jf))	
				}		
						
			}			
		}
	}			
	p2p<-data.frame(time, stress)			
	p2p 			
}


qjitter<-function(x,q, jf=.001)  {		
	outvec<-NULL	
	for(n in q:1)  {	
		jit<-(n-1)*jf
		outvec<-c(outvec, x*(1+jit))
		jf<-jf*-1
	}	
	outvec	
}
