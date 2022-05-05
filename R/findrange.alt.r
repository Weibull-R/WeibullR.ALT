findrange.alt<-function(alt)  {			
	alltimes<-NULL		
	 if(!is.null(alt$data)){		
		for(set in 1:length(alt$data)) {	
			alltimes<-c(alltimes, alt$data[[set]]$data$left, alt$data[[set]]$data$right[alt$data[[set]]$data$right>0])
		}	
		if(!is.null(alt$goal)) {	
			alltimes<-c(alltimes, alt$goal$data$right)
		}	
		if(!is.null(alt$parallel_par)) {	
			if(alt$dist=="weibull") alltimes<-c(alltimes, na.omit(alt$parallel_par$P1))
			if(alt$dist=="lognormal") alltimes<-c(alltimes, exp(na.omit(alt$parallel_par$P1)))
		}	
			
	allstress<-NULL		
		for(set in 1:length(alt$data)) {	
			allstress<-c(allstress, alt$data[[set]]$stress[1])
		}	
		if(!is.null(alt$goal)) {	
			allstress<-c(allstress, alt$goal$stress[1])
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