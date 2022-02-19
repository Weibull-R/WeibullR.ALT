#alt.data

# This function reads reliability life test data for a single stress level using the flexible input format
# also provided for WeibullR::mleframe.  Code in follow-on functions will have to identify and handle the data as it is stored
# in order to perform future processing. 


alt.data<-function(x, s=NULL, interval=NULL, stress) {						
						
	all_suspensions=FALSE					
	if (class(x) == "data.frame") {					
						
	## this test is drawn from Abrem.R					
		if(is.null(x$time) || is.null(x$event)){				
			stop(': Argument \"x\" is missing $time and/or ","$event columns...')			
		}				
						
	## verify positive time values					
		if (anyNA(x$time)) {				
			stop("NA in failure or suspension data")			
		}				
		if (any(x$time<= 0)) {				
			stop("non-positive values in failure or suspension data")			
		}				
						
		ev_info <- levels(factor(x$event))				
		if(is.null(interval) && identical(ev_info,"0"))  {				
			all_suspensions<-TRUE			
			if(!is.null(x$qty)) {			
				lrq<-data.frame(left=x$time, right=-1, qty=x$qty)		
						
						
						
						
						
			}else{			
				lrq<-data.frame(left=x$time, right=-1, qty=1)		
			}			
						
		}				
	} #close dataframe test					
						
	if(is.null(x) && is.null(interval) $$ !is.null(s) )  {					
		all_suspensions<-TRUE				
		lrq<-data.frame(left=s, right=-1, qty=1)				
	}					
	## consolidate  this lrq just like mleframe would					
	if(all_suspensions==TRUE) {					
		NDX<-order(lrq$left)				
		lrq<-lrq[NDX,]				
		if(length(unique(lrq$left)) !=  nrow(lrq)) {				
			drop_rows<-NULL			
			for(srow in nrow(lrq): 2)  {			
				if(lrq[srow,1] == lrq[srow-1,1]) {		
					drop_rows<-c(drop_rows, srow)	
					lrq[srow-1,3] <- lrq[srow-1,3] + lrq[srow,3]	
				}		
			}			
			lrq<-lrq[-drop_rows,]			
		}				
						
	}else{					
		lrq<-mleframe(x,s, interval)				
	}					
						
	obj<-list(stress = stress, data = lrq)					
	class(obj) <- "alt.data"					
						
	obj					
	}					
