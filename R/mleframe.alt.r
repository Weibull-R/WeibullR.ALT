
mleframe.alt<-function(x, s=NULL, interval=NULL)  {		
		
		retx<-evaluate_x_arg(x)
	# evaluate_s_arg might need to know of suspension data in retx	
	# regardless, rets is the combined ouitput so far	
		rets<-evaluate_s_arg(retx,s)
	# evaluate_interval_arg might need to know of failure or suspension data in rets	
	# regardless, reti is the combined ouitput so far	
		reti<-evaluate_interval_arg(rets, interval)
	# now return a single dataframe with non-null items	
	# it will be a problem if rbind fails on NULL items	
		ret<-rbind(reti$failures, reti$suspensions, reti$intervals)
		row.names(ret)<-seq(1:nrow(ret))
	ret	
}		

evaluate_x_arg<-function(x)  {								
	ret<-NULL
	failures<-NULL		
	suspensions<-NULL							
	if(!is.null(x))  {							
	## here a time-event dataframe can be evaluated, if provided as x							
	## This is the support for a time-event dataframe							
		if (is(x, "data.frame")) {						
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
	## verify 1's or 0's only in event							
	## using Jurgen's validation code modified now for possibility of suspensions only in x							
			event_column_ok=FALSE					
			ev_info <- levels(factor(x$event))					
			if(identical(ev_info,c("0","1")) || identical(ev_info,"1") || identical(ev_info,"0")){					
				event_column_ok=TRUE				
			}					
			if(!event_column_ok) {					
				stop("event column contains other than '1' or '0' ")				
			}					
								
	## Let's be sure the qty field, if present, is all integer, else future havoc could ensue							
			if(!is.null(x$qty)) {					
				if(any(!is.integer(x$qty))) x$qty<-ceiling(x$qty)
	## eliminate any rows with non-positive qty				
				drop_rows<-which(x$qty<=0)	
				if(length(drop_rows)>0) { 
					x<-x[-drop_rows,]	
					warning("non-positive qty data eliminated")
				}
			}					
								
##  test that exact failures indeed exist								
			if("1" %in% ev_info) {					
				if(is.null(x$qty)) {				
					fail_vec<-x$time[x$event==1]			
					fdf<-as.data.frame(table(fail_vec))			
					ft<-as.numeric(levels(fdf[,1]))			
					fq<-fdf[,2]			
					failures<-data.frame(left=ft, right=ft, qty=fq)			
				}else{				
					f<-x$time[x$event==1]			
					failures <- data.frame(left = f, right = f, qty = x$qty[x$event==1])			
			# sort failure data according to default decreasing=FALSE					
					NDX<-order(failures$left)				
					failures<-failures[NDX,]			
			## Cannot assume that data input with a qty field is appropriately  consolidated					
					if(length(unique(failures$left)) !=  nrow(failures)) {			
						drop_rows<-NULL		
						for(frow in nrow(failures): 2)  {		
							if(failures[frow,1] == failures[frow-1,1]) {	
								drop_rows<-c(drop_rows, frow)
								failures[frow-1,3] <- failures[frow-1,3] + failures[frow,3]
							}	
						}		
						failures<-failures[-drop_rows,]		
					}			
				}				
			}					
## Test whether suspensions exist								
			if("0" %in% ev_info) {					
				if(is.null(x$qty)) {				
					susp_vec<-x$time[x$event==0]			
					sdf<-as.data.frame(table(susp_vec))			
					st<-as.numeric(levels(sdf[,1]))			
					sq<-sdf[,2]			
					suspensions<-data.frame(left=st, right=-1, qty=sq)			
				}else{				
					s<-x$time[x$event==0]			
					suspensions <- data.frame(left = s, right = -1, qty = x$qty[x$event==0])			
			# sort suspension data according to default decreasing=FALSE					
					NDX<-order(suspensions$left)		
					suspensions<-suspensions[NDX,]			
			## Cannot assume that data input with a qty field is appropriately  consolidated					
					if(length(unique(suspensions$left)) !=  nrow(suspensions)) {			
						drop_rows<-NULL		
						for(srow in nrow(suspensions): 2)  {		
							if(suspensions[srow,1] == suspensions[srow-1,1]) {	
								drop_rows<-c(drop_rows, srow)
								suspensions[srow-1,3] <- suspensions[srow-1,3] + suspensions[srow,3]
							}	
						}		
						suspensions<-suspensions[-drop_rows,]		
					}			
				}				
			}					
		}else if(is.vector(x))  {						
			if(anyNA(x))  {					
			stop("NA in failure data")					
			}					
			if(any(x<=0))  {					
			stop("non-positive values in failure/occurrence data")					
			}					
			fail_vec<-sort(x)					
			fdf<-as.data.frame(table(fail_vec))					
			ft<-as.numeric(levels(fdf[,1]))					
			fq<-fdf[,2]					
			failures<-data.frame(left=ft, right=ft, qty=fq)					
		}else{						
			if (length(x) > 0) {					
				stop("error in x argument type")				
			}					
		}						
		ret<-list(failures=failures,suspensions=suspensions)						
	}							
	ret							
}								

evaluate_s_arg<-function(retx,s) {
# if s is NULL this function simply returns retx
	if(length(s)>0)  {
		if(anyNA(s))  {
		stop("NA  in suspension data")
		}
		if(any(s<=0))  {
		stop("non-positive values in suspension data")
		}
		# sort suspension  data according to default decreasing=FALSE
		susp_vec<-sort(s)
		sdf<-as.data.frame(table(susp_vec))
		st<-as.numeric(levels(sdf[,1]))
		sq<-sdf[,2]
		suspensions<-data.frame(left=st, right=-1, qty=sq)


		## test for any suspension data in retx
		if(!is.null(retx$suspensions)) {
			##If(any(retx$right<0)) {
			warning("suspensions detected in both x and s arguments")
		## need to split out failures and suspensions for extra processing
				failures<-retx$failures
				suspinx<-retx$suspensions
				suspensions<-rbind(suspensions, suspinx)
		# sort suspension  data according to default decreasing=FALSE
				NDX<-order(suspensions$left)
				suspensions<-suspensions[NDX,]
		## Cannot assume that data input with a qty field is appropriately  consolidated
				if(length(unique(suspensions$left)) !=  nrow(suspensions)) {
					drop_rows<-NULL
					for(srow in nrow(suspensions): 2)  {
						if(suspensions[srow,1] == suspensions[srow-1,1]) {
							drop_rows<-c(drop_rows, srow)
							suspensions[srow-1,3] <- suspensions[srow-1,3] + suspensions[srow,3]
						}
					}
					suspensions<-suspensions[-drop_rows,]
				}
			ret<-list(failures=failures, suspensions=suspensions)
		}else{
			ret<-list(failures=retx$failures, suspensions=suspensions)
		}
	}else{
		ret<-retx
	}
	ret
}

evaluate_interval_arg<-function(rets, interval)  {
# if interval is NULL this function simply returns rets
if(!is.null(interval)) {
	fail_intervals<-NULL
	susp_intervals<-NULL
## interval dataframe validation
	colname_error<-FALSE
	if(is(interval, "data.frame"))  {
## test names in first two columns
		test_names<-names(interval)
			if(test_names[1] !="left") {
				stop("'left' column name error in interval dataframe object")
			}
			if(test_names[2] !="right") {
				stop("'right' column name error in interval dataframe object")
			}
## test qty column name
		if(ncol(interval)>2)  {
			if(test_names[3] != "qty")  {
				stop("'qty' column name error in interval dataframe object")
			}
		}
## assure no extraneous columns exist in interval
		if(!(ncol(interval)==2 || ncol(interval)==3))  {
			stop("extraneous columns in interval argument")
		}

## additional validations on interval argument, such as positive numeric checking
## removal of potential na's, etc. could take place here
		if(anyNA(interval))  {
		stop("NA not handled in interval data")
		}

## Let's be sure the qty field, if present, is all integer, else future havoc could ensue
## by handling this here even fail or suspension data are covered, if present
		if(!is.null(interval$qty)) {
			if(any(!is.integer(interval$qty))) interval$qty<-ceiling(interval$qty)
## eliminate any rows with non-positive qty
			drop_rows<-which(interval$qty<=0)
			if(length(drop_rows)>0) {
				interval<-interval[-drop_rows,]
				warning("non-positive qty data eliminated from interval")
			}
		}

## initialize possible interval categories			
		fail_intervals<-NULL	
		susp_intervals<-NULL	
		true_intervals<-interval[which(interval$right-interval$left>0),]	
		if(any((interval$right-interval$left)<=0))  {
			fail_intervals<-interval[which(interval$right==interval$left),]
			susp_intervals<-interval[which(interval$right-interval$left<0),]		
			if(any(susp_intervals$right>0)) {	
				stop("error in interval data, right less than left, but not less than or equal 0")
			}else{	
				susp_intervals$right<-rep(-1, nrow(susp_intervals))
			}
		}
		
## test whether any true_intervals were found
		if(nrow(true_intervals)>0) {
			interval<-true_intervals
## Now procede with original interval handler code
## add qty column if not provided
			if(ncol(interval)<3)  {

				ivalchar<- apply(interval,2,as.character)
				ivalstr<-paste0(ivalchar[,1],"_",ivalchar[,2])
				ivaldf<-as.data.frame(table(ivalstr))
				ivalstr2<-as.character(levels(ivaldf[,1]))
## much done here, but this returns the tabled left and right columns
## in a dataframe with rows corresponding to the tabled quantities
				lrdf<-data.frame(
					matrix(
						as.numeric(
							unlist(
								strsplit(ivalstr2,"_")
							)
						)
					,ncol=2, byrow=T
					)
				)
## now just complete the consolidation of duplicates in the interval dataframe
				intervals<-cbind(lrdf,ivaldf[,2])
				names(intervals)<-c("left","right","qty")

				# interval<- cbind(interval, qty=c(rep(1,nrow(interval))))
			} else{
## here is the place to process true_intervals (as interval) for duplicate entries
				intervals<-interval
## sort to facilitate to assure uniform presentation and consolidation of any duplicated entries
##  only required for dataframe with qty field
				NDX<-order(intervals$left,intervals$right)
				intervals<-intervals[NDX,]

				interval_test<-paste(as.character(intervals$left), as.character(intervals$right))
				if(length(unique(interval_test)) < length(interval_test))  {
					drop_rows<-NULL
					for(frow in nrow(intervals): 2)  {
						if((intervals[frow,1] == intervals[frow-1,1])  && (intervals[frow,2] == intervals[frow-1,2])) {
							drop_rows<-c(drop_rows, frow)
							intervals[frow-1,3] <- intervals[frow-1,3] + intervals[frow,3]
						}
					}
					intervals<-intervals[-drop_rows,]
				}
			}
		}else{
## this completes the test that no true_intervals were found, setting output intervals to NULL
			intervals<-NULL
		}

## finally, reject any object type that is not dataframe (we are within a test for !is.null)
	}else{
		#if(!is.null(interval))  {
			stop("error in interval argument type")
		#}
	}



## test whether fail_intervals were found
	if(!is.null(nrow(fail_intervals))) {
	if(nrow(fail_intervals)>0) {
## this code will rarely ever be called
## add qty column if not provided
		if(ncol(fail_intervals)<3)  {
			if(nrow(fail_intervals)>1)  {						
				ivalchar<- apply(fail_intervals,2,as.character)					
				 ivalstr<-paste0(ivalchar[,1],"_",ivalchar[,2])					
				ivaldf<-as.data.frame(table(ivalstr))					
				ivalstr2<-as.character(levels(ivaldf[,1]))					
## much done here, but this returns the tabled left and right columns									
## in a dataframe with rows corresponding to the tabled quantities									
				lrdf<-data.frame(					
					matrix(				
						as.numeric(			
							unlist(		
								strsplit(ivalstr2,"_")	
							)		
						)			
					,ncol=2, byrow=T				
					)				
				)					
				fail_intervals<-cbind(lrdf,ivaldf[,2])					
				names(fail_intervals)<-c("left","right","qty")					
			}else{						
				fail_intervals<-cbind(fail_intervals, qty=1)					
			}
		}

		## actually rets$failures could be NULL and ignored by rbind
			failures<-rbind(fail_intervals, rets$failures)

# sort failure data according to default decreasing=FALSE
			NDX<-order(failures$left)
			failures<-failures[NDX,]
## Cannot assume that data input with a qty field is appropriately  consolidated
			if(length(unique(failures$left)) !=  nrow(failures)) {
				drop_rows<-NULL
				for(frow in nrow(failures): 2)  {
					if(failures[frow,1] == failures[frow-1,1]) {
						drop_rows<-c(drop_rows, frow)
						failures[frow-1,3] <- failures[frow-1,3] + failures[frow,3]
					}
				}
				failures<-failures[-drop_rows,]
			}


	}else{
## since intervals have been defined, must prepare for return of faiilures already found
		failures<-rets$failures
	}
	} # close the test for no fail_intervals found 

	if(!is.null(nrow(susp_intervals))) {
	if(nrow(susp_intervals)>0) {
## this code will rarely ever be called
## add qty column if not provided
		if(ncol(susp_intervals)<3)  {
			if(nrow(susp_intervals)>1)  {					
				ivalchar<- apply(susp_intervals,2,as.character)				
				ivalstr<-paste0(ivalchar[,1],"_",ivalchar[,2])				
				ivaldf<-as.data.frame(table(ivalstr))				
				ivalstr2<-as.character(levels(ivaldf[,1]))				
## much done here, but this returns the tabled left and right columns								
## in a dataframe with rows corresponding to the tabled quantities								
				lrdf<-data.frame(				
					matrix(			
						as.numeric(		
							unlist(	
								strsplit(ivalstr2,"_")
							)	
						)		
					,ncol=2, byrow=T			
					)			
				)				
				susp_intervals<-cbind(lrdf,ivaldf[,2])				
				names(susp_intervals)<-c("left","right","qty")				
			}else{					
				susp_intervals<-cbind(susp_intervals, qty=1)				
			}					

		}

			## actually rets$suspensions could be NULL and will be ignored by rbind
				suspensions<-rbind(susp_intervals, rets$suspensions)

# sort failure data according to default decreasing=FALSE
				NDX<-order(suspensions$left)
				suspensions<-suspensions[NDX,]
## Cannot assume that data input with a qty field is appropriately  consolidated
				if(length(unique(suspensions$left)) !=  nrow(suspensions)) {
					drop_rows<-NULL
					for(frow in nrow(suspensions): 2)  {
						if(suspensions[frow,1] == suspensions[frow-1,1]) {
							drop_rows<-c(drop_rows, frow)
							suspensions[frow-1,3] <- suspensions[frow-1,3] + suspensions[frow,3]
						}
					}
					suspensions<-suspensions[-drop_rows,]
				}

	}else{
## since intervals have been defined, must prepare for return of suspensions already found
		suspensions<-rets$suspensions
	}
	} #close the test for no susp_intervals found

if(!exists("failures")) {
		## actually rets$failures could be NULL and ignored by rbind
			failures<-rbind(fail_intervals, rets$failures)
}
if(!exists("suspensions")) {
		## actually rets$suspensions could be NULL and will be ignored by rbind
			suspensions<-rbind(susp_intervals, rets$suspensions)
}
	ret<-list(failures=failures, suspensions=suspensions, intervals=intervals)

}else{
## the interval argument is NULL so return object will be rets
	ret<-list(failures=rets$failures, suspensions=rets$suspensions, intervals=NULL)
}


ret
}