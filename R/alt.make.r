# alt.make.r

# simple initiation of an alt object using data, expected distribution, and ALT model (along with optional goal data)
# error checking is performed on all input. This newly formed object is plot-able using the s3 registered plot function
# or plot.alt (particularly during developement prior to s3 registration.)

alt.make<-function(x, dist, alt.model, method.fit="mle-rba", goal=NULL, set_validation=NULL, view_dist_fits=TRUE) {	
	
	obj<-list()		
	# x must be a list of alt.data objects		
	for(da in 1:length(x) )  {		
		if(class(x[[da]])!="alt.data") stop("x is not a list of alt.data objects")	
	}		
	obj$data<-x		
			
	## dist must be either "lognormal" or "weibull"		
	if(!dist %in% c("lognormal", "weibull")) stop("dist not recognized")		
	obj$dist<-dist		
		 	
	## alt.model must be either "arrhenius" or "power"		
	if(!alt.model %in% c( "arrhenius", "power")) stop("alt.model not recognized")		
	obj$alt.model<-alt.model		
			
	## if goal is not NULL it must be of class "alt.data"		
	if(! is.null(goal))  {		
		if(!class(goal)=="alt.data") stop("goal must be provided as an alt.data object")	
		obj$goal<-goal	
	}		

valid<-list(fail_points=2, num_fails=3, fail_range=.03)
if(!is.null(set_validation)) valid<-modifyList(valid, set_validation)

valid_sets<-0
	
## need to establislh number of failures or any intervals in this data
## set once and save for quick checking in later processing			
	for(set in 1:length(obj$data))  {
		intervals_present<-FALSE
		fail_medians<-NULL
		fail_points<-0
		Nf<-0	
		fail_range<-0
		for(row in 1:nrow(obj$data[[set]]$data))  {	
			if(obj$data[[set]]$data$right[row]>0) { 	
				Nf<-Nf+obj$data[[set]]$data$qty[row]
				fail_medians<-c(fail_medians, (obj$data[[set]]$data$left + obj$data[[set]]$data$right)/2) 
			}
			if((obj$data[[set]]$data$right[row]-obj$data[[set]]$data$left[row])>0) intervals_present<-TRUE
		}
		
		obj$data[[set]]$num_fails<-Nf
		if(Nf > 0) {	
			fail_points<-length(unique(fail_medians))
			fail_range<-(max(fail_medians)-min(fail_medians))/max(fail_medians)
		}	
			
		if(fail_points<valid$fail_points || Nf<valid$num_fails || fail_range<valid$fail_range) {	
			obj$data[[set]]$valid_set<-FALSE
		}else{	
			obj$data[[set]]$valid_set<-TRUE
			valid_sets<-valid_sets + 1
		}
	}

	if(intervals_present==TRUE && (!method.fit %in% c("mle", "mle-rba", "mle-unbias"))) {
		warning("method.fit altered to 'mle-rba' due to intervals detected")
		obj$method.fit<-"mle-rba"	
	}else{
		obj$method.fit<-method.fit
	}
	
## will continue to populate the alt object, perhaps with additional alt.xxx functions			
	class(obj) <- "alt"	
	if(valid_sets>0) {		
		if(view_dist_fits==TRUE) view_dist_fits(obj)	
	}else{		
		warning("no valid sets in this alt object")	
	}		
			
## each set now has a valid_set element holding logical TRUE/FALSE for future testing			
if(valid_sets<2) warning("insufficient data for accelerated life relationship")			

	obj		
}			
		
view_dist_fits<-function(x)  {								
	# must confirm x is an alt object							
	if(class(x)!="alt") stop("x is not an alt object")							
								
	wblr_list<-list()							
	colors<-c("blue", "darkgreen", "red", "purple", "darkseagreen", "chocolate",  "lightblue4", "indianred")							
								
	ignore_slope<-0							
	for(set in 1:length(x$data))  {							
		if(x$data[[set]]$num_fails>2)  {						
			fa<-NULL					
			su<-NULL					
								
			le<-NULL					
			ri<-NULL					
			interval<-NULL					
								
								
			for(li in 1:nrow(x$data[[set]]$data))  {					
				if(x$data[[set]]$data$right[li] == x$data[[set]]$data$left[li])  {				
					if( x$data[[set]]$data$qty[li] == 1)  {			
						fa<-c(fa, x$data[[set]]$data$left[li])		
					}else{			
						fa<-c(fa, rep(x$data[[set]]$data$left[li], x$data[[set]]$data$qty[li] ))		
					}			
				}				
				if(x$data[[set]]$data$right[li] == -1)  {				
					if( x$data[[set]]$data$qty[li] == 1)  {			
						su<-c(su, x$data[[set]]$data$left[li])		
					}else{			
						su<-c(su, rep(x$data[[set]]$data$left[li], x$data[[set]]$data$qty[li] ))		
					}			
								
				}				
				if(x$data[[set]]$data$right[li] > x$data[[set]]$data$left[li])  {				
					if( x$data[[set]]$data$qty[li] == 1)  {			
						le<-c(le, x$data[[set]]$data$left[li])		
						ri<-c(ri, x$data[[set]]$data$right[li])		
					}else{			
						le<-c(le, rep(x$data[[set]]$data$left[li], x$data[[set]]$data$qty[li] ))		
						ri<-c(ri, rep(x$data[[set]]$data$right[li], x$data[[set]]$data$qty[li] ))		
					}			
				}				
			}					
			if(!is.null(le)) {					
				interval<-data.frame(left=le, right=ri)				
			}					
								
			wblr_obj<-wblr(x=fa, s=su, interval=interval, dist=x$dist, canvas=x$dist, col=colors[set])
			
			if(x$method.fit=="lslr") {
				wblr_obj<-wblr.fit(wblr_obj, method.fit="rr-xony")	
			}else{
				wblr_obj<-wblr.fit(wblr_obj, method.fit=x$method.fit)				
			}
			
			wblr_list[[set]]<-wblr_obj					
		}else{						
			ignore_slope<-c(ignore_slope, set)					
		}						
	}							
	if(length(ignore_slope)>1)  wblr_list<-wblr_list[-ignore_slope[-1]]							
	## ignore_slope cannot be set as an object element here, because this function is optionally called							
	## however, the test is simple during a loop through data sets if(x$data[[set]]$num_fails<3) 							
								
	plot.wblr(wblr_list)							
}								
