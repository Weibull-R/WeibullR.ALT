# alt.make.r

# simple initiation of an alt object using data, expected distribution, and ALT model (along with optional goal data)
# error checking is performed on all input. This newly formed object is plot-able using the s3 registered plot function
# or plot.alt (particularly during developement prior to s3 registration.)

alt.make<-function(x, dist, alt.model, goal=NULL) {		
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
## will continue to populate the alt object, perhaps with additional alt.xxx functions		
	class(obj) <- "alt"	
	obj	
}		
