alt.fit<-function(x, omit_set=0)  {			
	# must confirm x is an alt object		
	#if(class(x)!="alt") stop("x is not an alt object")	
	if(!is(x, "alt")) stop("x is not an alt object")	
	# object x must include a parallel_par element		
	if(is.null(x$parallel_par)) stop("x must include a parallel_par element")		
	modelDF<-x$parallel_par	
	# set wt of omit_set to zero		
	if(omit_set[1]!=0)		
	for(omit in 1:length(omit_set)) {		
		modelDF[omit_set[omit],4]<-0	
	}		
			
	P1vec<-NULL		
	stressvec<-NULL		
	#expand modelDF accordint to wt		
	for(row in 1:nrow(modelDF))  {		
		if(modelDF$wt[row]>0) {	
		P1vec<-c(P1vec, rep(modelDF$P1[row], modelDF$wt[row]))	
		stressvec<-c(stressvec, rep(modelDF$stress[row][1], modelDF$wt[row]))	
		}	
	}		
	## fixes attributed to debug of LuValle example
	if(x$dist == "lognormal") Tvec<-P1vec
	if(x$dist == "weibull") Tvec<-log(P1vec)
	if(x$alt.model=="arrhenius")  fit<-lm(Tvec ~ stressvec)
	if(x$alt.model=="power")  fit<-lm(Tvec ~ log(stressvec))

		
	x$alt_coef<-unname(fit$coefficients)		
	# now alt models can plot a fitted line at the percentile of the distribution parameter P1
	# this is the median (50th percebtile) for lognormal, "characteristic life" (63.2 percentile) for weibull.
		
		
	x	
}		
		
