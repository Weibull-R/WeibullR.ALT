alt.fit<-function(x, omit_set=0, best_set=1, percentiles=c(10,90))  {			
	# must confirm x is an alt object		
	if(class(x)!="alt") stop("x is not an alt object")		
	# object x must include a parallel_par element		
	if(is.null(x$parallel_par)) stop("x must include a parallel_par element")		
	modelDF<-x$parallel_par	
	if((omit_set %in% 1:nrow(modelDF))[1])  {	
		modelDF<-modelDF[(-1*omit_set),]
	}	
	if(x$alt.model=="arrhenius")  fit<-lm(modelDF$P1 ~ modelDF$stress)	
	if(x$alt.model=="power")  fit<-lm(log(modelDF$P1) ~ log(modelDF$stress))			
	x$alt_coef<-unname(fit$coefficients)		
	# now alt models can plot a fitted line at the percentile of the distribution parameter P1
	# this is the median (50th percebtile) for lognormal, "characteristic life" (63.2 percentile) for weibull.
	
	# now it will depend on the x$dist as to how the percentile lines will be drawn	
	# I will use the best_set (default is first set in provided list) as a basis for determining the new abline coeficients	
	if(x$dist=="lognormal")  {	
		quants<-qlnorm(percentiles/100, x$parallel_par$P1[best_set], x$parallel_par$P2[best_set])
	}	
	if(x$dist=="weibull")  {	
		quants<-qweibull(percentiles/100, x$parallel_par$P2[best_set], x$parallel_par$P1[best_set])
	}	
		
	log_offsets<-NULL	
	for(percentile in 1:length(percentiles))  {	
		if(x$dist=="lognormal")  log_offset<-log(quants[percentile]) - x$parallel_par$P1[best_set]
		if(x$dist=="weibull")  log_offset<-log(quants[percentile]) -log(x$parallel_par$P1[best_set])
		log_offsets<-c(log_offsets, log_offset)
	}	
		
	x$percentiles <- percentiles	
	x$log_offsets <- log_offsets	
		
	x	
}		
		
