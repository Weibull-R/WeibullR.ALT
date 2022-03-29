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
