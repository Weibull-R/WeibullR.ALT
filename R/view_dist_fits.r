view_dist_fits<-function(x)  {					
	# must confirm x is an alt object				
	if(class(x)!="alt") stop("x is not an alt object")				
					
	wblr_list<-list()				
	colors<-c("blue", "darkgreen", "red", "purple", "darkseagreen", "chocolate",  "lightblue4", "indianred")				
					
					
	for(set in 1:length(x$data))  {				
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
		if(is.null(interval))  {			
			wblr_obj<-wblr.fit(wblr_obj)		
		}else{			
			wblr_obj<-wblr.fit(wblr_obj, method.fit="mle")		
		}			
		wblr_list[[set]]<-wblr_obj			
	}				
					
	plot.wblr(wblr_list)				
}					
