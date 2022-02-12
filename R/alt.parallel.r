alt.parallel<-function(x, method.fit="lslr", ignore_slope=0, set.exponential=FALSE, view_parallel_fits=TRUE)  {
	# must confirm x is an alt object
	if(class(x)!="alt") stop("x is not an alt object")
	# set.exponential only applies when distribution is set to weibull
	if(set.exponential==TRUE && x$dist!="weibull") stop("exponential fitting requires weibull distribution")
	# if data contains intervals, must set method.fit to "mle"
	found_interval=FALSE		
	# identify whether interval data exists		
	for(set in 1:length(x$data))  {		
		if(any((x$data[[set]]$data$right - x$data[[set]]$data$left) > 0)) {	
			found_interval=TRUE
		}	
		if(found_interval==TRUE) break	
	}		
	if(method.fit == "lslr" && found_interval==TRUE) {		
		method.fit = "mle"	
		warning("method.fit set to 'mle' due to interval data")	
	}		


	num_valid_pts<-0
	cumm_valid_P2<-0
	fit_list<-list()

	wblr_list<-list()
	colors<-c("blue", "darkgreen", "red", "purple", "darkseagreen", "chocolate",  "lightblue4", "indianred")

		for(set in 1:length(x$data))  {
			if(method.fit=="mle")  {
				 this_fit<-mlefit(x$data[[set]]$data, dist=x$dist)
			}
			if(method.fit=="lslr" || view_parallel_fits==TRUE)  {
		## extract fail and suspension data from the lrq dataframe
				fa<-NULL
				su<-NULL

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
				}
				this_fit<-lslr(getPPP(x=fa, s= su), dist=x$dist)
			}
			if(view_parallel_fits==TRUE)  {
			# extract the interval data for this set
				le<-NULL
				ri<-NULL
				interval<-NULL
				for(li in 1:nrow(x$data[[set]]$data))  {
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

				# wblr_list now contains only the points
				wblr_obj<-wblr(x=fa, s=su, interval=interval, dist=x$dist, canvas=x$dist, col=colors[set])
				wblr_list[[set]]<-wblr_obj

			}

			fit_list[[length(fit_list)+1]]<-
			unname(this_fit)

			if(set.exponential == FALSE)  {
				if(! set %in% ignore_slope) {
					num_pts<-0
					num_pts<-length(fa) + length(su)
					if(found_interval==TRUE) num_pts<-num_pts+nrow(interval)
					num_valid_pts<-num_valid_pts+num_pts
					cumm_P2<-num_pts * this_fit[2]
					cumm_valid_P2<-cumm_valid_P2 + cumm_P2
				}
			}

		} # close processing of each data set

	if(set.exponential == FALSE)  {
		parallel_P2<-cumm_valid_P2 / num_valid_pts
	}else{
		parallel_P2<- 1.0
	}


	parallelDF<-data.frame(P1=0, P2=0, stress=0)
	for(li in 1:length(fit_list))  {
		this_P1<-fit_list[[li]][1]
		this_stress<-x$data[[li]]$stress[1]
		thisDFrow<-data.frame(P1=this_P1, P2=unname(parallel_P2), stress=this_stress)
		parallelDF <- rbind(parallelDF, thisDFrow)
	}
		parallelDF <- parallelDF[-1,]
		x<- modifyList(x,list(parallel_par=parallelDF))

	# Prepare a WeibullR view of the parallel fits
	if(view_parallel_fits==TRUE)  {
		partype<-"Parallel "
		if(set.exponential==TRUE) partype<-"Exponential "
		main<-paste0(partype,"Fit for \nAccelerated Life Tests")
	# plot just the data points
		suppressWarnings(plot.wblr(wblr_list,
			main=main,
			is.plot.legend=FALSE)
		)
		yrange<-c(.001, .999)
	# now add the parallel lines from parameters
		for(row in 1:nrow(parallelDF))  {
			if(x$dist=="lognormal")  {
			points(qlnorm(yrange, parallelDF[row,1], parallelDF[row,2]),
				 p2y(yrange, canvas="lognormal"),
				type="l", lwd=2, col=colors[row])
			}
			if(x$dist=="weibull")  {
			points(x=qweibull(yrange, parallelDF[row,2], parallelDF[row,1]),
				 y=p2y(yrange, canvas=x$dist),
				type="l", lwd=2, col=colors[row])
			}
		}
	}

	return(x)
}
