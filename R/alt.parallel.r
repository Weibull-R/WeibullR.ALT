alt.parallel<-function(x, ignore_slope=0, set.exponential=FALSE, view_parallel_fits=TRUE)  {
	# must confirm x is an alt object
	if(class(x)!="alt") stop("x is not an alt object")
	# set.exponential only applies when distribution is set to weibull
	if(set.exponential==TRUE && x$dist!="weibull") stop("exponential fitting requires weibull distribution")

	num_valid_pts<-0
	cumm_valid_P2<-0
	fit_list<-list()

	wblr_list<-list()
	colors<-c("blue", "darkgreen", "red", "purple", "darkseagreen", "chocolate",  "lightblue4", "indianred")
	drop_wblr<-NULL
		for(set in 1:length(x$data))  {

		if(x$data[[set]]$valid_set==FALSE)  {
			drop_wblr<-c(drop_wblr, set)
			if(!set  %in% ignore_slope) ignore_slope<-c(ignore_slope, set)
			this_fit<-c(NA, NA)
		}else{  ## the rest of the fit

			if(x$method.fit=="lslr" || view_parallel_fits==TRUE)  {
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
				if(x$method.fit=="lslr" ) {
					this_fit<-lslr(getPPP(x=fa, s= su), dist=x$dist)
				}

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

				# wblr_list now contains only the points for sets with num_fails > 2
				wblr_obj<-wblr(x=fa, s=su, interval=interval, dist=x$dist, canvas=x$dist, col=colors[set])
				wblr_list[[set]]<-wblr_obj

			}

			if(x$method.fit %in% c("mle", "mle-rba", "mle-unbias"))  {
				 this_fit<-mlefit(x$data[[set]]$data, dist=x$dist)
			}
		}  # close the >2 failure else block

			fit_list[[length(fit_list)+1]]<-
			unname(this_fit)

			if(set.exponential == FALSE)  {
				if(! set %in% ignore_slope) {
					num_pts<-0
					num_pts<-sum(x$data[[set]]$data$qty)
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

	# alternate fitting of P1 by graphical means, due to insufficient fail points		
	for(set in 1:length(x$data))  {	
	# ignore_slope now includes both invalid sets and those specified as ignore_slope
		if( x$data[[set]]$num_fails>0 && set %in% ignore_slope) {	
			eppp<-extract_ppp(x,set)
			graphical_point<-c(log(mean(eppp$time)), mean(p2y(eppp$ppp,canvas=x$dist)))
			if(x$dist == "lognormal")  fit_list[[set]][1]<-graphical_point[1]-log(graphical_point[2])*parallel_P2
			if(x$dist=="weibull") fit_list[[set]][1]<-exp(graphical_point[1]-graphical_point[2]/parallel_P2)
		}	
	}		


	parallelDF<-data.frame(P1=0, P2=0, stress=0, wt=0)
	for(li in 1:length(fit_list))  {
		this_P1<-fit_list[[li]][1]
		this_stress<-x$data[[li]]$stress[1]
		this_weight<-x$data[[li]]$num_fails
		thisDFrow<-data.frame(P1=this_P1, P2=unname(parallel_P2), stress=this_stress, wt=this_weight)
		parallelDF <- rbind(parallelDF, thisDFrow)
	}

		parallelDF <- parallelDF[-1,]
		x<- modifyList(x,list(parallel_par=parallelDF))

	# Prepare a WeibullR view of the parallel fits
	if(view_parallel_fits==TRUE)  {
		partype<-"Parallel "
		if(set.exponential==TRUE) partype<-"Exponential "
		main<-paste0(partype,"Fit for \nAccelerated Life Tests")

		if(!is.null(drop_wblr))  wblr_list<-wblr_list[-drop_wblr]

	# plot just the data points
		suppressWarnings(plot.wblr(wblr_list,
			main=main,
			is.plot.legend=FALSE)
		)

	##  need to add the points for sets with num_fails < 3 here
		for(set in drop_wblr) {
			if(x$data[[set]]$num_fails > 0) {
				eppp<-extract_ppp(x,set)
				points(eppp$time, p2y(eppp$ppp, x$dist), col=colors[set], pch=1, lwd=2, cex=1)

			}
		}

		yrange<-c(.001, .999)
	# now add the parallel lines from parameters
		for(row in 1:nrow(parallelDF))  {
			if(!is.na(parallelDF[row,1])) {
			#if(x$data[[row]]$num_fails > 2) {
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
		
	#finally add a legend
		le<-NULL; col<-NULL; lty<-NULL; cex<-NULL; lwd<-NULL
		for(set in 1:length(x$data))  {
			if(x$data[[set]]$num_fails > 0) {
				le<-c(le, paste0("set ", set,", stress ",x$data[[set]]$stress))
				col<-c(col, colors[set])
				lty<-c(lty, 1)
				lwd<-c(lwd,2)
				cex<-c(cex, 0.8)
			}
	}
	legend("topleft", inset=0.01, legend=le,
		   col=col, lty=lty, cex=cex, lwd=lwd, bg="white")		
		
	}

	return(x)

}

extract_ppp<-function(x, set, ties="none") {
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
		# intervals as points for the view_parallel_fits only
		if(x$data[[set]]$data$right[li] > x$data[[set]]$data$left[li])  {
			if( x$data[[set]]$data$qty[li] == 1)  {
				fa<-c(fa, (x$data[[set]]$data$left[li]+x$data[[set]]$data$right[li])/2)
			}else{
				fa<-c(fa, c((x$data[[set]]$data$left[li]+x$data[[set]]$data$right[li])/2), x$data[[set]]$data$qty[li] )
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
	ppp<-getPPP(x=fa, s=su, ties=ties)

	ppp
}

extract_ppp<-function(x, set, ties="none") {
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
		# intervals as points for the view_parallel_fits only
		if(x$data[[set]]$data$right[li] > x$data[[set]]$data$left[li])  {
			if( x$data[[set]]$data$qty[li] == 1)  {
				fa<-c(fa, (x$data[[set]]$data$left[li]+x$data[[set]]$data$right[li])/2)
			}else{
				fa<-c(fa, c((x$data[[set]]$data$left[li]+x$data[[set]]$data$right[li])/2), x$data[[set]]$data$qty[li] )
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
	ppp<-getPPP(x=fa, s=su, ties=ties)

	ppp
}

