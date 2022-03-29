#alt.data

# This function reads reliability life test data for a single stress level using the flexible input format
# also provided for WeibullR::mleframe.  Code in follow-on functions will have to identify and handle the data as it is stored
# in order to perform future processing. 


alt.data<-function(x, s=NULL, interval=NULL, stress) {						
	lrq<-mleframe.alt(x,s, interval)					
	obj<-list(stress = stress, data = lrq)					
	class(obj) <- "alt.data"					
	obj					
	}					
