## options.alt.R
## Based upon options.abrem.R originally authored by Jurgen Symynck
## (c) 2014-2022 OpenReliability.org
##-------------------------------------------------------------------------------
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.

options.alt<- function(...){
    # function to handle the many plotting options for the alt object
    # the option list should only be manipulated through this function?

    # TODO: WARNING: partial matching is in effect!
    # options.alt()$ylim will return options.alt()$ylim.default if
    # $ylim was set to NULL!
	
	# unknown effect of single
	 single <- FALSE
	 args <- list(...)

    if(!exists(as.character(substitute(options_alt))))
        # if the globally accessible variable was not defined yet, then
        # create it here with default values OR reset to default values
        # message ("Resetting Weibull-R options to default values...")
	
	options_alt<-list(	
		dev.width=5,
		dev.height=7,
		# x object is unknown upon call to options.alt
		#main=paste0(toupper(x$alt.model)," - ", toupper(x$dist), "\n RELATIONSHIP PLOT"),
		xlab= "Stress",  ## expression(paste("Temperature ",degree,"C"))    ## "Voltage in kV"
		ylab= "Time To Failure",    ##"HOURS", "MINUTES"
		
		is.plot.grid=TRUE,
		col.grid="gray",
		
		## unknown effect of these controls
		coordinate.text.size=.7,
		mar=c(5.1,4.1,5.1,2.1),
		
		
		points.col="black",
		points.pch=21,
		points.lwd=2,
		points.cex=1,
		points.jf= .003,
		
		interval.col="black",
		interval.lty="dashed",
		interval.lwd=1,
		
		fit.type = "l",
		fit.lwd=2,
		fit.col="red",
		is.plot.fit_points=TRUE,
		
		parallel_params.pch=4,
		parallel_params.cex=2,
		parallel_params.col="red",
		is.plot.parallel_params=FALSE,
		
		is.plot.fit=TRUE,
		fit.lty="solid",
		fit.col="red",
		fit.lwd=2,
		
		is.plot.goal=TRUE,
		goal.pch=8,
		goal.cex=2,
		goal.col="orange",
		goal.lwd=2,
		
		
		#percentiles = c(10,90), ## percentiles are now an arguement to alt.fit
		is.plot.percentiles=TRUE,
		percentile.col="blue",
		percentile.lty="solid",
		percentile.lwd=2,
		
		# something about percent label?
		
		persistent=TRUE
	)	
	
	
	
	## It is generally uncertain how any of the below really impacts plotting
	
	    if (!length(args))
        args <- options_alt
           # return the current option list
    else {
        if (all(unlist(lapply(args, is.character))))
            # if all items in the args are characters, then
            # treat them as the names of the options.
            args <- as.list(unlist(args))
        if (length(args) == 1) {
            if (is.list(args[[1L]]) | is.null(args[[1L]]))
                args <- args[[1L]]
                # unlist the first (and only) argument to a string
            else if(is.null(names(args)))
                # if there is no name to args then
                # the arg itself is the name (?)
            single <- TRUE
        }
    }
    value <- args
    if(options_alt$persistent){
        options_alt <-modifyList(options_alt, value)
    }
    if(!is.null(args$persistent)){
        value <- args
        if(args$persistent){
            options_alt <-modifyList(options_alt, value)
        }
    }
    # make the options stick between calls of options.alt()
    if(is.null(names(args)))
        value <- options_alt[match(args,names(options_alt))]
    if(single) value <- value[[1L]]
    value
}
# TODO :options that are NULL are not shown in the printout

plot_default_args <- function(){
    paronly <- c("ask","fig", "fin","lheight","mai", "mar", "mex", "mfcol",
        "mfrow", "mfg","new","oma", "omd", "omi","pin", "plt", "ps", "pty",
        "usr","xlog", "ylog","ylbias")
        # parameters that can only be set using par()
        # see $par() for the origin of this list
    parreadonly <- c("xlog", "ylog", "adj", "ann", "ask",
        "bg", "bty", "cex", "cex.axis", "cex.lab",
        "cex.main", "cex.sub", "col", "col.axis", "col.lab",
        "col.main", "col.sub", "crt", "err", "family",
        "fg", "fig", "fin", "font", "font.axis",
        "font.lab", "font.main", "font.sub", "lab", "las",
        "lend", "lheight", "ljoin", "lmitre", "lty",
        "lwd", "mai", "mar", "mex", "mfcol",
        "mfg", "mfrow", "mgp", "mkh", "new",
        "oma", "omd", "omi", "pch", "pin",
        "plt", "ps", "pty", "smo", "srt",
        "tck", "tcl", "usr", "xaxp", "xaxs",
        "xaxt", "xpd", "yaxp", "yaxs", "yaxt",
        "ylbias")
        # par() parameter that can be set
        # par(no.readonly=TRUE)
    parplot <- unique(sort(c(parreadonly[!(parreadonly %in% paronly)],
        "type","xlim","ylim","log","main","sub","xlab","ylab",
          "ann","axes","frame.plot","panel.first","panel.last","asp")))
          # all valid (?) graphical parameters that can be supplied
          # to plot.default
    parplot
}

