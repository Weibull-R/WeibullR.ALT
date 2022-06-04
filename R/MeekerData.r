MeekerData<-function(x) {			
	        xchar<-as.character(x)		
	        subx<-substr(xchar, nchar(xchar)-1, nchar(xchar))		
	if(!is.na(suppressWarnings(as.numeric(subx)))) {		
		if(subx=="10")  {        	
			ret<-generateTable_C.10()
		}	
		if(subx=="13")  {        	
			ret<-generateTable_C.13()
		}	
		if(subx=="15")  {        	
			ret<-generateTable_C.15()
		}	
		if(subx=="16")  {        	
			ret<-generateTable_C.16()
		}	
		if(subx=="3")  {        	
			ret<-generateTable_C.3()
		}	
	}else{		
	# handle single character arguments		
		subx<-substr(xchar, nchar(xchar), nchar(xchar))	
		if(!is.na(suppressWarnings(as.numeric(subx)))) {	
			if(subx=="3")  {        
			ret<-generateTable_C.3()
			}
		}else{	
			stop(paste0("table ",x, " not recognized"))
		}	
	}		
	    ret		
}			
			
			
			
			
## Table C.3 presents degeneration data.  			
## I cannot find anywhere in the text where this is handled. 			
## However, the concept with degeneration data is to establish a failure limit at which we can treat the data the same way as definative failure data. 			
## Since the data demonstrates continuous degeneration at fixed inspection points defined failures will be intervals.			
			
generateTable_C.3<-function() {			
ret<-data.frame(			
Unit=c( 1,  2,  3,  4,  5,  6,  7,  8,  10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30),			
TempC=c(    83, 83, 83, 83, 83, 83, 83, 83, 83, 133,    133,    133,    133,    133,    133,    133,    133,    133,    133,    173,    173,    173,    173,    173,    173,    173,    173,    173,    173),			
InitialResistance=c(    217.97, 217.88, 224.67, 215.92, 219.88, 219.63, 218.27, 217.27, 219.98, 218.05, 219.38, 218.35, 217.78, 218.28, 216.38, 217.65, 221.91, 218.47, 217.59, 216.31, 216.62, 221.98, 217.83, 217.3,  216.75, 220.39, 216.26, 217.86, 217.49),			
Hr452=c(    0.28,   0.22,   0.41,   0.25,   0.25,   0.32,   0.36,   0.24,   0.33,   0.4,    0.88,   0.53,   0.47,   0.57,   0.55,   0.78,   0.83,   0.64,   0.55,   0.87,   1.25,   2.64,   0.98,   1.62,   1.59,   2.29,   0.98,   1.04,   1.19),			
Hr1030=c(   0.32,   0.24,   0.46,   0.29,   0.26,   0.36,   0.41,   0.28,   0.4,    0.47,   1.19,   0.64,   0.62,   0.75,   0.67,   0.96,   1.12,   0.8,    0.74,   1.29,   1.88,   3.78,   1.36,   2.34,   2.41,   2.14,   1.37,   1.54,   1.59),			
Hr4341=c(   0.38,   0.26,   0.54,   0.32,   0.42,   0.45,   0.52,   0.34,   0.44,   0.72,   2.06,   0.99,   1,  1.26,   1.09,   1.48,   1.96,   1.23,   1.29,   2.62,   3.54,   7.01,   2.66,   3.82,   3.46,   6.3,    2.47,   2.77,   3.03),			
Hr8084=c(   0.62,   0.38,   0.81,   0.48,   0.57,   0.58,   0.7,    0.55,   0.85,   1.05,   3.15,   1.6,    1.5,    2.03,   1.79,   2.27,   3.29,   1.84,   2.03,   4.44,   5.25,   11.12,  4.42,   6.14,   6.75,   8.34,   3.74,   4.16,   4.52)			
)			
ret}			
			
			
# Temperature-Accelerated Life Test Data for Device-A		
# from Hooper and Amster (1990)		
# This data is used as a first example of applying the alt data analysis process pages 494-500		
		
generateTable_C.10<-function() {		
ret<-data.frame(		
time=c( 5000,   1298,   1390,   3187,   3241,   3261,   3313,   4501,   4568,   4841,   4982,   5000,   581,    925,    1432,   1586,   2452,   2734,   2772,   4106,   4674,   5000,   283,    361,    515,    638,    854,    1024,   1030,   1045,   1767,   1777,   1856,   1951,   1964,   2884,   5000),
event=c( 0,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  0,  1,  1,  1,  1,  1,  1,  1,  1,  1,  0,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  0),
qty=c( 30, 1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  90, 1,  1,  1,  1,  1,  1,  1,  1,  1,  11, 1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1),
TempC=c( 10, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80))
ret		
}		
			
			
			
generateTable_C.15<-function() {			
ret<-data.frame(			
left=c( 1536,   1536,   96, 384,    788,    1536,   2304,   192,    384,    788,    1536),			
right=c(    -1,  -1,  -1,  788,    1536,   2304,   -1,  384,    788,    1536,   -1),			
qty=c(  50, 50, 50, 1,  3,  5,  41, 4,  27, 16, 3),			
TempC=c(    150,    175,    200,    250,    250,    250,    250,    300,    300,    300,    300)			
)			
ret}			
			
			
			
## Table C.16 shows data on accelerated life tests on tantalum elctrolytic capacitors			
## both voltage and temperature stresses are believed to be accelerating factors			
			
generateTable_C.16<-function() {			
ret<-data.frame(			
time=c( 20, 90, 700,    3700,   3700,   20, 3600,   9500,   27000,  27000,  800,    2800,   2800,   500,    800,    2400,   10700,  10700,  110,    1200,   7500,   2000,   2600,   27300,  27300,  1000,   1000,   25, 50, 165,    500,    620,    720,    820,    910,    980,    1270,   1600,   2270,   2370,   4590,   4880,   7560,   8730,   12500,  12500,  8900,   8900),			
event=c(    1,  1,  1,  1,  0,  1,  1,  1,  1,  0,  1,  1,  0,  1,  1,  1,  1,  0,  1,  1,  1,  1,  1,  1,  0,  1,  0,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  0,  1,  0),			
qty=c(  1,  1,  1,  1,  996,    1,  1,  1,  1,  196,    1,  1,  48, 1,  1,  1,  1,  49, 1,  1,  1,  1,  1,  1,  496,    1,  174,    1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  156,    1,  49),
Volts=c(    35, 35, 35, 35, 35, 40.6,   40.6,   40.6,   40.6,   40.6,   46.5,   46.5,   46.5,   51.5,   51.5,   51.5,   51.5,   51.5,   46.5,   46.5,   46.5,   46.5,   46.5,   46.5,   46.5,   46.5,   46.5,   62.5,   62.5,   62.5,   62.5,   62.5,   62.5,   62.5,   62.5,   62.5,   62.5,   62.5,   62.5,   62.5,   62.5,   62.5,   62.5,   62.5,   62.5,   62.5,   57, 57),
TempC=c(    85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 45, 45, 45, 45, 45, 45, 45, 5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,  45, 45))			
ret			
}			
