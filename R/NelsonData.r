# NelsonData.r
#
# This function is used to generate data objects demonstrated in examples presented by
# Wayne Nelson in his book "Accelerated Testing"
#
# This method of data provision has been found necessary since inclusion of table3.1 and table5.1
# created a problem with a "Warning: object '.Random.seed' is created by more than one data call"
# which prevented lazyData=yes to be in the package definition.  Without lazyData I could never seem
# to make \data files avialable to example scripts within the package.
#
# Now, as an exported function these data objects can be generated as called for in example scripts.

NelsonData<-function(x) {
	if(is.numeric(x)) {
		if(x>2 && x<3) {
			ret<-generate_table2.2()
		}
		if(x>3 && x<4) {
			ret<-generate_table3.1()
		}
		if(x>4 && x<5) {
			ret<-generate_table4.1()
		}
		if(x>5 && x<6) {
			ret<-generate_table5.1()
		}
	}
	if(is.character(x)) {
		if(x == "table2.2") {
			ret<-generate_table2.2()
		}
		if(x == "table3.1") {
			ret<-generate_table3.1()
		}		
		if(x == "table4.1") {
			ret<-generate_table4.1()
		}
		if(x == "table5.1") {
			ret<-generate_table5.1()
		}
	}
ret
}


# Table 2.2 is presented on page115 of Wayne Nelson's book "Accelerated Testing".
# It purports to represent a series tests on  a Class H insulation at elevated temperatures (in deg c)
generate_table2.2<-function() {	
	C190<-c( 7228,   7228,   7228,   8448,   9167,   9167,   9167,   9167,   10511,  10511) 
	C220<-c( 1764,   2436,   2436,   2436,   2436,   2436,   3108,   3108,   3108,   3108) 
	C240<-c( 1175,   1175,   1521,   1569,   1617,   1665,   1665,   1713,   1761,   1953) 
	C260<-c( 600,    744,    744,    744,    912,    1128,   1320,   1464,   1608,   1896)
	table2.2<-data.frame(C190,C220, C240, C260)
	table2.2
}

# Table 3.1 is presented on page129 of Wayne Nelson's book "Accelerated Testing".
# It purports to represent a series tests on an insulating fluid at elevated voltages.
generate_table3.1<-function() {	
	kV26<-c( 5.79, 1579.52, 2323.7)
	kV28<-c( 68.85,  108.29, 110.29, 426.07, 1067.6)
	kV30<-c( 7.74,   12.05,  20.46,  21.02,  22.66,  43.4,   47.3,   139.07, 144.12, 175.88, 194.9)
	kV32<-c( 0.27,   0.4,    0.69,   0.79,   2.75,   3.91,   9.88,   13.95,  15.93,  27.8,   53.24,  82.85,  89.29,  100.58, 215.1)
	kV34<-c( 0.19,   0.78,   0.96,   1.31,   2.78,   3.16,   4.15,   4.67,   4.85,   6.5,    7.35,   8.01,   8.27,   12.06,  31.75,  32.52,  33.91,  36.71)
	kV36<-c( 0.35,   0.59,   0.96,   0.99,   1.69,   1.97,   2.07,   2.58,   2.71,   2.9,    3.67,   3.99,   5.35,   13.77,  25.5)
	kV38<-c(  0.09,   0.39,   0.47,   0.73,   1.13,   1.4,    2.38)
	table3.1<-list(kV26=kV26, kV28=kV28, kV30=kV30, kV32=kV32, kV34=kV34,kV36=kV36, kV38=kV38)
	
	table3.1
}

# Table 4.1 is presented on page135 of Wayne Nelson's book "Accelerated Testing".
# It purports to represent a series tests on  a Class B insulation at elevated temperatures (in deg c)
generate_table4.1<-function() {	
	C170f<-c(1764,   2772,   3444,   3542,   3780,   4860,   5196)
	C170s<-c(5448,   5448,   5448)               
	C190f<-c(408,    408,    1344,   1344,   1440)       
	C190s<-c(1680,   1680,   1680,   1680,   1680)       
	C220f<-c(408,    408,    504,    504,    504)        
	C220s<-c(528,    528,    528,    528,    528)
	
	table4.1<-list(C170f=C170f, C170s=C170s, C190f=C190f, C190s=C190s, C220f=C220f, C220s=C220s)
	table4.1
}

# Table 5.1 is presented on page 393 of Wayne Nelson's book "Accelerated Testing".
# It is interesting to note that items $T190, $T220, $T240, and $T260 represent
# a Table 5.1 presented  on page 140. In Chapter 7 this data forms an example of 
# competing modes analysis covering modes of failure on motorette turns, phase, and ground.

generate_table5.1<-function() {		
	    T190<-data.frame(   time=c( 7228,   7228,   7228,   8448,   9167,   9167,   9167,   9167,   10511,  10511)  ,   event=c(    1,  1,  1,  1,  1,  1,  1,  1,  1,  1   ))	
	    P190<-data.frame(   time=c( 10511,  11855,  11855,  11855,  12191,  12191,  12191,  12191,  12191,  12191)  ,   event=c(    1,  1,  1,  1,  0,  0,  0,  0,  0,  0)  )	
	    G190<-data.frame(   time=c( 10511,  11855,  11855,  11855,  12191,  12191,  12191,  12191,  12191,  12191)  ,   event=c(    0,  0,  0,  0,  0,  0,  0,  0,  0,  0)  )	
	    T220<-data.frame(   time=c( 1764,   2436,   2436,   2436,   2436,   2436,   3108,   3108,   3108,   3108)   ,   event=c(    1,  1,  1,  1,  1,  1,  1,  1,  1,  1)  )	
	    P220<-data.frame(   time=c( 2436,   2436,   2436,   2772,   2436,   4116,   4116,   4116,   3108,   4116)   ,   event=c(    1,  1,  1,  0,  0,  0,  0,  0,  1,  0)  )	
	    G220<-data.frame(   time=c( 2436,   2490,   2436,   2772,   2436,   4116,   4116,   4116,   3108,   4116)   ,   event=c(    1,  1,  1,  1,  1,  0,  0,  0,  0,  0)  )	
	    T240<-data.frame(   time=c( 1175,   1881,   1521,   1569,   1617,   1665,   1665,   1713,   1761,   1953)   ,   event=c(    1,  0,  1,  1,  1,  1,  1,  1,  1,  1)  )	
	    P240<-data.frame(   time=c( 1775,   1881,   1881,   1761,   1881,   1881,   1881,   1881,   1881,   1953)   ,   event=c(    0,  0,  0,  1,  0,  0,  0,  0,  0,  0)  )	
	    G240<-data.frame(   time=c( 1775,   1775,   1881,   1761,   1881,   1881,   1881,   1881,   1881,   1953)   ,   event=c(    1,  1,  0,  0,  0,  0,  0,  0,  0,  0)  )	
	    T260<-data.frame(   time=c( 1632,   1632,   1632,   1632,   1632,   1128,   1512,   1464,   1608,   1896)   ,   event=c(    0,  0,  0,  0,  0,  1,  1,  1,  1,  1)  )	
	    P260<-data.frame(   time=c( 1632,   1632,   1632,   1632,   1632,   1128,   1512,   1632,   1608,   1896)   ,   event=c(    0,  0,  0,  0,  0,  0,  0,  0,  0,  1)  )	
	    G260<-data.frame(   time=c( 600,    744,    744,    744,    912,    1118,   1320,   1632,   1608,   1896)   ,   event=c(    1,  1,  1,  1,  1,  1,  1,  0,  1,  1)  )	
		
	table5.1<-list(G190  =   G190 ,  G220    =   G220 , G240  =   G240 , G260  =  G260, P190  =  P190, P220  =  P220 ,  P240  =  P240, P260  =  P260, T190  =  T190, T220  = T220 , T240  =  T240, T260  =  T260  ) 	
		
	table5.1	
}	
	
		
		
		