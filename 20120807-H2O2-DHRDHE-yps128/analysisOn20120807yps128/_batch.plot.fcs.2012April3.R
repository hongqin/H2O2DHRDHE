# 2012 March 28. Qin add gate to filter the data. 
#    It looks like the gates can changes substaintially between BY strains and wild isolates, which
#     poses problems for batch analysis. 


rm(list=ls())

debug = 0; 
require(flowCore);  require(flowClust); 

facFlList = list.files( path="fcs/");  facFlList

for (  i in 1: length(facFlList) ) {
 # i = 1; 
 myfl.full = paste( 'fcs/', facFlList[i], sep='');
 myfcs = read.FCS( myfl.full );

 if(debug>0) {    myfcs@exprs = myfcs@exprs[500:5500, ]   }
 
 #remove debris, I used E03 before 2012 March 28.
 my.gate = rectangleGate(filterId="Cells", list("FSC-H"=c(0.5E3, Inf), "SSC-H"=c(0.5E3, Inf), "FL3-H"=c(0, Inf) ))
 my.filter = filter( myfcs, my.gate)
 myfcs2 = Subset( myfcs, my.filter )
 
 mydata = data.frame(log10(myfcs2@exprs))
 mynames = names(mydata)
 
 pdf(paste("sandbox/_", facFlList[i], ".pdf", sep=""), width=8, height=11);
 par(new=T)
 par(mfrow = c(3, 3)) 
 
 for (j in 1:5 ) {
    hist( mydata[,j], breaks=100, xlab=mynames[j], main= facFlList[i], xlim=c(0,4.5), freq=F )
 }
 plot( mydata[,2] ~ mydata[,1], pch=3, xlab=mynames[1], ylab=mynames[2], xlim=c(0,4.5), ylim=c(0,4.5))
 plot( mydata[,5] ~ mydata[,1], pch=3, xlab=mynames[1], ylab=mynames[5], xlim=c(0,4.5), ylim=c(0,4.5))
 plot( mydata[,4] ~ mydata[,1], pch=3, xlab=mynames[1], ylab=mynames[4], xlim=c(0,4.5), ylim=c(0,4.5)) 

 #hexbin(x,y)
 #plot( hexbin(mydata[,1], mydata[,2] ), pch=3, xlab=mynames[1], ylab=mynames[2], xlim=c(0,4.5), ylim=c(0,4.5))
 #plot( hexbin(mydata[,1], mydata[,5] ), pch=3, xlab=mynames[1], ylab=mynames[5], xlim=c(0,4.5), ylim=c(0,4.5))
 #plot( hexbin(mydata[,4] ~ mydata[,1]), pch=3, xlab=mynames[1], ylab=mynames[4], xlim=c(0,4.5), ylim=c(0,4.5)) 
 
 dev.off();
 
} #i loop




quit("yes")
 
 #beadsRG = rectangleGate(filterId="beads", list("FSC-A"=c(0,2E4), "SSC-A"=c(1E5,1.5E5), "PerCP-A"=c(mymap$beads.cutoff.PI[i], Inf) ))
 #b = filter( myfcs, beadsRG)
 #beads = Subset( myfcs, b )
 #summary(beads@exprs)
 #mymap$beads[i] = length( beads@exprs[,1]) 
 #myfcs2 = myfcs; 
 #res1 = flowClust( myfcs@exprs[,1:2], K=1 )
  
 doit = function() {
  s1filter = tmixFilter( "s1filter", c("FSC-A","SSC-A"), K=2)
  res1 = filter( myfcs, s1filter);
  myfcs2 = Subset( myfcs, res1 );
  bmp(paste("sandbox/", mystrain, ".", tokens[[1]][1], tokens[[1]][2],".SSC-FSC",".bmp", sep="") );
  plot(res1, data=myfcs, level=0.95);
  points( beads@exprs[,1:2], col="red", pch=19);
  dev.off()
 }
 #try( doit(), TRUE)
 
 exprs2 = data.frame( myfcs2@exprs );
 #alive = exprs2[ exprs2$PerCP.A < mymap$dead.cutoff.PI[i],  ]
 #dead  = exprs2[ exprs2$PerCP.A > 1.5 * mymap$dead.cutoff.PI[i],  ]
 
#} #i loop




#quit("yes")

