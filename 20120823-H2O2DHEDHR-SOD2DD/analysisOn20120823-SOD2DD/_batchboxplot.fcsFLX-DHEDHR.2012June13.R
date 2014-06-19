#2012 June 13, add patterns for DHR and DHE multiple channels, FL1, FL3

rm(list=ls())

debug = 0; 
require(flowCore);  require(flowClust); require(hexbin)

mypatterns = c("DHR","DHE");

for( myp in mypatterns ) {
facFlList = list.files( path="fcs", pattern = myp);  facFlList
facFlList2 = list.files( path="fcs", pattern = "BLANK");  facFlList2
facFlList = sort( c(facFlList, facFlList2))

#generate a dataframe
Row.Len = 5E3; 
merged.FLX = matrix(data=NA, nrow=Row.Len,ncol=1)
names(merged.FLX) = c("tmp")

#for( i in 1:4) {
for (  i in 1: length(facFlList) ) {
 # i = 1; 
 myfl.full = paste( 'fcs/', facFlList[i], sep='');
 myfcs = read.FCS( myfl.full );
# if(debug>0) {    myfcs@exprs = myfcs@exprs[1:Row.Len, ]   }
 
 #remove debris. pcp1D has much broad distribution and has to be done separately
 my.gate = rectangleGate(filterId="Cells", list("FSC-H"=c(2.5E2, Inf), "SSC-H"=c(2.5E2, Inf), "FL3-H"=c(0, Inf) ))
 my.filter = filter( myfcs, my.gate)
 myfcs2 = Subset( myfcs, my.filter )
 
 mydata = data.frame(log10(myfcs2@exprs))
 mynames = names(mydata)

 current.FLX = mydata$FL1.H   ### default for DHR; change here for other probes
 if ( myp =='DHE') { current.FLX = mydata$FL3.H } #change here for other probes
 
 merged.FLX = cbind( merged.FLX, current.FLX)
 oldnames = colnames( merged.FLX )
 newnames = c(oldnames[-length(oldnames)], facFlList[i]) 
 colnames( merged.FLX ) = newnames
 
} #i loop

merged.FLX = merged.FLX[,-1] ###2012 June 10, change

#boxplot
pdf( paste("batch-boxplot", myp, "pdf",sep="."),  width=10,height=11)
par(oma=c(2,12,2,2), las=1) #adjust margin for labels, 2012June10 add las=1
#par(mar=c(2,20,2,2), las=1) #adjust margin for labels
boxplot( as.data.frame(merged.FLX), horizontal=TRUE, main=getwd())
dev.off()

}#myp 

#quit("yes")
 
 