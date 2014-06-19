
rm(list=ls())

debug = 0; 
require(flowCore);  require(flowClust); require(hexbin); require(stringr)

mystrains=c("M5","M13")
mypatterns = c("DHR","DHE");

for(mys in mystrains) {
List1 = list.files( path="fcs", pattern = mys );  
Blanks =  List1[ str_detect(List1, perl("BLANK")) ]
DHEfile = sort( c(List1[ str_detect(List1, perl("DHE")) ], Blanks) )
DHRfile = sort( c(List1[ str_detect(List1, perl("DHR")) ], Blanks ) )
facFlList = c(DHRfile, DHEfile);

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
# if ( myp =='DHE') { current.FLX = mydata$FL3.H } #change here for other probes ##use stringr detect to change here !!!
 if( str_detect(myfl.full, perl("DHE"))) {  current.FLX = mydata$FL3.H }; #2012June 14 change

 if ( length(current.FLX) > Row.Len ) {current.FLX = current.FLX[1:Row.Len] 
 } else if (length(current.FLX < Row.Len)) {
   current.FLX = c(current.FLX, rep(NA, (Row.Len - length(current.FLX))))
 }                                     
   
 merged.FLX = cbind( merged.FLX, current.FLX)
 oldnames = colnames( merged.FLX )
 newnames = c(oldnames[-length(oldnames)], facFlList[i]) 
 colnames( merged.FLX ) = newnames
 
} #i loop

merged.FLX = merged.FLX[,-1] ###2012 June 10, change

#boxplot
pdf( paste("batch-boxplot", mys, "pdf",sep="."),  width=10,height=11)
par(oma=c(2,12,2,2), las=1) #adjust margin for labels, 2012June10 add las=1
#par(mar=c(2,20,2,2), las=1) #adjust margin for labels
boxplot( as.data.frame(merged.FLX), horizontal=TRUE, main=getwd())
dev.off()

}#mys

#quit("yes")
 
 