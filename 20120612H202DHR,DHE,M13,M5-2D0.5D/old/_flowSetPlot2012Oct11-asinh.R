
rm(list=ls())

debug = 0; 
require(flowCore);  require(flowClust); require(flowViz); require(stringr)

setwd("~/lab/spelman.lab/calibur/lab.calibur/H2O2-DHR-DHE-project/20120612H202DHR,DHE,M13,M5-2D0.5D/analysisOn2012612")
list.files("../fcs.20120612") #20121011 change

mydate = ''
myp='M13'
myp='M5'
mypath='../fcs.20120612/M5DHR2D'
mypath='../fcs.20120612/M5DHR0.5D'
myalias = paste(substr(mypath,4,27), myp, mydate, sep='.'); print(myalias)
myalias = str_replace( myalias, "/", "."); print(myalias)
      
 fs = read.flowSet(path=mypath, pattern=myp)
 print(myp)
 pData(phenoData(fs))
 xyplot(`SSC-H` ~ `FSC-H`, data=fs);
 xyplot(`FL1-H` ~ `FL3-H`, data=fs);

 #tf = transformList(from=colnames(fs), tfun=log10)
 tf = transformList(from=colnames(fs), tfun=asinh)
 #not working #tf = transformList(from=colnames(fs), tfun=function(x) if(x>0) log10(x) else 0 ) 
 fs2 = tf %on% fs
 xyplot(`SSC-H` ~ `FSC-H`, data=fs2);
 xyplot(`FL1-H` ~ `FL3-H`, data=fs2);

 rgate1 = rectangleGate('FSC-H'=c(asinh(100), asinh(1E4)), 'SSC-H'=c(asinh(100),asinh(1E4)))
 my.filter = filter(fs2, rgate1)
 fs3 = Subset(fs2, my.filter)
 print(fs3)
 
 pdf(paste(myalias, 'FL1-3scatter', 'pdf', sep='.'))
 xyplot(`FL3-H` ~ `FL1-H`, data=fs3, filter=rgate1)
 #xyplot(`FL3-H` ~ `FL1-H`, data=fs3)
 dev.off()

  pData(phenoData(fs3))
# fs3@phenoData@data$name = c('0% glu', '0.01% glu', '0.1% glu', '0.25% glu', '0.5% glu', '1% glu', '2% glu') #2012 July 11 change
 
 pdf(paste(myalias, 'FL1-3marginal', 'pdf', sep='.'), width=6, height=4)
 #densityplot(~ `FL1-H`, data=fs3, filter=curv1Filter("FL1-H")) 
 #densityplot(~ ., fs3, channels=c("FL1-H", "FL3-H"), filter=list(curv1Filter("FL1-H"),  curv1Filter("FL3-H")))

 densityplot(~ ., fs3, channels=c("FL1-H", "FL3-H"), filter=list(curv1Filter("FL1-H"),  curv1Filter("FL3-H")),
              xlab=c('H2O2 signals','Superoxide signals'));
 #densityplot(~ "FL1-H", fs3,  filter=curv1Filter("FL1-H") )
             
             dev.off()
 
 pdf(paste(myalias, 'SSC-FSC', 'pdf', sep='.'));
 xyplot(`SSC-H` ~ `FSC-H`, data=fs3, filter=rgate1);
 dev.off()
 

#quit("no")
