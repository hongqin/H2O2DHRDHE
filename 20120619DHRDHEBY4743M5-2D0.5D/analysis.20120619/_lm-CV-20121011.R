
csvfiles = list.files( pattern="csv", path='M5outputs'); csvfiles; 
mylen = length(csvfiles)
mydim = ceiling(sqrt(mylen))

mylabels = str_replace(csvfiles, "fcs", "")
mylabels = str_replace(mylabels, "csv", "")
mylabels = str_replace(mylabels, "firstDay", "")

pdf("20120619M5-lm.pdf", width=15, height=12)
layout(matrix(seq(1,mydim*mydim*2), mydim, mydim*2, byrow=T))
for ( i in 1:mylen ) {
  tb = read.csv(paste('M5outputs/',csvfiles[i], sep='') )
  #tb$H2O2 = c(0, 0.001, 0.0025, 0.005, 0.01, 0.025, 0.05, 0.075, 0.1)
  tb$H2O2 = c(0,  0.025, 0.05, 0.075, 0.1, 0.2)
  
  plot(tb$FL1.H ~ tb$H2O2, main=mylabels[i], xlab='H2O2',ylab='DHR')
  m = lm(tb$FL1.H ~ tb$H2O2)
  sm = summary(m)
  myp = 1 - pf( sm$fstat[1], sm$fstat[2], sm$fstat[3])
  myp = round(myp, digit=2)
  mtext(paste("p=",myp,sep=''), 3)
  
  plot(tb$FL3.H ~ tb$H2O2, main=mylabels[i], xlab='H2O2',ylab='DHE')
  m = lm(tb$FL3.H ~ tb$H2O2)
  sm = summary(m)
  myp = 1 - pf( sm$fstat[1], sm$fstat[2], sm$fstat[3])
  myp = round(myp, digit=2)
  mtext(paste("p=",myp,sep=''), 3)  
}
dev.off()