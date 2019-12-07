# Load grades 
df <- read.csv("grades-final.csv")
ea <- df$Finalgrades.num
ea <- ea[!is.na(ea)]

# grade breaks
lims <- sort(c(100,94.0,90.0,86.7,83.4,80.0,76.7,73.4,70.0,66.7,63.4,60.0,59.9,0))

# get center coords for histogram
tb <- hist(ea,breaks = c(110,90.0,80.0,70.0,60.0,0),freq = T,ylim=c(0,20))$counts


par(mfrow=c(1,2))
hist(ea,breaks = lims,freq = T,xlim=c(40,110),col="gold",
     main = "BIO114 Final Grade 2019 Spring")
#abline(v = lims,lty=3,lwd=4,col=c("lightgrey"))
abline(v = median(ea,na.rm = T), lty=4,col=scales::alpha("green",0.4),lw=5)
abline(v = mean(ea,na.rm = T), lty=4,col=scales::alpha("purple",0.4),lw=5)
abline(v = c(90,80,70,60),lty=3,lwd=4,col=c("dimgrey"))
text(x = mean(ea,na.rm = T),y = 0.3,labels = "mean",cex = .75)
text(x = median(ea,na.rm = T),y = 0.5,labels = "median",cex = .75)
text(x = c(95,85,75,65,55),y = 10,labels = c("A","B","C","D","F"))
text(x = c(55,65,75,85,95),y = 9,labels = tb)

boxplot(ea, notch = T,col = "gold",ylim=c(50,100), main="BIO114 Final Grade 2019 Spring")
abline(h = c(90,80,70,60),lty=3,lwd=4,col=c("dimgrey"))
abline(h = c(94.0,86.7,83.4,76.7,73.4,66.7,63.4,59.9),lty=3,lwd=4,col=c("azure3"))
points(mean(ea,na.rm = T),pch=10,cex=2,col="purple")
text(x = mean(ea,na.rm = T),labels = "mean",cex = .75,col="purple")
stripchart(ea,col = scales::alpha("blue",0.4),pch=19,method = "jitter",
           vertical = T,add=T,cex=1)
text(x = .5,y=c(95,85,75,65,55),labels = c("A","B","C","D","F"))
text(x = .6,y=c(55,65,75,85,95),labels = tb)
par(mfrow=c(1,1))
