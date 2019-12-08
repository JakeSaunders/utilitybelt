# histogram for grades
histGrades <- function(
    grades,
    lims = c(110,94.0,90.0,86.7,83.4,80.0,76.7,73.4,70.0,66.7,63.4,60.0,0),
    maj.lims = c(90,80,70,60,0),
    xrange = c(50,100),
    bar.col = "gold",
    med.lty = 4,
    med.col = "green",
    med.lwd = 5,
    mean.lty = 4,
    mean.col = "purple",
    mean.lwd = 5,
    ...
){
    grades <- na.omit(grades)
    lims <- sort(lims)
    hi <- hist(grades, breaks = lims, freq = T, xlim=xrange, col=bar.col, ...)
    
    # median line
    abline(v = median(grades,T), lty=med.lty,col=scales::alpha(med.col,0.4),lw=med.lwd)
    text(x = median(grades),y = 0.5,pos = 1,labels = "median",cex = .75)
    # mean line
    abline(v = mean(grades), lty=mean.lty,col=scales::alpha(mean.col,0.4),lw=mean.lwd)
    text(x = mean(grades),y = 0.25,pos = 1,labels = "mean",cex = .75)
    
    # add lines for grades
    abline(v = maj.lims,lty=3,lwd=4,col=c("dimgrey"))
    
    # add grades
    counts <- hist(grades,breaks = c(110,maj.lims),plot = F)
    text(x = c(95,85,75,65,55),y = max(hi$counts),labels = c("A","B","C","D","F"))
    text(x = c(55,65,75,85,95),y = max(hi$counts)-1,labels = counts$counts)
}

# histogram for grades
boxGrades <- function(
    grades,
    lims = c(94.0,90.0,86.7,83.4,80.0,76.7,73.4,70.0,66.7,63.4,60.0),
    maj.lims = c(90,80,70,60),
    yrange = c(40,100),
    box.col = "gold",
    notch=TRUE,
    frame=FALSE,
    dot.col="blue",
    dot.cex=1,
    med.pch = 15,
    med.col = "green",
    mean.pch = 18,
    mean.col = "purple",
    ABCDF=c(95,85,75,65,55),
    ...
){
    grades <- na.omit(grades)
    
    boxplot(grades, col = box.col,ylim=yrange, notch=notch, frame=FALSE,...)
    
    points(median(ea),pch=med.pch,cex=2,col=scales::alpha(med.col,0.4))
    points(mean(grades),pch=mean.pch,cex=2,col=scales::alpha(mean.col,0.4))
    legend("topright",legend = c("median","mean"),bty="n", pch=c(med.pch,mean.pch),
           text.col = c(med.col,mean.col),col=c(med.col,mean.col), cex=1.5)
    
    text(x = .5,y=ABCDF,labels = c("A","B","C","D","F"))
    abline(h = lims,lty=3,lwd=2,col="azure3")
    abline(h = maj.lims,lty=3,lwd=4,col="dimgrey")
    
    stripchart(grades,col = scales::alpha(dot.col,0.4),pch=19,method = "jitter",
               vertical = T,add=T,cex=dot.cex)
}

