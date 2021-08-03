plot.compositeKDE <- function(x, calendar="BP", type='envelope', ylim=NA, xlim=NA, fill.col='lightgrey',interval=0.95,line.col='black',line.type=2, multiline.alpha=NA, multiline.col='black',xaxt='s', yaxt='s', add=FALSE,...){
  
  types <- c("envelope","multiline")
  if (!type %in% types){
    stop("The plot type you have chosen is not currently an option.")
  }
  if (any(is.na(ylim))){ ylim <- c(0,max(x$res.matrix,na.rm=TRUE)*1.1)}
  
  plotyears = x$timeRange[1]:x$timeRange[2]
  
  if (calendar=="BP"){
    xlabel <- "Years cal BP"
    if (any(is.na(xlim))){ xlim <- c(max(plotyears),min(plotyears)) }
  } else if (calendar=="BCAD"){
    plotyears <- BPtoBCAD(plotyears)
    xlabel <- "Years BC/AD"
    if (all(range(plotyears)<0)){xlabel <- "Years BC"}
    if (all(range(plotyears)>0)){xlabel <- "Years AD"}
    if (any(is.na(xlim))){ xlim <- c(min(plotyears),max(plotyears)) }
  } else {
    stop("Unknown calendar type")
  }
  if (xaxt=='n'){ xlabel <- "" }
  if (yaxt=='n'){ ylabel <- "" } else { ylabel <- ylab }
  if (type=='multiline')
  {
    plot(x=plotyears,y=x$avg.res,type='n',xlab=xlabel,ylab="Summed Probability",xlim=xlim,ylim=ylim,xaxt="n", yaxt=yaxt,...)
    if (is.na(multiline.alpha)){multiline.alpha=10/ncol(x$res.matrix)}
    if (multiline.alpha>1){multiline.alpha=1}
    mc = c(as.numeric(col2rgb(multiline.col)/255),multiline.alpha)
    apply(x$res.matrix,2,lines,x=plotyears,col=rgb(mc[1],mc[2],mc[3],mc[4]))
  }
  
  if (type=='envelope')
  {
    avg=apply(x$res.matrix,1,mean,na.rm=TRUE)
    lo=apply(x$res.matrix,1,quantile,prob=(1-interval)/2,na.rm=TRUE)
    hi=apply(x$res.matrix,1,quantile,prob=interval+(1-interval)/2,na.rm=TRUE)
    index = which(!is.na(hi))
    plot(x=plotyears,y=avg,type='n',xlab=xlabel,ylab="Summed Probability",xlim=xlim,ylim=ylim,xaxt="n", yaxt=yaxt,...)
    polygon(x=c(plotyears[index],rev(plotyears[index])),y=c(lo[index],rev(hi[index])),border=NA,col=fill.col)
    lines(plotyears,avg,lty=line.type,col=line.col,lwd=2)
  }
  if (calendar=="BP"& xaxt!="n"){
    rr <- range(pretty(plotyears))    
    axis(side=1,at=seq(rr[2],rr[1],-100),labels=NA,tck = -.01)
    axis(side=1,at=pretty(plotyears),labels=abs(pretty(plotyears)))
  } else if (calendar=="BCAD"& xaxt!="n"){
    yy <-  plotyears
    rr <- range(pretty(yy))    
    prettyTicks <- seq(rr[1],rr[2],+100)
    prettyTicks[which(prettyTicks>=0)] <-  prettyTicks[which(prettyTicks>=0)]-1
    axis(side=1,at=prettyTicks, labels=NA,tck = -.01)
    py <- pretty(yy)
    pyShown <- py
    if (any(pyShown==0)){pyShown[which(pyShown==0)]=1}
    py[which(py>1)] <-  py[which(py>1)]-1
    axis(side=1,at=py,labels=abs(pyShown))
  }
  axis(2)
  box()
}