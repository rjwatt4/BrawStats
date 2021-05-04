
reportPlot<-function(outputText,nc,nr){
  
  margin=0.5
  top=12.5
  colSpace=2
  font_size=3.5
  font_size_extra=0
  characterWidth=1/14
  
  oT<-matrix(outputText,ncol=nc,byrow=TRUE)
  nT<-nchar(oT)
  nrT<-rowSums(nT)
  
  # now break into blocks separated by empty rows
  blockEnds<-c(0,which(nrT==0),nrow(nT))
  colX<-c()
  for (i in 2:length(blockEnds)){
    block<-nT[(blockEnds[i-1]+1):blockEnds[i],]
    if (is.null(dim(block))){
      colSize<-block+colSpace
      colOffset<-cumsum(c(0,colSize))
      colX<-c(colX,rep(colOffset[1:length(block)],1))
    }
    else     {
      colSize<-apply(block,2,max)+colSpace
      colOffset<-cumsum(c(0,colSize))
      colX<-c(colX,rep(colOffset[1:ncol(block)],nrow(block)))
    }
  }
  x_gap1<-colX*font_size*characterWidth
  
  # x_gap=apply(nchar(matrix(outputText,nrow=nr,ncol=nc,byrow=TRUE)),2,max)*0.25+2
  # x_gap=cumsum(c(0,x_gap[1:nc-1]))
  # x_gap1=rep(x_gap,nr)
  
  d<-expand.grid(x=1:nc,y=1:nr)

  boldlabels<-grepl("\b",outputText)
  outputText<-sub("\b","",outputText)
  pts<-data.frame(x=x_gap1,y=d$y,labels=outputText)
  g<-ggplot(pts,aes(x=x,y=y))
  
  if (any(boldlabels)){
    pts1<-data.frame(x=x_gap1[boldlabels],y=d$y[boldlabels],labels=outputText[boldlabels])
    g<-g+geom_text(data=pts1,aes(x=x+1, y=top+1-y, label=labels), hjust=0, vjust=0, size=font_size+font_size_extra, fontface="bold")
    pts<-data.frame(x=x_gap1[!boldlabels],y=d$y[!boldlabels],labels=outputText[!boldlabels])
  }

  g<-g+geom_text(data=pts,aes(x=x+1, y=top+1-y, label=labels), hjust=0, vjust=0, size=font_size)
  g+labs(x="",y="")+
    plotTheme+
    theme(legend.position = "none")+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.background = element_rect(fill=maincolours$graphC, colour=maincolours$graphC)
    )+
    coord_cartesian(xlim = c(1-margin,25+margin), ylim = c(1-margin,top+margin))
}
