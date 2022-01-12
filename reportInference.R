reportInference<-function(IV,IV2,DV,effect,result){

  nc<-length(result$anova)+1
  
  an_name<-result$an_name
    outputText<-rep("",nc*2)
    outputText[1]<-paste("\b",an_name,sep="")
    
    if (is.null(IV2)){
      outputText<-c(outputText,"\btest-statistic","\b(df) ","\bvalue   ","\bp",rep("",nc-4))
      IVs<-result$iv
      DVs<-result$dv
      an<-result$anova
      pval<-result$pIV
      if (pval>=0.0001) {
        pvalText<-paste("p = ",format(pval,digits=report_precision),sep="")
      } else {
        pvalText<-"p < 0.0001"
      }
      
      t_name<-result$test_name
      df<-result$df
      tval<-result$test_val
      
      outputText<-c(outputText,t_name,df,format(tval,digits=report_precision),pvalText,rep("",nc-4))
    }
    
    outputText<-c(outputText,rep(" ",nc))
    
    outputText<-c(outputText,"\bANOVA",sub("^","\b",colnames(result$anova)))
    total_done<-FALSE
    for (i in 1:nrow(result$anova)){
      vn<-rownames(result$anova)[i]
      if (vn!="(Intercept)") {
        if (vn=="NULL") vn<-"Total"
        if (vn=="iv1"){vn<-paste("",result$IVs$name,sep="")}
        if (vn=="iv2"){vn<-paste("",result$IV2s$name,sep="")}
        if (vn=="iv1:iv2"){vn<-paste("",result$IVs$name,":",result$IV2s$name,sep="")}
        if (vn=="Residuals"){vn<-"Error"}
        if (vn=="Total"){
          vn<-"\bTotal"
          total_done<-TRUE
          }
        
        outputText<-c(outputText,vn)
        for (j in 1:ncol(result$anova)){
          if (is.na(result$anova[i,j])){
            outputText<-c(outputText,"")
          } else {
            outputText<-c(outputText,format(result$anova[i,j],digits=report_precision))
          }
        }
      }
    }
    if (!total_done) {
    ssq<-sum(result$anova[,1])
    df<-sum(result$anova[,2])
    # outputText<-c(outputText,rep(" ",nc))
    outputText<-c(outputText,"\bTotal",format(ssq,digits=report_precision),format(df,digits=report_precision),rep("",nc-3))
    }
    outputText<-c(outputText,rep(" ",nc))

    outputText<-c(outputText,"\bPower(w)", "\bObserved","\bActual",rep("",nc-3))   
    if (is.na(effect$rIV)) {effect$rIV<-0}
    outputText<-c(outputText," ",format(rn2w(result$rIV,result$nval),digits=3),
                                 format(rn2w(effect$rIV,result$nval),digits=3),
                  rep("",nc-3))
    
    nr=length(outputText)/nc

  reportPlot(outputText,nc,nr)        

}
