makeFormula<-function(IV,IV2,DV,result,an_vars){
  
  assign_string = "<<"  
  when_string = "="
  times_string = HTML("&times;")
  times_string = "x"
  a<-result$uModel
  use<-!grepl("participant",names(a$coefficients))
  coeffs<-a$coefficients[use]
  use<-!grepl("participant",an_vars)
  an_vars<-an_vars[use]

  if (1==2){
  if (DV$type=="Interval") {
    dvm<-mean(result$dv,na.rm=TRUE)
    dvs<-sd(result$dv,na.rm=TRUE)
  } else {
    dvm<-0
    dvs<-1
  }
  
  if (IV$type=="Interval") {
    iv1m<-mean(result$iv,na.rm=TRUE)
    iv1s<-sd(result$iv,na.rm=TRUE)
    iv1nc<-1
  } else {
    iv1m<-0
    iv1s<- 1
    iv1nc<-IV$ncats-1
  }
    
    if (!is.null(IV2)){
      if (IV2$type=="Interval") {
        iv2m<-mean(result$iv2,na.rm=TRUE)
        iv2s<-sd(result$iv2,na.rm=TRUE)
        iv2nc<-1
      } else {
        iv2m<-0
        iv2s<- 1
        iv2nc<-IV2$ncats-1
      }
    }
    
    if (is.null(IV2)){
      intercept<-1
      iv1<-intercept+(1:iv1nc)
      c0<-c(
        -coeffs[intercept]-sum(coeffs[iv1])*iv1m/iv1s,
        coeffs[iv1]/iv1s
      )
      c1<- c(dvm,rep(0,length(c0)-1))+c0*dvs
      coeffs<-c1
    } else {
      intercept<-1
      iv1<-intercept+(1:iv1nc)
      iv2<-max(iv1)+(1:iv2nc)
      iv1iv2<-max(iv2)+(1:iv1nc*iv2nc)
      if (length(coeffs)<4) {
        c1<-c(
          -coeffs[intercept]-coeffs[iv1]*iv1m/iv1s-coeffs[iv2]*iv2m/iv2s,
          coeffs[iv1]/iv1s,
          coeffs[iv2]/iv2s
        )
      } else {
        c1<-c(
          coeffs[intercept]-coeffs[iv1]*iv1m/iv1s-coeffs[iv2]*iv2m/iv2s+coeffs[iv1iv2]*iv1m/iv1s*iv2m/iv2s,
          coeffs[iv1]/iv1s-coeffs[iv1iv2]*iv2m/iv1s/iv2s,
          coeffs[iv2]/iv2s-coeffs[iv1iv2]*iv1m/iv1s/iv2s,
          coeffs[iv1iv2]/iv1s/iv2s
        )
      }
      c1<- c(dvm,rep(0,length(c1)-1))+c1*dvs
      coeffs<-c1
    }
  }
  switch (DV$type,
          "Interval"={
            an_model<-paste(DV$name,assign_string,sep="")
          },
          "Categorical"={
            an_model<-paste("logit(", DV$name, when_string, DV$cases[2], ") ", assign_string, sep="")
          }
  )
  
  if (coeffs[1]>=0){join<-" +"}else{join<-" -"}
  an_model<-paste(an_model, join, format(abs(coeffs[1]),digits=report_precision)   ,sep="")
  
  
  for (i in 2:length(coeffs)){
    if (!is.na(coeffs[i])) {
      if (coeffs[i]>=0){join<-" +"}else{join<-" -"}
      an_model<-paste(an_model, join, format(abs(coeffs[i]),digits=report_precision)   ,times_string,an_vars[i])
    }
  }
  
  an_model
}



reportDescription<-function(IV,IV2,DV,result){
  
  switch (DV$type,
          "Interval"={
            outputText<-c("\bLinear Model", "","","")
          },
          "Categorical"={
            outputText<-c("\bGeneralized Linear Model","","","")
          }
  )
  
  a<-result$model
  
  an_vars<-variable.names(a)
  an_vars<-sub("iv1$",IV$name,an_vars)
  
  if (!is.null(IV2)) {
    an_vars<-sub("iv2$",IV2$name,an_vars)
    an_vars<-sub("iv2",paste(IV2$name,"|",sep=""),an_vars)
  } 
  an_vars<-sub("iv1:",paste(IV$name,":",sep=""),an_vars)
  an_vars<-sub("iv1",paste(IV$name,"|",sep=""),an_vars)
  
  an_model<-makeFormula(IV,IV2,DV,result,an_vars)
  outputText<-c(outputText,paste("Formula:",an_model),"","","")
  outputText<-c(outputText,paste("R^2=",format(result$rFull^2,digits=report_precision),sep=""),"","","")
  
  switch (no_ivs,
          {
            if (IV$type=="Categorical" && DV$type=="Interval"){
              outputText<-c(outputText,"","","","")
              outputText<-c(outputText,"","Mean","SD","")
              mn<-c()
              for (i in 1:IV$ncats){
                use<-(result$iv==IV$cases[i])
                v<-result$dv[use]
                mn[i]<-mean(v)
                s<-sd(v)
                outputText<-c(outputText,IV$cases[i],format(mn[i],digits=report_precision),format(s,digits=report_precision),"")
              }
              rsd<-sd(result$uModel$residuals,na.rm=TRUE)
              if (IV$ncats==2){
                outputText<-c(outputText,"Difference:",format(diff(mn),digits=report_precision),"sd(residuals):",format(rsd,digits=report_precision))
              } else {
                outputText<-c(outputText,"sd(means):",format(sd(result$uModel$fitted),digits=report_precision),"sd(residuals):",format(rsd,digits=report_precision))
              }
            }
          })
  
  outputText<-c(outputText,"","","","")
  outputText<-c(outputText,"\bEffect Size ","\bNormalized","","")
  
  switch (no_ivs,
          { result$rIVse<-r2se(result$rIV,result$nval)
          outputText<-c(outputText,paste(IV$name," ="),paste(format(result$rIV,digits=report_precision),
                                                             " +/- ",format(result$rIVse,digits=report_precision),
                                                             sep=""),"","")
          },{
            outputText<-c(outputText,"\bVariable","\bdirect","\bunique","\btotal")
            outputText<-c(outputText,IV$name,
                          format(result$r$direct[1],digits=report_precision),format(result$r$unique[1],digits=report_precision),format(result$r$total[1],digits=report_precision))
            outputText<-c(outputText,IV2$name,
                          format(result$r$direct[2],digits=report_precision),format(result$r$unique[2],digits=report_precision),format(result$r$total[2],digits=report_precision))
            outputText<-c(outputText,paste(IV$name,":",IV2$name,sep=""),
                          format(result$r$direct[3],digits=report_precision),format(result$r$unique[3],digits=report_precision),format(result$r$total[3],digits=report_precision))
          }
  )
  
  
  an_rt<-format(result$rFull,digits=report_precision) 
  an_rset<-format(result$rFullse,digits=report_precision)
  
  outputText<-c(outputText,"","","","")
  outputText<-c(outputText,"\bFull model =",paste(an_rt,"+/-",an_rset),"","")
  nc=4
  nr=length(outputText)/nc
  
  reportPlot(outputText,nc,nr)        
  
  
}
