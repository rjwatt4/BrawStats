reportLikelihood<-function(Iv,DV,effect,design,likelihood,likelihoodResult){

  switch (likelihood$type,
          "Samples"={likelihoodResult<-likelihoodResult$samples},
          "Populations"={likelihoodResult<-likelihoodResult$populations}
  )
  
  nc<-3
  outputText<-rep("",nc)
  outputText[1]<-paste("\bPossible:",likelihood$type,sep="")
  
  switch (likelihood$type,
          "Samples"={
            outputText[1]<-paste(outputText[1]," (no sims = ",format(length(likelihoodResult$sSims)),")",sep="")
            
            outputText<-c(outputText,paste("Population ","effect-size=", format(likelihood$populationES,digits=report_precision),sep=""),"","")
            outputText<-c(outputText,rep("",nc))
            outputText<-c(outputText," ","Theory","Simulation")
            outputText<-c(outputText,"max(samples)  ",format(likelihoodResult$rs_peak,digits=report_precision),format(likelihoodResult$rsSim_peak,digits=report_precision))
            outputText<-c(outputText,"sd(samples)",format(likelihoodResult$rs_sd,digits=report_precision),format(likelihoodResult$rsSim_sd,digits=report_precision))
            outputText<-c(outputText,"CI(samples)",
                          paste("[", format(likelihoodResult$rs_ci[1],digits=report_precision), ",", format(likelihoodResult$rs_ci[2],digits=report_precision), "]"),
                          paste("[", format(likelihoodResult$rsSim_ci[1],digits=report_precision), ",", format(likelihoodResult$rsSim_ci[2],digits=report_precision), "]")
            )
              outputText<-c(outputText,rep(" ",nc))
              theory<-1-pnorm(likelihood$sampleES,atanh(likelihood$populationES),1/sqrt(likelihood$sampleN-3))+
                pnorm(-likelihood$sampleES,atanh(likelihood$populationES),1/sqrt(likelihood$sampleN-3))
              if (length(likelihoodResult$sSims)>0) {
              sims<-mean(abs(likelihoodResult$sr)>abs(likelihood$sampleES))
              } else {sims<-0}
              outputText<-c(outputText,"Sample Probability:",
                            format(theory,digits=report_precision),
                            format(sims,digits=report_precision))
            if (length(likelihoodResult$sSims)==0) {
              outputText[seq(12,27,3)]<-" "
              }
          },
          "Populations"={
            outputText[1]<-paste(outputText[1]," (no sims = ",format(length(likelihoodResult$pSims)),")",sep="")
            outputText<-c(outputText,paste("Sample ","effect-size=", format(mean(likelihood$sampleES),digits=report_precision),sep=""),"","")
            outputText<-c(outputText,rep("",nc))
            outputText<-c(outputText," ","Theory","Simulation")
            outputText<-c(outputText,"max(populations)  ",format(likelihoodResult$rp_peak,digits=report_precision),format(likelihoodResult$rpSim_peak,digits=report_precision))
            outputText<-c(outputText,"sd(populations)",format(likelihoodResult$rp_sd,digits=report_precision),format(likelihoodResult$rpSim_sd,digits=report_precision))
            outputText<-c(outputText,"CI(samples)",
                          paste("[", format(likelihoodResult$rp_ci[1],digits=report_precision), ",", format(likelihoodResult$rp_ci[2],digits=report_precision), "]"),
                          paste("[", format(likelihoodResult$rpSim_ci[1],digits=report_precision), ",", format(likelihoodResult$rpSim_ci[2],digits=report_precision), "]")
            )
            outputText<-c(outputText,rep("",nc))
            outputText<-c(outputText,"Likelihood(population=sample):",paste0("max * ",format(likelihoodResult$expected_r_at_peak_dens,digits=report_precision)),"")
            if (length(likelihoodResult$pSims)==0){
              outputText[seq(12,21,3)]<-" "
              }
          }
          )
  nr=length(outputText)/nc

  reportPlot(outputText,nc,nr)        

}
