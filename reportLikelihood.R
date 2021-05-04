reportLikelihood<-function(Iv,DV,effect,design,likelihood,likelihoodResult){
  rho<-effect$rIV
  
  nc<-3
  outputText<-rep("",nc)
  outputText[1]<-paste("Possible:",likelihood$type,sep="")
  
  switch (likelihood$type,
          "Samples"={
            outputText[3]<-paste("no sims = ",format(length(likelihoodResult$sSims)),sep="")
            outputText<-c(outputText,"Fixed Population:",paste("effect-size=", format(likelihood$populationES,digits=report_precision),sep=""),"")
            outputText<-c(outputText,rep("",nc))
            outputText<-c(outputText," ","Theory","Simulation")
            outputText<-c(outputText,"max(samples)  ",format(likelihoodResult$rs_peak,digits=report_precision),format(likelihoodResult$rsSim_peak,digits=report_precision))
            outputText<-c(outputText,"sd(samples)",format(likelihoodResult$rs_sd,digits=report_precision),format(likelihoodResult$rsSim_sd,digits=report_precision))
            outputText<-c(outputText,"CI(samples)",
                          paste("[", format(likelihoodResult$rs_ci[1],digits=report_precision), ",", format(likelihoodResult$rs_ci[2],digits=report_precision), "]"),
                          paste("[", format(likelihoodResult$rsSim_ci[1],digits=report_precision), ",", format(likelihoodResult$rsSim_ci[2],digits=report_precision), "]")
            )
            outputText<-c(outputText,rep("",nc))
            outputText<-c(outputText,"Sample Likelihood:",format(likelihoodResult$expected_r_at_peak_dens,digits=report_precision),"")
            if (length(likelihoodResult$sSims)==0){
              outputText[seq(12,21,3)]<-" "
              }
          },
          "Populations"={
            outputText[3]<-paste("no sims = ",format(length(likelihoodResult$pSims)),sep="")
            outputText<-c(outputText,"Fixed Sample:",paste("effect-size=", format(mean(likelihood$sampleES),digits=report_precision),sep=""),"")
            outputText<-c(outputText,rep("",nc))
            outputText<-c(outputText," ","Theory","Simulation")
            outputText<-c(outputText,"max(populations)  ",format(likelihoodResult$rp_peak,digits=report_precision),format(likelihoodResult$rpSim_peak,digits=report_precision))
            outputText<-c(outputText,"sd(populations)",format(likelihoodResult$rp_sd,digits=report_precision),format(likelihoodResult$rpSim_sd,digits=report_precision))
            outputText<-c(outputText,"CI(samples)",
                          paste("[", format(likelihoodResult$rp_ci[1],digits=report_precision), ",", format(likelihoodResult$rp_ci[2],digits=report_precision), "]"),
                          paste("[", format(likelihoodResult$rpSim_ci[1],digits=report_precision), ",", format(likelihoodResult$rpSim_ci[2],digits=report_precision), "]")
            )
            outputText<-c(outputText,rep("",nc))
            outputText<-c(outputText,"Population Likelihood:",format(likelihoodResult$expected_r_at_peak_dens,digits=report_precision),"")
            if (length(likelihoodResult$pSims)==0){
              outputText[seq(12,21,3)]<-" "
              }
          }
          )
  nr=length(outputText)/nc

  reportPlot(outputText,nc,nr)        

}
