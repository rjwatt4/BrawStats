library(ggplot2)

# maincolours<-list(windowC="#002D40",panelC="#005E86",graphC="#BFECFF")
# maincolours<-list(windowC="#002D40",panelC="#005E86",graphC="#FFFFFF")
# 
# hypHue=0.986667
# desHue=0.1
# eviHue=0.33333
# posHue=0.583
# expHue=0.8738
# filHue=0.0833
# 
# mainSat=0.25
# mainBright=0.95
# 
# subSat=0.16
# subBright=1
# 
# darkSat=0.5
# darkBright=0.75
# 
# panelcolours<-list(hypothesisC=hsv(hypHue,mainSat,mainBright),
#                       designC=hsv(desHue,mainSat,mainBright),
#                       simulateC=hsv(eviHue,mainSat,mainBright),
#                       exploreC=hsv(expHue,mainSat,mainBright),
#                       likelihoodC=hsv(posHue,mainSat,mainBright),
#                       filesC=hsv(filHue,mainSat,mainBright)
#                       )
# subpanelcolours<-list(hypothesisC=hsv(hypHue,subSat,subBright),
#                       designC=hsv(desHue,subSat,subBright),
#                       simulateC=hsv(eviHue,subSat,subBright),
#                       exploreC=hsv(expHue,subSat,subBright),
#                       likelihoodC=hsv(posHue,subSat,subBright),
#                       filesC=hsv(filHue,subSat,subBright)
# )
# darkpanelcolours<-list(hypothesisC=hsv(hypHue,darkSat,darkBright),
#                       designC=hsv(desHue,darkSat,darkBright),
#                       simulateC=hsv(eviHue,darkSat,darkBright),
#                       exploreC=hsv(expHue,darkSat,darkBright),
#                       likelihoodC=hsv(posHue,darkSat,darkBright),
#                       filesC=hsv(filHue,darkSat,darkBright)
# )
# 
# # subpanelcolours<-list(hypothesisC="#FFD6DB",designC="#F6DFBD",simulateC="#CFF8CF",exploreC="#DDBBDD",filesC="#EEBB88",likelihoodC="#DDDDBB")
# # darkpanelcolours<-list(hypothesisC=dataC,designC="#F6DFBD",simulateC="#CFF8CF",exploreC="#DDBBDD",filesC="#EEBB88",likelihoodC="#DDDDBB")
# # panelcolours<-list(hypothesisC="#F3B6BB",designC="#E6CFAD",simulateC="#ADE6AD",exploreC="#BB99BB",filesC="#BE966D",likelihoodC="#BBBB99")
# 
# plotcolours<-list(sampleC="#FFCC00",descriptionC="#FF8833",
#                   descriptionC1="#FF5533",descriptionC2="#CCBB33",
#                   infer_sigC="#22FF00",infer_nsigC="#FF2222",
#                   infer_err="#333333",infer_nerr="#00CCFF")
useSignificanceCols<-FALSE

localStyle="font-size:8pt;font-weight:bold;text-align: right;"
helpStyle=paste("font-size:7pt;line-height:75%;margin:0px;margin-top:-6px;padding:0px;", "color:", maincolours$panelC, ";",sep="")

fullShowHelp<-FALSE

report_precision<-3
graph_precision<-2

mainplotMargins<-margin(1,3,1,3,"cm");
popplotMargins<-margin(0.15,0.8,0,0.25,"cm");

plotTheme=theme(panel.background = element_rect(fill="#666666", colour="black"),
                panel.grid.major = element_line(linetype="blank"),panel.grid.minor = element_line(linetype="blank"),
                plot.background = element_rect(fill=maincolours$graphC, colour=maincolours$graphC),
                axis.title=element_text(size=16,face="bold")
)

plotBlankTheme=theme(panel.background = element_rect(fill=maincolours$graphC, colour=maincolours$graphC),
                panel.grid.major = element_line(linetype="blank"),panel.grid.minor = element_line(linetype="blank"),
                plot.background = element_rect(fill=maincolours$graphC, colour=maincolours$graphC),
                axis.title=element_text(size=16,face="bold")
)


mergeVariables<-FALSE
showInteractionOnly<-TRUE
hideIV2Tab<-FALSE

is_local <- Sys.getenv('SHINY_PORT') == ""
if (is_local) {
  quickHypos<-TRUE
} else {
  quickHypos<-FALSE
}
shiftKeyOn<-FALSE
controlKeyOn<-FALSE
altKeyOn<-FALSE

validSample<-FALSE
validExpected<-FALSE
validExplore<-FALSE
validLikelihood<-0

show<-0

points_threshold=50
wPlotScale="log10"
# wPlotScale="linear"
pPlotScale="log10"

alpha<-0.05
anovaSSQType<-2

makeVar<-function(name,type="Interval",
                  mu=0,sd=1,skew=0,kurtosis=3,
                  nlevs=7,iqr=3,median=4,discrete=TRUE,
                  ncats=2,cases="C1,C2",proportions="1,1",source="Discrete",
                  deploy="Between",process="sim"){
  
  var<-list(name=name,type=type,
       mu=mu,sd=sd,skew=skew,kurtosis=kurtosis,
       nlevs=nlevs,iqr=iqr,median=median,discrete=discrete,
       ncats=ncats,cases=cases,proportions=proportions,source=source,
       deploy=deploy,process=process)
  # do ordinal mean and sd (for graphs)
  if (var$type=="Ordinal") {
    var$mu<-var$median
    var$sd<-var$iqr/2
  }
  # check for cases
  cs<-strsplit(var$cases,",")
  cs<-cs[[1]]
  if (length(cs)<var$ncats){
    cs<-c(cs,paste("C",(length(cs)+1):var$ncats,sep=""))
  }
  if (length(cs)>var$ncats){
    cs<-cs[1:var$ncats]
  }
  var$cases<-paste(cs,sep='',collapse=',')
  # check for proportions
  pp<-strsplit(var$proportions,",")
  pp<-pp[[1]]
  if (length(pp)<var$ncats) {
    pp<-c(pp,rep("1",var$ncats-length(pp)))
  }
  if (length(pp)>var$ncats){
    pp<-pp[1:var$ncats]
  }
  var$proportions<-paste(pp,sep='',collapse=',')
  # return var
  var
}

defaultVars<-list(
  makeVar(name="IV",type="Interval",mu=0,sd=1,ncats=2,cases="C1,C2"),
  makeVar(name="IV2",type="Interval",mu=0,sd=1,ncats=2,cases="D1,D2"),
  makeVar(name="DV",type="Interval",mu=0,sd=1,ncats=2,cases="E1,E2"),

  makeVar(name="IQ",type="Interval",mu=100,sd=15),
  makeVar(name="Diligence",type="Interval",mu=0,sd=2),
  makeVar(name="Perfectionism",type="Interval",mu=0,sd=2),
  makeVar(name="Happiness",type="Interval",mu=50,sd=12),
  makeVar(name="Grade",type="Interval",mu=65,sd=10),
  makeVar(name="RiskTaking",type="Interval",mu=30,sd=6),
  
  makeVar(name="Smoker?",type="Categorical",ncats=2,cases="no,yes",proportions="2,1"),
  makeVar(name="RiskTaker?",type="Categorical",ncats=2,cases="no,yes"),
  makeVar(name="Musician?",type="Categorical",ncats=2,cases="no,yes"),
  
  makeVar(name="StudySubject",type="Categorical",ncats=3,cases="psych,phil,sports",proportions="1.5,1,2"),
  makeVar(name="BirthOrder",type="Categorical",ncats=4,cases="first,middle,last,only",proportions="1,0.4,0.6,0.2")
)

variables<-data.frame(defaultVars[[1]])
for (i in 2:length(defaultVars)){
  variables<-rbind(variables,defaultVars[[i]])
}

defaultVariables<-variables
variablesHeld<-"Simulations"

if (switches$startBlank) {
  variables[1,]$type="empty"
  variables[3,]$type="empty"
}

emptyVariable<-makeVar(name="none")

# make basic variables    
IV<-variables[1,]
IV2<-emptyVariable
DV<-variables[3,]
MV<-IV

no_ivs<-1
simData<-TRUE

getCases<-function(var) {
  cs<-strsplit(var$cases,",")
  cs<-cs[[1]]
  if (length(cs)<var$ncats){
    cs<-c(cs,paste("C",(length(cs)+1):var$ncats,sep=""))
  }
}

effect<-list(rIV=0,rIV2=0,rIVIV2=0,rIVIV2DV=0,Heteroscedasticity=0,Welch=FALSE)
# effect<-list(rIV=0.3,rIV2=0.3,rIVIV2=0,rIVIV2DV=0)

design<-list(sN=42, sMethod="Random" ,sIV1Use="Between",sIV2Use="Between", 
             sRangeOn=FALSE, sIVRange=c(-3,3), sDVRange=c(-3,3), 
             sDependence=0, sOutliers=0, sClustering=0,
             sN_Strata=5, sR_Strata=2,
             sNClu_Cluster=5,     sRClu_Cluster=0.7,
             sNClu_Convenience=1, sRClu_Convenience=0.7, sNCont_Convenience=5, sRCont_Convenience=0.7, sRSpread_Convenience=0.5,
             sNClu_Snowball=2,   sRClu_Snowball=0.7,   sNCont_Snowball=2,    sRCont_Snowball=0.7,    sRSpread_Snowball=0.1
)    

evidence<-list(rInteractionOn=TRUE,showType="direct")

varTypes<- c("Interval" = "Interval",
             "Ordinal" = "Ordinal",
             "Categorical" = "Categorical"
)


importedData<-c()
lastSample<-c()

expectedRunning<-FALSE

exploreResultHold<-c()
likelihood_P_ResultHold<-c()
likelihood_S_ResultHold<-c()

# allInputs=c("DVname", "DVtype", "DVmu", "DVsd", "DVncats", "DVcases", 
#             "IVname", "IVtype", "IVmu", "IVsd", "IVncats", "IVcases", 
#             "IV2name", "IV2type", "IV2mu", "IV2sd", "IV2ncats", "IV2cases", 
#             "rIV2DV", "rIVDV", "rIVIV2", "rIVIV2DV", "Heteroscedasticity", 
#             "sMethod", "sN", "sIV1Use", "sIV2Use", "sDependence", "sOutliers", "sIVRange", "sDVRange", "sRangeOn" )
