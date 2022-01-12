library(ggplot2)

switches<-list(do_explore=TRUE,do_files=FALSE)

maincolours<-list(windowC="#002D40",panelC="#005E86",graphC="#BFECFF")
# maincolours<-list(windowC="#002D40",panelC="#005E86",graphC="#FFFFFF")
subpanelcolours<-list(hypothesisC="#FFD6DB",designC="#F6DFBD",simulateC="#CFF8CF",exploreC="#DDBBDD",filesC="#EEBB88",likelihoodC="#DDDDBB")
panelcolours<-list(hypothesisC="#F3B6BB",designC="#E6CFAD",simulateC="#ADE6AD",exploreC="#BB99BB",filesC="#BE966D",likelihoodC="#BBBB99")

plotcolours<-list(sampleC="#FFCC00",descriptionC="#FF8833",
                  descriptionC1="#FF5533",descriptionC2="#CCBB33",
                  infer_sigC="#22FF00",infer_nsigC="#FF2222",
                  infer_err="#333333",infer_nerr="#00CCFF")

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
debug<-FALSE
shiftKeyOn<-FALSE
controlKeyOn<-FALSE

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
                  nlevs=7,spread=1.5,centre=0,
                  ncats=2,cases="C1,C2",proportions="1,1",
                  deploy="Between",process="sim"){
  
  var<-list(name=name,type=type,
       mu=mu,sd=sd,skew=skew,kurtosis=kurtosis,
       nlevs=nlevs,spread=spread,centre=centre,
       ncats=ncats,cases=cases,proportions=proportions,
       deploy=deploy,process=process)
  # check for cases
  cs<-strsplit(var$cases,",")
  cs<-cs[[1]]
  if (length(cs)<var$ncats){
    cs<-c(cs,paste("C",(length(cs)+1):var$ncats,sep=""))
  }
  var$cases<-paste(cs,sep='',collapse=',')
  # check for proportions
  pp<-strsplit(var$proportions,",")
  pp<-pp[[1]]
  if (length(pp)<var$ncats) {
    pp<-c(pp,rep("1",var$ncats-length(pp)))
  }
  var$proportions<-paste(pp,sep='',collapse=',')
  # return var
  var
}

vars<-list(
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
  
  makeVar(name="StudySubject",type="Categorical",ncats=3,cases="psych,phil,sports",proportions="1,1,1"),
  makeVar(name="BirthOrder",type="Categorical",ncats=4,cases="1st,middle,last,only",proportions="1,1,1,1")
)

variables<-data.frame(vars[[1]])
for (i in 2:length(vars)){
  variables<-rbind(variables,vars[[i]])
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

effect<-list(rIV=0,rIV2=0,rIVIV2=0,rIVIV2DV=0)
# effect<-list(rIV=0.3,rIV2=0.3,rIVIV2=0,rIVIV2DV=0)

design<-list(sN=42, sMethod="Random" ,sIV1Use="Between",sIV2Use="Between", 
             sRangeOn=FALSE, sIVRange=c(-3,3), sDVRange=c(-3,3), 
             sHeteroscedasticity=0,
             sDependence=0, sOutliers=0, sClustering=0
             )    

evidence<-list(rInteractionOn=TRUE,showType="direct")

varTypes<- c("Interval" = "Interval",
             "Ordinal" = "Ordinal",
             "Categorical" = "Categorical"
)


importedData<-c()
lastSample<-c()

expectedRunning<-FALSE
expectedResultHold<-c()
exploreResultHold<-c()
likelihoodPResultHold<-c()
likelihoodSResultHold<-c()

allInputs=c("DVname", "DVtype", "DVmu", "DVsd", "DVncats", "DVcases", 
            "IVname", "IVtype", "IVmu", "IVsd", "IVncats", "IVcases", 
            "IV2name", "IV2type", "IV2mu", "IV2sd", "IV2ncats", "IV2cases", 
            "rIV2DV", "rIVDV", "rIVIV2", "rIVIV2DV", 
            "sMethod", "sN", "sIV1Use", "sIV2Use", "sDependence", "sOutliers", "sHeteroscedasticity", "sIVRange", "sDVRange", "sRangeOn" )
