library(ggplot2)

useSignificanceCols<-FALSE

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
