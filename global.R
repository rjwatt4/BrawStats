# doBootstrap and extraApply should really go together
switches<-list(do_explore=TRUE,do_files=TRUE,startBlank=FALSE,doLikelihood=TRUE,doBootstrap=FALSE,longHandLikelihood=TRUE,importOrdinals=TRUE)
debug<-FALSE

maincolours<-list(windowC="#002D40",panelC="#005E86",graphC="#BFECFF")
# maincolours<-list(windowC="#002D40",panelC="#005E86",graphC="#FFFFFF")

hypHue=0.986667
desHue=0.1
eviHue=0.33333
posHue=0.583
expHue=0.65
filHue=0.0833

mainSat=0.25
mainBright=0.95

subSat=0.16
subBright=1

darkSat=0.5
darkBright=0.85

fileBright=0.9
likeBright=0.8
exploreSat=0.8

panelcolours<-list(hypothesisC=hsv(hypHue,mainSat,mainBright),
                   designC=hsv(desHue,mainSat,mainBright),
                   simulateC=hsv(eviHue,mainSat,mainBright),
                   exploreC=hsv(expHue,mainSat*exploreSat,mainBright),
                   likelihoodC=hsv(posHue,mainSat,mainBright*likeBright),
                   filesC=hsv(filHue,mainSat,mainBright*fileBright)
)
subpanelcolours<-list(hypothesisC=hsv(hypHue,subSat,subBright),
                      designC=hsv(desHue,subSat,subBright),
                      simulateC=hsv(eviHue,subSat,subBright),
                      exploreC=hsv(expHue,subSat*exploreSat,subBright),
                      likelihoodC=hsv(posHue,subSat,subBright),
                      filesC=hsv(filHue,subSat,subBright*fileBright)
)
darkpanelcolours<-list(hypothesisC=hsv(hypHue,darkSat,darkBright),
                      designC=hsv(desHue,darkSat,darkBright),
                      simulateC=hsv(eviHue,darkSat,darkBright),
                      exploreC=hsv(expHue,darkSat*exploreSat,darkBright),
                      likelihoodC=hsv(posHue,darkSat,darkBright*likeBright),
                      filesC=hsv(filHue,darkSat,darkBright*fileBright)
)

plotcolours<-list(sampleC="#FFCC00",descriptionC="#FF8833",
                  descriptionC1="#FF5533",descriptionC2="#CCBB33",
                  infer_sigC="#22FF00",infer_nsigC="#FF2222",
                  infer_err="#333333",infer_nerr="#00CCFF")

localStyle="font-size:8pt;font-weight:bold;text-align: right;"
localPlainStyle="font-size:8pt;font-weight:normal;text-align: right;"
helpStyle=paste("font-size:7pt;line-height:75%;margin:0px;margin-top:-6px;padding:0px;", "color:", maincolours$panelC, ";",sep="")
helpChar=HTML("<span style=\"color:#005E86;\"><b>?</b></span>")

IV<-list(name="IV",type="Interval",mu=0,sd=1,ncats=2,cases="C1,C2",proportions="1,1")
IV2<-list(name="none",type="Interval",mu=0,sd=1,ncats=2,cases="D1,D2",proportions="1,1")
DV<-list(name="DV",type="Interval",mu=0,sd=1,ncats=2,cases="E1,E2",proportions="1,1")

effect<-list(rIV=0,rIV2=0,rIVIV2=0,rIVIV2DV=0,Heteroscedasticity=0,Welch=FALSE,ResidDistr="normal")

design<-list(sN=42, sMethod="Random" ,sIV1Use="Between",sIV2Use="Between", 
             sRangeOn=FALSE, sIVRange=c(-3,3), sDVRange=c(-3,3), 
             sDependence=0, sOutliers=0, sClustering=0,
             sN_Strata=5, sR_Strata=2,
             sNClu_Cluster=5,     sRClu_Cluster=0.7,
             sNClu_Convenience=1, sRClu_Convenience=0.7, sNCont_Convenience=5, sRCont_Convenience=0.7, sRSpread_Convenience=0.5,
             sNClu_Snowball=2,   sRClu_Snowball=0.7,   sNCont_Snowball=2,    sRCont_Snowball=0.7,    sRSpread_Snowball=0.1
)    

evidence<-list(rInteractionOn=TRUE,showType="direct")


fullRange<-3
allScatter<-"all"

warn3Cat2<-FALSE
warnOrd<-FALSE
warn3Ord<-FALSE

is_local <- Sys.getenv('SHINY_PORT') == ""
if (is_local) {
  # switches$doBootstrap<-TRUE
  quickHypos<-"y"
} else {
  quickHypos<-"n"
}

