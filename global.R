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

showLikelihood=TRUE
t.var.assume.equal<-TRUE

fullRange<-3
allScatter<-"all"

warn3Cat2<-FALSE
warnOrd<-FALSE
warn3Ord<-FALSE

is_local <- Sys.getenv('SHINY_PORT') == ""
if (is_local) {
  quickHypos<-"y"
} else {
  quickHypos<-"n"
}

