
evidencePanel <- function(prefix="") {

  if (switches$doMetaAnalysis) {
    wellPanel(
      style = paste("background: ",subpanelcolours$simulateC,";"),
      tabsetPanel(id=paste0(prefix,"Show"), type="tabs",
                  tabPanel("Evidence:",value="Evidence",
                  ),
                  # single tab
                  metaPanel(prefix)
      )
    )
  } else {
    return(c())
}
}
