#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source("plotStatistic.R")
source("plotES.R")
source("plotReport.R")

source("drawVariable.R")
source("drawPopulation.R")
source("drawPrediction.R")

source("drawSample.R")
source("drawDescription.R")
source("drawInference.R")
if (switches$doMetaAnalysis) {source("drawMeta.R")}
source("drawExplore.R")
source("drawLikelihood.R")

source("sampleMake.R")
source("sampleAnalyse.R")
source("samplePower.R")
source("sampleRead.R")
source("sampleCheck.R")
source("Johnson_M.R")
source("sampleShortCut.R")

source("reportSample.R")
source("reportDescription.R")
source("reportInference.R")
source("reportExpected.R")
if (switches$doMetaAnalysis) {source("reportMetaAnalysis.R")}
source("reportExplore.R")
source("reportLikelihood.R")

if (switches$doMetaAnalysis) {source("runMetaAnalysis.R")}
source("runExplore.R")
source("runLikelihood.R")
source("runBatchFiles.R")

source("wsRead.R")
source("typeCombinations.R")

source("drawInspect.R")
source("isSignificant.R")

graphicSource="Main"

####################################
####################################
####################################

shinyServer(function(input, output, session) {
  
  source("myGlobal.R")
  source("runDebug.R")
  
####################################
# BASIC SET UP that cannot be done inside ui.R  
  shinyjs::hideElement(id= "EvidenceHypothesisApply1")
  shinyjs::hideElement(id= "EvidenceHypothesisApply")
  shinyjs::hideElement(id= "LGEvidenceHypothesisApply")
  shinyjs::hideElement(id= "Using")
  shinyjs::hideElement(id="EvidenceExpectedStop")
  updateSelectInput(session, "IVchoice", choices = variables$name, selected = variables$name[1])
  updateSelectInput(session, "IV2choice", choices = c("none",variables$name), selected = "none")
  updateSelectInput(session, "DVchoice", choices = variables$name, selected = variables$name[3])
  
  if (switches$doCheating) {
    exploreDesignChoices<<-c(exploreDesignChoices,"Cheating")
  } else {
    shinyjs::hideElement(id="Cheating")
    shinyjs::hideElement(id="LGEvidenceCheating")
    shinyjs::hideElement(id="LGExploreCheating")
    shinyjs::hideElement(id="LGlikelihoodCheating")
  }
  
  if (switches$doReplications) {
    exploreDesignChoices<<-c(exploreDesignChoices,"Replications")
  }
  updateSelectInput(session,"Explore_typeD",choices=designChoices[exploreDesignChoices])
  updateSelectInput(session,"LGExplore_typeD",choices=designChoices[exploreDesignChoices])
  
  if (switches$doWorlds) {
    exploreHypothesisChoices<<-c(exploreHypothesisChoices,"Worlds")
  }
  updateSelectInput(session,"Explore_typeH",choices=hypothesisChoices2[exploreHypothesisChoices])
  updateSelectInput(session,"LGExplore_typeH",choices=hypothesisChoices2[exploreHypothesisChoices])
  
  # observe( {
  #   req(input$width,input$height)
  #   print(c(input$width,input$height))
  # })
  
  ####################################
  
  source("serverKeys.R")
  serverKeys(session,input)
  
####################################
# other housekeeping
  observeEvent(input$allScatter,{
    allScatter<<-input$allScatter
  }
  )

  # observeEvent(input$sig_ns,{
  #   useSignificanceCols<<-input$sig_ns
  # }
  # )
  
  observeEvent(input$Explore_VtypeH, {
    if (debug) print("1")
      if (input$Explore_VtypeH=="levels") {
        updateSelectInput(session,"Explore_typeH",selected="DV")
      }
  }
  )
  
  observeEvent(input$sN, {
    if (debug) print("2")
    n<-input$sN
    if (n<1) {
      n<-rw2n(input$rIV,n,2)
      updateNumericInput(session,"sN",value=n)
    }
  }
  )
  
  observeEvent(input$Hypothesis,{
    if (debug) print("3")
    if (input$Hypothesis=="World") {
      updateTabsetPanel(session,"HypothesisDiagram",selected = "World")
    }
  })
  
  observeEvent(input$Evidence,{
    if (debug) print("4")
    
    if (input$Evidence=="Expected") {
      updateTabsetPanel(session,"Graphs",selected = "Expected")
    }
    if (input$Evidence=="MetaAnalysis") {
      updateTabsetPanel(session,"Graphs",selected = "MetaAnalysis")
    }
  })
  
  observeEvent(input$world_distr, {
    if (debug) print("5")
    if (input$world_distr!="Single" && input$world_distr_k==0) {
      updateNumericInput(session,"world_distr_k",value=0.2)
    }
  }
  )
  
  observeEvent(input$STMethod, {
    STMethod<<-input$STMethod
    switch (STMethod,
            "NHST"={
              updateNumericInput(session,"alpha",value=alpha)
              shinyjs::hideElement("evidencePrior")
              shinyjs::hideElement("STPrior")
              shinyjs::hideElement("evidenceLLR1")
              shinyjs::hideElement("evidenceLLR2")
              shinyjs::hideElement("llr1")
              shinyjs::hideElement("llr2")
            },
            "sLLR"={
              shinyjs::hideElement("evidencePrior")
              shinyjs::hideElement("STPrior")
              shinyjs::showElement("evidenceLLR1")
              shinyjs::showElement("evidenceLLR2")
              shinyjs::showElement("llr1")
              shinyjs::showElement("llr2")
              },
            "dLLR"={
              shinyjs::showElement("evidencePrior")
              shinyjs::showElement("STPrior")
              shinyjs::hideElement("evidenceLLR1")
              shinyjs::hideElement("evidenceLLR2")
              shinyjs::hideElement("llr1")
              shinyjs::hideElement("llr2")
            }
    )
  })
  observeEvent(input$alpha, {
    alpha<<-input$alpha
    alphaLLR<<-0.5*qnorm(1-alpha/2)^2
  })
####################################
# generic warning dialogue
  
  hmm<-function (cause) {
    showModal(
      modalDialog(style = paste("background: ",subpanelcolours$hypothesisC,";",
                                "modal {background-color: ",subpanelcolours$hypothesisC,";}"),
                  title="Careful now!",
                  size="s",
                  cause,
                  
                  footer = tagList( 
                    actionButton("MVproceed", "OK")
                  )
      )
    )
  }
  
  observeEvent(input$MVproceed, {
    removeModal()
  })
  
####################################
# QUICK HYPOTHESES
  
  
  observeEvent(input$Hypchoice,{
    if (debug) print("7")
    
    result<-getTypecombination(input$Hypchoice)
    
    setIVanyway(result$IV)
    setIV2anyway(result$IV2)
    setDVanyway(result$DV)
    
    updateSelectInput(session,"sIV1Use",selected=result$IV$deploy)
    updateSelectInput(session,"sIV2Use",selected=result$IV2$deploy)

    # 3 variable hypotheses look after themselves
    #
    if (!is.null(IV2)) {
      editVar$data<<-editVar$data+1
    }    
  })
  
  observeEvent(input$Effectchoice,{
    if (debug) print("8")
    
    switch (input$Effectchoice,
            "e0"={
              updateNumericInput(session,"rIV",value=0)    
              updateNumericInput(session,"rIV2",value=0)    
              updateNumericInput(session,"rIVIV2",value=0)    
              updateNumericInput(session,"rIVIV2DV",value=0)    
            },
            "e1"={
              updateNumericInput(session,"rIV",value=0.3)    
              updateNumericInput(session,"rIV2",value=-0.3)    
              updateNumericInput(session,"rIVIV2",value=0.0)    
              updateNumericInput(session,"rIVIV2DV",value=0.5)    
            },
            "e2"={
              updateNumericInput(session,"rIV",value=0.2)    
              updateNumericInput(session,"rIV2",value=0.4)    
              updateNumericInput(session,"rIVIV2",value=-0.8)    
              updateNumericInput(session,"rIVIV2DV",value=0.0)    
            }
    )
    
  })
  

####################################
# change between data and simulation
  
  changeUI2Data<-function() {
    # get the variables into the ui
    if (switches$rigidWithin) {
      if (variables$deploy[1]=="Within") {
        DVchoices<-strsplit(substring(variables$targetDeploys[1],2,nchar(variables$targetDeploys[1])-1),",")[[1]]
        use<-!(variables$name %in% DVchoices) & (sapply(variables$targetDeploys,nchar)==0)
        DVchoices<-list("applicable"=c(DVchoices," "),"not applicable"=variables$name[use])
        updateSelectInput(session, "DVchoice", choices = DVchoices, selected = DVchoices$available[1])
      } else {
        DVchoices=variables$name[sapply(variables$targetDeploys,nchar)==0]
        updateSelectInput(session, "DVchoice", choices = DVchoices, selected = DVchoices[length(DVchoices)])
      }
      updateSelectInput(session, "IVchoice", choices = variables$name, selected = variables$name[1])
      updateSelectInput(session, "IV2choice", choices = c("none",variables$name), selected = "none")
    } else {
      updateSelectInput(session, "IVchoice", choices = variables$name, selected = variables$name[1])
      updateSelectInput(session, "IV2choice", choices = c("none",variables$name), selected = "none")
      updateSelectInput(session, "DVchoice", choices = variables$name, selected = variables$name[nrow(variables)])
    }
    setIVanyway()
    setIV2anyway()
    setDVanyway()
    
    updateNumericInput(session,"sN",value=length(unique(importedData[[1]])))
    if (switches$doBootstrap) {
    shinyjs::showElement(id= "EvidenceHypothesisApply")
      shinyjs::showElement(id= "LGEvidenceHypothesisApply")
    }
    updateTabsetPanel(session, "Hypothesis",selected = "Variables")
    updateTabsetPanel(session, "Evidence",selected = "Single")
    updateNumericInput(session,"rIV",value=NA)
    
    shinyjs::disable(id= "rIV")
    shinyjs::disable(id= "rIV2")
    shinyjs::disable(id= "rIVIV2")
    shinyjs::disable(id= "rIVIV2DV")
    
    shinyjs::disable(id= "sN")
    shinyjs::disable(id= "sMethod")
    shinyjs::disable(id= "sIV1Use")
    shinyjs::disable(id= "sIV2Use")
    shinyjs::disable(id= "sDependence")
    shinyjs::disable(id= "sOutliers")
    shinyjs::disable(id= "sRangeOn")
    
    if (!switches$doBootstrap) {
      shinyjs::hideElement(id="EvidenceHypothesisApply")
      shinyjs::hideElement(id= "LGEvidenceHypothesisApply")
      updateActionButton(session,"EvidencenewSample", label="Analyze")
      hideTab("Hypothesis","Effects")
      hideTab("Evidence","Multiple")
      shinyjs::hideElement(id="uiExplore")
    } else {
      updateActionButton(session,"EvidencenewSample", label="Resample")
      if (input$IV2choice=="none") {
        updateSelectInput(session,"Explore_typeH", choices=hypothesisChoices2Plain)
      }
      else {
        updateSelectInput(session,"Explore_typeH", choices=hypothesisChoices3Plain)
      }
      updateSelectInput(session,"Explore_VtypeH",choices=c("& type"="Type"))
      updateSelectInput(session,"Explore_typeD",choices=c("Sample Size" = "SampleSize"))
    }
    hideTab("Design","Anomalies")
    shinyjs::hideElement(id="DesignMethod")
    runjs(sprintf("document.getElementById('Variables').style.backgroundColor = '%s';",darkpanelcolours$hypothesisC))
    runjs(sprintf("document.getElementById('Sampling').style.backgroundColor = '%s';",darkpanelcolours$designC))
    runjs(sprintf("document.getElementById('Single').style.backgroundColor = '%s';",darkpanelcolours$simulateC))
    runjs(sprintf("document.getElementById('Multiple').style.backgroundColor = '%s';",darkpanelcolours$simulateC))
    runjs(sprintf("document.getElementById('ExploreHypothesis').style.backgroundColor = '%s';",darkpanelcolours$exploreC))
    runjs(sprintf("document.getElementById('ExploreDesign').style.backgroundColor = '%s';",darkpanelcolours$exploreC))
    
    variablesHeld<<-"Data"
    
    updateSelectInput(session,"Using",choices=c("Simulations"="Simulations","Data"="Data"),selected="Data")
    shinyjs::showElement(id= "Using")

  }
  
  
  changeUI2Simulations<-function() {
    # get the variables into the ui
    updateSelectInput(session, "IVchoice", choices = variables$name, selected = variables$name[1])
    updateSelectInput(session, "IV2choice", choices = c("none",variables$name), selected = "none")
    updateSelectInput(session, "DVchoice", choices = variables$name, selected = variables$name[3])
    setIVanyway()
    setIV2anyway()
    setDVanyway()
    
    if (switches$doBootstrap) {
      shinyjs::hideElement(id= "EvidenceHypothesisApply")
      shinyjs::hideElement(id= "LGEvidenceHypothesisApply")
    }
    updateTabsetPanel(session, "Hypothesis",selected = "Variables")
    updateNumericInput(session,"rIV",value=0)
    
    shinyjs::enable(id= "rIV")
    shinyjs::enable(id= "rIV2")
    shinyjs::enable(id= "rIVIV2")
    shinyjs::enable(id= "rIVIV2DV")
    
    shinyjs::enable(id= "sN")
    shinyjs::enable(id= "sMethod")
    shinyjs::enable(id= "sIV1Use")
    shinyjs::enable(id= "sIV2Use")
    shinyjs::enable(id= "sDependence")
    shinyjs::enable(id= "sOutliers")
    shinyjs::enable(id= "sRangeOn")
    
      updateActionButton(session,"EvidencenewSample", label="New Sample")
      showTab("Hypothesis","Effects")
      showTab("Evidence","Multiple")
      shinyjs::showElement(id="uiExplore")
      if (input$IV2choice=="none") {
        updateSelectInput(session,"Explore_typeH", choices=hypothesisChoices2)
      } else {
        updateSelectInput(session,"Explore_typeH", choices=hypothesisChoices3)
      }
      updateSelectInput(session,"Explore_VtypeH",choices=variableChoices)
      updateSelectInput(session,"Explore_typeD",choices=designChoices)

    showTab("Design","Anomalies")
    shinyjs::showElement(id="DesignMethod")
    
    runjs(sprintf("document.getElementById('Variables').style.backgroundColor = '%s';",subpanelcolours$hypothesisC))
    runjs(sprintf("document.getElementById('Sampling').style.backgroundColor = '%s';",subpanelcolours$designC))
    runjs(sprintf("document.getElementById('Single').style.backgroundColor = '%s';",subpanelcolours$simulateC))
    runjs(sprintf("document.getElementById('Multiple').style.backgroundColor = '%s';",subpanelcolours$simulateC))
    runjs(sprintf("document.getElementById('ExploreHypothesis').style.backgroundColor = '%s';",subpanelcolours$exploreC))
    runjs(sprintf("document.getElementById('ExploreDesign').style.backgroundColor = '%s';",subpanelcolours$exploreC))
    
    updateSelectInput(session,"Using",choices=c("Simulations"="Simulations","Data"="Data"),selected="Simulations")
    shinyjs::showElement(id= "Using")
    
    variablesHeld<<-"Simulations"
    
  }
  
####################################
# VARIABLES  
  # make basic variables    
  IV<-variables[1,]
  IV2<-variables[2,]
  DV<-variables[3,]
  MV<-IV
  
  observeEvent(input$AllowResampling,{
    if (debug) print("9")
    switches$doBootstrap<<-input$AllowResampling
    
    switch(input$Using,
           "Simulations"={
             changeUI2Simulations()
           },
           "Data"={
             changeUI2Data()
           })
  })
  
  observeEvent(input$Using,{
    if (debug) print("10")
    
    if (variablesHeld==input$Using) {return()}
    if (input$Using=="OK") {return()}
    
    local<-variables
    variables<<-defaultVariables
    defaultVariables<<-local
    
    switch(input$Using,
           "Simulations"={
             changeUI2Simulations()
           },
           "Data"={
             changeUI2Data()
           })
    # get the variables into the ui
    setIVanyway()
    setIV2anyway()
    setDVanyway()
  })
  
  
  observeEvent(c(input$IVchoice,input$IV2choice,input$DVchoice),
                {
                  validSample<<-FALSE
                  validExpected<<-FALSE
                  validExplore<<-FALSE
                })
  
  observeEvent(input$IVchoice,{
    if (debug) print("11")
    use<-match(input$IVchoice,variables$name)
    if (!is.na(use)){
      newMV<-variables[use,]
    }
    else return(NULL)
    updateSelectInput(session,"sIV1Use", selected=newMV$deploy)
    if (switches$rigidWithin && variablesHeld=="Data") {
      if(newMV$deploy=="Within" && nchar(newMV$targetDeploys)>0) {
        DVchoices<-strsplit(substring(newMV$targetDeploys,2,nchar(newMV$targetDeploys)-1),",")[[1]]
        use<-!(variables$name %in% DVchoices) & (sapply(variables$targetDeploys,nchar)==0)
        DVchoices<-list("applicable"=c(DVchoices," "),"not applicable"=variables$name[use])
        updateSelectInput(session, "DVchoice", choices = DVchoices, selected = DVchoices$available[1])
      } else {
        DVchoices<-variables$name[sapply(variables$targetDeploys,nchar)==0]
        updateSelectInput(session, "DVchoice", choices = DVchoices, selected = DVchoices[length(DVchoices)])
      }
    }
  })
  
  observeEvent(input$IV2choice,{
    use<-match(input$IV2choice,variables$name)
    if (!is.na(use)){
      newMV<-variables[use,]
    }
    else return(NULL)
    updateSelectInput(session,"sIV2Use", selected=newMV$deploy)
  })
  
  # modalDialog to edit each variable
  # all of this code only gets used if the modalDialog mechanism is set up in ui.R
  # if we are using the older tabs mechanism, then this code never gets called
  source("uiVariable.R")
  
  modalVar<-c()
  editVar<-reactiveValues(data=0)
  oldName<-""
  
  updateMVType<-function(MV) {
    if (debug) print("updateMVType")
    
    switch (MV$type,
            "Interval"={
              shinyjs::hideElement(id= "MVOrdVal")
              shinyjs::hideElement(id= "MVCatVala")
              shinyjs::hideElement(id= "MVCatValb")
              shinyjs::showElement(id= "MVIntVal")
            },
            "Ordinal"={
              shinyjs::hideElement(id= "MVIntVal")
              shinyjs::hideElement(id= "MVCatVala")
              shinyjs::hideElement(id= "MVCatValb")
              shinyjs::showElement(id= "MVOrdVal")
            },
            "Categorical"={
              shinyjs::hideElement(id= "MVIntVal")
              shinyjs::hideElement(id= "MVOrdVal")
              shinyjs::showElement(id= "MVCatVala")
              shinyjs::showElement(id= "MVCatValb")
            },
    )
    
    # if we are editing imported variables, there is less we can change
    if (MV$process=="data") {
      shinyjs::hideElement(id= "MVIntVal")
      shinyjs::hideElement(id= "MVOrdVal")
      shinyjs::hideElement(id= "MVCatVala")
      shinyjs::hideElement(id= "MVCatValb")
    }
    if (debug) print("updateMVType - exit")
    
  }
  observeEvent(input$MVtype, {
    updateMVType (list(type=input$MVtype,process=MV$process))
  })
  observeEvent(input$MVnlevs, {
    updateNumericInput(session,"MVcentre",value=(input$MVnlevs+1)/2)
    updateNumericInput(session,"MVspread",value=(input$MVnlevs-1)/2)
  })
  #Press "OK": make the new variable
  observeEvent(input$MVok, {
    if (debug) print("13")
    MV<<-makeVar(name=input$MVname, type=input$MVtype,
                 mu=checkNumber(input$MVmu), sd=checkNumber(input$MVsd),
                 skew=checkNumber(input$MVskew), kurtosis=checkNumber(input$MVkurt),
                 nlevs=input$MVnlevs,median=input$MVcentre,iqr=checkNumber(input$MVspread),discrete=input$MVdiscrete,
                 ncats=input$MVncats,cases=input$MVcases,proportions=checkNumber(input$MVprop),source=input$MVsource,
                 deploy=MV$deploy,process=MV$process)
    
    switch (modalVar,
            "IV" ={
              MV$deploy<<-input$sIV1Use
              setIVanyway(MV)
              },
            "IV2"={
              MV$deploy<<-input$sIV2Use
              setIV2anyway(MV)
              },
            "DV" ={setDVanyway(MV)}
    )
    removeModal()
    # a change of name looks after itself
    # when the uiHypothesis is updated
    # no other types of change are registered
    # so we trigger a reactiveValue events
    if (oldName==MV$name) {
    editVar$data<<-editVar$data+1
    }
  })
  
  
  # create the modalDialog for each variable 
  observeEvent(input$editIV,{
              modalVar<<-"IV"
              IV<-updateIV()
              MV<<-IV

    # now set up the controls in the dialogue with up to date values
    updateTextInput(session,"MVname",value=MV$name)
    updateSelectInput(session,"MVtype",selected=MV$type)
    updateNumericInput(session,"MVmu",value=MV$mu)
    updateNumericInput(session,"MVsd",value=MV$sd)
    updateNumericInput(session,"MVskew",value=MV$skew)
    updateNumericInput(session,"MVkurt",value=MV$kurtosis)
    updateNumericInput(session,"MVnlevs",value=MV$nlevs)
    updateNumericInput(session,"MVcentre",value=MV$median)
    updateNumericInput(session,"MVspread",value=MV$iqr)
    updateNumericInput(session,"MVncats",value=MV$ncats)
    updateTextInput(session,"MVcases",value=MV$cases)
    updateTextInput(session,"MVprop",value=MV$proportions)
    updateSelectInput(session,"source",selected=MV$source)    
    
    # now show the dialog
    showModal(
      modalDialog(style = paste("background: ",subpanelcolours$hypothesisC,";",
                                "modal {background-color: ",subpanelcolours$hypothesisC,";}"),
                  title=modalVar,
                  size="s",
                  variableDialog,
                  
                  footer = tagList( 
                    modalButton("Cancel"),
                    actionButton("MVok", "OK")
                  )
      )
    )
    updateMVType(MV)
    
    oldName<<-MV$name
    # make sure we get the current values
  })
  
  observeEvent(input$editIV2,{
              modalVar<<-"IV2"
              IV2<-updateIV2()
              MV<<-IV2

    # now set up the controls in the dialogue with up to date values
    updateTextInput(session,"MVname",value=MV$name)
    updateSelectInput(session,"MVtype",selected=MV$type)
    updateNumericInput(session,"MVmu",value=MV$mu)
    updateNumericInput(session,"MVsd",value=MV$sd)
    updateNumericInput(session,"MVskew",value=MV$skew)
    updateNumericInput(session,"MVkurt",value=MV$kurtosis)
    updateNumericInput(session,"MVnlevs",value=MV$nlevs)
    updateNumericInput(session,"MVcentre",value=MV$median)
    updateNumericInput(session,"MVspread",value=MV$iqr)
    updateNumericInput(session,"MVncats",value=MV$ncats)
    updateTextInput(session,"MVcases",value=MV$cases)
    updateTextInput(session,"MVprop",value=MV$proportions)
    updateSelectInput(session,"source",selected=MV$source)    
    
    # now show the dialog
    showModal(
      modalDialog(style = paste("background: ",subpanelcolours$hypothesisC,";",
                                "modal {background-color: ",subpanelcolours$hypothesisC,";}"),
                  title=modalVar,
                  size="s",
                  variableDialog,
                  
                  footer = tagList( 
                    modalButton("Cancel"),
                    actionButton("MVok", "OK")
                  )
      )
    )
    updateMVType(MV)
    
    oldName<<-MV$name
    # make sure we get the current values
  })
  
  observeEvent(input$editDV,{
              modalVar<<-"DV"
              DV<-updateDV()
              MV<<-DV

        # now set up the controls in the dialogue with up to date values
    updateTextInput(session,"MVname",value=MV$name)
    updateSelectInput(session,"MVtype",selected=MV$type)
    updateNumericInput(session,"MVmu",value=MV$mu)
    updateNumericInput(session,"MVsd",value=MV$sd)
    updateNumericInput(session,"MVskew",value=MV$skew)
    updateNumericInput(session,"MVkurt",value=MV$kurtosis)
    updateNumericInput(session,"MVnlevs",value=MV$nlevs)
    updateNumericInput(session,"MVcentre",value=MV$median)
    updateNumericInput(session,"MVspread",value=MV$iqr)
    updateNumericInput(session,"MVncats",value=MV$ncats)
    updateTextInput(session,"MVcases",value=MV$cases)
    updateTextInput(session,"MVprop",value=MV$proportions)
    updateSelectInput(session,"source",selected=MV$source)    

    # now show the dialog
      showModal(
      modalDialog(style = paste("background: ",subpanelcolours$hypothesisC,";",
                                "modal {background-color: ",subpanelcolours$hypothesisC,";}"),
                  title=modalVar,
                  size="s",
                  variableDialog,
                  
                  footer = tagList( 
                    modalButton("Cancel"),
                    actionButton("MVok", "OK")
                  )
      )
    )
      updateMVType(MV)

      oldName<<-MV$name
    # make sure we get the current values
  })


  setIVanyway<-function(newMV=NULL){
    if (debug) {print("setIVanyway")}
    newName<-FALSE    
    if (is.null(newMV)) {
      use<-match(input$IVchoice,variables$name)
      if (!is.na(use)){
        newMV<-variables[use,]
      }
      else return(NULL)
    } else {
      if (any(newMV$name==variables$name)) {
        use<-which(newMV$name==variables$name)
        variables[use,]<<-newMV
      } else {
        variables<<-rbind(variables,newMV)
        newName<-TRUE
      }

      if (newName) {
        if (debug) print(paste("IV new name detected",newMV$name))
        updateSelectInput(session, "IVchoice", choices=variables$name)
        updateSelectInput(session, "IV2choice", choices = c("none",variables$name))
        if (input$IV2choice!="none") {updateSelectInput(session, "IV2choice", selected=input$IV2choice)}
        updateSelectInput(session, "DVchoice", choices = variables$name, selected=input$DVchoice)
      }
      if (newMV$name!=input$IVchoice) {
        if (debug) print(paste("IV changed name detected",newMV$name))
        updateSelectInput(session, "IVchoice", selected=newMV$name)
      }
    }
    
    updateSelectInput(session,"sIV1Use", selected=newMV$deploy)
    switch (newMV$type,
            "Interval"={
              shinyjs::disable(id= "sIV1Use")
            },
            "Categorical"={
              shinyjs::enable(id= "sIV1Use")
            }
    )
    
    validSample<<-FALSE
    validExpected<<-FALSE
    validExplore<<-FALSE
  }
  
  setIV2anyway<-function(newMV=NULL){
    if (debug) {print("setIV2anyway")}
    newName<-FALSE
    if (is.null(newMV)) {
      if (input$IV2choice=="none") {
        no_ivs<<-1
        shinyjs::disable("editIV2")
        return(NULL)
      }
      else {
        no_ivs<<-2
        shinyjs::enable("editIV2")
      }
      
      use<-match(input$IV2choice,variables$name)
      if (!is.na(use)){
        newMV<-variables[use,]
      } else {return(NULL)}
    } else {
      if (newMV$name !="none") {
        use<-match(newMV$name,variables$name)
        if (is.na(use)){
          use<-nrow(variables)+1
          newName<-TRUE
        }
        variables[use,]<<-newMV
      }
    }
    updateSelectInput(session,"sI21Use", selected=newMV$deploy)
    switch (newMV$type,
            "Interval"={
              shinyjs::disable(id= "sIV2Use")
            },
            "Categorical"={
              shinyjs::enable(id= "sIV2Use")
            }
    )
     
    if (newName) {
      if (debug) print(paste("IV2 new name detected",newMV$name))
      updateSelectInput(session, "IVchoice", choices=variables$name, selected=input$IVchoice)
      updateSelectInput(session, "IV2choice", choices = c("none",variables$name), selected=newMV$name)
      updateSelectInput(session, "DVchoice", choices = variables$name, selected=input$DVchoice)
    } else {
      if (newMV$name!=input$IV2choice) {
        if (debug)   print(paste("IV2 changed name detected",newMV$name))
      updateSelectInput(session, "IV2choice", selected=newMV$name)
      }
    }
    
    validSample<<-FALSE
    validExpected<<-FALSE
    validExplore<<-FALSE
  }
  
  setDVanyway<-function(newMV=NULL){
    if (debug) {print("setDVanyway")}
    newName<-FALSE
    if (is.null(newMV)) {
      use<-match(input$DVchoice,variables$name)
      if (!is.na(use)){
        newMV<-variables[use,]
      }
      else return(NULL)
    } else {
      use<-match(newMV$name,variables$name)
      if (is.na(use)){
        use<-nrow(variables)+1
        newName<-TRUE
      }
      variables[use,]<<-newMV
    }

    validSample<<-FALSE
    validExpected<<-FALSE
    validExplore<<-FALSE

    if (newName) {
      if (debug)       print(paste("DV new name detected",newMV$name))
      updateSelectInput(session, "IVchoice", choices=variables$name, selected=input$IVchoice)
      updateSelectInput(session, "IV2choice", choices = c("none",variables$name))
      if (input$IV2choice!="none") {updateSelectInput(session, "IV2choice", selected=input$IV2choice)}
      updateSelectInput(session, "DVchoice", choices = variables$name)
    }
    if (newMV$name!=input$DVchoice) {
      if (debug)       print(paste("DV changed name detected",newMV$name))
    updateSelectInput(session, "DVchoice", selected=newMV$name)
    }
    
  }

#################################################################
## inspect variables functions

inspectData<-c()  
inspectVar<-c()
inspectSource<-c()
inspectHistory<-c()

  updateInspect<-function() {
    inspect<-list(inspectOrder=input$inspectOrder,whichResiduals=input$whichResiduals,
                  showResiduals=input$showResiduals,inspectHistory=inspectHistory,
                  ResidVal=input$ResidVal,n=input$sN,
                  data=inspectData)
  }
  
  inspectVariable<-function(var) {
    inspectVar<<-var
    if (!simData) {
      use<-which(variables$name==var$name)
      inspectData<<-importedData[,use+1]
    } else {
      inspectData<<-c()
    }
    inspectHistory<<-c()
    
    updateCheckboxInput(session,"showResiduals",value=FALSE)
    switch (var$type,
            "Categorical"={
              updateCheckboxInput(session,"showMean",label="Mode")
              updateCheckboxInput(session,"showSD",label=" ")
              # c(1,var$ncats)+c(-1,1)*(var$ncats-1)/10
              updateSliderInput(session,"ResidVal",min=1-(var$ncats-1)/10,max=var$ncats+(var$ncats-1)/10,value=1.7)
              updateSelectInput(session,"inspectOrder",choices=c("unsorted","piled"),selected="unsorted")
              shinyjs::hideElement(id= "showSD")
            },
            "Ordinal"={
              updateCheckboxInput(session,"showMean",label="Median")
              updateCheckboxInput(session,"showSD",label="IQR")
              # c(1,var$nlevs)+c(-1,1)*(var$nlevs-1)/10
              updateSliderInput(session,"ResidVal",min=1-(var$nlevs-1)/10,max=var$nlevs+(var$nlevs-1)/10,value=1.7)
              updateSelectInput(session,"inspectOrder",choices=c("unsorted","sorted","piled"),selected="unsorted")
              shinyjs::showElement(id= "showSD")
            },
            "Interval"={
              updateCheckboxInput(session,"showMean",label="Mean")
              updateCheckboxInput(session,"showSD",label="SD")
              updateSliderInput(session,"ResidVal",min=var$mu-var$sd*3,max=var$mu+var$sd*3,value=var$mu-var$sd)
              updateSelectInput(session,"inspectOrder",choices=c("unsorted","sorted","piled"),selected="unsorted")
              shinyjs::showElement(id= "showSD")
            }
    )
    toggleModal(session, modalId = "inspectOutput", toggle = "open")
    
  }
  
  observeEvent(input$inspectIV,{
    var<-updateIV()
    inspectSource<<-"inspectIV"
    inspectVariable(var)
  }
  )
  observeEvent(input$inspectDV,{
    var<-updateDV()
    inspectSource<<-"inspectDV"
    inspectVariable(var)
  }
  )

  observeEvent(input$ResidVal, {
    inspectHistory<<-c(inspectHistory,input$ResidVal)

  }
  )
  
  observeEvent(input$inspectNewSample,{
    IV<-updateIV()
    DV<-updateDV()
    effect<-updateEffect()
    design<-updateDesign()
    sample<-makeSample(IV,NULL,DV,effect,design)
    switch (inspectSource,
            "inspectIV"={inspectData<<-sample$iv},
            "inspectDV"={inspectData<<-sample$dv}
    )
    
  })
  
  getInspect1<-eventReactive(c(input$inspectOrder,input$inspectNewSample,input$showResiduals,input$whichResiduals,input$ResidVal,input$showMean,input$showSD),{
    
    inspect<-list(inspectOrder=input$inspectOrder,whichResiduals=input$whichResiduals,
                  showResiduals=input$showResiduals,inspectHistory=inspectHistory,
                  ResidVal=input$ResidVal,n=input$sN,
                  showMean=input$showMean,showSd=input$showSD,
                  var=inspectVar,
                  data=inspectData)
  }
  )
  
  output$mainInspect<-renderPlot( {
    doIt<-input$inspectNewSample
    inspect<-getInspect1()
    return(inspectMainGraph(inspect))
  }
  )
  
  output$penaltyInspect<-renderPlot( {
    doIt<-input$inspectNewSample
    inspect<-getInspect1()
    return(inspectPenaltyGraph(inspect))
  }
  )
  
  output$explainResiduals<-renderText( {
    if (input$showResiduals) {
    switch (inspectVar$type,
    "Categorical"={txt1<-"<br> 0: v<sub>i</sub> = v&#772;<br> 1: v<sub>i</sub> &ne; v&#772;"
                   txt2<-"<b>mode</b>(v) = v&#772; when <br> abs(&Sigma;residual) is minimum (nearest to 0)"
    },
    "Ordinal"={txt1<-"<br> -1: v<sub>i</sub> < v&#772;<br> +1: v<sub>i</sub> more than v&#772;"
               txt2<-"<b>median</b>(v) = v&#772; when <br> abs(&Sigma;residual) is minimum (nearest to 0)"
    },
    "Interval"={txt1<-"v<sub>i</sub>-v&#772;"
                txt2<-"<b>mean</b>(v) = v&#772; when <br> abs(&Sigma;residual) is minimum (equals 0)"
    }
    )
      paste0("<p style='font-size:12px'>", "<b>residual:</b> ",txt1, "<br>",  txt2, "</p>")
    } else {""}
  }
  )

######################################################
## large display output functions

  designFields<-list(select=c("sMethod","sCheating"),
                     number=c("sN","sNRandK","sCheatingK"),
                     check=c("sNRand"))
  hypothesisFields<-list(select=c("world_distr","world_distr_rz"),
                         number=c("rIV","rIV2","rIVIV2","rIVIV2DV","world_distr_k","world_distr_Nullp"),
                         check=c())
  worldFields<-list(select=c("world_distr","world_distr_rz"),
                    number=c("world_distr_k","world_distr_Nullp"),
                    check=c("world_on"))
  evidenceFields<-list(select=c("EvidenceInfer_type","EvidenceExpected_type","EvidenceExpected_par1","EvidenceExpected_par2","EvidenceEffect_type","EvidenceExpected_length"),
                       number=c(),
                       check=c("EvidenceExpected_append"))
  metaFields<-list(select=c("meta_fixedAnal","meta_runlength"),
                       number=c("meta_nStudies"),
                       check=c("meta_psigStudies","meta_nullAnal","meta_psigAnal","meta_append"))
  exploreFields<-list(select=c("Explore_typeH","Explore_VtypeH","Explore_showH","Explore_whichShowH","Explore_typeShowH","Explore_lengthH",
                               "Explore_typeD","Explore_showD","Explore_whichShowD","Explore_typeShowD","Explore_lengthD",
                               "Explore_typeM","Explore_showM","Explore_lengthM"),
                      number=c("Explore_nRange","Explore_npoints","Explore_esRange","Explore_quants","ExploreFull_ylim","Explore_anomRange","Explore_metaRange"),
                      check=c("ExploreAppendH","ExploreAppendD","ExploreAppendM","Explore_xlog")
  )
  possibleFields<-list(select=c("likelihoodP_length","likelihood_length","likelihoodViewRZ","likelihoodPrior_distr","likelihoodPrior_distr_rz","likelihoodUsePrior","likelihoodUseSource"),
                       number=c("likelihoodSampRho","likelihoodSimSlice","likelihoodPSampRho","likelihoodPrior_distr_k","likelihoodPrior_distr_Nullp",
                                "LikelihoodAzimuth","LikelihoodElevation","likelihoodRange"),
                       check=c("likelihood_cutaway","likelihoodTheory","likelihoodLongHand","likelihoodCorrection","likelihoodP_append","likelihood_append")
                       )

  updateHypothesisFields<-function(prefix) {
    if (input$IV2choice=="none") {
      shinyjs::hideElement(paste0(prefix,"IV2-DV"))
      shinyjs::hideElement(paste0(prefix,"rIV2"))
      shinyjs::hideElement(paste0(prefix,"IV1-IV2"))
      shinyjs::hideElement(paste0(prefix,"rIVIV2"))
      shinyjs::hideElement(paste0(prefix,"IV1-IV2-DV"))
      shinyjs::hideElement(paste0(prefix,"rIVIV2DV"))
    } else {
      shinyjs::showElement(paste0(prefix,"IV2-DV"))
      shinyjs::showElement(paste0(prefix,"rIV2"))
      shinyjs::showElement(paste0(prefix,"IV1-IV2"))
      shinyjs::showElement(paste0(prefix,"rIVIV2"))
      shinyjs::showElement(paste0(prefix,"IV1-IV2-DV"))
      shinyjs::showElement(paste0(prefix,"rIVIV2DV"))
    }
  }
  updateDesignFields<-function(prefix) {
    if (!input$sNRand) {
      shinyjs::hideElement(paste0(prefix,"sNRandK"))
      shinyjs::hideElement(paste0(prefix,"gamma"))
    } else {
      shinyjs::showElement(paste0(prefix,"snRandK"))
      shinyjs::showElement(paste0(prefix,"gamma"))
    }
  }
  observeEvent(c(input$LGEvidencesNRand,input$LGMetasNRand,input$LGExploreNRand),{
    updateDesignFields("LGEvidence")
    updateDesignFields("LGMeta")
    updateDesignFields("LGExplore")
  })
  updateEvidenceFields<-function(prefix) {
    if (input$IV2choice=="none") {
      shinyjs::hideElement(paste0(prefix,"Effect_type"))
    } else {
      shinyjs::showElement(paste0(prefix,"Effect_type"))
    }
  }
  updateMetaFields<-function(prefix) {
    if (input$IV2choice=="none") {
      shinyjs::hideElement(paste0(prefix,"Effect_type"))
    } else {
      shinyjs::showElement(paste0(prefix,"Effect_type"))
    }
  }
  updateExploreFields<-function(prefix) {
    if (input$IV2choice=="none") {
      shinyjs::hideElement(paste0(prefix,"_whichShowH"))
      shinyjs::hideElement(paste0(prefix,"_typeShowH"))
      shinyjs::hideElement(paste0(prefix,"_whichShowD"))
      shinyjs::hideElement(paste0(prefix,"_typeShowD"))
    } else {
      shinyjs::showElement(paste0(prefix,"_whichShowH"))
      shinyjs::showElement(paste0(prefix,"_typeShowH"))
      shinyjs::showElement(paste0(prefix,"_whichShowD"))
      shinyjs::showElement(paste0(prefix,"_typeShowD"))
    }
    if (input$LGExplore_typeD!="SampleSize") {
      shinyjs::hideElement(paste0(prefix,"_max"))
      shinyjs::hideElement(paste0(prefix,"_nRange"))
      shinyjs::hideElement(paste0(prefix,"_log"))
      shinyjs::hideElement(paste0(prefix,"_xlog"))
    } else {
      shinyjs::showElement(paste0(prefix,"_max"))
      shinyjs::showElement(paste0(prefix,"_nRange"))
      shinyjs::showElement(paste0(prefix,"_log"))
      shinyjs::showElement(paste0(prefix,"_xlog"))
    }
    if (!is.element(input$LGExplore_typeH,c("IV","IV2","DV"))) {
      shinyjs::hideElement(paste0(prefix,"_VtypeH"))
    } else {
      shinyjs::showElement(paste0(prefix,"_VtypeH"))
    }
  }
  observeEvent(c(input$LGExplore_typeD,input$LGExplore_typeH),{
    updateExploreFields("LGExplore")
  })
  
  saveFields <-function(fields,suffix="") {
    if (length(fields$select)>0) {
      for (i in 1:length(fields$select)) {
        updateSelectInput(session,fields$select[i],selected=input[[paste0("LG",suffix,fields$select[i])]])
      }
    }
    if (length(fields$number)>0) {
      for (i in 1:length(fields$number)) {
        updateNumericInput(session,fields$number[i],value=input[[paste0("LG",suffix,fields$number[i])]])
      }
    }
    if (length(fields$check)>0) {
      for (i in 1:length(fields$check)) {
        updateCheckboxInput(session,fields$check[i],value=input[[paste0("LG",suffix,fields$check[i])]])
      }
    }
  } 
  
  loadFields <-function(fields,suffix="") {
    if (length(fields$select)>0) {
      for (i in 1:length(fields$select)) {
        updateSelectInput(session,paste0("LG",suffix,fields$select[i]),selected=input[[fields$select[i]]])
      }
    }
    if (length(fields$number)>0) {
      for (i in 1:length(fields$number)) {
        updateNumericInput(session,paste0("LG",suffix,fields$number[i]),value=input[[fields$number[i]]])
      }
    }
    if (length(fields$check)>0) {
      for (i in 1:length(fields$check)) {
        updateCheckboxInput(session,paste0("LG",suffix,fields$check[i]),value=input[[fields$check[i]]])
      }
    }
  } 
  
  saveLGEvidence <- function (){
    saveFields(designFields,"Evidence")
    saveFields(hypothesisFields,"Evidence")
    saveFields(evidenceFields)
  }
  
  observeEvent(c(input$LGEvidenceStart,input$LGEvidenceStart1,input$LGEvidenceStart2,input$LGEvidenceStart3),{
    req(input$changed)

    loadFields(designFields,"Evidence")
    loadFields(hypothesisFields,"Evidence")
    loadFields(evidenceFields)
    
    toggleModal(session, "LGmodalEvidence", toggle = "open")
    updateTabsetPanel(session,"LGEvidenceShow",selected=input$Graphs)
    
    updateHypothesisFields("LGEvidence")
    updateDesignFields("LGEvidence")
    updateEvidenceFields("LGEvidence")
    
    validExpected<<-FALSE
  }
  )
  observeEvent(input$LGEvidenceClose,{
    saveLGEvidence()
    toggleModal(session, "LGmodalEvidence", toggle = "close")
  }
  )
  output$LGshowSampleOutput<-renderPlot( {
    doIt<-c(input$LGEvidencenewSample)
    saveLGEvidence()
    plotTheme<<-mainTheme+LGplotTheme
    g<-makeSampleGraph()
    plotTheme<<-mainTheme+SMplotTheme
    g
  }
  )
  output$LGshowDescribeOutput<-renderPlot( {
    doIt<-c(input$LGEvidencenewSample)
    saveLGEvidence()
    plotTheme<<-mainTheme+LGplotTheme
    g<-makeDescriptiveGraph()
    plotTheme<<-mainTheme+SMplotTheme
    g
  }
  )
  output$LGshowInferOutput<-renderPlot( {
    doIt<-c(input$LGEvidencenewSample)
    saveLGEvidence()
    plotTheme<<-mainTheme+LGplotTheme
    g<-makeInferentialGraph()
    plotTheme<<-mainTheme+SMplotTheme
    g
  }
  )
  output$LGshowExpectOutput<-renderPlot( {
    doIt<-c(input$LGEvidencenewSample)
    saveLGEvidence()
    plotTheme<<-mainTheme+LGplotTheme
    g<-makeExpectedGraph()
    plotTheme<<-mainTheme+SMplotTheme
    g
  }
  )
  

  saveLGMeta <- function() {
    saveFields(designFields,"Meta")
    saveFields(hypothesisFields,"Meta")
    saveFields(metaFields)
  }
  
  observeEvent(input$LGMetaStart,{
    req(input$changed)
    
    toggleModal(session, "LGmodalMeta", toggle = "open")
    
    loadFields(designFields,"Meta")
    loadFields(hypothesisFields,"Meta")
    loadFields(metaFields)
    
    updateHypothesisFields("LGMeta")
    updateDesignFields("LGMeta")
  }
  )
  observeEvent(input$LGMetaClose,{
    saveLGMeta()
    toggleModal(session, "LGmodalMeta", toggle = "close")
  }
  )
  output$LGMetaShowOutput<-renderPlot( {
    doIt<-c(input$LGmetaRun)
    saveLGMeta()
    plotTheme<<-mainTheme+LGplotTheme
    g<-makeMetaGraph()
    plotTheme<<-mainTheme+SMplotTheme
    g
  })
  
  
  saveLGExplore <- function() {
    saveFields(designFields,"Explore")
    saveFields(hypothesisFields,"Explore")
    saveFields(metaFields,"Explore")
    saveFields(exploreFields)
    updateTabsetPanel(session,"ExploreTab",input$LGExploreShow)
  }
  
  observeEvent(input$LGExploreStart,{
    req(input$changed)
    
    toggleModal(session, "LGmodalExplore", toggle = "open")
    
    loadFields(designFields,"Explore")
    loadFields(hypothesisFields,"Explore")
    loadFields(metaFields,"Explore")
    loadFields(exploreFields)
    if (is.element(input$ExploreTab,c("Hypothesis","Design"))) {
    updateSelectInput(session,"LGExploreShow",selected = input$ExploreTab)
    }
    
    updateHypothesisFields("LGExplore")
    updateDesignFields("LGExplore")
    updateMetaFields("LGExplore")
    updateExploreFields("LGExplore")
    
  }
  )
  observeEvent(input$LGExploreClose,{
    saveLGExplore()
    toggleModal(session, "LGmodalExplore", toggle = "close")
  }
  )
  output$LGExploreShowOutput<-renderPlot( {
    doIt<-c(input$LGexploreRunH,input$LGexploreRunD,input$LGexploreRunM)
    saveLGExplore()

    plotTheme<<-mainTheme+LGplotTheme
    g<-makeExploreGraph()
    plotTheme<<-mainTheme+SMplotTheme
    g
  })
  
  
  saveLGPossible <- function () {
    saveFields(designFields,"likelihood")
    saveFields(worldFields,"likelihood")
    saveFields(possibleFields)
  }
  
  observeEvent(input$LGPossibleStart,{
    req(input$changed)
    graphicSource<<-"None"
    toggleModal(session, "LGmodalPossible", toggle = "open")
    loadFields(designFields,"likelihood")
    loadFields(worldFields,"likelihood")
    loadFields(possibleFields)
    if (is.element(input$Likelihood,c("Samples","Populations"))) {
      updateSelectInput(session,"LGshowPossible",selected = input$Likelihood)
    }
    graphicSource<<-"Modal"
  }
  )
  observeEvent(input$LGlikelihoodClose, {
    saveLGPossible()
    toggleModal(session, "LGmodalPossible", toggle = "close")
  })

  output$LGshowPossibleOutput<-renderPlot( {
    saveLGPossible()
    updateTabsetPanel(session,"Likelihood",input$LGshowPossible)

    par(cex=1.8)
    makeLikelihoodGraph()
  })

######################################################  
## update variables functions
  
    updateIV<-function(){
      if (debug) print("     updateIV")
      use<-match(input$IVchoice,variables$name)
      if (is.na(use)) return(NULL)
      
      IV<-as.list(variables[use,])

      if (IV$type=="Categorical") {
                  cs<-IV$cases
                  cs<-strsplit(cs,",")
                  cs<-cs[[1]]
                  if (length(cs)<IV$ncats){
                    cs<-c(cs,paste("C",(length(cs)+1):IV$ncats,sep=""))
                  }
                  IV$cases<-cs
                  if (is.character(IV$proportions)) {
                    IV$proportions<-as.numeric(unlist(strsplit(IV$proportions,",")))
                  }
      }
      IV$deploy<-input$sIV1Use
      if (debug) print("     updateIV - exit")
      return(IV)
    }
    
    updateIV2<-function(){
      if (debug) print("     updateIV2")
      if (input$IV2choice=="none"){
        no_ivs<<-1
        if (debug) print("     updateIV2 - exit unused")
        return(NULL)
      } else {
        no_ivs<<-2
      }
      
      use<-match(input$IV2choice,variables$name)
      if (is.na(use)) return(NULL)
      
      IV2<-as.list(variables[use,])
      
      if (IV2$type=="Categorical") {
        cs<-IV2$cases
        cs<-strsplit(cs,",")
        cs<-cs[[1]]
        if (length(cs)<IV$ncats){
          cs<-c(cs,paste("C",(length(cs)+1):IV$ncats,sep=""))
        }
        IV2$cases<-cs
        if (is.character(IV2$proportions)) {
          IV2$proportions<-as.numeric(unlist(strsplit(IV2$proportions,",")))
        }
        #             IV$proportions<-MV$prop
      }
      IV2$deploy<-input$sIV2Use
      if (debug) print("     updateIV2 - exit")
      return(IV2)
    }
    
    updateDV<-function(){
      if (debug) print("     updateDV")
      use<-match(input$DVchoice,variables$name)
      if (is.na(use)) return(NULL)
      
      DV<-as.list(variables[use,])
      if (DV$type=="Ordinal" && input$IV2choice!="none") {
        if (warn3Ord==FALSE) {
          hmm("Ordinal DV with more than 1 IV. It will be treated as Interval.")
          warn3Ord<<-TRUE
        }
      }
      if (DV$type=="Categorical") {
        cs<-DV$cases
        cs<-strsplit(cs,",")
        cs<-cs[[1]]
        if (length(cs)<IV$ncats){
          cs<-c(cs,paste("C",(length(cs)+1):DV$ncats,sep=""))
        }
        DV$cases<-cs
        if (is.character(DV$proportions)) {
          DV$proportions<-as.numeric(unlist(strsplit(DV$proportions,",")))
        }
        #             IV$proportions<-MV$prop
      }
      if (debug) print("     updateDV - exit")
      return(DV)
    }
    
# UI changes    
    observeEvent(c(input$rIV,input$rIV2,input$rIVIV2,input$rIVIV2DV,
                                  input$sN,input$sMethod,input$sIV1Use,input$sIV2Use),{
      if (debug) print("     effectChanged")
      
      # remove out of date sample and other 
      validSample<<-FALSE
      validExpected<<-FALSE
      validExplore<<-FALSE
      
      # expectedResult<-c()
      exploreResultHold<-list(Hypothesis=c(),Design=c(),MetaAnalysis=c())
      likelihood_P_ResultHold<-c()
      likelihood_S_ResultHold<-c()
      
      updateCheckboxInput(session,"EvidenceExpected_append",value=FALSE)
      updateCheckboxInput(session,"ExploreAppendH",value=FALSE)
      updateCheckboxInput(session,"ExploreAppendD",value=FALSE)
      updateCheckboxInput(session,"ExploreAppendM",value=FALSE)
      
      if (debug) print("     effectChanged - exit")
    },priority=100)
    

################################################################        
# SYSTEM diagrams   
    # global variables
    # set prediction, design, evidence variables from UI
    # hypothesis diagram
    # population diagram
    # prediction diagram
#
# housekeeping    

    observeEvent(input$evidenceInteractionOnly,{
      showInteractionOnly<<-input$evidenceInteractionOnly
    })
    
    observeEvent(input$pScale,{
      pPlotScale<<-input$pScale
    })
    
    observeEvent(input$wScale,{
      wPlotScale<<-input$wScale
    })
    
    observeEvent(input$nScale,{
      nPlotScale<<-input$nScale
    })
    
    # PREDICTION & DESIGN & EVIDENCE
    updateEffect<-function(type=0){
      if (debug) print("     updateEffect")
      if (is.null(type)) {
        effect<-list(rIV=0,rIV2=0,rIVIV2=0,rIVIV2DV=0,
                     Heteroscedasticity=input$Heteroscedasticity,Welch=input$Welch,ResidDistr=input$ResidDistr,
                     world=list(worldOn=FALSE,populationPDF="Single",populationPDFk=NA,populationRZ=NA,populationNullp=NA)
        )
      } else {
        effect<-list(rIV=input$rIV,rIV2=input$rIV2,rIVIV2=input$rIVIV2,rIVIV2DV=input$rIVIV2DV,
                     Heteroscedasticity=input$Heteroscedasticity,Welch=input$Welch,ResidDistr=input$ResidDistr,
                     world=list(worldOn=input$world_on,populationPDF=input$world_distr,
                                populationPDFk=input$world_distr_k,populationRZ=input$world_distr_rz,
                                populationNullp=input$world_distr_Nullp)
        )
      }
      if (effect$world$worldOn==FALSE) {
        effect$world$populationPDF<-"Single"
        effect$world$populationRZ<-"r"
        effect$world$populationPDFk<-effect$rIV
        effect$world$populationNullp<-0
      }
      if (is.null(oldEffect)) {
        effect$Heteroscedasticity<-checkNumber(effect$Heteroscedasticity)
        effect$world$populationPDFk<-checkNumber(effect$world$populationPDFk)
        effect$world$populationNullp<-checkNumber(effect$world$populationNullp)
      } else {
        effect$Heteroscedasticity<-checkNumber(effect$Heteroscedasticity,oldEffect$Heteroscedasticity)
        effect$world$populationPDFk<-checkNumber(effect$world$populationPDFk,oldEffect$world$populationPDFk)
        effect$world$populationNullp<-checkNumber(effect$world$populationNullp,oldEffect$world$populationNullp)
      }
      oldEffect<<-effect
      
      if (debug) print("     updateEffect - exit")
      effect
    }
    
    updateDesign<-function(){
      if (debug) print("     updateDesign")
      design<-list(sN=input$sN, sNRand=input$sNRand,sNRandK=input$sNRandK,
                   sMethod=input$sMethod ,sIV1Use=input$sIV1Use,sIV2Use=input$sIV2Use, 
                   sRangeOn=input$sRangeOn, sIVRange=input$sIVRange, sDVRange=input$sDVRange, 
                   sDependence=input$sDependence, sOutliers=input$sOutliers, sClustering=input$sClustering,
                   sCheating=input$sCheating,sCheatingK=input$sCheatingK,
                   sReplicationOn=input$sReplicationOn,sReplPower=input$sReplPower,
                   sReplSigOnly=input$sReplSigOnly,sReplRepeats=input$sReplRepeats,sReplCorrection=input$sReplCorrection,
                   sReplKeep=input$sReplKeep,sReplTails=input$sReplTails,
                   sN_Strata=input$sN_Strata, sR_Strata=input$sR_Strata,
                   sNClu_Cluster=input$sNClu_Cluster, sRClu_Cluster=input$sRClu_Cluster,
                   sNClu_Convenience=input$sNClu_Convenience, sRClu_Convenience=input$sRClu_Convenience, sNCont_Convenience=input$sNCont_Convenience, sRCont_Convenience=input$sRCont_Convenience, sRSpread_Convenience=input$sRSpread_Convenience,
                   sNClu_Snowball=input$sNClu_Snowball, sRClu_Snowball=input$sRClu_Snowball, sNCont_Snowball=input$sNCont_Snowball, sRCont_Snowball=input$sRCont_Snowball, sRSpread_Snowball=input$sRSpread_Snowball
      )
      if (is.null(oldDesign)) {
        design$sNRandK<-checkNumber(design$sNRandK)
        design$sReplPower<-checkNumber(design$sReplPower)
      } else {
        design$sNRandK<-checkNumber(design$sNRandK,oldDesign$sNRandK)
        design$sReplPower<-checkNumber(design$sReplPower,oldDesign$sReplPower)
      }
      oldDesign<<-design
      if (variablesHeld=="Data" && !applyingAnalysis && switches$doBootstrap) {design$sMethod<-"Resample"}
      if (debug) print("     updateDesign - exit")
      design
    }
    
    updateEvidence<-function(){
      if (debug) print("     updateEvidence")
      evidence<-list(rInteractionOn=input$rInteractionOn,
                     rInteractionOnly=input$rInteractionOnly,
                     showType=input$EvidenceEffect_type,
                     showTheory=input$evidenceTheory,
                     allScatter=input$allScatter,
                     longHand=input$evidenceLongHand,
                     ssqType=input$ssqType,
                     llr=list(e1=input$llr1,e2=input$llr2),
                     evidenceCaseOrder=input$evidenceCaseOrder,Welch=input$Welch,
                     dataType=input$dataType,analysisType=input$analysisType,
                     pScale=input$pScale,wScale=input$wScale,nScale=input$nScale,
                     usePrior=input$STPrior,
                     prior=list(worldOn=FALSE,populationPDF="",
                                populationPDFk=0,populationRZ="r",
                                populationNullp=0)
      )
      switch(input$STPrior,
             "none"={
               evidence$prior=list(worldOn=input$world_on,populationPDF="Uniform",
                                   populationPDFk=0,populationRZ="z",
                                   populationNullp=0.5)
             },
             "world"={
               evidence$prior=list(worldOn=input$world_on,populationPDF=input$world_distr,
                          populationPDFk=input$world_distr_k,populationRZ=input$world_distr_rz,
                          populationNullp=input$world_distr_Nullp)
             },
             "prior"={
               evidence$prior=list(worldOn=input$world_on,populationPDF=input$likelihoodPrior_distr,
                          populationPDFk=input$likelihoodPrior_distr_k,populationRZ=input$likelihoodPrior_distr_rz,
                          populationNullp=input$likelihoodPrior_Nullp)
             }
      )
      if (debug) print("     updateEvidence - exit")
      evidence
    }
    
# hypothesis diagram
    output$HypothesisPlot<-renderPlot({
      doIt<-editVar$data
      if (debug) print("HypothesisPlot")

      IV<-updateIV()
      IV2<-updateIV2()
      DV<-updateDV()
      if (variablesHeld=="Data") {
      if (IV$deploy=="Within" && !isempty(IV$targetDeploys) && !grepl(paste0(",",DV$name,","),IV$targetDeploys)) {
        hmm(paste0("Warning: ", IV$name," requires matched DV (",substr(IV$targetDeploys,2,nchar(IV$targetDeploys)),")"))
      }
      if (DV$deploy=="Within") {
        hmm(paste0("Warning: ", DV$name," is a within-participants IV and cannot be used as a DV"))
      }
      }
      effect<-updateEffect()

      PlotNULL<-ggplot()+plotBlankTheme+theme(plot.margin=margin(0,-0.1,0,0,"cm"))+
        scale_x_continuous(limits = c(0,10),labels=NULL,breaks=NULL)+scale_y_continuous(limits = c(0,10),labels=NULL,breaks=NULL)
        
      xmin<-2
      xmax<-8
      switch (no_ivs,
              {
                g<-PlotNULL+
                  # annotation_custom(grob=ggplotGrob(PlotIV()),xmin=3,xmax=7,ymin=6,ymax=10)+
                  annotation_custom(grob=ggplotGrob(drawVariable(IV)),xmin=xmin,xmax=xmax,ymin=6,ymax=10)+
                  annotation_custom(grob=ggplotGrob(drawVariable(DV)),xmin=xmin,xmax=xmax,ymin=0,ymax=4)
                
                  g<-g+annotation_custom(grob=ggplotGrob(drawEffectES(effect$rIV,1)),xmin=xmin,xmax=xmax,ymin=3.5,ymax=6)
              },
              {
                g<-PlotNULL+
                  annotation_custom(grob=ggplotGrob(drawVariable(IV)), xmin=0,  xmax=4,  ymin=6, ymax=9)+
                  annotation_custom(grob=ggplotGrob(drawVariable(IV2)),xmin=6,  xmax=10, ymin=6, ymax=9)+
                  annotation_custom(grob=ggplotGrob(drawVariable(DV)), xmin=3,  xmax=7,  ymin=0.5, ymax=3.5)
                  
                  g<-g+annotation_custom(grob=ggplotGrob(drawEffectES(effect$rIV,2)),xmin=1.5,xmax=5.5,ymin=3, ymax=7)+
                     annotation_custom(grob=ggplotGrob(drawEffectES(effect$rIV2,3)),xmin=4.5,xmax=8.5,ymin=3, ymax=7)+
                     annotation_custom(grob=ggplotGrob(drawEffectES(effect$rIVIV2,4)),xmin=3,  xmax=7,  ymin=6, ymax=9)+
                     annotation_custom(grob=ggplotGrob(drawEffectES(effect$rIVIV2DV,5)),xmin=3,  xmax=7,  ymin=3, ymax=7)
              }
      )
      if (debug) print("HypothesisPlot - exit")
      
      g
    }
    )

# world diagram
    output$WorldPlot<-renderPlot({
      doIt<-editVar$data
      effect<-updateEffect()
      design<-updateDesign()

      PlotNULL<-ggplot()+plotBlankTheme+theme(plot.margin=margin(0,-0.1,0,0,"cm"))+
        scale_x_continuous(limits = c(0,10),labels=NULL,breaks=NULL)+scale_y_continuous(limits = c(0,10),labels=NULL,breaks=NULL)
      
      if (debug) print("WorldPlot")
      x<-seq(-0.99,0.99,length.out=101)
      if (effect$world$populationRZ=="z") {r<-atanh(x)} else {r<-x}
      switch (effect$world$populationPDF,
              "Single"={
                y<-r*0
                use=which.min(abs(x-effect$world$populationPDFk))
                y[use]=1
                },
              "Uniform"={
                y<-r*0+0.5
                },
              "Exp"={
                y<-exp(-abs(r/effect$world$populationPDFk))
                },
              "Gauss"={
                y<-exp(-0.5*abs(r/effect$world$populationPDFk)^2)
                },
      )
      if (effect$world$populationRZ=="z") {
        y<-y/(1-x^2)
        y<-y/max(y)
        }
      y<-y*(1-effect$world$populationNullp)
      
      if (effect$world$populationPDF=="Single" && effect$world$populationNullp>0) {
        use<-which.min(abs(x-0))
        y[use]<-effect$world$populationNullp
      }
      
      x<-c(-1,x,1)
      y[y==0]<-0.01
      y<-c(0,y,0)
      pts=data.frame(x=x,y=y)
      g1<-ggplot(pts,aes(x=x,y=y))
      g1<-g1+geom_polygon(data=pts,aes(x=x,y=y),fill="yellow")+scale_y_continuous(limits = c(0,1.05),labels=NULL,breaks=NULL)
      g1<-g1+plotTheme+theme(plot.margin=popplotMargins)+labs(x=bquote(r[population]),y="Density")

      if (design$sNRand) {
        nbin<-5+seq(0,qgamma(0.99,shape=design$sNRandK,scale=(design$sN-5)/design$sNRandK),length.out=101)
        ndens<-dgamma(nbin-5,shape=design$sNRandK,scale=(design$sN-5)/design$sNRandK)
        ndens<-ndens/max(ndens)
      } else {
        nbin<-seq(1,250,length.out=251)
        ndens<-nbin*0+0.01
        use=which.min(abs(nbin-input$sN))
        ndens[use]<-1
      }
      x<-c(min(nbin),nbin,max(nbin))
      y<-c(0,ndens,0)
      pts=data.frame(x=x,y=y)
      g2<-ggplot(pts,aes(x=x,y=y))
      g2<-g2+geom_polygon(data=pts,aes(x=x,y=y),fill="yellow")+scale_y_continuous(limits = c(0,1.05),labels=NULL,breaks=NULL)
      g2<-g2+plotTheme+theme(plot.margin=popplotMargins)+labs(x="n",y="Density")
      if (debug) print("WorldPlot - exit")
      
      g<-PlotNULL+annotation_custom(grob=ggplotGrob(g1), xmin=0,  xmax=10,  ymin=5.5, ymax=10)+
                  annotation_custom(grob=ggplotGrob(g2), xmin=0,  xmax=10,  ymin=0.5, ymax=5)
      
        g
    }
    )
    
# population diagram
    output$PopulationPlot <- renderPlot({
      doIt<-editVar$data
      if (debug) print("PopulationPlot")
      
        IV<-updateIV()
        IV2<-updateIV2()
        DV<-updateDV()
        if (is.null(IV) || is.null(DV)) {return(ggplot()+plotBlankTheme)}
        
        effect<-updateEffect()

        switch (no_ivs,
                {
                  g<-drawPopulation(IV,DV,effect,alpha=1)
                  },
                { 
                  effect1<-effect
                effect2<-effect1
                effect2$rIV<-effect2$rIV2
                effect3<-effect1
                effect3$rIV<-effect3$rIVIV2
                
                  g<-ggplot()+plotBlankTheme+theme(plot.margin=margin(0,-0.2,0,0,"cm"))+
                    scale_x_continuous(limits = c(0,10),labels=NULL,breaks=NULL)+scale_y_continuous(limits = c(0,10),labels=NULL,breaks=NULL)+
                    
                    annotation_custom(grob=ggplotGrob(drawPopulation(IV,DV,effect1,alpha=1)+gridTheme),xmin=0.5,xmax=4.5,ymin=0,ymax=5)+
                    annotation_custom(grob=ggplotGrob(drawPopulation(IV2,DV,effect2,alpha=1)+gridTheme),xmin=5.5,xmax=9.5,ymin=0,ymax=5)+
                    annotation_custom(grob=ggplotGrob(drawPopulation(IV,IV2,effect3,alpha=1)+gridTheme),xmin=3,xmax=7,ymin=5,ymax=10)
                }
        )
        if (debug) print("PopulationPlot - exit")
        g
    })  
    
# prediction diagram
    output$PredictionPlot <- renderPlot({
      doIt<-editVar$data
      if (debug) print("PredictionPlot")
      
      IV<-updateIV()
      IV2<-updateIV2()
      DV<-updateDV()
        if (is.null(IV) || is.null(DV)) {return(ggplot()+plotBlankTheme)}
        
        design<-updateDesign()
        effect<-updateEffect()
        evidence<-updateEvidence()

        switch (no_ivs,
                {g<-drawPrediction(IV,IV2,DV,effect,design)},
                {
                  if (evidence$rInteractionOn==FALSE){
                    effect1<-effect
                    effect2<-effect
                    effect2$rIV<-effect2$rIV2
                    
                    g<-ggplot()+plotBlankTheme+theme(plot.margin=margin(0,-0.2,0,0,"cm"))+
                      scale_x_continuous(limits = c(0,10),labels=NULL,breaks=NULL)+scale_y_continuous(limits = c(0,10),labels=NULL,breaks=NULL)+
                      
                      annotation_custom(grob=ggplotGrob(drawPrediction(IV,NULL,DV,effect1,design)+gridTheme),xmin=0.5,xmax=4.5,ymin=0,ymax=5)+
                      annotation_custom(grob=ggplotGrob(drawPrediction(IV2,NULL,DV,effect2,design)+gridTheme),xmin=5.5,xmax=9.5,ymin=0,ymax=5)
                  } else{
                    if (showInteractionOnly){
                      g<-drawPrediction(IV,IV2,DV,effect,design)
                    } else{
                      effect1<-effect
                      effect2<-effect
                      effect2$rIV<-effect2$rIV2
                      
                      g<-ggplot()+plotBlankTheme+theme(plot.margin=margin(0,-0.2,0,0,"cm"))+
                        scale_x_continuous(limits = c(0,10),labels=NULL,breaks=NULL)+scale_y_continuous(limits = c(0,10),labels=NULL,breaks=NULL)+
                        
                        annotation_custom(grob=ggplotGrob(drawPrediction(IV,NULL,DV,effect1,design)+gridTheme),xmin=0.5,xmax=4.5,ymin=0,ymax=5)+
                        annotation_custom(grob=ggplotGrob(drawPrediction(IV2,NULL,DV,effect2,design)+gridTheme),xmin=5.5,xmax=9.5,ymin=0,ymax=5)+
                        annotation_custom(grob=ggplotGrob(drawPrediction(IV,IV2,DV,effect,design)+gridTheme),xmin=3,xmax=7,ymin=5,ymax=10)
                    }
                  }
                }
        )
        if (debug) print("PredictionPlot - exit")
        g
    })  

##################################################################################    
# SINGLE SAMPLE
    # UI changes
    # calculations
    # graphs (sample, describe, infer)
    # report (sample, describe, infer)
#    
    applyingAnalysis<-FALSE
    
    # UI changes
    # go to the sample tabs 
    sampleUpdate<-observeEvent(c(input$Single,input$EvidencenewSample,input$EvidenceHypothesisApply),{
      if (any(c(input$Single,input$EvidencenewSample))>0) {
        if (!is.element(input$Graphs,c("Sample","Describe","Infer","Possible")))
        {updateTabsetPanel(session, "Graphs",
                           selected = "Sample")
          updateTabsetPanel(session, "Reports",
                            selected = "Sample")
        }
      }
    }
    )
    observeEvent(input$LGEvidencenewSample,{
      if (input$LGEvidencenewSample>0) {
        if (!is.element(input$LGEvidenceGraphs,c("Sample","Describe","Infer")))
        {updateTabsetPanel(session, "LGEvidenceGraphs",
                           selected = "Sample")
        }
      }
    }
    )
    
    whichAnalysisSample<-observeEvent(input$EvidencenewSample,{
      applyingAnalysis<<-FALSE
    },priority=100)
    whichAnalysisApply<-observeEvent(input$EvidenceHypothesisApply,{
      applyingAnalysis<<-TRUE
    },priority=100)
    
# single sample calculations
    doSampleAnalysis<-function(IV,IV2,DV,effect,design,evidence){
      
      if (IV$type=="Ordinal") {
        if (warnOrd==FALSE) {
          hmm("Ordinal IV will be treated as Interval.")
          warnOrd<<-TRUE
        }
      }
      if (!is.null(IV2)) {
        if (IV2$type=="Ordinal") {
          if (warnOrd==FALSE) {
            hmm("Ordinal IV2 will be treated as Interval.")
            warnOrd<<-TRUE
          }
        }
      }
      
      result<-runSimulation(IV,IV2,DV,effect,design,evidence)

      result
    }

    # eventReactive wrapper
    sampleAnalysis<-eventReactive(c(input$EvidenceHypothesisApply,input$EvidencenewSample,input$LGEvidencenewSample),{
      if (any(input$EvidenceHypothesisApply,input$EvidencenewSample,input$LGEvidencenewSample)>0){
        validSample<<-TRUE
        IV<-updateIV()
        IV2<-updateIV2()
        DV<-updateDV()
        
        effect<-updateEffect()
        design<-updateDesign()
        evidence<-updateEvidence()
        
        showNotification("Sample: starting",id="counting",duration=Inf,closeButton=FALSE,type="message")
        result<-doSampleAnalysis(IV,IV2,DV,effect,design,evidence)
        ResultHistory<<-result$ResultHistory

        # set the result into likelihood: populations
        if (!is.na(result$rIV)) {
          updateNumericInput(session,"likelihoodPSampRho",value=result$rIV)
          updateNumericInput(session,"likelihoodSampRho",value=result$rIV)
        }
        removeNotification(id = "counting")
      } else {
        result<-NULL
      }
      result
    })
    
        
    # SINGLE graphs
    # single sample graph
      makeSampleGraph <- function () {
      if (debug) print("SamplePlot")
      doIt<-editVar$data

      IV<-updateIV()
      IV2<-updateIV2()
      DV<-updateDV()
        if (is.null(IV) || is.null(DV)) {return(ggplot()+plotBlankTheme)}
        
      effect<-updateEffect()
      design<-updateDesign()
      evidence<-updateEvidence()

      # make the sample
      result<-sampleAnalysis()
        if (is.null(result) ||  !validSample)  {return(ggplot()+plotBlankTheme)}
  
      # draw the sample
      g<-ggplot()+plotBlankTheme+theme(plot.margin=margin(0,-0.2,0,0,"cm"))+
        scale_x_continuous(limits = c(0,10),labels=NULL,breaks=NULL)+scale_y_continuous(limits = c(0,10),labels=NULL,breaks=NULL)
        
        switch (no_ivs,{
          g<-g+annotation_custom(grob=ggplotGrob(drawSample(IV,DV,effect,result)+gridTheme),xmin=0.5,xmax=9.5,ymin=0.5,ymax=9.5)
        },
        { 
          effect1<-effect
          effect2<-effect
          effect2$rIV<-effect2$rIV2
          effect3<-effect
          effect3$rIV<-effect3$rIVIV2
          
          result1<-result
          result2<-list(IVs=result$IV2s, DVs=result$DVs, rIV=result$rIV2, ivplot=result$iv2plot,dvplot=result$dvplot)
          result3<-list(IVs=result$IVs, DVs=result$IV2s, rIV=result$rIVIV2, ivplot=result$ivplot,dvplot=result$iv2plot)
          
          g<-g+
            annotation_custom(grob=ggplotGrob(drawSample(IV,DV,effect1,result1)+gridTheme),xmin=0.5,xmax=4.5,ymin=0,ymax=5)+
            annotation_custom(grob=ggplotGrob(drawSample(IV2,DV,effect2,result2)+gridTheme),xmin=5.5,xmax=9.5,ymin=0,ymax=5)+
            annotation_custom(grob=ggplotGrob(drawSample(IV,IV2,effect3,result3)+gridTheme),xmin=3,xmax=7,ymin=5,ymax=10)
        }
        )
      g
    }
      output$SamplePlot <- renderPlot({
        doIt<-editVar$data
        makeSampleGraph()
      })
      
    # single descriptive graph
    makeDescriptiveGraph <- function(){
      if (debug) print("DescriptivePlot")
      doIt<-editVar$data
      # doIt<-input$MVok
      IV<-updateIV()
        IV2<-updateIV2()
        DV<-updateDV()
        if (is.null(IV) || is.null(DV)) {return(ggplot()+plotBlankTheme)}

        effect<-updateEffect()
        design<-updateDesign()
        evidence<-updateEvidence()
        
        # make the sample
        result<-sampleAnalysis()
        if (is.null(result) ||  !validSample)  {
          # validate("Sample is empty")
          return(ggplot()+plotBlankTheme)
          }
        if (is.na(result$rIV)) {
          validate("IV has no variability")
          return(ggplot()+plotBlankTheme)
        }
        
        # draw the description
        g<-ggplot()+plotBlankTheme+theme(plot.margin=margin(0,-0.2,0,0,"cm"))+
          scale_x_continuous(limits = c(0,10),labels=NULL,breaks=NULL)+scale_y_continuous(limits = c(0,10),labels=NULL,breaks=NULL)
        
        switch (no_ivs, 
                {
                  g<-g+annotation_custom(grob=ggplotGrob(drawDescription(IV,NULL,DV,effect,design,result)+gridTheme),xmin=0.5,xmax=9.5,ymin=0.5,ymax=9.5)
                },
                { 
                  if (evidence$rInteractionOn==FALSE){
                    effect2<-effect
                    effect2$rIV<-effect2$rIV2
                    
                    result2<-list(IVs=result$IV2s, DVs=result$DVs, rIV=result$rIV2, ivplot=result$iv2plot,dvplot=result$dvplot)
                    
                    g<-g+
                      annotation_custom(grob=ggplotGrob(drawDescription(IV,NULL,DV,effect,design,result)+gridTheme),xmin=0.5,xmax=4.5,ymin=0,ymax=5)+
                      annotation_custom(grob=ggplotGrob(drawDescription(IV2,NULL,DV,effect2,design,result2)+gridTheme),xmin=5.5,xmax=9.5,ymin=0,ymax=5)
                  }
                  else{
                    if (showInteractionOnly){
                      if (DV$type=="Categorical") {
                          if (IV2$type=="Interval") {
                        effect1<-effect
                        result1<-result
                        use<-result1$iv2<median(result$iv2)
                        result1$iv<-result$iv[use]
                        result1$dv<-result$dv[use]
                        result1$IVs$vals<-result$iv[use]
                        result1$DVs$vals<-result$dv[use]
                        
                        effect2<-effect
                        result2<-result
                        result2$iv<-result$iv[!use]
                        result2$dv<-result$dv[!use]
                        result2$IVs$vals<-result$iv[!use]
                        result2$DVs$vals<-result$dv[!use]
                        
                        g<-g+annotation_custom(grob=ggplotGrob(drawDescription(IV,NULL,DV,effect1,design,result1)+gridTheme+ggtitle(paste0(IV2$name,">",format(median(result$iv2),digits=3)))),xmin=0.5,xmax=4.5,ymin=0,ymax=5)
                        g<-g+annotation_custom(grob=ggplotGrob(drawDescription(IV,NULL,DV,effect2,design,result2)+gridTheme+ggtitle(paste0(IV2$name,"<",format(median(result$iv2),digits=3)))),xmin=5.5,xmax=9.5,ymin=0,ymax=5)
                          } else {
                            switch (IV2$ncats,
                                    {},
                                    {xmin<-c(0.5,5.5)
                                     xmax<-c(4.5,9.5)
                                     ymin<-c(0,0)
                                     ymax<-c(5,5)},
                                    {xmin<-c(0.5,5.5,3)
                                    xmax<-c(4.5,9.5,7)
                                    ymin<-c(0,0,5)
                                    ymax<-c(4.25,4.25,9.25)},
                                    {xmin<-c(0.5,5.5,0.5,5.5)
                                    xmax<-c(4.5,9.5,4.5,9.5)
                                    ymin<-c(0,0,5,5)
                                    ymax<-c(4.25,4.25,9.25,9.25)},
                                    {}
                            )
                            for (i in 1:IV2$ncats) {
                            effect1<-effect
                            result1<-result
                            use<-result1$iv2<-as.numeric(result$iv2)==i
                            result1$iv<-result$iv[use]
                            result1$dv<-result$dv[use]
                            result1$IVs$vals<-result$iv[use]
                            result1$DVs$vals<-result$dv[use]
                            
                            g<-g+annotation_custom(grob=ggplotGrob(drawDescription(IV,NULL,DV,effect1,design,result1)+gridTheme+ggtitle(paste0(IV2$name,"==",IV2$cases[i]))),xmin=xmin[i],xmax=xmax[i],ymin=ymin[i],ymax=ymax[i])
                            }
                          }
                        # effect2<-effect
                        # result2<-list(IVs=result$IV2s, DVs=result$DVs, rIV=result$rIV2, ivplot=result$iv2plot,dvplot=result$dvplot)
                      } else {
                        g<-g+annotation_custom(grob=ggplotGrob(drawDescription(IV,IV2,DV,effect,design,result)+gridTheme),xmin=0.5,xmax=9.5,ymin=0.5,ymax=9.5)
                      }
                    } else{
                      effect2<-effect
                      effect2$rIV<-effect2$rIV2
                      
                      result2<-list(IVs=result$IV2s, DVs=result$DVs, rIV=result$rIV2, iv=result$iv, dv=result$dv, ivplot=result$iv2plot,dvplot=result$dvplot)
                      
                      g<-g+annotation_custom(grob=ggplotGrob(drawDescription(IV,NULL,DV,effect,design,result)+gridTheme),xmin=0.5,xmax=4.5,ymin=0,ymax=5)
                      g<-g+annotation_custom(grob=ggplotGrob(drawDescription(IV2,NULL,DV,effect2,design,result2)+gridTheme),xmin=5.5,xmax=9.5,ymin=0,ymax=5)
                      g<-g+annotation_custom(grob=ggplotGrob(drawDescription(IV,IV2,DV,effect,design,result)+gridTheme),xmin=3,xmax=7,ymin=5,ymax=10)
                    }
                  }
                }
        )
        g
    }
   output$DescriptivePlot <- renderPlot({
      doIt<-editVar$data
      makeDescriptiveGraph()
      })
      
    # single inferential graph
   makeInferentialGraph <- function() {
     doit<-c(input$EvidenceInfer_type,input$LGEvidenceInfer_type,input$evidenceTheory)
      if (debug) print("InferentialPlot")
      doIt<-editVar$data
      llrConsts<-c(input$llr1,input$llr2)
      
      # doIt<-input$MVok
      IV<-updateIV()
        IV2<-updateIV2()
        DV<-updateDV()
        if (is.null(IV) || is.null(DV)) {return(ggplot()+plotBlankTheme)}
        
        effect<-updateEffect()
        design<-updateDesign()
        evidence<-updateEvidence()
        
        result<-sampleAnalysis()
        if (is.null(result)) {
          result<-list(rIV=NA,effect=effect,design=design,evidence=evidence)
        }
        # if (is.null(result) ||  !validSample)  {return(ggplot()+plotBlankTheme)}
        if (is.na(result$rIV) && validSample) {
          validate("IV has no variability")
          return(ggplot()+plotBlankTheme)
        }
        
        result$showType<-evidence$showType
        result$evidence$showTheory<-input$evidenceTheory
        
        g<-ggplot()+plotBlankTheme+theme(plot.margin=margin(0,-0.2,0,0,"cm"))+
          scale_x_continuous(limits = c(0,10),labels=NULL,breaks=NULL)+scale_y_continuous(limits = c(0,10),labels=NULL,breaks=NULL)
        
        switch (input$EvidenceInfer_type,
                "EffectSize"={
                  g1<-drawInference(IV,IV2,DV,effect,design,evidence,result,"r")
                  g2<-drawInference(IV,IV2,DV,effect,design,evidence,result,"p")
                },
                "Power"= {
                  g1<-drawInference(IV,IV2,DV,effect,design,evidence,result,"w")
                  g2<-drawInference(IV,IV2,DV,effect,design,evidence,result,"nw")
                },
                "log(lrs)"={
                  g1<-drawInference(IV,IV2,DV,effect,design,evidence,result,"log(lrs)")
                  g2<-drawInference(IV,IV2,DV,effect,design,evidence,result,"p")
                },
                "log(lrd)"={
                  g1<-drawInference(IV,IV2,DV,effect,design,evidence,result,"log(lrd)")
                  g2<-drawInference(IV,IV2,DV,effect,design,evidence,result,"p")
                }
        )
        g<-g+annotation_custom(grob=ggplotGrob(g1+gridTheme),xmin=0,xmax=4.5,ymin=0,ymax=10)
        g<-g+annotation_custom(grob=ggplotGrob(g2+gridTheme),xmin=5,xmax=10,ymin=0,ymax=10)
        return(g)
    }
      output$InferentialPlot <- renderPlot({
        doIt<-editVar$data
        makeInferentialGraph()
      })
      
# SINGLE reports    
    # single sample report
    output$SampleReport <- renderPlot({
      if (debug) print("SampleReport")

      doIt<-editVar$data
      # doIt<-input$MVok
      IV<-updateIV()
        IV2<-updateIV2()
        DV<-updateDV()
        if (is.null(IV) || is.null(DV)) {return(ggplot()+plotBlankTheme)}
        
        effect<-updateEffect()
        design<-updateDesign()
        
        result<-sampleAnalysis()        
        if (is.null(result) ||  !validSample)  {return(ggplot()+plotBlankTheme)}

        reportSample(IV,IV2,DV,design,result)     
    })
    
    # single descriptive report
    output$DescriptiveReport <- renderPlot({
      if (debug) print("DescriptiveReport")
      doIt<-editVar$data
      # doIt<-input$MVok
      IV<-updateIV()
      IV2<-updateIV2()
      DV<-updateDV()
        if (is.null(IV) || is.null(DV)) {return(ggplot()+plotBlankTheme)}
        
      effect<-updateEffect()
      design<-updateDesign()
      evidence<-updateEvidence()
      
        result<-sampleAnalysis()
        if (is.null(result) ||  !validSample)  {return(ggplot()+plotBlankTheme)}
        if (is.na(result$rIV)) {
          validate("IV has no variability")
          return(ggplot()+plotBlankTheme)
        }
        result$showType<-evidence$showType
        
        reportDescription(IV,IV2,DV,result)
    })
    
    # single inferential report
    output$InferentialReport <- renderPlot({
      if (debug) print("InferentialReport")
      doIt<-editVar$data
      llrConsts<-c(input$llr1,input$llr2)
      
      # doIt<-input$MVok
      IV<-updateIV()
        IV2<-updateIV2()
        DV<-updateDV()
        if (is.null(IV) || is.null(DV)) {return(ggplot()+plotBlankTheme)}
        
        effect<-updateEffect()
        design<-updateDesign()
        evidence<-updateEvidence()
        
        result<-sampleAnalysis()
        if (is.null(result) ||  !validSample)  {return(ggplot()+plotBlankTheme)}
        if (is.na(result$rIV)) {
          validate("IV has no variability")
          return(ggplot()+plotBlankTheme)
        }
        
        result$showType<-evidence$showType
        reportInference(IV,IV2,DV,effect,evidence,result)        
    })
    
##################################################################################    
# META-ANALYSIS
    # UI changes
    # calculations
    # graphs (sample, describe, infer)
    # report (sample, describe, infer)
    #    
    validMeta<<-FALSE
    notRunningMeta<-TRUE
    
    # UI changes
    observeEvent(input$metaRun,{
      if (debug) print("metaRun")
      if (input$metaRun>0) {
        if (notRunningMeta) {
          updateTabsetPanel(session, "Graphs",selected = "MetaAnalysis")
        updateTabsetPanel(session, "Reports",selected = "MetaAnalysis")
        validMeta<<-TRUE
        if (!input$meta_append) {
          resetMeta()
        }
        metaResult$nsims<<-metaResult$count+as.numeric(input$meta_runlength)
          updateActionButton(session,"metaRun",label=stopLabel)
          updateActionButton(session,"LGmetaRun",label=stopLabel)
          notRunningMeta<<-FALSE
        } else {
          metaResult$nsims<<-metaResult$count
          updateActionButton(session,"metaRun",label="Run")
          updateActionButton(session,"LGmetaRun",label="Run")
          notRunningMeta<<-TRUE
        }
      }
    }
    ,priority=100
    )
    
    observeEvent(input$LGmetaRun,{
      if (debug) print("LGmetaRun")
      if (input$LGmetaRun>0) {
        if (notRunningMeta) {
          validMeta<<-TRUE
          if (!input$LGmeta_append) {
          resetMeta()
        }
        metaResult$nsims<<-metaResult$count+as.numeric(input$LGmeta_runlength)
        updateActionButton(session,"metaRun",label=stopLabel)
        updateActionButton(session,"LGmetaRun",label=stopLabel)
        notRunningMeta<<-FALSE
        } else {
          metaResult$nsims<<-metaResult$count
          updateActionButton(session,"metaRun",label="Run")
          updateActionButton(session,"LGmetaRun",label="Run")
          notRunningMeta<<-TRUE
        }
      }
    }
    ,priority=100
    )
    
    applyingMetaAnalysis<-FALSE
    
    
    # set expected variable from UI
    updateMetaAnalysis<-function(){
      metaAnalysis<-list(
        nstudies=input$meta_nStudies,
        meta_fixedAnal=input$meta_fixedAnal,
        sig_only=input$meta_psigStudies,
        meta_psigAnal=input$meta_psigAnal,
        meta_nullAnal=input$meta_nullAnal,
        nsims=as.numeric(input$meta_runlength),
        longHand=input$evidenceLongHand,
        showTheory=input$evidenceTheory,
        append=input$meta_append
      )
      metaAnalysis
    }    
    
    # function to clear 
    resetMeta<-function(){
      metaResult<<-list(single=list(kmax=c(),Smax=c()),
                        gauss=list(kmax=c(),Smax=c()),
                        exp=list(kmax=c(),Smax=c()),
                        bestDist=c(),
                        bestK=c(),
                        bestNull=c(),
                        bestS=c(),
                        count=0,
                        nsims=0,
                        result=list(rpIV=c(),rIV=c(),pIV=c(),rIV2=c(),pIV2=c(),rIVIV2DV=c(),pIVIV2DV=c(),nval=c(),
                                    r=list(direct=c(),unique=c(),total=c(),coefficients=c()),
                                    p=list(direct=c(),unique=c(),total=c())
                        )
      )
    }
    # and do it at the start
    resetMeta()

    # make this a stand-alone function to be called from observEvent
    doMetaAnalysis<-function(IV,IV2,DV,effect,design,evidence,metaAnalysis,metaResult) {
      if (debug) {print("     doMetaAnalysis - start")}
      # if (metaAnalysis$longHand) {
        result<-multipleAnalysis(IV,IV2,DV,effect,design,evidence,metaAnalysis$nstudies,FALSE,metaResult$result,sigOnly=metaAnalysis$sig_only,
                                 showProgress=FALSE)
      # } else {
      #   nsims<-metaAnalysis$nstudies
      #   result<-sampleShortCut(IV,IV2,DV,effect,design,evidence,metaAnalysis$nstudies,FALSE,metaResult$result,sigOnly=metaAnalysis$sig_only)
      # }
      metaResult$result<-result
      metaResult<-runMetaAnalysis(metaAnalysis,metaResult)
      metaResult$effect<-effect
      metaResult$design<-design
      if (debug) {print("     doMetaAnalysis - end")}
      metaResult
    }
    
    # Expected outputs
    # show expected result    
    makeMetaGraph <- function() {
      doit<-c(input$metaRun,input$LGmetaRun)

      if (!validMeta) {return(ggplot()+plotBlankTheme)}
      
      if (metaResult$count<2) {
        silentTime<<-0
        pauseWait<<-10
      }
      if (metaResult$count==2) {
        silentTime<<-Sys.time()-time2
      }
      if (metaResult$count>2 && metaResult$count<=cycles2observe) {
        silentTime<<-rbind(silentTime,Sys.time()-time2)
      }
      if (metaResult$count>cycles2observe) {
        pauseWait<<-500
      }

      IV<-updateIV()
      IV2<-updateIV2()
      DV<-updateDV()
      
      effect<-updateEffect()
      design<-updateDesign()
      evidence<-updateEvidence()

      metaAnalysis<-updateMetaAnalysis()
      
      if (debug) {print("MetaPlot1 - start")}
      if (showProgress) {
            showNotification("MetaAnalysis: starting",id="counting",duration=Inf,closeButton=FALSE,type="message")
      }

      stopRunning<-TRUE
      if (metaResult$count<metaResult$nsims) {
        stopRunning<-FALSE
        metaAnalysis$append<-TRUE
        ns<-10^(min(2,floor(log10(max(1,metaResult$count)))))
        if (showProgress) {
          showNotification(paste0("A MetaAnalysis: ",metaResult$count,"/",metaResult$nsims),id="counting",duration=Inf,closeButton=FALSE,type="message")
        }
        for (i in 1:ns) {
        metaResult<<-doMetaAnalysis(IV,IV2,DV,effect,design,evidence,metaAnalysis,metaResult)
        }
      }
      if (metaResult$count>=metaResult$nsims) {
        if (showProgress) {removeNotification(id = "counting")}
      }

      g<-ggplot()+plotBlankTheme+theme(plot.margin=margin(0,-0.2,0,0,"cm"))
      g<-g+scale_x_continuous(limits = c(0,10),labels=NULL,breaks=NULL)+scale_y_continuous(limits = c(0,10),labels=NULL,breaks=NULL)

      if (metaAnalysis$nsims==1) {
        g1<-drawMeta(metaAnalysis,metaResult,"Plain")
        g<-g+annotation_custom(grob=ggplotGrob(g1+gridTheme),xmin=0.5,xmax=9.5,ymin=0.5,ymax=9.5)
        
      } else {
        g1<-drawMeta(metaAnalysis,metaResult,"Single")
        g2<-drawMeta(metaAnalysis,metaResult,"Gauss")
        g3<-drawMeta(metaAnalysis,metaResult,"Exp")
        g<-g+annotation_custom(grob=ggplotGrob(g1+gridTheme),xmin=0.5,xmax=4,ymin=0,ymax=10)+
          annotation_custom(grob=ggplotGrob(g2+gridTheme),xmin=4,xmax=7,ymin=0,ymax=10)+
          annotation_custom(grob=ggplotGrob(g3+gridTheme),xmin=7,xmax=10,ymin=0,ymax=10)
      }
      
      time2<<-Sys.time()
      if (!stopRunning) {
        if (doStop) {
          invalidateLater(mean(as.numeric(silentTime))*1000+pauseWait)
        } else {
          invalidateLater(10)
        }
      } else {
        updateActionButton(session,"metaRun",label="Run")
        updateActionButton(session,"LGmetaRun",label="Run")
        notRunningMeta<<-TRUE
      }
      
      if (debug) {print("MetaPlot1 - plots done ")}
      return(g)
    }
    
    output$MetaAnalysisPlot <- renderPlot({
      doit<-c(input$metaRun)
      makeMetaGraph()
    })
    
    
    output$MetaAnalysisReport <- renderPlot({
      doit<-c(input$metaRun)

      if (metaResult$count<metaResult$nsims) {
        if (doStop) {
          invalidateLater(mean(as.numeric(silentTime))*1000+pauseWait)
        } else {
          invalidateLater(10)
        }
      }
      
      if (metaResult$count>0) {
        g<-reportMetaAnalysis(metaResult)
      } else {
        g<-ggplot()+plotBlankTheme
      }
      g      
    })
    
##################################################################################    
# EXPECTED    
    # UI changes  
    # set expected variable from UI
    # calculations
    # outputs (2 graphs and report)
# 
    showProgress<-TRUE
    
    # function to clear 
    resetExpected<-function(){
    expectedResult<<-list(result=list(rpIV=c(),roIV=c(),rIV=c(),pIV=c(),rIV2=c(),pIV2=c(),rIVIV2DV=c(),pIVIV2DV=c(),nval=c(),r=list(direct=c(),unique=c(),total=c(),coefficients=c()),showType=c()),
                          nullresult=list(rpIV=c(),roIV=c(),rIV=c(),pIV=c(),rIV2=c(),pIV2=c(),rIVIV2DV=c(),pIVIV2DV=c(),nval=c(),r=list(direct=c(),unique=c(),total=c(),coefficients=c()),showType=c()),
                          count=0,
                          nullcount=0,
                          nsims=0,
                          running=FALSE)
    }
    # and do it at the start
    resetExpected()
    
    notRunningExpected<-TRUE
    
    # here's where we start a run
    observeEvent(c(input$EvidenceExpectedRun,input$LGEvidenceExpectedRun),{
      if (notRunningExpected) {
      startTime<<-Sys.time()
      cycleTime<<-0
      if (!input$EvidenceExpected_append) {resetExpected()} 
      if (input$evidenceLongHand) {
        expectedResult$nsims<<-expectedResult$count+as.numeric(input$EvidenceExpected_length)
      } else {
        expectedResult$nsims<<-expectedResult$count+as.numeric(input$EvidenceExpected_length)*10
      }
      if (input$EvidenceExpectedRun>0 || input$LGEvidenceExpectedRun>0) {
        updateActionButton(session,"EvidenceExpectedRun",label=stopLabel)
        updateActionButton(session,"LGEvidenceExpectedRun",label=stopLabel)
        notRunningExpected<<-FALSE
      }
      } else {
        expectedResult$nsims<<-expectedResult$count
        updateActionButton(session,"EvidenceExpectedRun",label="Run")
        updateActionButton(session,"LGEvidenceExpectedRun",label="Run")
        notRunningExpected<<-TRUE
      }
    })


    # UI changes
    # go to the expected tabs 
  expectedUpdate<-observeEvent(input$EvidenceExpectedRun,{
    if (input$EvidenceExpectedRun>0) {
        updateTabsetPanel(session, "Graphs",selected = "Expect")
        updateTabsetPanel(session, "Reports",selected = "Expect")
      validExpected<<-TRUE
    }
  }
  ,priority=100
  )
  expectedLGUpdate<-observeEvent(input$LGEvidenceExpectedRun,{
    if (input$LGEvidenceExpectedRun>0) {
      updateTabsetPanel(session, "LGEvidenceGraphs",selected = "Expect")
      validExpected<<-TRUE
    }
  }
  ,priority=100
  )
  observeEvent(input$EvidenceExpected_type,{
    if (!is.element(input$EvidenceExpected_type,c("NHSTErrors","FDR","CILimits","2D"))) {
      switch(input$EvidenceExpected_type,
           "EffectSize"={
             updateSelectInput(session,"EvidenceExpected_par1",selected="r")
             updateSelectInput(session,"EvidenceExpected_par2",selected="p")
           },
           "Power" = {
             updateSelectInput(session,"EvidenceExpected_par1",selected="nw")
             updateSelectInput(session,"EvidenceExpected_par2",selected="w")
           },
           "log(lrs)" = {
             updateSelectInput(session,"EvidenceExpected_par1",selected="p")
             updateSelectInput(session,"EvidenceExpected_par2",selected="log(lrs)")
           },           
           "log(lrd)" = {
             updateSelectInput(session,"EvidenceExpected_par1",selected="p")
             updateSelectInput(session,"EvidenceExpected_par2",selected="log(lrd)")
           }           
      )
    }
  })
  
# set expected variable from UI
    updateExpected<-function(){
        list(
          type=input$EvidenceExpected_type,
          Expected_par1=input$EvidenceExpected_par1,Expected_par2=input$EvidenceExpected_par2,
          nsims=as.numeric(input$EvidenceExpected_length),
          append=input$EvidenceExpected_append
          )
    }    
    
    
# make this a stand-alone function to be called from observEvent
    doExpectedAnalysis<-function(IV,IV2,DV,effect,design,evidence,expected,result,nsim=1) {
      if (debug) {print("     doExpectedAnalysis - start")}

        append<-TRUE
        # if (evidence$longHand || design$sReplicationOn) {
          result<-multipleAnalysis(IV,IV2,DV,effect,design,evidence,nsim,append,result,sigOnly=FALSE,showProgress=!showProgress)
        # } else {
        #   result<-sampleShortCut(IV,IV2,DV,effect,design,evidence,nsim,append,result,sigOnly=FALSE)
        # }
      if (debug) {print("     doExpectedAnalysis - end")}
      result
    }

# Expected outputs
    # show expected result    
    makeExpectedGraph <- function() {
      doit<-c(input$EvidenceExpected_type,input$EvidenceExpected_par1,input$EvidenceExpected_par2,input$EvidenceEffect_type,
              input$evidenceTheory,
              input$STMethod,input$alpha,
              input$LGEvidenceExpected_type,input$LGEvidenceExpected_par1,input$LGEvidenceExpected_par2,input$LGEvidenceEffect_type,
              input$world_distr,input$world_distr_rz,input$world_distr_k,input$world_distr_Nullp,
              input$LGEvidenceworld_distr,input$LGEvidenceworld_distr_rz,input$LGEvidenceworld_distr_k,input$LGEvidenceworld_distr_Nullp,
              input$EvidenceExpectedRun,input$LGEvidenceExpectedRun)
      
      if (expectedResult$count<2) {
        silentTime<<-0
        pauseWait<<-10
      }
      if (expectedResult$count==2) {
        silentTime<<-Sys.time()-time2
      }
      if (expectedResult$count>2 && expectedResult$count<=cycles2observe) {
        silentTime<<-rbind(silentTime,Sys.time()-time2)
      }
      if (expectedResult$count>cycles2observe) {
        pauseWait<<-500
      }
      
      llrConsts<-c(input$llr1,input$llr2)

      IV<-updateIV()
      IV2<-updateIV2()
      DV<-updateDV()
      
      effect<-updateEffect()
      design<-updateDesign()
      evidence<-updateEvidence()
      if (variablesHeld=="Data" && !applyingAnalysis && switches$doBootstrap) {design$sMethod<-"Resample"}
      
      expected<-updateExpected()

      expectedResult$result$showType<<-input$EvidenceEffect_type
      expectedResult$result$effect<<-effect
      expectedResult$result$design<<-design
      expectedResult$result$evidence<<-evidence
      expectedResult$nullresult$showType<<-input$EvidenceEffect_type
      expectedResult$nullresult$effect<<-nulleffect
      expectedResult$nullresult$design<<-design
      expectedResult$nullresult$evidence<<-evidence
      
      if (debug) {print("ExpectedPlot1 - start")}
      stopRunning<-TRUE
      
      if (validExpected) {
        
        if (switches$showAnimation && evidence$longHand) {
          ns<-10^(min(2,floor(log10(max(1,expectedResult$count)))))
          if (expectedResult$count+ns>expectedResult$nsims) {
            ns<-expectedResult$nsims-expectedResult$count
          }
        } else {
          ns<-expectedResult$nsims-expectedResult$count
        }
        if (ns>0) {
          expected$doingNull<-FALSE
          if (showProgress) {
            if (expectedResult$count==0) {
              showNotification("Expected: starting",id="counting",duration=Inf,closeButton=FALSE,type="message")
            } else {
              showNotification(paste0("Expected: ",format(expectedResult$count),"/",format(expectedResult$nsims)),id="counting",duration=Inf,closeButton=FALSE,type="message")
            }
          }
          expectedResult$result<<-doExpectedAnalysis(IV,IV2,DV,effect,design,evidence,expected,expectedResult$result,ns)
        }
        if (expectedResult$count<expectedResult$nsims) {
          stopRunning<-FALSE
        }
        if (!effect$world$worldOn  && expected$type=="NHSTErrors" && expectedResult$nullcount<expectedResult$nsims) {
          if (switches$showAnimation) {
            ns<-10^(min(2,floor(log10(max(1,expectedResult$nullcount)))))
            if (expectedResult$nullcount+ns>expectedResult$nsims) {
              ns<-expectedResult$nsims-expectedResult$nullcount
            }
          } else {
            ns<-expectedResult$nsims-expectedResult$nullcount
          }
          if (ns>0) {
            expected$doingNull<-TRUE
            if (showProgress) {
              showNotification(paste0("Expected|Null: ",format(expectedResult$nullcount),"/",format(expectedResult$nsims)),id="counting",duration=Inf,closeButton=FALSE,type="message")
            }
            expectedResult$nullresult<<-doExpectedAnalysis(IV,IV2,DV,updateEffect(NULL),design,evidence,expected,expectedResult$nullresult,ns)
          }
          if (expectedResult$nullcount<expectedResult$nsims) {
            stopRunning<-FALSE
          }
        }
        # wind up
        
        expectedResult$count<<-length(expectedResult$result$rIV)
        expectedResult$nullcount<<-length(expectedResult$nullresult$rIV)
        
        if (effect$world$worldOn && is.element(expected$type,c("NHSTErrors","FDR"))){
          nulls<-expectedResult$result$rpIV==0
          expectedResult$nullresult$rpIV<-expectedResult$result$rpIV[nulls]
          expectedResult$nullresult$roIV<-expectedResult$result$roIV[nulls]
          expectedResult$nullresult$rIV<-expectedResult$result$rIV[nulls]
          expectedResult$nullresult$pIV<-expectedResult$result$pIV[nulls]
          expectedResult$nullresult$nval<-expectedResult$result$nval[nulls]
          
          expectedResult$result$rpIV<-expectedResult$result$rpIV[!nulls]
          expectedResult$result$roIV<-expectedResult$result$roIV[!nulls]
          expectedResult$result$rIV<-expectedResult$result$rIV[!nulls]
          expectedResult$result$pIV<-expectedResult$result$pIV[!nulls]
          expectedResult$result$nval<-expectedResult$result$nval[!nulls]
          expectedResult$count<-length(expectedResult$result$rIV)
          expectedResult$nullcount<-length(expectedResult$nullresult$rIV)
        }
        
        
        # ? stop running
        if (stopRunning) {
          if (showProgress) {removeNotification(id = "counting")}
        }
      }
      
      # if (expectedResult$count==0) { return(ggplot()+plotBlankTheme) }

      g<-ggplot()+plotBlankTheme+theme(plot.margin=margin(0,-0.2,0,0,"cm"))
      g<-g+scale_x_continuous(limits = c(0,10),labels=NULL,breaks=NULL)+scale_y_continuous(limits = c(0,10),labels=NULL,breaks=NULL)
      
      if (expected$type=="2D") {
        if (expectedResult$count==0) {
          g<-ggplot()+plotBlankTheme
        } else {
          g1<-draw2Inference(IV,IV2,DV,effect,design,evidence,expectedResult$result,expected$Expected_par1,expected$Expected_par2)
          g<-g+annotation_custom(grob=ggplotGrob(g1+gridTheme),xmin=1,xmax=9,ymin=0,ymax=10)
        }
      } else {
        if (is.element(expected$type,c("NHSTErrors","FDR","CILimits"))) {
                switch (expected$type,
                        "NHSTErrors"={
                          g1<-e1_plot(expectedResult$nullresult,effect=effect)
                          g2<-e2_plot(expectedResult$result,effect=effect)
                        },
                        "FDR"={
                          g1<-e1_plot(expectedResult$nullresult,effect=effect)
                          g2<-e2_plot(expectedResult$result,effect=effect)
                        },
                        "CILimits"=  {
                          g1<-ci1_plot(expectedResult$result,effect=effect)
                          g2<-ci2_plot(expectedResult$result,effect=effect)
                        }
                )
        } else {
          g1<-drawInference(IV,IV2,DV,effect,design,evidence,expectedResult$result,expected$Expected_par1)
          g2<-drawInference(IV,IV2,DV,effect,design,evidence,expectedResult$result,expected$Expected_par2)
        }
        g<-g+annotation_custom(grob=ggplotGrob(g1+gridTheme),xmin=0,xmax=4.5,ymin=0,ymax=10)
        g<-g+annotation_custom(grob=ggplotGrob(g2+gridTheme),xmin=5,xmax=10,ymin=0,ymax=10)
      }
      
    if (!stopRunning) {
      time2<<-Sys.time()
      if (doStop) {
        invalidateLater(mean(as.numeric(silentTime))*1000+pauseWait)
      } else {
        invalidateLater(10)
      }
    } else {
      updateActionButton(session,"EvidenceExpectedRun",label="Run")
      updateActionButton(session,"LGEvidenceExpectedRun",label="Run")
      notRunningExpected<<-TRUE
    }
      if (debug) {print("ExpectedPlot1 - plots done ")}
      return(g)
    }

    output$ExpectedPlot <- renderPlot({
      doit<-c(input$EvidenceExpected_type,input$EvidenceExpected_par1,input$EvidenceExpected_par2,input$EvidenceEffect_type,
              input$LGEvidenceExpected_type,input$LGEvidenceExpected_par1,input$LGEvidenceExpected_par2,input$LGEvidenceEffect_type,
              input$EvidenceExpectedRun,input$LGEvidenceExpectedRun)
      makeExpectedGraph()
    })
    
    # expected report
    output$ExpectedReport <- renderPlot({
      doIt<-input$EvidenceExpectedRun
      llrConsts<-c(input$llr1,input$llr2)
      
      if (debug) {print("ExpectedReport - start")}
      IV<-updateIV()
      IV2<-updateIV2()
      DV<-updateDV()

      effect<-updateEffect()
      design<-updateDesign()
      evidence<-updateEvidence()

      expected<-updateExpected()
      expectedResult$result$showType<-input$EvidenceEffect_type

      if (expectedResult$count>1) {
        
        if (effect$world$worldOn && expected$type=="NHSTErrors"){
          nulls<-expectedResult$result$rpIV==0
          expectedResult$nullresult$rpIV<-expectedResult$result$rpIV[nulls]
          expectedResult$nullresult$roIV<-expectedResult$result$roIV[nulls]
          expectedResult$nullresult$rIV<-expectedResult$result$rIV[nulls]
          expectedResult$nullresult$pIV<-expectedResult$result$pIV[nulls]
          expectedResult$nullresult$nval<-expectedResult$result$nval[nulls]
          
          expectedResult$result$rpIV<-expectedResult$result$rpIV[!nulls]
          expectedResult$result$roIV<-expectedResult$result$roIV[!nulls]
          expectedResult$result$rIV<-expectedResult$result$rIV[!nulls]
          expectedResult$result$pIV<-expectedResult$result$pIV[!nulls]
          expectedResult$result$nval<-expectedResult$result$nval[!nulls]
          expectedResult$count<-length(expectedResult$result$rIV)
          expectedResult$nullcount<-length(expectedResult$nullresult$rIV)
        }
        
        g<-reportExpected(IV,IV2,DV,effect,evidence,expected,expectedResult$result,expectedResult$nullresult)
      } else {
        g<-ggplot()+plotBlankTheme
      }
      
      if (expectedResult$count<expectedResult$nsims) {
        # if (debug) {print("ExpectedPlot2 - timer set ")}
        if (doStop) {
          invalidateLater(mean(as.numeric(silentTime))*1000+pauseWait)
        } else {
          invalidateLater(10)
        }
        # invalidateLater(pauseWait)
      } 
      
      if (debug) {print("ExpectedReport - end")}
      return(g)
    })
    
    
##################################################################################    
# EXPLORE    
    # UI changes  
    # set explore variable from UI
    # calculations
    # outputs (graph and report)

# local variables
    lastExplore<-explore
    showProgress<-TRUE
    
    notRunningExplore<-TRUE
    
# main variable    
    resetExplore<-function(){
      exploreResult<<-list(result=list(count=0,rIVs=c(),pIVs=c(),rIV2=c(),pIV2=c(),rIVIV2DV=c(),pIVIV2DV=c(),nvals=c(),wIVs=c(),psig25=c(),psig=c(),psig75=c(),vals=c(),ks=c(),pnulls=c(),Ss=c()),
                           nullresult=list(count=0,rIVs=c(),pIVs=c(),rIV2=c(),pIV2=c(),rIVIV2DV=c(),pIVIV2DV=c(),nvals=c(),wIVs=c(),psig25=c(),psig=c(),psig75=c(),vals=c()),
                           nsims=0,
                           Explore_show=NA,
                           Explore_typeShow=NA,
                           evidence=evidence
                           )
    }
    resetExplore()
    
# UI changes
    # go to the explore tabs 
    # and set the explore run valid
    observeEvent(c(input$exploreRunH,input$exploreRunD,input$exploreRunM),{
      if (any(c(input$exploreRunH,input$exploreRunD,input$exploreRunM))>0) {
        validExplore<<-TRUE
        updateTabsetPanel(session, "Graphs",selected = "Explore")
        updateTabsetPanel(session, "Reports",selected = "Explore")
      }
    },priority=100)
    observeEvent(c(input$LGexploreRunH,input$LGexploreRunD,input$LGexploreRunM),{
      if (any(c(input$LGexploreRunH,input$LGexploreRunD,input$LGexploreRunM)>0))
      {validExplore<<-TRUE
      }
    },priority=100)
    
    # and watch for IV2 appearing
    observeEvent(input$IV2choice,{
      if (input$IV2choice=="none") {
        updateSelectInput(session,"Explore_typeH", choices=hypothesisChoices2[exploreHypothesisChoices])
      }
      else {
        updateSelectInput(session,"Explore_typeH", choices=hypothesisChoices3)
      }
    })
    # watch for changes to design
    observeEvent(input$Explore_typeD,{
      if (input$Explore_typeD=="SampleSize") {
        updateNumericInput(session,"Explore_nRange",value=250,min=10,step=50)
        updateNumericInput(session,"LGExplore_nRange",value=250,min=10,step=50)
      }
      if (input$Explore_typeD=="Repeats") {
        updateNumericInput(session,"Explore_nRange",value=7,min=1,step=1)
        updateNumericInput(session,"LGExplore_nRange",value=7,min=1,step=1)
      }
    })
    
# here's where we start a run
    observeEvent(c(input$exploreRunH,input$exploreRunD,input$exploreRunM,
                   input$LGexploreRunH,input$LGexploreRunD,input$LGexploreRunM),
                   {
                     runPressed<-c(input$exploreRunH,input$exploreRunD,input$LGexploreRunH,input$LGexploreRunD)
                     if (switches$doMetaAnalysis) runPressed<-c(runPressed,input$exploreRunM,input$LGexploreRunM)
                     
                     if (notRunningExplore) {
                        if (input$evidenceLongHand) {
                          gain<-1
                        } else {
                          gain<-1
                        }
                       switch (input$ExploreTab,
                             "Hypothesis"={
                               if (!input$ExploreAppendH) {resetExplore()}
                               exploreResult$nsims<<-exploreResult$result$count+as.numeric(input$Explore_lengthH)*gain
                               },
                             "Design"={
                               if (!input$ExploreAppendD) {resetExplore()}
                               exploreResult$nsims<<-exploreResult$result$count+as.numeric(input$Explore_lengthD)*gain
                             },
                             "MetaAnalysis"={
                               if (!input$ExploreAppendM) {resetExplore()}
                               exploreResult$nsims<<-exploreResult$result$count+as.numeric(input$Explore_lengthM)*gain
                             }
                     )
                       if (any(runPressed)) {
                         updateActionButton(session,"exploreRunH",label=stopLabel)
                         updateActionButton(session,"exploreRunD",label=stopLabel)
                         updateActionButton(session,"exploreRunM",label=stopLabel)
                         updateActionButton(session,"LGexploreRunH",label=stopLabel)
                         updateActionButton(session,"LGexploreRunD",label=stopLabel)
                         updateActionButton(session,"LGexploreRunM",label=stopLabel)
                         notRunningExplore<<-FALSE
                       }
                     } else {
                       exploreResult$nsims<<-exploreResult$result$count
                       updateActionButton(session,"exploreRunH",label="Run")
                       updateActionButton(session,"exploreRunD",label="Run")
                       updateActionButton(session,"exploreRunM",label="Run")
                       updateActionButton(session,"LGexploreRunH",label="Run")
                       updateActionButton(session,"LGexploreRunD",label="Run")
                       updateActionButton(session,"LGexploreRunM",label="Run")
                       notRunningExplore<<-TRUE
                     }
    },priority=100)
    
    
# set explore variable from UI    
    # update explore values    
    updateExplore<-function(){
        explore<-lastExplore
        if (is.element(input$ExploreTab,c("Hypothesis","Design","MetaAnalysis"))) {
        switch (input$ExploreTab,
                "Hypothesis"={
                  l<-list(Explore_type=input$Explore_typeH,
                          Explore_show=input$Explore_showH, 
                          Explore_typeShow=input$Explore_typeShowH, 
                          Explore_whichShow=input$Explore_whichShowH, 
                          Explore_length=as.numeric(input$Explore_lengthH),
                          Append=input$ExploreAppendH)  
                },
                "Design"={
                  l<-list(Explore_type=input$Explore_typeD,
                          Explore_show=input$Explore_showD, 
                          Explore_typeShow=input$Explore_typeShowD, 
                          Explore_whichShow=input$Explore_whichShowD, 
                          Explore_length=as.numeric(input$Explore_lengthD),
                          Append=input$ExploreAppendD)  
                },
                "MetaAnalysis"={
                  l<-list(Explore_type=input$Explore_typeM,
                          Explore_show=input$Explore_showM, 
                          Explore_typeShow=explore$Explore_typeShow, 
                          Explore_whichShow=explore$Explore_whichShow, 
                          Explore_length=as.numeric(input$Explore_lengthM),
                          Append=input$ExploreAppendM)  
                }
        )
        explore<-c(l,list(Explore_npoints=input$Explore_npoints,Explore_xlog = input$Explore_xlog,
                          Explore_quants=input$Explore_quants,
                          Explore_esRange=input$Explore_esRange,Explore_nRange=input$Explore_nRange,
                          Explore_metaRange=input$Explore_metaRange,Explore_Mxlog = input$Explore_Mxlog,Explore_nrRange=input$Explore_nRange,
                          ExploreFull_ylim=input$ExploreFull_ylim,
                          ExploreTheory=input$evidenceTheory,ExploreLongHand=input$evidenceLongHand,
                          Explore_family=input$ExploreTab)
                   )
        
        if (is.element(explore$Explore_type,c("IV","IV2","DV"))) {
          explore$Explore_type<-paste(explore$Explore_type,input$Explore_VtypeH,sep="")
        }
        }
        if (!input$evidenceLongHand) {
          explore$Explore_length<-explore$Explore_length*10
          }
        lastExplore<<-explore
        explore
    } 
    
# Main calculations    
    doExploreAnalysis <- function(IV,IV2,DV,effect,design,evidence,metaAnalysis,explore,result,nsim=1,doingNull=FALSE) {
      if (debug) {print("     doExploreAnalysis - start")}
      if (is.null(result$rIVs) || nrow(result$rIVs)<exploreResult$nsims) {
      if (nsim==exploreResult$nsims) {showProgress<-FALSE} else {showProgress<-TRUE}
        result$nsims<-exploreResult$nsims
        result<-exploreSimulate(IV,IV2,DV,effect,design,evidence,metaAnalysis,explore,result,nsim,doingNull,showProgress)
      }
      result
    }
      
    
   # Explore outputs
   # show explore analysis        
   makeExploreGraph <- function() {
     doit<-c(input$Explore_showH,input$Explore_showD,input$Explore_showM,
             input$LGExplore_showH,input$LGExplore_showD,input$LGExplore_showM,
             input$STMethod,input$alpha,input$STPrior)
     
     if (!is.null(exploreResult$Explore_family) && exploreResult$Explore_family!=input$ExploreTab) {
       if (is.null(exploreResultHold[[input$ExploreTab]])) {
         return(ggplot()+plotBlankTheme)
       }
       exploreResult<<-exploreResultHold[[input$ExploreTab]]
       }
     
     if (exploreResult$result$count<2) {
       silentTime<<-0
       pauseWait<<-10
     }
     if (exploreResult$result$count==2) {
       silentTime<<-Sys.time()-time2
     }
     if (exploreResult$result$count>2 && exploreResult$result$count<=cycles2observe) {
       silentTime<<-rbind(silentTime,Sys.time()-time2)
     }
     if (exploreResult$result$count>cycles2observe) {
       pauseWait<<-500
     }

     IV<-updateIV()
     IV2<-updateIV2()
     DV<-updateDV()

     if (!validExplore || is.null(IV) || is.null(DV)) {return(ggplot()+plotBlankTheme)}
     
     effect<-updateEffect()
     design<-updateDesign()
     evidence<-updateEvidence()
     metaAnalysis<-updateMetaAnalysis()
     
     # this guarantees that we update without recalculating if possible
     explore<-updateExplore()
     
     stopRunning<-TRUE
     # expectedResult$result$showType<<-input$EvidenceEffect_type
     
     if (switches$showAnimation) {
       if (explore$Explore_family!="MetaAnalysis" && !explore$ExploreLongHand) {
         ns<-10^(min(3,floor(log10(max(100,exploreResult$result$count)))))
       } else {
         ns<-10^(min(2,floor(log10(max(1,exploreResult$result$count)))))
       }
     } else {
       ns<-exploreResult$nsims-exploreResult$result$count
     }
     # if (explore$Explore_family!="MetaAnalysis" && !explore$ExploreLongHand) {
     #   ns<-ns*100
     # }
     if (exploreResult$result$count+ns>exploreResult$nsims){
       ns<-exploreResult$nsims-exploreResult$result$count
     }
     if (ns>0) {
       showNotification(paste0("Explore ",explore$Explore_family," : starting"),id="counting",duration=Inf,closeButton=FALSE,type="message")
       exploreResult$result<<-doExploreAnalysis(IV,IV2,DV,effect,design,evidence,metaAnalysis,explore,exploreResult$result,ns,doingNull=FALSE)
       exploreResult$result$count<<-nrow(exploreResult$result$rIVs)
     }
     if (exploreResult$result$count<exploreResult$nsims) {
       stopRunning<-FALSE
     }
     
     if (!effect$world$worldOn  && (explore$Explore_show=="NHSTErrors" || explore$Explore_show=="FDR")) {
       if (switches$showAnimation) {
         ns<-10^(min(2,floor(log10(max(1,exploreResult$nullresult$count)))))
         if (exploreResult$nullresult$count+ns>exploreResult$nsims){
           ns<-exploreResult$nsims-exploreResult$nullresult$count
         }
       } else {
         ns<-exploreResult$nsims-exploreResult$nullresult$count
       }
       if (ns>0) {
       showNotification(paste0("Explore(null) ",explore$Explore_family," : starting"),id="counting",duration=Inf,closeButton=FALSE,type="message")
       exploreResult$nullresult<<-doExploreAnalysis(IV,IV2,DV,updateEffect(NULL),design,evidence,metaAnalysis,explore,exploreResult$nullresult,ns,doingNull=TRUE)
       exploreResult$nullresult$count<<-nrow(exploreResult$nullresult$rIVs)
       }
       if (exploreResult$nullresult$count<exploreResult$nsims) {
         stopRunning<-FALSE
       }
     } else{
       exploreResult$nullresult<<-NULL
     }
     exploreResult$Explore_type<<-explore$Explore_type
     exploreResult$Explore_show<<-explore$Explore_show
     exploreResult$Explore_typeShow<<-explore$Explore_typeShow
     exploreResult$Explore_family<<-explore$Explore_family
     exploreResult$evidence<<-evidence
     
     switch (explore$Explore_family,
             "Hypothesis"={exploreResultHold$Hypothesis<<-exploreResult},
             "Design"={exploreResultHold$Design<<-exploreResult},
             "MetaAnalysis"={exploreResultHold$MetaAnalysis<<-exploreResult}
             )
     
     
     g<-ggplot()+plotBlankTheme+theme(plot.margin=margin(0,-0.2,0,0,"cm"))
     g<-g+scale_x_continuous(limits = c(0,10),labels=NULL,breaks=NULL)+scale_y_continuous(limits = c(0,10),labels=NULL,breaks=NULL)
     g1<-drawExplore(IV,IV2,DV,effect,design,explore,exploreResult)
     
     g<-g+annotation_custom(grob=ggplotGrob(g1+gridTheme),xmin=0.5,xmax=9.5,ymin=0.5,ymax=9.5)
     
     time2<<-Sys.time()
     if (!stopRunning) {
       if (doStop) {
         invalidateLater(mean(as.numeric(silentTime)*1000)+pauseWait)
       } else {
         invalidateLater(1)
       }
     } else {
       updateActionButton(session,"exploreRunH",label="Run")
       updateActionButton(session,"exploreRunD",label="Run")
       updateActionButton(session,"exploreRunM",label="Run")
       updateActionButton(session,"LGexploreRunH",label="Run")
       updateActionButton(session,"LGexploreRunD",label="Run")
       updateActionButton(session,"LGexploreRunM",label="Run")
       notRunningExplore<<-TRUE
     }
     
     return(g)
   }

    output$ExplorePlot <- renderPlot( {
      doIt<-c(input$exploreRunH,input$exploreRunD,input$exploreRunM)
      makeExploreGraph()
    })
    
    
    # report explore analysis        
    output$ExploreReport <- renderPlot({
      doIt<-c(input$exploreRunH,input$exploreRunD,input$exploreRunM,
              input$STMethod,input$alpha)
      
      if (!is.null(exploreResult$Explore_family) && exploreResult$Explore_family!=input$ExploreTab) {
        if (is.null(exploreResultHold[[input$ExploreTab]])) {
          return(ggplot()+plotBlankTheme)
        }
        exploreResult<<-exploreResultHold[[input$ExploreTab]]
      }
      
      if (exploreResult$result$count<exploreResult$nsims) {
        if (doStop) {
          invalidateLater(as.numeric(mean(silentTime)*1000)+pauseWait)
        } else {
          invalidateLater(1)
        }
      } 
      
      IV<-updateIV()
      IV2<-updateIV2()
      DV<-updateDV()

      effect<-updateEffect()
      design<-updateDesign()
      evidence<-updateEvidence()
      
      explore<-updateExplore()
      
      if (!validExplore || is.null(IV) || is.null(DV)) {return(ggplot()+plotBlankTheme)}
      
      reportExplore(IV,IV2,DV,effect,design,explore,exploreResult)
    })

##################################################################################    
# LIKELIHOOD
    # UI changes
    # set ui variable from UI
    # calculations
    # outputs
#    

    likelihoodResult<-list(samples=c(),populations=c())
# UI changes
    # if the tabs are selected
    likelihoodUpdateTabs<-observeEvent(input$Likelihood,{
      if (input$Likelihood=="Samples" || input$Likelihood=="Populations")
      {
        updateTabsetPanel(session, "Graphs",
                          selected = "Possible")
        updateTabsetPanel(session, "Reports",
                          selected = "Possible")
        showPossible<<-input$Likelihood
      }
    },priority=100)
    # if the tabs are selected
    likelihoodUpdateTabs<-observeEvent(input$LGshowPossible,{
      if (input$LGshowPossible=="Samples" || input$LGshowPossible=="Populations")
      {
        showPossible<<-input$LGshowPossible
      }
    },priority=100)
    
    # update samples from populations and vice versa
    # we check to make sure that we are only copying from the current tab to the hidden one
    observeEvent(c(input$likelihoodPSampRho),
                 {
                   if (input$Likelihood=="Populations") {
                     updateNumericInput(session,"likelihoodSampRho",value=input$likelihoodPSampRho)
                   }
                 })
    observeEvent(c(input$likelihoodSampRho),
                 {
                   if (input$Likelihood=="Samples") {
                     updateNumericInput(session,"likelihoodPSampRho",value=input$likelihoodSampRho)
                   }
                 })
    
    
        
# set likelihood variable from UI 
    updateLikelihood<-function(){
      IV<-updateIV()
      DV<-updateDV()
      effect<-updateEffect()
      
      # if (graphicSource=="Modal") {
      #           switch (showPossible,
      #                   "Populations"={
      #                     likelihood<-
      #                       list(type=showPossible,
      #                            Use=input$LGlikelihoodUsePrior,
      #                            prior=list(populationPDF=input$LGlikelihoodPrior_distr,populationRZ=input$LGlikelihoodPrior_distr_rz, 
      #                                       populationPDFk=input$LGlikelihoodPrior_distr_k,
      #                                       populationNullp=input$LGlikelihoodPrior_Nullp
      #                            ),
      #                            world=list(populationPDF=input$LGlikelihoodworld_distr,populationRZ=input$LGlikelihoodworld_distr_rz, 
      #                                       populationPDFk=input$LGlikelihoodworld_distr_k,
      #                                       populationNullp=input$LGlikelihoodworld_distr_Nullp
      #                            ),
      #                            design=list(sampleN=input$LGlikelihoodsN,sampleNRand=input$LGlikelihoodsNRand,sampleNRandK=input$LGlikelihoodsNRandK),
      #                          targetSample=input$LGlikelihoodPSampRho,targetPopulation=effect$rIV,
      #                          ResultHistory=ResultHistory,
      #                          likelihoodTheory=input$LGlikelihoodTheory,likelihoodLongHand=input$LGlikelihoodLongHand,likelihoodSimSlice=input$LGlikelihoodSimSlice,likelihoodCorrection=input$LGlikelihoodCorrection,
      #                          appendSim=input$LGlikelihoodP_append,Likelihood_length=as.numeric(input$LGlikelihoodP_length),
      #                          view=input$LGlikelihoodView,azimuth=input$LGlikelihoodAzimuth,elevation=input$LGlikelihoodElevation,range=input$LGlikelihoodRange,
      #                          textResult=TRUE
      #                     )
      #                   },
      #                   "Samples"={
      #                     likelihood<-
      #                       list(type=showPossible,
      #                            Use=input$LGlikelihoodUsePrior,
      #                            prior=list(populationPDF=input$LGlikelihoodPrior_distr,populationRZ=input$LGlikelihoodPrior_distr_rz, 
      #                                       populationPDFk=input$LGlikelihoodPrior_distr_k,
      #                                       populationNullp=input$LGlikelihoodPrior_Nullp
      #                            ),
      #                            world=list(populationPDF=input$LGlikelihoodworld_distr,populationRZ=input$LGlikelihoodworld_distr_rz, 
      #                                       populationPDFk=input$LGlikelihoodworld_distr_k,
      #                                       populationNullp=input$LGlikelihoodworld_distr_Nullp
      #                            ),
      #                            design=list(sampleN=input$LGlikelihoodsN,sampleNRand=input$LGlikelihoodsNRand,sampleNRandK=input$LGlikelihoodsNRandK),
      #                            targetSample=input$LGlikelihoodSampRho,cutaway=input$LGlikelihood_cutaway,targetPopulation=input$LGlikelihoodPrior_distr_k,
      #                            ResultHistory=ResultHistory,
      #                            likelihoodTheory=input$LGlikelihoodTheory,likelihoodLongHand=input$LGlikelihoodLongHand,likelihoodSimSlice=input$LGlikelihoodSimSlice,likelihoodCorrection=input$LGlikelihoodCorrection,
      #                          appendSim=input$LGlikelihood_append,Likelihood_length=as.numeric(input$LGlikelihood_length),
      #                          view=input$LGlikelihoodView,azimuth=input$LGlikelihoodAzimuth,elevation=input$LGlikelihoodElevation,range=input$LGlikelihoodRange,
      #                          textResult=TRUE
      #                     )
      #                   }
      #           )
      #   
      #         } else {
                switch (showPossible,
                        "Populations"={
                          likelihood<-
                            list(type=showPossible,
                                 UsePrior=input$likelihoodUsePrior,
                                 UseSource=input$likelihoodUseSource,
                                 prior=list(populationPDF=input$likelihoodPrior_distr,populationRZ=input$likelihoodPrior_distr_rz, 
                                            populationPDFk=input$likelihoodPrior_distr_k,
                                            populationNullp=input$likelihoodPrior_Nullp
                                            ),
                                 world=list(worldOn=input$world_on,populationPDF=input$world_distr,populationRZ=input$world_distr_rz, 
                                            populationPDFk=input$world_distr_k,
                                            populationNullp=input$world_distr_Nullp
                                            ),
                                 design=list(sampleN=input$sN,sampleNRand=input$sNRand,sampleNRandK=input$sNRandK),
                               targetSample=input$likelihoodPSampRho,targetPopulation=effect$rIV,
                               ResultHistory=ResultHistory,
                               likelihoodTheory=input$likelihoodTheory,likelihoodLongHand=input$likelihoodLongHand,
                               likelihoodSimSlice=input$likelihoodSimSlice,likelihoodCorrection=input$likelihoodCorrection,
                               appendSim=input$likelihoodP_append,Likelihood_length=as.numeric(input$likelihoodP_length),
                               view=input$LikelihoodView,viewRZ=input$likelihoodViewRZ,azimuth=input$LikelihoodAzimuth,elevation=input$LikelihoodElevation,range=input$LikelihoodRange,
                               textResult=FALSE
                          )
                        },
                        "Samples"={
                          likelihood<-
                            list(type=showPossible,
                                 UsePrior=input$likelihoodUsePrior,
                                 UseSource=input$likelihoodUseSource,
                                 prior=list(populationPDF=input$likelihoodPrior_distr,populationRZ=input$likelihoodPrior_distr_rz, populationPDFk=input$likelihoodPrior_distr_k,
                                            populationNullp=input$likelihoodPrior_Nullp),
                                 world=list(worldOn=input$world_on,populationPDF=input$world_distr,populationRZ=input$world_distr_rz, populationPDFk=input$world_distr_k,
                                            populationNullp=input$world_distr_Nullp),
                                 design=list(sampleN=input$sN,sampleNRand=input$sNRand,sampleNRandK=input$sNRandK),
                                 targetSample=input$likelihoodSampRho,targetPopulation=effect$world$populationPDFk,
                                 cutaway=input$likelihood_cutaway,
                                 ResultHistory=ResultHistory,
                                 likelihoodTheory=input$likelihoodTheory,likelihoodLongHand=input$likelihoodLongHand,likelihoodSimSlice=input$likelihoodSimSlice,likelihoodCorrection=input$likelihoodCorrection,
                               appendSim=input$likelihood_append,Likelihood_length=as.numeric(input$likelihood_length),
                               view=input$LikelihoodView,viewRZ=input$likelihoodViewRZ,azimuth=input$LikelihoodAzimuth,elevation=input$LikelihoodElevation,range=input$LikelihoodRange,
                               textResult=FALSE
                          )
                        }
                )
              # }
      if (input$world_on==FALSE) {
        likelihood$world$populationPDF<-"Single"
        likelihood$world$populationRZ<-"r"
        likelihood$world$populationPDFk<-effect$rIV
        likelihood$world$populationNullp<-0
      }
      if (is.null(oldLikelihood)) {
        likelihood$world$populationPDFk<-checkNumber(likelihood$world$populationPDFk)
        likelihood$prior$populationPDFk<-checkNumber(likelihood$prior$populationPDFk)
        likelihood$world$populationNullp<-checkNumber(likelihood$world$populationNullp)
        likelihood$prior$populationNullp<-checkNumber(likelihood$prior$populationNullp)
        
        likelihood$design$sampleNRandK<-checkNumber(likelihood$design$sampleNRandK)
      } else {
        likelihood$world$populationPDFk<-checkNumber(likelihood$world$populationPDFk,oldLikelihood$world$populationPDFk)
        likelihood$prior$populationPDFk<-checkNumber(likelihood$prior$populationPDFk,oldLikelihood$prior$populationPDFk)
        likelihood$world$populationNullp<-checkNumber(likelihood$world$populationNullp,oldLikelihood$world$populationNullp)
        likelihood$prior$populationNullp<-checkNumber(likelihood$prior$populationNullp,oldLikelihood$prior$populationNullp)
        
        likelihood$design$sampleNRandK<-checkNumber(oldLikelihood$design$sampleNRandK)
      }
      oldLikelihood<<-likelihood
      likelihood
    }
    
# main likelihood calcuations    
    likelihoodReset<-observeEvent(c(input$likelihoodPrior_Nullp,
                                        input$likelihoodPrior_distr,input$likelihoodPrior_distr_rz,input$likelihoodPrior_distr_k,
                                        input$likelihoodUsePrior,
                                        input$sN,
                                        input$likelihoodLongHand,
                                        input$LGlikelihoodPrior_Nullp,
                                        input$LGlikelihoodPrior_distr,input$LGlikelihoodPrior_distr_rz,input$LGlikelihoodPrior_distr_k,input$LGlikelihoodUsePrior,
                                        input$LGlikelihoodsN,
                                        input$LGlikelihoodLongHand
    ),{
      likelihood_P_ResultHold<<-c()
      likelihood_S_ResultHold<<-c()
  })
      
    likelihoodAnalysis<-eventReactive(c(input$Likelihood,
                                        input$likelihood_run,input$likelihoodP_run,
                                        input$likelihoodPSampRho,
                                        input$likelihoodUsePrior,input$likelihoodUseSource,
                                        input$likelihoodPrior_distr,input$likelihoodPrior_distr_rz,input$likelihoodPrior_distr_k,input$likelihoodPrior_Nullp,
                                        input$rIV,
                                        input$world_on,input$world_distr,input$world_distr_rz,input$world_distr_k,input$world_distr_Nullp,
                                        input$sN,input$sNRand,input$sNRandK,
                                        input$EvidencenewSample,
                                        input$likelihoodTheory,
                                        input$likelihoodLongHand,input$likelihoodSimSlice,input$likelihoodCorrection,
                                        input$likelihoodViewRZ,
                                        input$LGshowPossible,
                                        input$LGlikelihood_run,input$LGlikelihoodP_run,
                                        input$LGlikelihoodSampRho,input$LGlikelihoodPSampRho,
                                        input$LGlikelihoodUsePrior,input$LGlikelihoodUseSource,
                                        input$LGlikelihoodPrior_distr,input$LGlikelihoodPrior_distr_rz,input$LGlikelihoodPrior_distr_k,input$LGlikelihoodPrior_Nullp,
                                        input$LGlikelihoodworld_on,input$LGlikelihoodworld_distr,input$LGlikelihoodworld_distr_rz,input$LGlikelihoodworld_distr_k,input$LGlikelihoodworld_distr_Nullp,
                                        input$LGlikelihoodsN,input$LGlikelihoodsNRand,input$LGlikelihoodsNRandK,
                                        input$LGlikelihoodTheory,
                                        input$LGlikelihoodLongHand,input$LGlikelihoodSimSlice,input$LGlikelihoodCorrection
                                        ),{
        if (graphicSource=="None") {return(likelihoodResult)}
                                          
        req(input$changed)
        
        if (is.element(input$changed,c("Likelihood","likelihood_run","likelihoodP_run","likelihoodPSampRho",
                                       "world_on","world_distr","world_distr_rz","world_distr_k","world_distr_Nullp",
                                       "Prior_distr","Prior_distr_rz","Prior_distr_k","Prior_Nullp",
                                       "likelihoodTheory","LGlikelihoodSimSlice","LGlikelihoodCorrection",
                                       "sN","sNRand","sNRandK")))
        {
          graphicSource<<-"Main"
         showPossible<-input$Likelihood
        }
        if (is.element(input$changed,c("LGshowPossible",
                                       "LGlikelihood_run","LGlikelihood_run","LGlikelihoodSampRho",
                                       "LGlikelihoodworld_on","LGlikelihoodworld_distr","LGlikelihoodworld_distr_rz","LGlikelihoodworld_distr_k","LGlikelihoodworld_distr_Nullp",
                                       "LGlikelihoodPrior_distr","LGlikelihoodPrior_distr_rz","LGlikelihoodPrior_distr_k","LGlikelihoodPrior_Nullp",
                                       "LGlikelihoodTheory","LGlikelihoodSimSlice","LGlikelihoodCorrection",
                                       "LGlikelihoodsN","LGlikelihoodsNRand","LGlikelihoodsNRandK")))
        {
          graphicSource<<-"Modal"
          showPossible<-input$LGshowPossible
        }
        IV<-updateIV()
        DV<-updateDV()

        effect<-updateEffect()
        design<-updateDesign()
        evidence<-updateEvidence()
        result<-sampleAnalysis()
        likelihood<-updateLikelihood()

        if ((input$likelihood_run+input$likelihoodP_run+input$LGlikelihood_run+input$LGlikelihoodP_run>validLikelihood)){
          showNotification(paste0("Possible ",likelihood$type," : starting"),id="counting",duration=Inf,closeButton=FALSE,type="message")
          validLikelihood<<-validLikelihood+1
          likelihoodRes<-likelihood_run(IV,DV,effect,design,evidence,likelihood,doSample = TRUE)
          removeNotification(id="counting")
          keepSamples<-FALSE
        } else {
          likelihoodRes<-likelihood_run(IV,DV,effect,design,evidence,likelihood,doSample = FALSE)
          keepSamples<-all(unlist(lapply(seq(3),function(i)likelihoodRes$likelihood$world[[i]]==likelihoodResult$samples$likelihood$world[[i]])))
          keepSamples<-keepSamples && all(unlist(lapply(seq(3),function(i)likelihoodRes$likelihood$design[[i]]==likelihoodResult$populations$likelihood$design[[i]])))
        }
        
        switch (showPossible,
                "Samples"={          
                  # if (keepSamples && !is.null(likelihoodResult$samples$Sims$sSims)) {
                    # likelihoodRes$Sims<-likelihoodResult$samples$Sims
                  # }
                  likelihoodResult$samples<<-likelihoodRes
                  likelihood_S_ResultHold<<-list(sSims=likelihoodResult$samples$Sims$sSims,sSimBins=likelihoodResult$samples$Sims$sSimBins,sSimDens=likelihoodResult$samples$Sims$sSimDens)
                },
                "Populations"={
                  # if (keepSamples && !is.null(likelihoodResult$populations$Sims$pSims)) {
                  #   likelihoodRes$Sims<-likelihoodResult$populations$Sims
                  # }
                  likelihoodResult$populations<<-likelihoodRes
                  # likelihood_P_ResultHold<<-list(pSims=likelihoodResult$populations$Sims$pSimsP,sSims=likelihoodResult$populations$Sims$pSims)
                }
        )
        likelihoodResult
    }
    )
    
    makeLikelihoodGraph<-function(){
      if (!is.element(showPossible,c("Samples","Populations"))) {return(ggplot()+plotBlankTheme)}
      IV<-updateIV()
      DV<-updateDV()
      if (is.null(IV) || is.null(DV)) {return(ggplot()+plotBlankTheme)}
      
      effect<-updateEffect()
      design<-updateDesign()
      
      # this guarantees that we update without recalculating if possible
      likelihood<-updateLikelihood()
      doIt<-c(ResultHistory)
      
      likelihoodResult<-likelihoodAnalysis()

      drawLikelihood(IV,DV,effect,design,likelihood,likelihoodResult)
    }

# likelihood outputs    
    # show likelihood analysis        
    output$LikelihoodPlot <- renderPlot( {
      LKtype<-c(input$Likelihood,input$EvidencenewSample)
      par(cex=1.2)
      makeLikelihoodGraph()
    })
    
    # report likelihood analysis        
    output$LikelihoodReport <- renderPlot({
      if (!is.element(showPossible,c("Samples","Populations"))) {return(ggplot()+plotBlankTheme)}
      IV<-updateIV()
      DV<-updateDV()
      if (is.null(IV) || is.null(DV)) {return(ggplot()+plotBlankTheme)}
      
      effect<-updateEffect()
      design<-updateDesign()
      
      likelihood<-updateLikelihood()
      likelihoodResult<-likelihoodAnalysis()

      reportLikelihood(Iv,DV,effect,design,likelihood,likelihoodResult)
      
    })
    
    
##################################################################################    
#  IMPORT/EXPORT data and workspace
    
    getNewVariables<-function(raw_data,header=c()){
      keep<-!apply(is.na(raw_data),2,all)
      raw_data<-raw_data[,keep]
      
      keep<-!apply(is.na(raw_data),1,all)
      raw_data<-raw_data[keep,]
      
      newVariables<-readSample(raw_data,input$ImportOrdinals,input$MaxOrdinal,header)

      # save the current set of variables
      defaultVariables<<-variables
      
      # store the variables in global workspace
      if (mergeVariables){
        variables<<-rbind(newVariables,variables)
      } else{
        variables<<-newVariables
        simData<<-FALSE
      }
      changeUI2Data()
    }    

    # respond to file selection by getting sheet names
    inspectDataFile<-observeEvent(input$dataInputFile, {
      sheet_names<-excel_sheets(input$dataInputFile$datapath)
      updateSelectInput(session, "dataInputSheet", choices = sheet_names)
      # get the raw data
      if (length(sheet_names)==1){
        mergeVariables<<-FALSE
        raw_data<-read_excel(input$dataInputFile$datapath,sheet = sheet_names[1])
        if (nrow(raw_data)>0 && ncol(raw_data)>0)
          getNewVariables(raw_data)
      }
    })
    
    # data input    
    importDataFile<-observeEvent(input$dataInputFileLoad, {
      mergeVariables<<-FALSE
      # get the raw data
      if (is.character(input$dataInputFile$datapath)) {
      raw_data<-read_excel(input$dataInputFile$datapath,sheet = input$dataInputSheet)
      if (nrow(raw_data)>0 && ncol(raw_data)>0)
        getNewVariables(raw_data)
      }
    })
    
    readCLipDataFile<-observeEvent(input$dPaste, {
      mergeVariables<<-FALSE
      # get the raw data
      raw_h1<-read_clip()
      header<-strsplit(raw_h1[1],"\t")[[1]]
      raw_data<-read_clip_tbl()
      # read_clip_tbl doesn't like some characters like | and =
      colnames(raw_data)<-header
      if (nrow(raw_data)>0 && ncol(raw_data)>0)
      getNewVariables(raw_data)
    })
    
    exportData<-function() {
      IV<-updateIV()
      IV2<-updateIV2()
      DV<-updateDV()
      
      result<-sampleAnalysis()
      if (is.null(result)) return(NULL)
      
      id<-result$participant
      iv<-result$iv
      dv<-result$dv
      if (input$ExportShortForm && (IV$deploy=="Within" && (!is.null(IV2) && IV2$deploy=="Within"))) {
        iv2<-result$iv2
        id.d<-unique(id)
        data<-data.frame(Participant=id.d)
        
        for (j1 in 1:IV$ncats) {
          for (j2 in (1:IV2$ncats)) {
            iv.j<-c()
            for (i in 1:length(id.d)) {
              use<-(id==id.d[i]) & (iv==IV$cases[j1]) & (iv2==IV2$cases[j2])
              iv.j<-c(iv.j,mean(dv[use],na.rm=TRUE))
            }
            data[[paste0(DV$name,"|",IV$name,"=",IV$cases[j1],";",IV2$name,"=",IV2$cases[j2])]]<-iv.j
          }
        }
      } else {
      if (input$ExportShortForm && (IV$deploy=="Within" || (!is.null(IV2) && IV2$deploy=="Within"))) {
        id.d<-unique(id)
        data<-data.frame(Participant=id.d)
        
        if (IV$deploy=="Between") {
          use.d<-c()
          for (i in 1:length(id.d)) {
            use<-which(id==id.d[i])
            use.d<-c(use.d,use[1])
          }
          data[IV$name]<-iv[use.d]
        } else {
          for (j in 1:IV$ncats) {
            iv.j<-c()
            for (i in 1:length(id.d)) {
              use<-(id==id.d[i]) & (iv==IV$cases[j])
              iv.j<-c(iv.j,mean(dv[use],na.rm=TRUE))
            }
            data[[paste0(DV$name,"|",IV$name,"=",IV$cases[j])]]<-iv.j
          }
        }

        if (!is.null(IV2)) {
          iv2<-result$iv2
          if (IV2$deploy=="Between") {
            use.d<-c()
            for (i in 1:length(id.d)) {
              use<-which(id==id.d[i])
              use.d<-c(use.d,use[1])
            }
            data[IV2$name]<-iv2[use.d]
          } else {
            for (j in 1:IV2$ncats) {
              iv2.j<-c()
              for (i in 1:length(id.d)) {
                use<-(id==id.d[i]) & (iv2==IV2$cases[j])
                iv2.j<-c(iv2.j,mean(dv[use],na.rm=TRUE))
              }
              data[[paste0(DV$name,"|",IV2$name,"=",IV2$cases[j])]]<-iv2.j
            }
          }
        }
      } else {
        if (is.null(IV2)){
          data<-data.frame(participant=id,iv=iv,dv=dv)
          colnames(data)<-c("Participant",IV$name,DV$name)
        } else {
          iv2<-result$iv2
          data<-data.frame(participant=id,iv=iv,iv=iv2,dv=dv)
          colnames(data)<-c("Participant",IV$name,IV2$name,DV$name)
        }
      }
      }
      data      
    }
    
    exportDataClip<-observeEvent(input$dCopy, {
      data<-exportData()      
      if (!is.null(data)) write_clip(data,allow_non_interactive = TRUE,col.names=input$ExportHeader)
    })
    
    exportDataFile<-observeEvent(input$dataOutputFileSave, {
      data<-exportData()      
      if (!is.null(data)) 
        {filename<-input$DataoutputFile
        ext<-file_ext(filename)
        if (ext!="xlsx" && ext!="xls") {filename=paste(filename,".xlsx",sep="")}
        
        write_xlsx(data, path = filename)
      }
    })
    
    addList<-function(L,name) {
      addFields<-names(L)
      header<-name
      fields<-""
      vals<-""
      for (i in 1:length(addFields)){
        v<-L[[addFields[i]]]
        if (!is.null(v) && length(v)==1){
          if (is.logical(v)){
            if (v){v<-"TRUE"} else {v<-"FALSE"}
          }
          fields<-c(fields,addFields[i])
          vals<-c(vals,v)
          header<-c(header,name)
        }
      }
      data.frame(header=header,field=fields,value=vals)
    }
    
    exportWSFile<-observeEvent(input$WSOutputFileSave, {
      IV<-updateIV()
      IV2<-updateIV2()
      DV<-updateDV()
      
      effect<-updateEffect()
      design<-updateDesign()
      
      data1<-addList(IV,"IV")
      data2<-addList(IV2,"IV2")
      data3<-addList(DV,"DV")
      data4<-addList(effect,"effect")
      data5<-addList(design,"design")
      data<-rbind(data1,data2,data3,data4,data5)
      
      filename<-input$wsOutputFile
      ext<-file_ext(filename)
      if (ext!="xlsx" && ext!="xls") {filename=paste(filename,".xlsx",sep="")}
      
      write_xlsx(data, path = filename)
    })
    
    exportWSCLipboard<-observeEvent(input$wsCopy, {
      IV<-updateIV()
      IV2<-updateIV2()
      DV<-updateDV()
      
      effect<-updateEffect()
      design<-updateDesign()
      
      data1<-addList(IV,"IV")
      data2<-addList(IV2,"IV2")
      data3<-addList(DV,"DV")
      data4<-addList(effect,"effect")
      data5<-addList(design,"design")
      data<-rbind(data1,data2,data3,data4,data5)
      
      if (!is.null(data)) write_clip(data,allow_non_interactive = TRUE)
      
    })
    
    
    # respond to file selection by getting sheet names
    inspectWSFile<-observeEvent(input$wsInputFile, {
      sheet_names<-excel_sheets(input$wsInputFile$datapath)
      updateSelectInput(session, "wsInputSheet", choices = sheet_names, selected=sheet_names[1])
      
      if (length(sheet_names)==1){
      readWS(session,input$wsInputFile$datapath,sheet_names[1])
      }
    })
    
    importWSFile<-observeEvent(input$wsInputFileLoad, {
      if (is.character(input$dataInputFile$datapath)) {
              readWS(session,input$wsInputFile$datapath,input$wsInputSheet)
      editVar$data<<-editVar$data+1
      }
    })
    
    importWSClip<-observeEvent(input$wsPaste, {
      readWS(session,"clip")
      editVar$data<<-editVar$data+1
    })
    
    batchfiles<-observeEvent(input$batchFileRun,{
      IV<-updateIV()
      IV2<-updateIV2()
      DV<-updateDV()
      
      effect<-updateEffect()
      design<-updateDesign()
      evidence<-updateEvidence()
      runBatchFiles(IV,IV2,DV,effect,design,evidence,input)
      
    })
    # end of everything        
})

