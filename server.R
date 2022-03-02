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
source("drawExplore.R")
source("drawLikelihood.R")

source("sampleMake.R")
source("sampleAnalyse.R")
source("samplePower.R")
source("sampleRead.R")
source("Johnson_M.R")

source("reportSample.R")
source("reportDescription.R")
source("reportInference.R")
source("reportExpected.R")
source("reportExplore.R")
source("reportLikelihood.R")

source("runExplore.R")
source("runLikelihood.R")

source("wsRead.R")
source("typeCombinations.R")

source("drawInspect.R")

simCycles=20
simPeriod=1000

shinyServer(function(input, output, session) {

  source("myGlobal.R")
  source("testDebug.R")
  
####################################
# BASIC SET UP that cannot be done inside ui.R  
  shinyjs::hideElement(id= "hypothesisApply")
  shinyjs::hideElement(id= "Using")
  updateSelectInput(session, "IVchoice", choices = variables$name, selected = variables$name[1])
  updateSelectInput(session, "IV2choice", choices = c("none",variables$name), selected = "none")
  updateSelectInput(session, "DVchoice", choices = variables$name, selected = variables$name[3])

  if (!switches$showLikelihood) {
    shinyjs::hideElement(id= "MainLikelihood")
    hideTab("Graphs", "Possible", session)
    hideTab("Reports", "Possible", session)
  }
####################################
#KEYBOARD: capture keyboard events

  # observeEvent(input$local, {print(input$local)})

  keyrespond<-observeEvent(input$pressedKey,{
     # print(input$keypress)
    
    if (input$keypress==16) shiftKeyOn<<-TRUE
    if (input$keypress==17) controlKeyOn<<-TRUE
    if (input$keypress==18) altKeyOn<<-TRUE
    
    # control-V
    if (input$keypress==86 && controlKeyOn){
      mergeVariables<<-FALSE
      # get the raw data
      raw_h1<-read_clip()
      header<-strsplit(raw_h1[1],"\t")[[1]]
      raw_data<-read_clip_tbl()
      # read_clip_tbl doesn't like some characters like | and =
      colnames(raw_data)<-header
      if (nrow(raw_data)>0 && ncol(raw_data)>0)
        getNewVariables(raw_data)
    } 
    
    # control-c
    if (input$keypress==67 && controlKeyOn){
      data<-exportData()      
      write_clip(data)
    }
    
    if (quickHypos) {
      # control-alt-n set sample size to big (1000)
      if (input$keypress==78 && controlKeyOn && altKeyOn){
        updateNumericInput(session,"sN",value=1000)    
      }
      
      # control-alt-d do debug
    if (input$keypress==68 && controlKeyOn && altKeyOn){
      toggleModal(session, modalId = "testedOutput", toggle = "open")
      IV<-updateIV()
      IV2<-updateIV2()
      DV<-updateDV()
      
      effect<-updatePrediction()
      design<-updateDesign()
      evidence<-updateEvidence()
      expected<-updateExpected()
      
      validSample<<-TRUE
      
      if (is.null(IV2)) {
        nc=7
        effect$rIV=0.3
      } else {
        nc=12
        effect$rIV=0.3
        effect$rIV2=-0.3
        effect$rIVIV2DV=0.5
      }
      design$sN<-1000
      
      expected$nSims<-100
      expected$Expected_type<-"EffectSize"
      expected$append<-FALSE
      
      if (is.null(IV2)) {
        result<-doSampleAnalysis(IV,IV2,DV,effect,design,evidence)
      } 
      doExpectedAnalysis(IV,IV2,DV,effect,design,evidence,expected)
      op<-testDebug(IV,IV2,DV,effect,design,evidence,expected,result,expectedResult)
      
      if (!is.null(IV2)) {
        effect$rIVIV2=0.25
        doExpectedAnalysis(IV,IV2,DV,effect,design,evidence,expected)
        op<-c(op,testDebug(IV,IV2,DV,effect,design,evidence,expected,result,expectedResult))
        
        effect$rIVIV2=-0.25
        doExpectedAnalysis(IV,IV2,DV,effect,design,evidence,expected)
        op<-c(op,testDebug(IV,IV2,DV,effect,design,evidence,expected,result,expectedResult))
      }
      
      output$plotPopUp<-renderPlot(reportPlot(op,nc,length(op)/nc,2))
      return()
    }
      
    } # end of quick hypos
  })
  
  keyrespondUp<-observeEvent(input$releasedKey,{
    if (input$keypress==18) altKeyOn<<-FALSE
    if (input$keypress==17) controlKeyOn<<-FALSE
    if (input$keypress==16) shiftKeyOn<<-FALSE
  })

  
  
####################################
# other housekeeping
  observeEvent(input$allScatter,{
    allScatter<<-input$allScatter
  }
  )

  observeEvent(input$sig_ns,{
    useSignificanceCols<<-input$sig_ns
  }
  )
  
  observeEvent(input$Explore_VtypeH, {
      if (input$Explore_VtypeH=="levels") {
        updateSelectInput(session,"Explore_typeH",selected="DV")
      }
  }
  )
  
####################################
# generic warning dialogue
  
  hmm<-function (cause) {
    showModal(
      modalDialog(style = paste("background: ",subpanelcolours$hypothesisC,";",
                                "modal {background-color: ",subpanelcolours$hypothesisC,";}"),
                  title="Alert!",
                  size="s",
                  cause,
                  
                  footer = tagList( 
                    modalButton("Cancel"),
                    actionButton("MVproceed", "OK")
                  )
      )
    )
  }
  
  observeEvent(input$MVproceed, {
    removeModal()
  })
  
####################################
#  Help Tab
  
  observeEvent(input$Help,
               { 
                 if (fullShowHelp)
                 switch(input$Help,
                        "Help:"={
                          updateTabsetPanel(session, "Hypothesis",selected = "Hypothesis")
                          updateTabsetPanel(session, "Design",selected = "Design")
                          updateTabsetPanel(session, "Evidence",selected = "Evidence")
                          updateTabsetPanel(session, "ExploreTab",selected = "Explore")
                          updateTabsetPanel(session, "FileTab", selected="Files")
                        },
                        "Step 1"={
                          updateTabsetPanel(session, "Hypothesis",selected = "?")
                          updateTabsetPanel(session, "Design",selected = "Design")
                          updateTabsetPanel(session, "Evidence",selected = "Evidence")
                          updateTabsetPanel(session, "ExploreTab",selected = "Explore")
                        },
                        "Step 2"={
                          updateTabsetPanel(session, "Design",selected = "?")
                          updateTabsetPanel(session, "Hypothesis",selected = "Hypothesis")
                          updateTabsetPanel(session, "Evidence",selected = "Evidence")
                          updateTabsetPanel(session, "ExploreTab",selected = "Explore")
                        },
                        "Step 3"={
                          updateTabsetPanel(session, "Evidence",selected = "?")
                          updateTabsetPanel(session, "Hypothesis",selected = "Hypothesis")
                          updateTabsetPanel(session, "Design",selected = "Design")
                          updateTabsetPanel(session, "ExploreTab",selected = "Explore")
                        },
                        "Step 4"={
                          updateTabsetPanel(session, "ExploreTab",selected = "?")
                          updateTabsetPanel(session, "Hypothesis",selected = "Hypothesis")
                          updateTabsetPanel(session, "Design",selected = "Design")
                          updateTabsetPanel(session, "Evidence",selected = "Evidence")
                        },
                        "Data"={
                          print(input$Help)
                          
                          updateTabsetPanel(session, "FileTab", selected="?")
                          updateTabsetPanel(session, "ExploreTab",selected = "ExploreTab")
                          updateTabsetPanel(session, "Hypothesis",selected = "Hypothesis")
                          updateTabsetPanel(session, "Design",selected = "Design")
                          updateTabsetPanel(session, "Evidence",selected = "Evidence")
                        }
                 )
                 })

####################################
# QUICK HYPOTHESES
  
  observeEvent(input$Hypchoice,{
    
    if (grepl("rjwatt42",session$clientData$url_hostname))
    {
      switches$startBlank<<-TRUE
      variables[1,]$type<<-"empty"
      variables[3,]$type<<-"empty"
    } else {
      switches$startBlank<<-FALSE
    }
    
    result<-getTypecombination(input$Hypchoice)
    
    setIVanyway(result$IV)
    setIV2anyway(result$IV2)
    setDVanyway(result$DV)
    
    # 3 variable hypotheses look after themselves
    #
    if (!is.null(IV2)) {
    editVar$data<<-editVar$data+1
    }    
  })
  
  observeEvent(input$Effectchoice,{
    
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
# VARIABLES  
  # make basic variables    
  IV<-variables[1,]
  IV2<-variables[2,]
  DV<-variables[3,]
  MV<-IV
  
  observeEvent(input$Using,{
    if (variablesHeld==input$Using) {return()}
    
    local<-variables
    variables<<-defaultVariables
    defaultVariables<<-local
    
    switch(input$Using,
           "Simulations"={
             shinyjs::enable(id= "rIV")
             shinyjs::enable(id= "rIV2")
             shinyjs::enable(id= "rIVIV2")
             shinyjs::enable(id= "rIVIV2DV")
             
             shinyjs::enable(id= "sN")
             shinyjs::enable(id= "sMethod")
             shinyjs::enable(id= "sIV1Use")
             shinyjs::enable(id= "sIV2Use")
             updateActionButton(session,"newSample", label="New Sample")
             shinyjs::hideElement(id= "hypothesisApply")
             updateNumericInput(session,"rIV",value=0)
             updateSelectInput(session, "IVchoice", choices = variables$name, selected = variables$name[1])
             updateSelectInput(session, "IV2choice", choices = c("none",variables$name), selected = "none")
             updateSelectInput(session, "DVchoice", choices = variables$name, selected = variables$name[3])
             variablesHeld<<-"Simulations"
             runjs(sprintf("document.getElementById('Variables').style.backgroundColor = '%s';",subpanelcolours$hypothesisC))
           },
           "Data"={
             shinyjs::disable(id= "rIV")
             shinyjs::disable(id= "rIV2")
             shinyjs::disable(id= "rIVIV2")
             shinyjs::disable(id= "rIVIV2DV")
             
             shinyjs::disable(id= "sN")
             shinyjs::disable(id= "sMethod")
             shinyjs::disable(id= "sIV1Use")
             shinyjs::disable(id= "sIV2Use")
             updateActionButton(session,"newSample", label="Analyze")
             shinyjs::showElement(id= "hypothesisApply")
             updateTabsetPanel(session, "Hypothesis",selected = "Variables")
             updateNumericInput(session,"rIV",value=NA)
             updateSelectInput(session, "IVchoice", choices = variables$name, selected = variables$name[1])
             updateSelectInput(session, "IV2choice", choices = c("none",variables$name), selected = "none")
             updateSelectInput(session, "DVchoice", choices = variables$name, selected = variables$name[nrow(variables)])
             variablesHeld<<-"Data"
             runjs(sprintf("document.getElementById('Variables').style.backgroundColor = '%s';",subpanelcolours$filesC))
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
  
  # modalDialog to edit each variable
  # all of this code only gets used if the modalDialog mechanism is set up in ui.R
  # if we are using the older tabs mechanism, then this code never gets called
  source("uiVariable.R")
  modalVar<-c()
  editVar<-reactiveValues(data=0)
  oldName<-""
  
  observeEvent(input$MVnlevs, {
    updateNumericInput(session,"MVcentre",value=(input$MVnlevs+1)/2)
    updateNumericInput(session,"MVspread",value=(input$MVnlevs-1)/2)
  })
  #Press "OK": make the new variable
  observeEvent(input$MVok, {
    MV<<-makeVar(name=input$MVname, type=input$MVtype,
                 mu=input$MVmu, sd=input$MVsd,
                 skew=input$MVskew, kurtosis=input$MVkurt,
                 nlevs=input$MVnlevs,median=input$MVcentre,iqr=input$MVspread,discrete=input$MVdiscrete,
                 ncats=input$MVncats,cases=input$MVcases,proportions=input$MVprop,source=input$MVsource,
                 deploy=MV$deploy,process=MV$process)

    switch (modalVar,
            "IV" ={setIVanyway(MV)},
            "IV2"={setIV2anyway(MV)},
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
  observeEvent(c(input$editIV,input$editIV2,input$editDV),{
    req(input$changed)
    switch (input$changed,
            "editIV"={
              modalVar<<-"IV"
              IV<-updateIV()
              MV<<-IV
            },
            "editIV2"={
              modalVar<<-"IV2"
              IV2<-updateIV2()
              MV<<-IV2
            },
            "editDV"={   
              modalVar<<-"DV"
              DV<-updateDV()
              MV<<-DV
            }
            )
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
      # if we are editing imported variables, there is less we can change
      if (MV$process=="data") {
        shinyjs::hideElement(id= "MVIntVal")
        shinyjs::hideElement(id= "MVOrdVal")
        # shinyjs::hideElement(id= "MVCatVal")
      }
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
      
    updateSelectInput(session,"sIV1Use", selected=newMV$deploy)
    switch (newMV$type,
            "Interval"={
              shinyjs::disable(id= "sIV1Use")
            },
            "Categorical"={
              shinyjs::enable(id= "sIV1Use")
            }
    )
    }
    
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
  
  observeEvent(c(input$inspectIV,input$inspectDV),{
    req(input$changed)
    switch (input$changed,
            "inspectIV"={var<-updateIV()},
            "inspectDV"={var<-updateDV()}
    )
    inspectSource<<-input$changed
    inspectVar<<-var
    inspectData<<-c()
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
  )

  observeEvent(input$ResidVal, {
    inspectHistory<<-c(inspectHistory,input$ResidVal)

  }
  )
  
  observeEvent(input$inspectNewSample,{
    IV<-updateIV()
    DV<-updateDV()
    effect<-updatePrediction()
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
                }
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
        #             IV$proportions<-MV$prop
      }
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
      exploreResultHold<-c()
      likelihood_P_ResultHold<-c()
      likelihood_S_ResultHold<-c()
      
      updateCheckboxInput(session,"expectedAppend",value=FALSE)
      updateCheckboxInput(session,"exploreAppendH",value=FALSE)
      updateCheckboxInput(session,"exploreAppendD",value=FALSE)
      updateCheckboxInput(session,"exploreAppendA",value=FALSE)
      
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
    
# PREDICTION & DESIGN & EVIDENCE
    updatePrediction<-function(){
      if (debug) print("     updatePrediction")
      prediction<-list(rIV=input$rIV,rIV2=input$rIV2,rIVIV2=input$rIVIV2,rIVIV2DV=input$rIVIV2DV,
                       Heteroscedasticity=input$Heteroscedasticity,Welch=input$Welch,ResidDistr=input$ResidDistr)
      if (debug) print("     updatePrediction - exit")
      prediction
    }
    
    updateDesign<-function(){
      if (debug) print("     updateDesign")
      design<-list(sN=input$sN, sMethod=input$sMethod ,sIV1Use=input$sIV1Use,sIV2Use=input$sIV2Use, 
                   sRangeOn=input$sRangeOn, sIVRange=input$sIVRange, sDVRange=input$sDVRange, 
                   sDependence=input$sDependence, sOutliers=input$sOutliers, sClustering=input$sClustering,
                   sN_Strata=input$sN_Strata, sR_Strata=input$sR_Strata,
                   sNClu_Cluster=input$sNClu_Cluster, sRClu_Cluster=input$sRClu_Cluster,
                   sNClu_Convenience=input$sNClu_Convenience, sRClu_Convenience=input$sRClu_Convenience, sNCont_Convenience=input$sNCont_Convenience, sRCont_Convenience=input$sRCont_Convenience, sRSpread_Convenience=input$sRSpread_Convenience,
                   sNClu_Snowball=input$sNClu_Snowball, sRClu_Snowball=input$sRClu_Snowball, sNCont_Snowball=input$sNCont_Snowball, sRCont_Snowball=input$sRCont_Snowball, sRSpread_Snowball=input$sRSpread_Snowball
      )
      if (debug) print("     updateDesign - exit")
      design
    } 
    
    updateEvidence<-function(){
      if (debug) print("     updateEvidence")
      evidence<-list(rInteractionOn=input$rInteractionOn,
                     showType=input$Effect_type,
                     ssqType=input$ssqType,
                     evidenceCaseOrder=input$evidenceCaseOrder,Welch=input$Welch)
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
      effect<-updatePrediction()

      PlotNULL<-ggplot()+plotBlankTheme+theme(plot.margin=margin(0,-0.1,0,0,"cm"))+
        scale_x_continuous(limits = c(0,10),labels=NULL,breaks=NULL)+scale_y_continuous(limits = c(0,10),labels=NULL,breaks=NULL)
        
      switch (no_ivs,
              {
                g<-PlotNULL+
                  # annotation_custom(grob=ggplotGrob(PlotIV()),xmin=3,xmax=7,ymin=6,ymax=10)+
                  annotation_custom(grob=ggplotGrob(drawVariable(IV)),xmin=3,xmax=7,ymin=6,ymax=10)+
                  annotation_custom(grob=ggplotGrob(drawVariable(DV)),xmin=3,xmax=7,ymin=0,ymax=4)
                
                  g<-g+annotation_custom(grob=ggplotGrob(drawEffectES(effect$rIV,1)),xmin=3,xmax=7,ymin=2.75,ymax=6.75)
              },
              {
                g<-PlotNULL+
                  annotation_custom(grob=ggplotGrob(drawVariable(IV)), xmin=0,  xmax=4,  ymin=6, ymax=10)+
                  annotation_custom(grob=ggplotGrob(drawVariable(IV2)),xmin=6,  xmax=10, ymin=6, ymax=10)+
                  annotation_custom(grob=ggplotGrob(drawVariable(DV)), xmin=3,  xmax=7,  ymin=0, ymax=4)
                  
                  g<-g+annotation_custom(grob=ggplotGrob(drawEffectES(effect$rIV,2)),xmin=1.5,xmax=5.5,ymin=3, ymax=7)+
                     annotation_custom(grob=ggplotGrob(drawEffectES(effect$rIV2,3)),xmin=4.5,xmax=8.5,ymin=3, ymax=7)+
                     annotation_custom(grob=ggplotGrob(drawEffectES(effect$rIVIV2,4)),xmin=3,  xmax=7,  ymin=6, ymax=10)+
                     annotation_custom(grob=ggplotGrob(drawEffectES(effect$rIVIV2DV,5)),xmin=3,  xmax=7,  ymin=3, ymax=7)
              }
      )
      if (debug) print("HypothesisPlot - exit")
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
        
        effect<-updatePrediction()

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
                
                  gridTheme<-theme(plot.margin=margin(0,0,0,0,"cm"),
                                   axis.title=element_text(size=7,face="bold"),axis.text.x=element_text(size=6),axis.text.y=element_text(size=6))
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
        effect<-updatePrediction()
        evidence<-updateEvidence()

        switch (no_ivs,
                {g<-drawPrediction(IV,IV2,DV,effect,design)},
                {
                  if (evidence$rInteractionOn==FALSE){
                    effect1<-effect
                    effect2<-effect
                    effect2$rIV<-effect2$rIV2
                    
                    gridTheme<-theme(plot.margin=margin(0,0,0,0,"cm"),
                                     axis.title=element_text(size=7,face="bold"),axis.text.x=element_text(size=6),axis.text.y=element_text(size=6))
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
                      
                      gridTheme<-theme(plot.margin=margin(0,0,0,0,"cm"),
                                       axis.title=element_text(size=7,face="bold"),axis.text.x=element_text(size=6),axis.text.y=element_text(size=6))
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

    # UI changes
    # go to the sample tabs 
    sampleUpdate<-observeEvent(c(input$Single,input$newSample,input$hypothesisApply),{
      if (any(c(input$Single,input$newSample))>0) {
        if (!is.element(input$Graphs,c("Sample","Describe","Infer","Possible")))
        {updateTabsetPanel(session, "Graphs",
                           selected = "Sample")
          updateTabsetPanel(session, "Reports",
                            selected = "Sample")
        }
      }
    }
    )

# single sample calculations
    doSampleAnalysis<-function(IV,IV2,DV,effect,design,evidence){

      sample<-makeSample(IV,IV2,DV,effect,design)
      if (is.null(sample)) return(NULL)
      analyseSample(IV,IV2,DV,design,evidence,sample)
      
    }
    
    # eventReactive wrapper
    sampleAnalysis<-eventReactive(c(input$newSample,input$hypothesisApply),{
      if (any(c(input$newSample,input$hypothesisApply)>0)){
        validSample<<-TRUE
        IV<-updateIV()
        IV2<-updateIV2()
        DV<-updateDV()
        
        effect<-updatePrediction()
        design<-updateDesign()
        evidence<-updateEvidence()
        
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
        
        result<-doSampleAnalysis(IV,IV2,DV,effect,design,evidence)
        # set the result into likelihood: populations
        if (!is.na(result$rIV)) {
        updateNumericInput(session,"likelihoodSampRho",value=result$rIV)
        updateNumericInput(session,"likelihoodSampRhoS",value=result$rIV)
        }
        result
      }
    })
    
  # SINGLE graphs
    # single sample graph
    output$SamplePlot <- renderPlot({
      if (debug) print("SamplePlot")
      doIt<-editVar$data

      IV<-updateIV()
      IV2<-updateIV2()
      DV<-updateDV()
        if (is.null(IV) || is.null(DV)) {return(ggplot()+plotBlankTheme)}
        
      effect<-updatePrediction()
      design<-updateDesign()
      evidence<-updateEvidence()

      # make the sample
      result<-sampleAnalysis()
        if (is.null(result) ||  !validSample)  {return(ggplot()+plotBlankTheme)}
  
      # draw the sample
        switch (no_ivs,{
          drawSample(IV,DV,effect,result)
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
          
          gridTheme<-theme(plot.margin=margin(0,0,0,0,"cm"),
                           axis.title=element_text(size=7,face="bold"),axis.text.x=element_text(size=6),axis.text.y=element_text(size=6))
          ggplot()+plotBlankTheme+theme(plot.margin=margin(0,-0.2,0,0,"cm"))+
            scale_x_continuous(limits = c(0,10),labels=NULL,breaks=NULL)+scale_y_continuous(limits = c(0,10),labels=NULL,breaks=NULL)+
            
            annotation_custom(grob=ggplotGrob(drawSample(IV,DV,effect1,result1)+gridTheme),xmin=0.5,xmax=4.5,ymin=0,ymax=5)+
            annotation_custom(grob=ggplotGrob(drawSample(IV2,DV,effect2,result2)+gridTheme),xmin=5.5,xmax=9.5,ymin=0,ymax=5)+
            annotation_custom(grob=ggplotGrob(drawSample(IV,IV2,effect3,result3)+gridTheme),xmin=3,xmax=7,ymin=5,ymax=10)
        }
        )
    })
    
    # single descriptive graph
    output$DescriptivePlot <- renderPlot({
      if (debug) print("DescriptivePlot")
      doIt<-editVar$data
      # doIt<-input$MVok
      IV<-updateIV()
        IV2<-updateIV2()
        DV<-updateDV()
        if (is.null(IV) || is.null(DV)) {return(ggplot()+plotBlankTheme)}

        effect<-updatePrediction()
        design<-updateDesign()
        evidence<-updateEvidence()
        
        # make the sample
        result<-sampleAnalysis()
        if (is.null(result) ||  !validSample)  {
          validate("Sample is empty")
          return(ggplot()+plotBlankTheme)
          }
        if (is.na(result$rIV)) {
          validate("IV has no variability")
          return(ggplot()+plotBlankTheme)
        }
        
        # draw the description
        switch (no_ivs,
                {g<-drawDescription(IV,IV2,DV,effect,design,result)},
                { 
                  if (evidence$rInteractionOn==FALSE){
                    effect2<-effect
                    effect2$rIV<-effect2$rIV2
                    
                    result2<-list(IVs=result$IV2s, DVs=result$DVs, rIV=result$rIV2, ivplot=result$iv2plot,dvplot=result$dvplot)
                    
                    gridTheme<-theme(plot.margin=margin(0,0,0,0,"cm"),
                                     axis.title=element_text(size=7,face="bold"),axis.text.x=element_text(size=6),axis.text.y=element_text(size=6))
                    g<-ggplot()+plotBlankTheme+theme(plot.margin=margin(0,-0.2,0,0,"cm"))+
                      scale_x_continuous(limits = c(0,10),labels=NULL,breaks=NULL)+scale_y_continuous(limits = c(0,10),labels=NULL,breaks=NULL)+
                      
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
                        
                        gridTheme<-theme(plot.margin=margin(0,0,0,0,"cm"),
                                         axis.title=element_text(size=7,face="bold"),axis.text.x=element_text(size=6),axis.text.y=element_text(size=6))
                        g<-ggplot()+plotBlankTheme+theme(plot.margin=margin(0,-0.2,0,0,"cm"))+
                          scale_x_continuous(limits = c(0,10),labels=NULL,breaks=NULL)+scale_y_continuous(limits = c(0,10),labels=NULL,breaks=NULL)
                        g<-g+annotation_custom(grob=ggplotGrob(drawDescription(IV,NULL,DV,effect1,design,result1)+gridTheme+ggtitle(paste0(IV2$name,">",format(median(result$iv2),digits=3)))),xmin=0.5,xmax=4.5,ymin=0,ymax=5)
                        g<-g+annotation_custom(grob=ggplotGrob(drawDescription(IV,NULL,DV,effect2,design,result2)+gridTheme+ggtitle(paste0(IV2$name,"<",format(median(result$iv2),digits=3)))),xmin=5.5,xmax=9.5,ymin=0,ymax=5)
                          } else {
                            gridTheme<-theme(plot.margin=margin(0,0,0,0,"cm"),
                                             axis.title=element_text(size=7,face="bold"),axis.text.x=element_text(size=6),axis.text.y=element_text(size=6))
                            g<-ggplot()+plotBlankTheme+theme(plot.margin=margin(0,-0.2,0,0,"cm"))+
                              scale_x_continuous(limits = c(0,10),labels=NULL,breaks=NULL)+scale_y_continuous(limits = c(0,10),labels=NULL,breaks=NULL)
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
                        g<-drawDescription(IV,IV2,DV,effect,design,result)
                      }
                    } else{
                      effect2<-effect
                      effect2$rIV<-effect2$rIV2
                      
                      result2<-list(IVs=result$IV2s, DVs=result$DVs, rIV=result$rIV2, iv=result$iv, dv=result$dv, ivplot=result$iv2plot,dvplot=result$dvplot)
                      
                      gridTheme<-theme(plot.margin=margin(0,0,0,0,"cm"),
                                       axis.title=element_text(size=7,face="bold"),axis.text.x=element_text(size=6),axis.text.y=element_text(size=6))
                      g<-ggplot()+plotBlankTheme+theme(plot.margin=margin(0,-0.2,0,0,"cm"))+
                        scale_x_continuous(limits = c(0,10),labels=NULL,breaks=NULL)+scale_y_continuous(limits = c(0,10),labels=NULL,breaks=NULL)
                      g<-g+annotation_custom(grob=ggplotGrob(drawDescription(IV,NULL,DV,effect,design,result)+gridTheme),xmin=0.5,xmax=4.5,ymin=0,ymax=5)
                      g<-g+annotation_custom(grob=ggplotGrob(drawDescription(IV2,NULL,DV,effect2,design,result2)+gridTheme),xmin=5.5,xmax=9.5,ymin=0,ymax=5)
                      g<-g+annotation_custom(grob=ggplotGrob(drawDescription(IV,IV2,DV,effect,design,result)+gridTheme),xmin=3,xmax=7,ymin=5,ymax=10)
                    }
                  }
                }
        )
        g
    })
    
    # single inferential graph
    output$InferentialPlot <- renderPlot({
      if (debug) print("InferentialPlot")
      doIt<-editVar$data
      # doIt<-input$MVok
      IV<-updateIV()
        IV2<-updateIV2()
        DV<-updateDV()
        if (is.null(IV) || is.null(DV)) {return(ggplot()+plotBlankTheme)}
        
        effect<-updatePrediction()
        design<-updateDesign()
        evidence<-updateEvidence()
        
        result<-sampleAnalysis()
        if (is.null(result) ||  !validSample)  {return(ggplot()+plotBlankTheme)}
        if (is.na(result$rIV)) {
          validate("IV has no variability")
          return(ggplot()+plotBlankTheme)
        }
        
        result$showType<-evidence$showType
        
        switch (input$Infer_type,
                "Power"= {
                  g1<-drawInference(IV,IV2,DV,effect,design,result,"w")
                  g2<-drawInference(IV,IV2,DV,effect,design,result,"nw")
                },
                "EffectSize"={
                  g1<-drawInference(IV,IV2,DV,effect,design,result,"r")
                  g2<-drawInference(IV,IV2,DV,effect,design,result,"p")
                }
        )
        gridTheme<-theme(plot.margin=margin(0,0,0,0,"cm"),
                         axis.title=element_text(size=7,face="bold"),axis.text.x=element_text(size=6),axis.text.y=element_text(size=6))
        
        g<-ggplot()+plotBlankTheme+theme(plot.margin=margin(0,-0.2,0,0,"cm"))+
          scale_x_continuous(limits = c(0,10),labels=NULL,breaks=NULL)+scale_y_continuous(limits = c(0,10),labels=NULL,breaks=NULL)
        g<-g+annotation_custom(grob=ggplotGrob(g1+gridTheme),xmin=0.5,xmax=4.5,ymin=0,ymax=10)
        g<-g+annotation_custom(grob=ggplotGrob(g2+gridTheme),xmin=5.5,xmax=9.5,ymin=0,ymax=10)
        return(g)
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
        
        effect<-updatePrediction()
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
        
      effect<-updatePrediction()
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
      # doIt<-input$MVok
      IV<-updateIV()
        IV2<-updateIV2()
        DV<-updateDV()
        if (is.null(IV) || is.null(DV)) {return(ggplot()+plotBlankTheme)}
        
        effect<-updatePrediction()
        design<-updateDesign()
        evidence<-updateEvidence()
        
        result<-sampleAnalysis()
        if (is.null(result) ||  !validSample)  {return(ggplot()+plotBlankTheme)}
        if (is.na(result$rIV)) {
          validate("IV has no variability")
          return(ggplot()+plotBlankTheme)
        }
        
        result$showType<-evidence$showType
        
        reportInference(IV,IV2,DV,effect,result)        
    })
    
##################################################################################    
# EXPECTED    
    # UI changes  
    # set expected variable from UI
    # calculations
    # outputs (2 graphs and report)
# 
    resetExpected<-function(){
    expectedResult<<-list(result=list(rIV=c(),pIV=c(),rIV2=c(),pIV2=c(),rIVIV2DV=c(),pIVIV2DV=c(),nval=c(),r=list(direct=c(),unique=c(),total=c(),coefficients=c()),showType=c()),
                          nullresult=list(rIV=c(),pIV=c(),rIV2=c(),pIV2=c(),rIVIV2DV=c(),pIVIV2DV=c(),nval=c(),r=list(direct=c(),unique=c(),total=c(),coefficients=c()),showType=c()),
                          count=0,
                          nullcount=0,
                          nsims=0,
                          running=FALSE)
    }

    # here's where we start a run
    observeEvent(input$expectedRun,{
      if (!input$expectedAppend) {
        resetExpected()
      } 
      expectedResult$nsims<<-expectedResult$count+as.numeric(input$Expected_length)
      expectedResult$running<<-TRUE
    })
    
  showProgress<-TRUE
  resetExpected()
  
# UI changes
    # go to the expected tabs 
    expectedUpdate<-observeEvent(input$expectedRun,{
      if (input$expectedRun>0) {
        if (!is.element(input$Graphs,c("Possible"))) {
          updateTabsetPanel(session, "Graphs",selected = "Expect")
          updateTabsetPanel(session, "Reports",selected = "Expect")
        }
      validExpected<<-TRUE
      }
    }
    ,priority=100
    )
    
# set expected variable from UI
    updateExpected<-function(){
      list(type=input$Expected_type,nsims=as.numeric(input$Expected_length),append=input$expectedAppend)
    }    
    
    
# make this a stand-alone function to be called from observEvent
    doExpectedAnalysis<-function(IV,IV2,DV,effect,design,evidence,expected,nsim=1) {
      if (debug) {print("     doExpectedAnalysis - start")}
      
      if (nsim==expectedResult$nsims) {showProgress<-FALSE} else {showProgress<-TRUE}
      if (showProgress) {showNotification(paste0(format(expectedResult$count),"/",format(expectedResult$nsims)),id="counting",duration=Inf,closeButton=FALSE,type="message")}
      append<-TRUE
      expectedResult$result<<-multipleAnalysis(IV,IV2,DV,effect,design,evidence,nsim,append,expectedResult$result,showProgress=!showProgress)
      expectedResult$count<<-length(expectedResult$result$rIV)
      if (expected$type=="NHSTErrors"){
        effectNull<-effect
        effectNull$rIV<-0
        effectNull$rIV2<-0
        effectNull$rIVIV2DV<-0
        expectedResult$nullresult<<-multipleAnalysis(IV,IV2,DV,effectNull,design,evidence,nsim,append,expectedResult$nullresult,showProgress=!showProgress)
        expectedResult$nullcount<<-length(expectedResult$nullresult$rIV)
      }
      if (expectedResult$count>=expectedResult$nsims) {
        expectedResult$running<<-FALSE
        if (showProgress) {removeNotification(id = "counting")}
      }
      if (debug) {print("     doExpectedAnalysis - end")}
      expectedResult
      
    }
    
# Expected outputs
    # show expected result    
    output$ExpectedPlot <- renderPlot({
      doIt<-input$expectedRun
      if (debug) {print("ExpectedPlot1 - start")}
      if (expectedResult$running) {
        IV<-updateIV()
        IV2<-updateIV2()
        DV<-updateDV()
        
        effect<-updatePrediction()
        design<-updateDesign()
        evidence<-updateEvidence()
        
        expected<-updateExpected()
        expectedResult$result$showType<<-input$Effect_type
        # expectedResult$nsims<<-expected$nsims
        
        if (input$showAnimation) {
        ns<-max(round(expectedResult$count/5),2)
        } else {
        ns<-expectedResult$nsims-expectedResult$count
        }
        if (expectedResult$count+ns>expectedResult$nsims) {ns<-expectedResult$nsims-expectedResult$count}
        expectedResult<<-doExpectedAnalysis(IV,IV2,DV,effect,design,evidence,expected,ns)
        # if (debug) {print("ExpectedPlo1t - sims done ")}
      }
      
      if (expectedResult$count==0) { return(ggplot()+plotBlankTheme) }
      
      if (expectedResult$count<expectedResult$nsims) {
        # if (debug) {print("ExpectedPlot1 - timer set ")}
        invalidateLater(1)
      } 
      switch (input$Expected_type,
              "EffectSize"={
                g1<-r_plot(expectedResult$result,r=effect$rIV,n=design$sN)
                g2<-p_plot(expectedResult$result,r=effect$rIV,n=design$sN)
              },
              "Power"=     {
                g1<-w_plot(expectedResult$result,r=effect$rIV,n=design$sN)
                g2<-nw_plot(expectedResult$result,r=effect$rIV,n=design$sN)
              },
              "NHSTErrors"={
                g2<-e2_plot(expectedResult$result,r=effect$rIV,n=design$sN)
                g1<-e1_plot(expectedResult$nullresult,r=0,n=design$sN)
              },
              "CILimits"=  {
                g1<-ci1_plot(expectedResult$result,r=effect$rIV,n=design$sN)
                g2<-ci2_plot(expectedResult$result,r=effect$rIV,n=design$sN)
              }
      )

      gridTheme<-theme(plot.margin=margin(0,0,0,0,"cm"),
                       axis.title=element_text(size=7,face="bold"),axis.text.x=element_text(size=6),axis.text.y=element_text(size=6))
      
      g<-ggplot()+plotBlankTheme+theme(plot.margin=margin(0,-0.2,0,0,"cm"))+
        scale_x_continuous(limits = c(0,10),labels=NULL,breaks=NULL)+scale_y_continuous(limits = c(0,10),labels=NULL,breaks=NULL)
      g<-g+annotation_custom(grob=ggplotGrob(g1+gridTheme),xmin=0.5,xmax=4.5,ymin=0,ymax=10)
      g<-g+annotation_custom(grob=ggplotGrob(g2+gridTheme),xmin=5.5,xmax=9.5,ymin=0,ymax=10)
      if (debug) {print("ExpectedPlot1 - plots done ")}
      return(g)
    })

    # # second graph    
    # output$ExpectedPlot2 <- renderPlot({
    #   doIt<-input$expectedRun
    #   if (debug) {print("ExpectedPlot2 - start")}
    #   if (expectedResult$count<expectedResult$nsims) {
    #     # if (debug) {print("ExpectedPlot2 - timer set ")}
    #     invalidateLater(1)
    #   } 
    #   
    #   if (expectedResult$count>1) {
    #     switch (input$Expected_type,
    #             "EffectSize"=g<-p_plot(expectedResult$result,r=effect$rIV,n=design$sN),
    #             "Power"=     g<-nw_plot(expectedResult$result,r=effect$rIV,n=design$sN),
    #             "NHSTErrors"=g<-e1_plot(expectedResult$result,r=effect$rIV,n=design$sN),
    #             "CILimits"=  g<-ci2_plot(expectedResult$result,r=effect$rIV,n=design$sN)
    #   )
    #   } else {
    #     g<-ggplot()+plotBlankTheme
    #   }
    #   if (debug) {print("ExpectedPlot2 - end")}
    #   return(g)
    # })
    
    # expected report
    output$ExpectedReport <- renderPlot({
      doIt<-input$expectedRun
      if (debug) {print("ExpectedReport - start")}
      if (expectedResult$count<expectedResult$nsims) {
        # if (debug) {print("ExpectedPlot2 - timer set ")}
        invalidateLater(1)
      } 
      IV<-updateIV()
      IV2<-updateIV2()
      DV<-updateDV()

      effect<-updatePrediction()
      design<-updateDesign()
      evidence<-updateEvidence()

      expected<-updateExpected()
      expectedResult$result$showType<-input$Effect_type

      if (expectedResult$count>1) {
        g<-reportExpected(IV,IV2,DV,evidence,expectedResult$result,expectedResult$nullresult,input$Expected_type)
      } else {
        g<-ggplot()+plotBlankTheme
      }
      if (debug) {print("ExpectedReport - start")}
      return(g)
    })
    
    
##################################################################################    
# EXPLORE    
    # UI changes  
    # set explore variable from UI
    # calculations
    # outputs (graph and report)
# 
    
# local variable
    lastExplore<-c()
    
# UI interface    
# go to the explore tabs 
    observeEvent(c(input$exploreRunH,input$exploreRunD,input$exploreRunA),{
      if (any(c(input$exploreRunH,input$exploreRunD,input$exploreRunA))>0)
      {validExplore<<-TRUE}
      
      if (validExplore){
        updateTabsetPanel(session, "Graphs",
                          selected = "Explore")
        updateTabsetPanel(session, "Reports",
                          selected = "Explore")
      }
    },priority=100)
    
    # # set explore options    
    # observeEvent(c(input$Explore_typeH,input$Explore_typeD,input$Explore_typeA),{
    #   
    #   if (is.element(input$Explore_typeH,c("EffectSize","EffectSize1","EffectSize2","Interaction","Covariation"))) {
    #     shinyjs::showElement(id= "Explore_esRange")
    #     shinyjs::showElement(id= "Explore_esRangeLabel")
    #   } else {
    #     shinyjs::hideElement(id= "Explore_esRange")
    #     shinyjs::hideElement(id= "Explore_esRangeLabel")
    #   }
    #   
    #   if (is.element(input$Explore_typeD,c("SampleSize","Dependence","Outliers","Heteroscedasticity","IVRange","DVRange"))) {
    #     shinyjs::showElement(id= "Explore_nRange")
    #     shinyjs::showElement(id= "Explore_nRangeLabel")
    #     if (input$Explore_typeD=="SampleSize"){ 
    #       updateNumericInput(session,"Explore_nRange",value=input$Explore_nRange2)
    #     } else {
    #       if (is.element(input$Explore_typeD,c("IVRange","DVRange"))) {
    #         updateNumericInput(session,"Explore_nRange",value=3)
    #       } else { updateNumericInput(session,"Explore_nRange",value=input$Explore_anomRange2) }
    #     }
    #   } else {
    #     shinyjs::hideElement(id= "Explore_nRange")
    #     shinyjs::hideElement(id= "Explore_nRangeLabel")
    #   }
    #   
    #   validExplore<<-FALSE
    # })
    
    observeEvent(input$IV2choice,{
      if (input$IV2choice=="none") {
        updateSelectInput(session,"Explore_typeH",
                          choices=list("Variables"=list("IV" = "IV",
                                                        "DV" = "DV",
                                                        "IV/DV Types" = "IVDVType"),
                                       "Effects"=list("Effect Size" = "EffectSize1")
                          )
        )
      }
      else {
        updateSelectInput(session,"Explore_typeH",
                          choices=list("Variables"=list("IV" = "IV",
                                                        "IV2" = "IV2",
                                                        "DV" = "DV",
                                                        "IV/IV2 Types" = "IVIV2Type"),
                                       "Effects"=list("Effect Size1" = "EffectSize1",
                                                      "Effect Size2" = "EffectSize2",
                                                      "Interaction" = "Interaction",
                                                      "Covariation" = "Covariation")
                          )
        )
      }
    })
    
# set explore variable from UI    
    # update explore values    
    updateExplore<-function(){
      if (!is.element(input$ExploreTab,c("Hypothesis","Design","Anomalies"))) {
        return(lastExplore)
      }
      switch (input$ExploreTab,
              "Hypothesis"={
                l<-list(Explore_type=input$Explore_typeH,
                        Explore_show=input$Explore_showH, 
                        Explore_extraShow=input$Explore_extraShowH, 
                        Explore_whichShow=input$Explore_whichShowH, 
                        Explore_length=as.numeric(input$Explore_lengthH),
                        Append=input$exploreAppendH)  
              },
              "Design"={
                l<-list(Explore_type=input$Explore_typeD,
                        Explore_show=input$Explore_showD, 
                        Explore_extraShow=input$Explore_extraShowD, 
                        Explore_whichShow=input$Explore_whichShowD, 
                        Explore_length=as.numeric(input$Explore_lengthD),
                        Append=input$exploreAppendD)  
              },
              "Anomalies"={
                l<-list(Explore_type=input$Explore_typeA,
                        Explore_show=input$Explore_showA, 
                        Explore_extraShow=input$Explore_extraShowA, 
                        Explore_whichShow=input$Explore_whichShowA, 
                        Explore_length=as.numeric(input$Explore_lengthA),
                        Append=input$exploreAppendA)  
              }
      )
      
      explore<-c(l,list(Explore_npoints=input$Explore_npoints,Explore_quants=input$Explore_quants,
                        Explore_esRange=input$Explore_esRange,Explore_nRange=input$Explore_nRange,Explore_anomRange=input$Explore_esRange,
                        full_ylim=input$full_ylim,
                        Explore_family=input$ExploreTab))
      if (is.element(explore$Explore_type,c("IV","IV2","DV")))
      {explore$Explore_type<-paste(explore$Explore_type,input$Explore_VtypeH,sep="")}
      
      lastExplore<<-explore
      explore
      
    } 
    
# Main calculations    
    exploreAnalysis<-eventReactive(c(input$exploreRunH,input$exploreRunD,input$exploreRunA),{
      if (!any(c(input$exploreRunH,input$exploreRunD,input$exploreRunA)>0))
      {return(ggplot()+plotBlankTheme)}
      
      IV<-updateIV()
      IV2<-updateIV2()
      DV<-updateDV()
      
      effect<-updatePrediction()
      design<-updateDesign()
      evidence<-updateEvidence()
      explore<-updateExplore()
      explore$doNull<-FALSE
      
      ex<-exploreSimulate(IV,IV2,DV,effect,design,evidence,explore)
      ex$null<-exploreNullAnalysis()
      ex$Explore_show<-explore$Explore_show
      ex$Explore_extraShow<-explore$Explore_extraShow
      
      ex      
    })
   # null hypothesis version 
     # keep this separate so that it alone can be called whe switching to NHST
   exploreNullAnalysis<-eventReactive(c(input$exploreRunH,input$exploreRunD,input$exploreRunA,
                                         input$Explore_showH,input$Explore_showD,input$Explore_showA),{
      if (any(c(input$Explore_showH=="NHSTErrors" && input$ExploreTab=="Hypothesis",
                input$Explore_showD=="NHSTErrors" && input$ExploreTab=="Design",
                input$Explore_showA=="NHSTErrors" && input$ExploreTab=="Anomalies")
              )
          ){

      IV<-updateIV()
      IV2<-updateIV2()
      DV<-updateDV()
      
      effect<-updatePrediction()
      design<-updateDesign()
      evidence<-updateEvidence()
      explore<-updateExplore()
      explore$doNull<-TRUE
      
      exploreSimulate(IV,IV2,DV,effect,design,evidence,explore)
      } else{
        NULL
      }
    })

# Explore outputs
   # show explore analysis        
    output$ExplorePlot <- renderPlot({
      doIt<-c(input$exploreRunH,input$exploreRunD,input$exploreRunA)
      
      IV<-updateIV()
      IV2<-updateIV2()
      DV<-updateDV()
      if (!validExplore || is.null(IV) || is.null(DV)) {return(ggplot()+plotBlankTheme)}

      effect<-updatePrediction()
      design<-updateDesign()
      evidence<-updateEvidence()
      
      # this guarantees that we update without recalculating if possible
      explore<-updateExplore()
      exploreResult<-exploreAnalysis()
      if (explore$Explore_family!=exploreResult$Explore_family) {return(ggplot()+plotBlankTheme)}

      if (explore$Explore_show=="NHSTErrors"){
        exploreResult$null<-exploreNullAnalysis()
      } 
      drawExplore(Iv,IV2,DV,effect,design,explore,exploreResult)

    })
    
    # report explore analysis        
    output$ExploreReport <- renderPlot({
      doIt<-c(input$exploreRunH,input$exploreRunD,input$exploreRunA)
      IV<-updateIV()
      IV2<-updateIV2()
      DV<-updateDV()

      effect<-updatePrediction()
      design<-updateDesign()
      evidence<-updateEvidence()
      
      explore<-updateExplore()
      
      if (!validExplore || is.null(IV) || is.null(DV)) {return(ggplot()+plotBlankTheme)}
      exploreResult<-exploreAnalysis()
      
      reportExplore(Iv,IV2,DV,effect,design,explore,exploreResult)
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
      }
      
    },priority=100)
    
# set likelihood variable from UI 
    updateLikelihood<-function(){
      IV<-updateIV()
      DV<-updateDV()
      effect<-updatePrediction()
      
      switch (input$Likelihood,
              "Populations"={
                list(type=input$Likelihood,
                     populationDist=paste0(input$Population_distrP,'_',input$Population_RZ), populationDistK=input$Population_distr_kP,
                     likelihoodNullp=input$likelihoodNullp,
                     sampleES=input$likelihoodSampRho,populationES=effect$rIV,sampleN=input$sN,
                     showTheory=input$likelihoodTheory,appendSim=input$likelihoodAppendP,
                     Likelihood_length=as.numeric(input$likelihood_lengthP)
                )
              },
              "Samples"={
                list(type=input$Likelihood,
                     populationDist=input$Population_distrP, populationDistK=input$Population_distr_kP,
                     likelihoodNullp=input$likelihoodNullp,
                     sampleES=input$likelihoodSampRhoS,populationES=input$likelihoodPopRho,sampleN=input$sN,
                     showTheory=input$likelihoodTheory,appendSim=input$likelihoodAppendS,
                     Likelihood_length=as.numeric(input$likelihood_lengthS)
                )
              }
      )
    } 
    
# main likelihood calcuations    
    likelihoodAnalysis<-eventReactive(c(input$likelihoodRunS,input$likelihoodRunP,
                                        input$likelihoodNullp,
                                        input$likelihoodPopRho,
                                        input$likelihoodSampRho,
                                        input$newSample,input$expectedRun,
                                        input$Population_distrP,input$Population_distr_kP,input$Population_distrS,input$Population_distr_kS),{

        IV<-updateIV()
        DV<-updateDV()
        
        effect<-updatePrediction()
        design<-updateDesign()
        evidence<-updateEvidence()
        
        likelihood<-updateLikelihood()
        likelihood$type<-input$Likelihood

        if (input$likelihoodRunS+input$likelihoodRunP>validLikelihood){
          validLikelihood<<-input$likelihoodRunS+input$likelihoodRunP
          likelihoodRes<-likelihoodRun(IV,DV,effect,design,evidence,likelihood,doSample = TRUE)
        } else {
          likelihoodRes<-likelihoodRun(IV,DV,effect,design,evidence,likelihood,doSample = FALSE)
        }
        
        switch (likelihood$type,
                "Samples"={likelihoodResult$samples<<-likelihoodRes},
                "Populations"={likelihoodResult$populations<<-likelihoodRes}
                )
        likelihoodResult
    })
    
# likelihood outputs    
    # show likelihood analysis        
    output$LikelihoodPlot <- renderPlot({
      if (!is.element(input$Likelihood,c("Samples","Populations"))) {return(ggplot()+plotBlankTheme)}
      IV<-updateIV()
      DV<-updateDV()
      if (is.null(IV) || is.null(DV)) {return(ggplot()+plotBlankTheme)}
      
      effect<-updatePrediction()
      design<-updateDesign()
      
      # this guarantees that we update without recalculating if possible
      likelihood<-updateLikelihood()
      likelihoodResult<-likelihoodAnalysis()

      likelihood$view<-input$LikelihoodView
      likelihood$azimuth<-input$LikelihoodAzimuth
      likelihood$elevation<-input$LikelihoodElevation
      likelihood$range<-input$LikelihoodRange
      drawLikelihood(Iv,DV,effect,design,likelihood,likelihoodResult)
      
    })
    
    # report likelihood analysis        
    output$LikelihoodReport <- renderPlot({
      if (!is.element(input$Likelihood,c("Samples","Populations"))) {return(ggplot()+plotBlankTheme)}
      IV<-updateIV()
      DV<-updateDV()
      if (is.null(IV) || is.null(DV)) {return(ggplot()+plotBlankTheme)}
      
      effect<-updatePrediction()
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

      newVariables<-readSample(raw_data,input$ImportOrdinals,input$MaxOrdinals,header)

      # save the current set of variables
      defaultVariables<<-variables
      
      # store the variables in global workspace
      if (mergeVariables){
        variables<<-rbind(newVariables,variables)
      } else{
        variables<<-newVariables
        simData<<-FALSE
      }
      # get the variables into the ui
      updateSelectInput(session, "IVchoice", choices = variables$name, selected = variables$name[1])
      updateSelectInput(session, "IV2choice", choices = c("none",variables$name), selected = "none")
      updateSelectInput(session, "DVchoice", choices = variables$name, selected = variables$name[nrow(variables)])
      setIVanyway()
      setIV2anyway()
      setDVanyway()
      
      updateNumericInput(session,"sN",value=length(unique(importedData[[1]])))
      updateActionButton(session,"newSample", label="Analyze")
      shinyjs::showElement(id= "hypothesisApply")
      updateTabsetPanel(session, "Hypothesis",selected = "Variables")
      updateNumericInput(session,"rIV",value=NA)
      
      shinyjs::disable(id= "rIV")
      shinyjs::disable(id= "rIV2")
      shinyjs::disable(id= "rIVIV2")
      shinyjs::disable(id= "rIVIV2DV")
      
      shinyjs::disable(id= "sN")
      shinyjs::disable(id= "sMethod")
      shinyjs::disable(id= "sIV1Use")
      shinyjs::disable(id= "sIV2Use")
      
      updateSelectInput(session,"Using",choices=c("Simulations"="Simulations","Data"="Data"),selected="Data")
      variablesHeld<<-"Data"
      shinyjs::showElement(id= "Using")
      runjs(sprintf("document.getElementById('Variables').style.backgroundColor = '%s';",subpanelcolours$filesC))
      
    }    

    # respond to file selection by getting sheet names
    inspectDataFile<-observeEvent(input$dataInputFile, {
      sheet_names<-excel_sheets(input$dataInputFile$datapath)
      updateSelectInput(session, "dataInputSheet", choices = sheet_names)
      # get the raw data
      # if (length(sheet_names)==1){
      #   mergeVariables<<-FALSE
      #   raw_data<-read_excel(input$dataInputFile$datapath,sheet = sheet_names[1])
      #   if (nrow(raw_data)>0 && ncol(raw_data)>0)
      #     getNewVariables(raw_data) 
      # }
    })
    
    # data input    
    importDataFile<-observeEvent(input$dataInputFileLoad, {
      mergeVariables<<-FALSE
      # get the raw data
      raw_data<-read_excel(input$dataInputFile$datapath,sheet = input$dataInputSheet)
      if (nrow(raw_data)>0 && ncol(raw_data)>0)
        getNewVariables(raw_data)
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
      
      iv<-result$iv
      dv<-result$dv
      if (is.null(IV2)){
        data<-data.frame(participant=result$participant,iv=iv,dv=dv)
        colnames(data)<-c("Participant",IV$name,DV$name)
      } else {
        iv2<-result$iv2
        data<-data.frame(participant=result$participant,iv=iv,iv=iv2,dv=dv)
        colnames(data)<-c("Participant",IV$name,IV2$name,DV$name)
      }
      data      
    }
    
    exportDataClip<-observeEvent(input$dCopy, {
      data<-exportData()      
      if (!is.null(data)) write_clip(data,allow_non_interactive = TRUE)
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
      
      effect<-updatePrediction()
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
      
      effect<-updatePrediction()
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
      readWS(session,input$wsInputFile$datapath,input$wsInputSheet)
      editVar$data<<-editVar$data+1
    })
    
    importWSClip<-observeEvent(input$wsPaste, {
      readWS(session,"clip")
      editVar$data<<-editVar$data+1
    })
    
    # end of everything        
})

