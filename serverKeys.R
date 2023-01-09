####################################
#KEYBOARD: capture keyboard events

ascii<-function(ch) strtoi(charToRaw(toupper(ch)),16L)

if (switches$doKeys) {
  keyrespond<-observeEvent(input$pressedKey,{
    # print(input$keypress)
    
    if (input$keypress==16) shiftKeyOn<<-TRUE
    if (input$keypress==17) controlKeyOn<<-TRUE
    if (input$keypress==18) altKeyOn<<-TRUE
    
    
    # control-alt-n - switch to online version
    if (is_local && input$keypress==78 && controlKeyOn && altKeyOn){
      switches$doReplications<<-FALSE
      switches$doWorlds<<-FALSE
      removeTab("Design","Replicate",session)
      removeTab("Hypothesis","World",session)
      removeTab("HypothesisDiagram","World",session)
      removeTab("FileTab","Batch",session)
    }
    
    # control-alt-m - switch to offline version
    if (input$keypress==77 && controlKeyOn && altKeyOn){
      # replications
      if (!switches$doReplications) {
        switches$doReplications<<-TRUE
        insertTab("Design",replicationTabReserve,"Anomalies","after",select=FALSE,session)
        exploreDesignChoices<<-c(exploreDesignChoices,"Replications")
      }
      # worlds
      if (!switches$doWorlds) {
        switches$doWorlds<<-TRUE
        insertTab("Hypothesis",worldPanelReserve,"Effects","after",select=FALSE,session)
        insertTab("HypothesisDiagram",worldDiagramReserve,"Hypothesis","after",select=FALSE,session)
        exploreHypothesisChoices<<-c(exploreHypothesisChoices,"Worlds")
      }
      # cheating
      if (!switches$doCheating) {
        switches$doCheating<<-TRUE
        shinyjs::showElement(id="Cheating")
        shinyjs::showElement(id="LGEvidenceCheating")
        shinyjs::showElement(id="LGExploreCheating")
        shinyjs::showElement(id="LGlikelihoodCheating")
        exploreDesignChoices<<-c(exploreDesignChoices,"Cheating")
      }
      # explore
      updateSelectInput(session,"Explore_typeH",choices=hypothesisChoices2)
      updateSelectInput(session,"LGExplore_typeH",choices=hypothesisChoices2)
      updateSelectInput(session,"Explore_typeD",choices=designChoices)
      updateSelectInput(session,"LGExplore_typeD",choices=designChoices)
    }
    
    # control-V
    if (is_local && input$keypress==86 && controlKeyOn){
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
    if (is_local && input$keypress==67 && controlKeyOn){
      data<-exportData()
      write_clip(data,allow_non_interactive = TRUE)
    }
    
    # control-alt-p set world to model psych
    if (input$keypress==ascii("p") && controlKeyOn && altKeyOn){
      updateCheckboxInput(session,"world_on",value=TRUE)
      updateSelectInput(session,"world_distr",selected="Exp")
      updateSelectInput(session,"world_distr_rz",selected="z")
      updateNumericInput(session,"world_distr_k",value=0.325)
      updateNumericInput(session,"world_distr_Nullp",value=0.74)
      updateCheckboxInput(session,"sNRand",value=TRUE)
      updateNumericInput(session,"sNRandK",value=1.76)
      updateNumericInput(session,"sN",value=72)
      updateTabsetPanel(session,"HypothesisDiagram",selected="World")
    }
    
    # control-alt-e set world to exp(0.2)
    if (input$keypress==ascii("e") && controlKeyOn && altKeyOn){
      updateCheckboxInput(session,"world_on",value=TRUE)
      updateSelectInput(session,"world_distr",selected="Exp")
      updateSelectInput(session,"world_distr_rz",selected="z")
      updateNumericInput(session,"world_distr_k",value=0.2)
      updateCheckboxInput(session,"sNRand",value=TRUE)
      updateTabsetPanel(session,"HypothesisDiagram",selected="World")
    }
    
    # control-alt-g set world to gauss(0.2)
    if (input$keypress==ascii("g") && controlKeyOn && altKeyOn){
      updateCheckboxInput(session,"world_on",value=TRUE)
      updateSelectInput(session,"world_distr",selected="Gauss")
      updateSelectInput(session,"world_distr_rz",selected="z")
      updateNumericInput(session,"world_distr_k",value=0.2)
      updateCheckboxInput(session,"sNRand",value=TRUE)
      updateTabsetPanel(session,"HypothesisDiagram",selected="World")
    }
    
    # control-alt-s set world to single(0.2)
    if (input$keypress==ascii("s") && controlKeyOn && altKeyOn){
      updateCheckboxInput(session,"world_on",value=TRUE)
      updateSelectInput(session,"world_distr",selected="Single")
      updateSelectInput(session,"world_distr_rz",selected="z")
      updateNumericInput(session,"rIV",value=0.2)
      updateCheckboxInput(session,"sNRand",value=TRUE)
      updateTabsetPanel(session,"HypothesisDiagram",selected="World")
    }
    
    # control-alt-l set longHand to FALSE
    if (input$keypress==ascii("l") && controlKeyOn && altKeyOn){
      updateCheckboxInput(session,"evidenceLongHand",value=FALSE)
      updateCheckboxInput(session,"likelihoodLongHand",value=FALSE)
    }
    
    
    # control-alt-n set sample size to big (1000)
    if (input$keypress==78 && controlKeyOn && altKeyOn){
      updateNumericInput(session,"sN",value=1000)
    }
    
    # control-alt-f set effect size to 0.3
    if (input$keypress==70 && controlKeyOn && altKeyOn){
      updateNumericInput(session,"rIV",value=0.3)
    }
    
    # control-alt-r set replication
    if (input$keypress==82 && controlKeyOn && altKeyOn){
      updateCheckboxInput(session,"sReplicationOn",value=TRUE)
      updateNumericInput(session,"sReplRepeats",value=3)
    }
    
    # control-alt-3 set IV2
    if (input$keypress==51 && controlKeyOn && altKeyOn){
      updateSelectInput(session,"IV2choice",selected="IV2")
    }
    
    # control-alt-w set sample usage to within
    if (input$keypress==87 && controlKeyOn && altKeyOn){
      updateSelectInput(session,"sIV1Use",selected="Within")
      updateSelectInput(session,"sIV2Use",selected="Within")
    }
    
    # control-alt-d do debug
    if (input$keypress==68 && controlKeyOn && altKeyOn){
      toggleModal(session, modalId = "debugOutput", toggle = "open")
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
      expected$EvidenceExpected_type<-"EffectSize"
      expected$append<-FALSE
      
      if (is.null(IV2)) {
        result<-doSampleAnalysis(IV,IV2,DV,effect,design,evidence)
      }
      doExpectedAnalysis(IV,IV2,DV,effect,design,evidence,expected)
      op<-runDebug(IV,IV2,DV,effect,design,evidence,expected,result,expectedResult)
      
      if (!is.null(IV2)) {
        effect$rIVIV2=0.25
        doExpectedAnalysis(IV,IV2,DV,effect,design,evidence,expected)
        op<-c(op,runDebug(IV,IV2,DV,effect,design,evidence,expected,result,expectedResult))
        
        effect$rIVIV2=-0.25
        doExpectedAnalysis(IV,IV2,DV,effect,design,evidence,expected)
        op<-c(op,runDebug(IV,IV2,DV,effect,design,evidence,expected,result,expectedResult))
      }
      
      output$plotPopUp<-renderPlot(reportPlot(op,nc,length(op)/nc,2))
      return()
    }
    
  })
  
  keyrespondUp<-observeEvent(input$keyrelease,{
    if (input$keyrelease==18) altKeyOn<<-FALSE
    if (input$keyrelease==17) controlKeyOn<<-FALSE
    if (input$keyrelease==16) shiftKeyOn<<-FALSE
  })
  
}

