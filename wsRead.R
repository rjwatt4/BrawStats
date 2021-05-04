readWS<-function(session,filename,sheetname){

  if (filename=="clip") {
    raw_data<-read_clip()
  } else {
  raw_data<-read_excel(filename,sheet=sheetname)
  }
  
  IV<-variables[1,]
  IV2<-variables[2,]
  DV<-variables[3,]
  IV2changed<-FALSE
  
  for (i in 1:nrow(raw_data)){
    val<-raw_data[[i,3]]
    if (!is.na(val)){
      if (is.element(raw_data[[i,1]],c("IV","IV2","DV")))
      { variable<-raw_data[[i,1]]
        if (variable=="IV2") IV2changed<-TRUE
        field<-raw_data[[i,2]]
        if (is.element(variable,variables$name)){
          use<-match(variable,variables$name)
          variables[use,field]<<-val
        }
        
        switch (variable,
                "IV"={IV[field]<-val},
                "IV2"={IV2[field]<-val},
                "DV"={DV[field]<-val}
        )
        
      }
      else {
        objectID<-raw_data[[i,2]] 
        done<-FALSE
        if (is.element(objectID,c("sMethod","sIV1Use","sIV2Use"))){
          updateSelectInput(session,objectID,selected=val)
          done<-TRUE
        }
        if (is.element(objectID,c("sRangeOn","rInteractionOn"))){
          updateCheckboxInput(session,objectID,value=val)
          done<-TRUE
        }
        if (!done){
          updateNumericInput(session,objectID,value=as.numeric(val))
        }
      }
    }
  }
  
  useIV<-match(IV$name,variables$name)
  if (is.na(useIV)){
    useIV<-nrow(variables)+1
    variables<<-rbind(variables,IV)
    updateSelectInput(session,"IVchoice",choices=variables$name)
    updateSelectInput(session,"IV2choice",choices=c("none",variables$name))
    updateSelectInput(session,"DVchoice",choices=variables$name)
  }
  variables[useIV,]<<-IV
  updateSelectInput(session,"IVchoice",selected=IV$name)
  
  if (IV2changed) {
    useIV2<-match(IV2$name,variables$name)
    if (is.na(useIV2)){
      useIV2<-nrow(variables)+1
      variables<<-rbind(variables,IV2)
      updateSelectInput(session,"IVchoice",choices=variables$name)
      updateSelectInput(session,"IV2choice",choices=c("none",variables$name))
      updateSelectInput(session,"DVchoice",choices=variables$name)
    }
    variables[useIV2,]<<-IV2
    updateSelectInput(session,"IV2choice",selected=IV2$name)
    no_ivs<<-2
  }
  
  useDV<-match(DV$name,variables$name)
  if (is.na(useDV)){
    useDV<-nrow(variables)+1
    variables<<-rbind(variables,DV)
    updateSelectInput(session,"IVchoice",choices=variables$name)
    updateSelectInput(session,"IV2choice",choices=c("none",variables$name))
    updateSelectInput(session,"DVchoice",choices=variables$name)
  }
  variables[useDV,]<<-DV
  updateSelectInput(session,"DVchoice",selected=DV$name)
}
