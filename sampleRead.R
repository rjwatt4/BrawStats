readSample<-function(raw_data, doOrdinals=FALSE, maxOrdinal=9, header=c()){
  if (is.null(raw_data)) {return(NULL)}
  if (nrow(raw_data)==0) {return(NULL)}


  if (is.null(header)) {
  vars<-colnames(raw_data)
  } else {
    vars<-header
  }

    # check for data type row
  if (all(is.element(raw_data[1,],c("Type","type","Interval","interval","Categorical","categorical")))){
    dataTypes<-raw_data[1,]
    raw_data<-raw_data[2:nrow(raw_data),]
  } else {
    dataTypes<-rep("Unknown",ncol(raw_data))
  }
  
  # check for a participant column
  if (is.element(vars[1],c("participant","Participant","participants","Participants"))) {
    vars[1] <- "participant"
  } else{
    participants<-1:nrow(raw_data)
    raw_data<-cbind(data.frame(participant=participants),raw_data)
    vars<-c("participant",vars)
    dataTypes<-c("Type",dataTypes)
  }
  
  withins<-grepl("[^[:space:]]{1,}[|]{1}[^[:space:]]{1,}[=]{1}[^[:space:]]{1,}",vars)
  added<-0
  if (any(withins)){
    DVs<-unique(sub("[|]{1}[^[:space:]]{1,}","",vars[withins]))
    IVs<-unique(sub("[=]{1}[^[:space:]]{1,}","",sub("[^[:space:]]{1,}[|]{1}","",vars[withins])))
    z<-raw_data
    for (iDV in 1:length(DVs)){
      tempName<-paste("Minions",iDV,sep="")
      
      z<-gather_(raw_data, tempName, DVs[iDV], vars[startsWith(vars,paste(DVs[iDV],"|",sep=""))])
      # pull out the new column "DVname|IVname=Cname|IV2name=C2name"
      v<-z[tempName]
      # remove the "DVname"
      v1<-sapply(v,function(x) sub(paste(DVs[iDV],"|",sep=""),"",x))
      while (any(str_length(v1)>0)){
        # "|IVname=Cname"
        # find the "|IVname" bit
        IVlocal<-str_extract(v1,"[^=|]+|[^=|]+")
        # now the "=Cname" bits
        IVlocalcases<-str_match(v1,"=([^=|]+)")[,2]
        # add this variable to z
        # column 1 is participants the last column is the DV
        z<-cbind(z[1],IVlocalcases,z[2:ncol(z)])
        # and change its name
        colnames(z)[2]<-IVlocal[1]
        added<-added+1
        # remove the part we have just dealt with
        v1<-sub("[|].[^|]+","",v1)
      }
      # now remove the temporary column
      z[tempName]<-NULL
      raw_data<-z
    }
    vars<-colnames(raw_data)
  }
  deploys<-rep("Between",ncol(raw_data))
  if (added>0) {
    change=(ncol(raw_data)-(added-1)):ncol(raw_data)
    deploys[change]<-"Within"
  }
  
  keep<-c()
  for (i in 1:length(vars)){
    data<-raw_data[,i]
    if (length(unique(data[!is.na(data)]))>1) {
      keep=c(keep,i)
    }
  }
  vars<-vars[keep]
  raw_data<-raw_data[,keep]
  dataTypes<-dataTypes[keep]
  
  importedData<<-raw_data
  importedData[[1]]<<-factor(importedData[[1]])
  
  # now prepare the variables
  newVariables<-variables[1:(length(vars)-1),]
  ivar<-1
  for (i in 2:length(vars)){
    varname<-vars[i]
    data<-raw_data[,i]
    # set up default values first
    varmu<-0
    varsd<-1
    varskew<-0
    varkurt<-3
    varMedian<-3
    varIQR<-1
    varnlevs<-5
    varcases<-"C1,C2"
    varncats<-2
    varnprop<-paste(c(1,1),collapse=",")
    ordProportions<-NA
    
    if ((is.character(data[[1]]) || 
         is.element(dataTypes[i],c("Categorical","categorical"))) &&
        !is.element(dataTypes[i],c("Interval","interval"))
    ) {
      if (!is.character(data)){data<-data[[1]]}
      importedData[[ivar+1]]<<-factor(importedData[[i]])
      vartype<-"Categorical"
      varcases<-sort(unique(data))
      varncats<-length(varcases)
      proportions<-hist(as.numeric(factor(data)),(0:varncats)+0.5,plot=FALSE)$density
      # proportions<-proportions/max(proportions)
      varnprop<-paste(format(proportions,digits=2),collapse=",")
    } else {
      data<-sapply(data,as.numeric)
      importedData[[ivar+1]]<<-importedData[[i]]
      if (doOrdinals && all(data==round(data)) && all(data>=0) && max(data)<maxOrdinal) {
        vartype<-"Ordinal"
        varMedian<-median(data,na.rm=TRUE)
        varIQR<-IQR(data,na.rm=TRUE)/2
        varnlevs<-max(data)
        ordProportions<-hist(data,(0:varnlevs)+0.5,plot=FALSE)$density
        ordProportions<-paste(format(ordProportions,digits=2),collapse=",")
      } else {
        vartype<-"Interval"
        varmu<-mean(data,na.rm=TRUE)
        varskew<-skewness(data,na.rm=TRUE)
        varkurt<-kurtosis(data,na.rm=TRUE)+3
        varsd<-sd(data,na.rm=TRUE)
      }
    }
    var<-makeVar(name=varname,type=vartype,
                 mu=varmu,sd=varsd,skew=varskew,kurtosis=varkurt,
                 median=varMedian,iqr=varIQR,nlevs=varnlevs,ordProportions=ordProportions,discrete=1,
                 ncats=varncats,cases=paste(varcases,collapse=","),proportions=varnprop,
                 deploy=deploys[i],process="data")
    newVariables[ivar,]<-var
    ivar<-ivar+1
  }
  newVariables
}
