#each month gets read in
#each month gets output file
#append outputs together sequentially in a separate bash script 
#setwd('Downloads/')
totalmos<-(2020-1951+1)*12
projectDir<-"/store/sfcnet/datasets/nclimgrid_daily/por/county/"
outputDir<-"output/"


#create list of files for each variable
prcpfilelist<-list.files(path=projectDir,pattern="prcp")
tminfilelist<-list.files(path=projectDir,pattern="tmin")
tmaxfilelist<-list.files(path=projectDir,pattern="tmax")
tavgfilelist<-list.files(path=projectDir,pattern="tavg")

#read in table with FIPS conversion info
#FIPScodesfile<-paste(systemDir,projectDir,"state_fips_ref.csv",sep="")

FIPScodes<-read.csv('state_fips_ref.csv',header=TRUE,sep=',')

#Create output table and assign first row header
#alloutput<-data.frame()
#alloutput[1,]<-c("Combined Date","Year","Month","Day","State","County","FIPS","TAVG","TMAX","TMIN","PRECIPITATION")

NAval<--999.99

#####################################################
#month loop
for (i in 1:length(prcpfilelist)){      #open month file loop
  prcpname<-paste(projectDir,prcpfilelist[i],sep="")
  prcpfile<-read.csv(prcpname,header=FALSE,sep=',')
  tminname<-paste(projectDir,tminfilelist[i],sep="")
  tminfile<-read.csv(tminname,header=FALSE,sep=',')
  tmaxname<-paste(projectDir,tmaxfilelist[i],sep="")
  tmaxfile<-read.csv(tmaxname,header=FALSE,sep=',')
  tavgname<-paste(projectDir,tavgfilelist[i],sep="")
  tavgfile<-read.csv(tavgname,header=FALSE,sep=',')
  #check that sizes match
  if (identical(prcpfile[,2],tavgfile[,2])&&identical(tminfile[,2],tmaxfile[,2])&&identical(tminfile[,2],tavgfile[,2])&&length(prcpfile[,2]==3107)){  #open FIPS match check loop
    #clean dates
    mo<-prcpfile[1,5]
    prettyyear<-prcpfile[1,4]
    if (nchar(mo)<2){mo<-paste("0",mo,sep="")}
    nFIPS<-matrix(data=NA,nrow=3107,ncol=1)
    stateabrev<-matrix(data=NA,nrow=3107,ncol=1)
    countyname<-matrix(data=NA,nrow=3107,ncol=1)
    for (k in 1:3107) {
      #get last three digits of FIPS code (the county code)
      FIPScounty<-substr(prcpfile[k,2], nchar(prcpfile[k,2])-2, nchar(prcpfile[k,2]))
      #get state digits of FIPS code (everything before last 3 digits)
      oFIPSstate<-substr(prcpfile[k,2], 1,nchar(prcpfile[k,2])-3)
      #convert state FIPS code to actual code
      nFIPSstate<-FIPScodes[grep(oFIPSstate,FIPScodes[,1],value=TRUE,fixed=TRUE)[1],3]
      #add preceding 0 if necessary
      if (nchar(nFIPSstate)<2){nFIPSstate<-paste("0",nFIPSstate,sep="")}
      #recombine FIPS code
      nFIPS[k]<-paste(nFIPSstate,FIPScounty,sep="")
      #separate state abbreviation and county name
      #find colon
      breakpos<-regexpr(":",prcpfile[k,3],fixed=TRUE)
      #separate parts 1 and 2 into new vars
      stateabrev[k]<-substring(prcpfile[k,3],0,breakpos-1)
      countyname[k]<-substring(prcpfile[k,3],breakpos+2)
    }
    for (j in 7:ncol(prcpfile)) {   #open day loop
      day<-j-6
      prettyday<-day
      if (nchar(prettyday)<2){prettyday<-paste("0",prettyday,sep="")}
      #combine into single MM/DD/YYYY format field
      fulldate<-paste(mo,"/",prettyday,"/",prettyyear,sep="")
      #if all values in an import column = NAval, the date doesn't exist- Feb. 30th, etc
      if (mean(prcpfile[,j])==-999.99){next}else{  #begin fake date check
        outputday<-data.frame(matrix(data=NA,nrow=3107,ncol=11))
        for (k in 1:nrow(prcpfile)) {  #open county loop
          #set up row to be added to table
          newrow<-c(fulldate,prettyyear,mo,prettyday,stateabrev[k],countyname[k],nFIPS[k],tavgfile[k,j],tmaxfile[k,j],tminfile[k,j],prcpfile[k,j])
          #add row
          #if (j==7&&k==1) {outputmonth[1,]<-newrow}else{outputmonth<-rbind(outputmonth,newrow)}
          outputday[k,]<-newrow
        }  #close county loop
        if (j==7) {outputmonth<-data.frame(outputday)} else {outputmonth<-rbind(outputmonth,outputday)}
      }  #close fake date check
    }    #close day loop
    
    }       #close FIPS match check loop
  #write to monthly output file
  oldname<-substring(prcpfilelist[i],6,11)
  outputfile<-paste(outputDir,oldname,".csv",sep="")
  write.csv(outputmonth,outputfile,row.names=FALSE)
}   #close month file loop



