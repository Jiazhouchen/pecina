#NFB Behavioral Data Compliation:

#######Utility functions (since we have so little, let's just use designated area)#########
ProcApply<-function(listx=NULL,FUNC=NULL,...,addNAtoNull=T) {
  proc_ls<-lapply(X = listx,FUN = FUNC,... = ...)
  if(addNAtoNull){
  allnames<-unique(unlist(lapply(proc_ls,names)))
  proc_ls<-lapply(proc_ls,function(lsx){
    lsx[allnames[which(!allnames %in% names(lsx))]]<-NA
    return(lsx)
  })
  }
  return(list(list=proc_ls,
              df=do.call(rbind,proc_ls)))
}
findbox<-function() {
  if (Sys.getenv("USER")=="jiazhouchen") {boxdir <- "/Users/jiazhouchen/Box Sync"
  } else if (Sys.getenv("USER")=="jiazhou") {boxdir <- "/Volumes/bek/Box Sync"} else {
    boxdir<-system("find ~ -iname 'Box*' -maxdepth 2 -type d",intern = T)}
  return(boxdir)
}
cleanuplist<-function(listx){
  if (any(sapply(listx, is.null))){
    listx[sapply(listx, is.null)] <- NULL}
  return(listx)
}

########################ACTUAL CODE###################################
##Arguments
usebek<-T
writeallcsv<-T
tagname<-"all_new"

if(usebek){boxdir<-"/Volumes/bek/Box Sync"} else {boxdir<-findbox()}
rootdatapath<-file.path(boxdir,"GitHub/Nfb_task/NFB_response")
outputdir<-file.path(rootdatapath,"SON1&2_behav_results")
source(file.path(outputdir,"startup.R"))
startup()

SON_index<-lapply(c("SON1","SON2"),function(studyx){
  sonnum<-gsub("^.*([0-9]+)", "\\1", studyx)
  datapath<-file.path(rootdatapath,paste0("SONRISA",sonnum))
  idindex<-data.frame(
    IDs=list.files(datapath,recursive = F),
    dirs=list.files(datapath,recursive = F,full.names = T),stringsAsFactors = F
  )
  return(list(index=idindex,sonnum=sonnum))
})
names(SON_index)<-c("SON1","SON2")
#Migrate SON2 data to SON1:
#The SON2 Plac will always be 1st event for SON1
idmapping<-son.getideventmap(ptc.from = ptcs$son2,naomit = T,sonfilter=F)
idmapping<-idmapping[duplicated(idmapping),]

indexTransfer<-SON_index$SON2$index[match(paste0(idmapping$idfield.from,"_Plac"),SON_index$SON2$index$IDs),]
indexTransfer$IDs<-paste0(idmapping$idfield.to,"_1")
SON_index$SON1$index<-rbind(SON_index$SON1$index,indexTransfer)

bothSONs<-lapply(SON_index,function(studyls){
  studyx<-paste0("SON",studyls$sonnum)
  sonnum<-studyls$sonnum
  IDs<-studyls$index$IDs
  IDs<-IDs[order(IDs)]
  tryList<-ProcApply(IDs, function(ID_full){
    allcsvs<-list.files(path = studyls$index$dirs[match(ID_full,studyls$index$IDs)],
                        pattern = paste0("SON[1-2]","_[0-9]*.*_Nfb_.*Run_.*.csv$"),full.names = T)
    if(length(allcsvs)!=0) {
    if(length(allcsvs)!=4){message("This one: [",ID_full,"] doesn't have 4 runs! But we will merge anyways.")}
    ss_data<-do.call(rbind,lapply(allcsvs,read.csv,stringsAsFactors = F))
    ss_data$og_ID<-ss_data$Participant
    ID_split<-strsplit(ID_full,split = "_")[[1]]
    ss_data$FullID<-ID_full
    ss_data$Participant<-ss_data$uID<-paste0(studyx,"_",ID_split[2])
    ss_data$subject_id<-as.numeric(ID_split[2])
    ss_data$VisitType<-ID_split[3]
    } else {message("This one: [",ID_full,"] has no data, returning NULL.")
      ss_data<-NULL}
    return(ss_data)
  })
  names(tryList$list)<-IDs
  
  if(writeallcsv){
    write.csv(tryList$df,file = file.path(outputdir,paste0(tolower(studyx),"_",tagname,".csv")),row.names=FALSE)
  }
  return(tryList)
})
names(bothSONs)<-c("SON1","SON2")

save(bothSONs,file = file.path(outputdir,"son_behav.rdata"))









