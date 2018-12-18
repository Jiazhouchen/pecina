#####NFB's stan overall script#########
boxdir <- "/Volumes/bek/Box Sync"
if(reloaddata){
  
}
cleanuplist<-function(listx){
  if (any(sapply(listx, is.null))){
    listx[sapply(listx, is.null)] <- NULL}
  return(listx)
}

son_all<-as.environment(list())
load(file.path(boxdir,"GitHub","Nfb_task","NFB_response","SON1&2_behav_results","son_behav.rdata"),envir = son_all)

#Later can switch bettwen
NFB_df_og<-son_all$bothSONs$SON2$df
NFBData_og<-son_all$bothSONs$SON2$list
with_baselineID<-unique(NFB_df$uID)[which(paste0(unique(NFB_df$uID),"_Plac") %in% names(NFBData_og))]
NFB_df<-NFB_df_og[NFB_df_og$uID %in% with_baselineID,]
NFBData<-cleanuplist(lapply(NFBData_og,function(dfj){if(any(dfj$uID %in% with_baselineID)){return(dfj)}else{return(NULL)} }))
nS=length(unique(NFB_df$uID))
nT=max(sapply(NFBData,nrow))
son_nfb_stan<-list(
  nSubject=nS,maxTrialN=nT,
  nSession=array(0,dim=c(nS)),
  nTrial=array(0,dim=c(nS,2)),SessionIndex=array(0,dim = c(nS,2)),
  Infusion=array(0,dim=c(nS,2,nT)),
  Trial_Feedback=array(0,dim=c(nS,2,nT)),
  Contingency_Feedback=array(0,dim=c(nS,2,nT)),
  ExpRat=array(0,dim=c(nS,2,nT)),
  missing_choice_exp=array(0,dim=c(nS,2,nT)),
  ID=array(0,dim=nS)
)

son_nfb_stan$ID<-unique(NFB_df$uID)
gx<-lapply(lapply(split(NFB_df,NFB_df$uID),function(dfy){split(dfy,dfy$VisitType)}),length)
son_nfb_stan$nSession<-unlist(gx[match(names(gx),son_nfb_stan$ID)])

for (dfx in NFBData) {
  idIndx<-match(unique(dfx$uID),son_nfb_stan$ID)
  if(unique(dfx$VisitType) %in% c("Plac","1")) {seNum=1; seIndx=0} else {seNum=2; seIndx=1} #Nalt diff Plac or 2 diff 1
  son_nfb_stan$nTrial [idIndx,seNum]<-nrow(dfx)
  son_nfb_stan$SessionIndex[idIndx,seNum]<-seIndx
  son_nfb_stan$Infusion[idIndx,seNum,]<-as.numeric(dfx$InfusionNum %in% c(1,2))
  son_nfb_stan$Trial_Feedback[idIndx,seNum,]<-as.numeric(dfx$Feedback=="Signal")
  son_nfb_stan$Contingency_Feedback[idIndx,seNum,]<-as.numeric(dfx$InfusionNum %in% c("1","3"))
  son_nfb_stan$ExpRat[idIndx,seNum,]<-as.numeric(dfx$WillImpRespText=="Yes")
  son_nfb_stan$missing_choice_exp[idIndx,seNum,]<-as.numeric(NFBData$SON2_023_Nalt$WillImpRespText=="NaN")
  son_nfb_stan$ExpRat[idIndx,seNum,which(NFBData$SON2_023_Nalt$WillImpRespText=="NaN")]<-0
  idIndx<-NULL
  seNum<-NULL
  seIndx<-NULL
}


nfb_stan_basic_output=stan(file='stan/nfb_basic_1LR.stan',
                           data=shark_stan_HC,verbose=FALSE,save_warmup=FALSE,
                           pars=c('Invert_Infus','Invert_ProbReinf','which_Infus','which_PorbReinf'),chains = 4,
                           include=FALSE,iter=5000,control=list(adapt_delta=0.99,stepsize=.01))
save(nfb_stan_basic_output,file = "stan/stan_output/nfb_stan_basic_output.rdata")
launch_shinystan(nfb_stan_basic_output)





