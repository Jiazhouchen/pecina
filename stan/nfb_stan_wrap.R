#####NFB's stan overall script#########
boxdir <- "/Volumes/bek/Box Sync"
reloaddata<-F
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
with_baselineID<-unique(NFB_df_og$uID)[which(paste0(unique(NFB_df_og$uID),"_Plac") %in% names(NFBData_og))]
gx<-lapply(lapply(split(NFB_df_og,NFB_df_og$uID),function(dfy){split(dfy,dfy$VisitType)}),length)
withBothID<-names(which(gx==2))
NFB_df<-NFB_df_og[NFB_df_og$uID %in% withBothID,]
NFBData<-cleanuplist(lapply(NFBData_og,function(dfj){if(any(dfj$uID %in% withBothID)){return(dfj)}else{return(NULL)} }))



nS=length(unique(NFB_df$uID))
nT=max(sapply(NFBData,nrow))
nfb_stan<-list(
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

nfb_stan$ID<-unique(NFB_df$uID)
gx<-lapply(lapply(split(NFB_df,NFB_df$uID),function(dfy){split(dfy,dfy$VisitType)}),length)
nfb_stan$nSession<-unlist(gx[match(names(gx),nfb_stan$ID)])

for (dfx in NFBData) {
  idIndx<-match(unique(dfx$uID),nfb_stan$ID)
  if(unique(dfx$VisitType) %in% c("Plac","1")) {seNum=1; seIndx=0} else {seNum=2; seIndx=1} #Nalt diff Plac or 2 diff 1
  ##message(idIndx,"     ",seNum)
  nfb_stan$nTrial [idIndx,seNum]<-nrow(dfx)
  nfb_stan$SessionIndex[idIndx,seNum]<-seIndx
  nfb_stan$Infusion[idIndx,seNum,]<-as.numeric(dfx$InfusionNum %in% c(1,2))
  nfb_stan$Trial_Feedback[idIndx,seNum,]<-as.numeric(dfx$Feedback=="Signal")
  nfb_stan$Contingency_Feedback[idIndx,seNum,]<-as.numeric(dfx$InfusionNum %in% c("1","3"))
  nfb_stan$ExpRat[idIndx,seNum,]<-as.numeric(dfx$WillImpRespText=="Yes")
  nfb_stan$missing_choice_exp[idIndx,seNum,]<-as.numeric(dfx$WillImpRespText=="NaN")
  nfb_stan$ExpRat[idIndx,seNum,which(dfx$WillImpRespText=="NaN")]<-0
  if(nrow(dfx)<nfb_stan$maxTrialN){
    nfb_stan$missing_choice_exp[idIndx,seNum,nrow(dfx):nfb_stan$maxTrialN]<-1
    nfb_stan$ExpRat[idIndx,seNum,nrow(dfx):nfb_stan$maxTrialN]<-0
    }
  idIndx<-NULL
  seNum<-NULL
  seIndx<-NULL
}

#Running Models Here:
library(rstan)
library(shinystan)
rstan_options(auto_write = TRUE)
options(mc.cores = 4)
stop("STOPPPP")
#####################This basic model had been pretty bad#######################
nfb_stan_basic_output_hwarm=stan(file='stan/nfb_basic_1LR.stan',
                           data=nfb_stan,verbose=FALSE,save_warmup=FALSE,
                           pars=c('Invert_Infus','Invert_ProbReinf','which_Infus','which_PorbReinf'),chains = 4,
                           include=FALSE,iter=4000,control=list(adapt_delta=0.99,stepsize=.01))
save(nfb_stan_basic_output_hwarm,file = "stan/stan_output/nfb_stan_basic_output_hwarm.rdata")
launch_shinystan(nfb_stan_basic_output)
#####################Let's run it without the decay factor; have it fixed at 0.99#######################
nfb_stan_1LR_nodecay_output=stan(file='stan/nfb_basic_1LR_nodecay.stan',
                                 data=nfb_stan,verbose=FALSE,save_warmup=FALSE,
                                 pars=c('Invert_Infus','Invert_ProbReinf','which_Infus','which_PorbReinf'),chains = 4,
                                 include=FALSE,iter=4000,control=list(adapt_delta=0.99,stepsize=.05))
save(nfb_stan_1LR_nodecay_output,file = "stan/stan_output/nfb_stan_1LR_nodecay_output.rdata")
launch_shinystan(nfb_stan_basic_output)
######################Well that was horrible....let's not do too much hierarchical modeling######################
nfb_lite_betaonly_1LR_nodecay_output=stan(file='stan/nfb_lite_beta_1LR_nodecay.stan',
                                 data=nfb_stan,verbose=FALSE,save_warmup=FALSE,
                                 pars=c('Invert_Infus','Invert_ProbReinf','which_Infus','which_PorbReinf'),chains = 4,
                                 include=FALSE,iter=2000,control=list(adapt_delta=0.99,stepsize=.05))
save(nfb_lite_betaonly_1LR_nodecay_output,file = "stan/stan_output/nfb_lite_betaonly_1LR_nodecay_output.rdata")

nfb_lite_betaonly_2LR_nodecay_output=stan(file='stan/nfb_lite_beta_2LR_nodecay.stan',
                                          data=nfb_stan,verbose=FALSE,save_warmup=FALSE,
                                          pars=c('Invert_Infus','Invert_ProbReinf','which_Infus','which_PorbReinf'),chains = 4,
                                          include=FALSE,iter=2000,control=list(adapt_delta=0.99,stepsize=.05))
save(nfb_lite_betaonly_2LR_nodecay_output,file = "stan/stan_output/nfb_lite_betaonly_2LR_nodecay_output.rdata")

