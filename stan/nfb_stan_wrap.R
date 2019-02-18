#####NFB's stan overall script#########
boxdir <- "~/Box"
reloaddata<-F
if(reloaddata){
  source(file.path(boxdir,"GitHub","Nfb_task","NFB_response","SON1&2_behav_results","nfb_behavimport.R"))
}
cleanuplist<-function(listx){
  if (any(sapply(listx, is.null))){
    listx[sapply(listx, is.null)] <- NULL}
  return(listx)
}

son_all<-as.environment(list())
load(file.path(boxdir,"GitHub","Nfb_task","NFB_response","SON1&2_behav_results","nfb_behav.rdata"),envir = son_all)


nfb_prep_stan<-function(son_all,whichSON="SON1",paired=T,adminfilter="NULL"){
  
  switch (whichSON,
    "SON1" = {baseline_gx="_1"},
    "SON2" = {baseline_gx="_Plac"}
  )
  
  #Later can switch bettwen
  
  NFBData_og<-son_all$bothSONs[[whichSON]]$list
  
  
  if(!paired){
    NFBData<-NFBData_og
    if(!is.null(adminfilter)){
      NFBData<-NFBData[grepl(adminfilter,names(NFBData))]
    }
    uIDs<-sapply(NFBData,function(j){unique(j$uID)})
    nS=length(uIDs)
    nT=max(sapply(NFBData,nrow))
    dim_1<-c(nS)
    dim_2<-c(nS,nT)
  } else {
    NFB_df_og<-son_all$bothSONs[[whichSON]]$df
    with_baselineID<-unique(NFB_df_og$uID)[which(paste0(unique(NFB_df_og$uID),baseline_gx) %in% names(NFBData_og))]
    gx<-lapply(lapply(split(NFB_df_og,NFB_df_og$uID),function(dfy){split(dfy,dfy$VisitType)}),length)
    withBothID<-names(which(gx==2))
    NFBData<-cleanuplist(lapply(NFBData_og,function(dfj){if(any(dfj$uID %in% withBothID)){return(dfj)}else{return(NULL)} }))
    uIDs<-sapply(NFBData,function(j){unique(j$uID)})
    nS=length(uIDs)
    nT=max(sapply(NFBData,nrow))
    dim_1<-c(nS,2)
    dim_2<-c(nS,2,nT)
  }
  
  nfb_stan<-list(
    nSubject=nS,maxTrialN=nT,
    nSession=array(0,dim=c(nS)),
    nTrial=array(0,dim=dim_1),
    SessionIndex=array(0,dim = dim_1),
    Infusion_f=array(0,dim=dim_2),
    Infusion=array(0,dim=dim_2),
    Trial_Feedback=array(0,dim=dim_2),
    Contingency_Feedback=array(0,dim=dim_2),
    ExpRat=array(0,dim=dim_2),
    missing_choice_exp=array(0,dim=dim_2),
    ID=array(0,dim=nS)
  )
  
  nfb_stan$ID<-uIDs
  
  if(paired){
  nfb_stan$nSession<-unlist(gx[match(names(gx),nfb_stan$ID)])
  } else {nfb_stan$nSession[]<-1}
  
  for (dfx in NFBData) {
    idIndx<-match(unique(dfx$uID),nfb_stan$ID)
    
    if(unique(dfx$VisitType) %in% c("Plac","1")) {seNum=1; seIndx=0} else {seNum=2; seIndx=1} #Nalt diff Plac or 2 diff 1
    
    if(!paired){
      nfb_stan$nTrial [idIndx]<-nrow(dfx)
      nfb_stan$SessionIndex[idIndx]<-seIndx
      nfb_stan$Infusion_f[idIndx,1:nrow(dfx)]<-as.numeric(dfx$InfusionNum)
      nfb_stan$Infusion[idIndx,1:nrow(dfx)]<-as.numeric(dfx$InfusionNum %in% c("1","2"))
      nfb_stan$Trial_Feedback[idIndx,1:nrow(dfx)]<-as.numeric(dfx$Feedback=="Signal")
      nfb_stan$Contingency_Feedback[idIndx,1:nrow(dfx)]<-as.numeric(dfx$InfusionNum %in% c("1","3"))
      nfb_stan$ExpRat[idIndx,1:nrow(dfx)]<-as.numeric(dfx$WillImpRespText=="Yes")
      nfb_stan$missing_choice_exp[idIndx,1:nrow(dfx)]<-as.numeric(dfx$WillImpRespText=="NaN")
      nfb_stan$ExpRat[idIndx,which(dfx$WillImpRespText=="NaN")]<-0
      if(nrow(dfx)<nfb_stan$maxTrialN){
        nfb_stan$missing_choice_exp[idIndx,nrow(dfx):nfb_stan$maxTrialN]<-1
        nfb_stan$ExpRat[idIndx,nrow(dfx):nfb_stan$maxTrialN]<-0
      }
      idIndx<-NULL
      seNum<-NULL
      seIndx<-NULL
    } else {
      nfb_stan$nTrial [idIndx,seNum]<-nrow(dfx)
      nfb_stan$SessionIndex[idIndx,seNum]<-seIndx
      nfb_stan$Infusion_f[idIndx,seNum,1:nrow(dfx)]<-as.numeric(dfx$InfusionNum)
      nfb_stan$Infusion[idIndx,seNum,1:nrow(dfx)]<-as.numeric(dfx$InfusionNum %in% c("1","2"))
      nfb_stan$Trial_Feedback[idIndx,seNum,1:nrow(dfx)]<-as.numeric(dfx$Feedback=="Signal")
      nfb_stan$Contingency_Feedback[idIndx,seNum,1:nrow(dfx)]<-as.numeric(dfx$InfusionNum %in% c("1","3"))
      nfb_stan$ExpRat[idIndx,seNum,1:nrow(dfx)]<-as.numeric(dfx$WillImpRespText=="Yes")
      nfb_stan$missing_choice_exp[idIndx,seNum,1:nrow(dfx)]<-as.numeric(dfx$WillImpRespText=="NaN")
      nfb_stan$ExpRat[idIndx,seNum,which(dfx$WillImpRespText=="NaN")]<-0
      if(nrow(dfx)<nfb_stan$maxTrialN){
        nfb_stan$missing_choice_exp[idIndx,seNum,nrow(dfx):nfb_stan$maxTrialN]<-1
        nfb_stan$ExpRat[idIndx,seNum,nrow(dfx):nfb_stan$maxTrialN]<-0
      }
      idIndx<-NULL
      seNum<-NULL
      seIndx<-NULL
    }
    
    
    
  }
  
  return(nfb_stan)
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


nfb_2Q_betaonly_1LR_nodecay_output=stan(file='stan/nfb_2Q_beta_1LR_nodecay.stan',
                                          data=nfb_stan,verbose=FALSE,save_warmup=FALSE,
                                          pars=c('Invert_Infus','Invert_ProbReinf','which_Infus','which_PorbReinf'),chains = 4,
                                          include=FALSE,iter=2000,control=list(adapt_delta=0.99,stepsize=.05))
save(nfb_2Q_betaonly_1LR_nodecay_output,file = "/Volumes/jiazhouchen/Documents/UPMC/RStation/pecina/nfb_2Q_betaonly_1LR_nodecay_output.rdata")


nfb_1Qa_beta_1LR_nodecay=stan(file='stan/nfb_1Qa_beta_1LR_nodecay.stan',
                                        data=nfb_stan,verbose=FALSE,save_warmup=FALSE,
                                        pars=c('Invert_Infus','Invert_ProbReinf','which_Infus','which_PorbReinf'),chains = 4,
                                        include=FALSE,iter=2000,control=list(adapt_delta=0.99,stepsize=.05))
save(nfb_1Qa_beta_1LR_nodecay,file = "/Volumes/jiazhouchen/Documents/UPMC/RStation/pecina/nfb_1Qa_beta_1LR_nodecay.rdata")

nfb_2Q_beta_1LR_nodecay_kappa_output=stan(file='stan/nfb_2Q_beta_1LR_nodecay_kappa.stan',
                                        data=nfb_stan,verbose=FALSE,save_warmup=FALSE,
                                        pars=c('Invert_Infus','Invert_ProbReinf','which_Infus','which_PorbReinf'),chains = 4,
                                        include=FALSE,iter=2000,control=list(adapt_delta=0.99,stepsize=.05))
save(nfb_2Q_beta_1LR_nodecay_kappa_output,file = "~/Documents/UPMC/RStation/pecina/nfb_2Q_beta_1LR_nodecay_kappa.rdata")

nfb_2Q_beta_1LR_nodecay_kappa_output=stan(file='stan/nfb_4Q_beta_1LR_nodecay_kappa.stan',
                                          data=nfb_stan,verbose=FALSE,save_warmup=FALSE,
                                          pars=c('Invert_Infus','Invert_ProbReinf','which_Infus','which_PorbReinf'),chains = 4,
                                          include=FALSE,iter=4000,control=list(adapt_delta=0.99,stepsize=.05))
save(nfb_2Q_beta_1LR_nodecay_kappa_output,file = "~/Documents/UPMC/RStation/pecina/stan/stan_output/nfb_4Q_beta_1LR_nodecay_kappa")

nfb1_4Q_beta_1LR_nodecay_kappa_output=stan(file='stan/nfb_4Q_beta_1LR_nodecay_kappa.stan',
                                          data=nfb_prep_stan(son_all,"SON1"),verbose=FALSE,save_warmup=FALSE,
                                          pars=c('Invert_Infus','Invert_ProbReinf','which_Infus','which_PorbReinf'),chains = 4,
                                          include=FALSE,iter=4000,control=list(adapt_delta=0.99,stepsize=.05))
save(nfb1_4Q_beta_1LR_nodecay_kappa_output,file = "~/Documents/UPMC/RStation/pecina/stan/stan_output/nfb1_4Q_beta_1LR_nodecay_kappa")

nfb_4Q_beta_1LR_nodecay_kappa_nopre_output=stan(file='stan/nfb_4Q_beta_1LR_nodecay_kappa_nopre.stan',
                                           data=nfb_prep_stan(son_all,"SON1"),verbose=FALSE,save_warmup=FALSE,
                                           pars=c('Invert_Infus','Invert_ProbReinf','which_Infus','which_PorbReinf'),chains = 4,
                                           include=FALSE,iter=4000,control=list(adapt_delta=0.99,stepsize=.05))
save(nfb_4Q_beta_1LR_nodecay_kappa_nopre_output,file = "~/Documents/UPMC/RStation/pecina/stan/stan_output/nfb_4Q_beta_1LR_nodecay_kappa_nopre.rdata")

nfb_4Q_beta_1LR_nodecay_kappa_nopre_newse_out=stan(file='stan/nfb_4Q_beta_1LR_nodecay_kappa_nopre_newse.stan',
                                                data=nfb_prep_stan(son_all,"SON1"),verbose=FALSE,save_warmup=FALSE,
                                                pars=c('Invert_Infus','Invert_ProbReinf','which_Infus','which_PorbReinf'),chains = 4,
                                                include=FALSE,iter=4000,control=list(adapt_delta=0.99,stepsize=.05))
save(nfb_4Q_beta_1LR_nodecay_kappa_nopre_newse_out,file = "~/Documents/UPMC/RStation/pecina/stan/stan_output/nfb_4Q_beta_1LR_nodecay_kappa_nopre_newse.rdata")

nfb_4Q_beta_1LR_nodecay_kappa_nopre_nxse_out=stan(file='stan/nfb_4Q_beta_1LR_nodecay_kappa_nopre_nxse.stan',
                                                   data=nfb_prep_stan(son_all,"SON1"),verbose=FALSE,save_warmup=FALSE,
                                                   pars=c('Invert_Infus','Invert_ProbReinf','which_Infus','which_PorbReinf'),chains = 4,
                                                   include=FALSE,iter=4000,control=list(adapt_delta=0.99,stepsize=.05))
save(nfb_4Q_beta_1LR_nodecay_kappa_nopre_nxse_out,file = "~/Documents/UPMC/RStation/pecina/stan/stan_output/nfb_4Q_beta_1LR_nodecay_kappa_nopre_nxse.rdata")



nfb_4Q_basic=stan(file='stan/nfb_4Q_basic.stan',
                 data=nfb_prep_stan(son_all,whichSON="SON1",paired=F,adminfilter="_1"),verbose=FALSE,save_warmup=FALSE,
                 pars=c('Invert_Infus','Invert_ProbReinf','which_Infus','which_PorbReinf'),chains = 4,
                include=FALSE,iter=4000,control=list(adapt_delta=0.99,stepsize=.05))
save(nfb_4Q_basic,file = "stan/stan_output/nfb_4Q_basic.rdata")


fitxr <- stan(file="stan/nfb_4Q_basic.stan",
              data = nfb_prep_stan(son_all,whichSON="SON1",paired=F,adminfilter="_1"), 
              iter=1, chains=1, seed=596858228, algorithm="Fixed_param")



