############################################
####### New Functions for regressor ########
############################################
#Do a source script from git function
if (file.exists("pecina_R_utility_function.R")){
  source("pecina_R_utility_function.R")
} else {
  devtools::source_url("https://raw.githubusercontent.com/Jiazhouchen/pecina/master/pecina_R_utility_function.R")
}

#Source the fMRI helper functions
source_script_github("https://raw.githubusercontent.com/DecisionNeurosciencePsychopathology/fMRI_R/master/dnpl_utility.R")

fsl_2_sys_env()
#Load son1's sepcific functions here:
prep.son1<-function(son1_single = NULL,
                    regualrvarinames=c('Participant','ColorSet','Feed1Onset','Feed2Onset','Feed3Onset','Feedback',
                                       'ImprovedOnset','ImprovedRespBin','ImprovedRespNum','ImprovedRespText','ImprovedRt',
                                       'InfOnset','Infusion','InfusionNum','J1Onset','J1Seconds','J2Onset','J2Seconds',
                                       'Jitter1','Jitter2','Run','TrialColor','TrialNum','Version','Waveform',
                                       'WillImpOnset','WillImpRespBin','WillImpRespNum','WillImpRespText',
                                       'WillImpRt','administration','subject_id','plac_ctrl','reinf_cont','plac','plac_ctrl_r','reinf_cont_r'),
                    adminfilter=1) {
  if (is.null(son1_all)) {stop("NO INPUT")}
  son1_single<-son1_single[which(son1_single$administration==adminfilter),]
  son1_single$plac_ctrl[son1_single$InfusionNum==1 | son1_single$InfusionNum==2] <- TRUE
  son1_single$plac_ctrl[son1_single$InfusionNum==3 | son1_single$InfusionNum==4] <- FALSE
  son1_single$plac_ctrl_r<-!son1_single$plac_ctrl
  
  son1_single$reinf_cont <- NA
  son1_single$reinf_cont[son1_single$InfusionNum==1 | son1_single$InfusionNum==3] <- TRUE
  son1_single$reinf_cont[son1_single$InfusionNum==2 | son1_single$InfusionNum==4] <- FALSE
  son1_single$reinf_cont_r<-!son1_single$reinf_cont
  
  son1_single$InfDur<-son1_single$WillImpOnset - son1_single$InfOnset
  son1_single$FeedDur<-son1_single$ImprovedOnset - son1_single$Feed2Onset
  
  vba<-as.list(son1_single[c(!names(son1_all) %in% regualrvarinames)])
  vba<-addcenterscaletolist(vba)  ##Function Coming from fMRI_Dev Script
  #Add taskness variables to value
  vba$plac_ctrl<-son1_single$plac_ctrl
  vba$plac_ctrl_r<-son1_single$plac_ctrl_r
  vba$reinf_cont<-son1_single$reinf_cont
  vba$reinf_cont_r<-son1_single$reinf_cont_r
  
  finalist<-list(infusion=data.frame(event="infusion",
                                     onset=son1_single$InfOnset,
                                     duration=son1_single$WillImpOnset - son1_single$InfOnset,
                                     run=son1_single$Run,
                                     trial=son1_single$TrialNum),
                 feedback=data.frame(event="feedback",
                                     onset=son1_single$Feed2Onset,
                                     duration=son1_single$ImprovedOnset - son1_single$Feed2Onset,
                                     run=son1_single$Run,
                                     trial=son1_single$TrialNum))
  for (i in 1:length(finalist)) {
    if (i==1) {ktz<-finalist[[i]]} else {
      ktz<-rbind(ktz,finalist[[i]])}
  }
  finalist[["allconcat"]]<-ktz
  output<-list(event.list=finalist,output.df=son1_single,value=vba)
}



#Get Data
if (Sys.getenv("RSTUDIO_USER_IDENTITY")=="jiazhouchen") {boxdir <- "/Users/jiazhouchen/Box Sync"
} else {boxdir<-system("find ~ -iname 'Box*' -maxdepth 2 -type d",intern = T)}

son1_all <- read.csv(file.path(boxdir,"GitHub","Nfb_task","NFB_response","SON1&2_behav_results","son1_all.csv"))

allsub.design<-as.environment(list())

for (xid in unique(son1_all$Participant)) {
  singlesub<-son1_all[which(son1_all$Participant %in% xid),]
tryCatch(
  {
    do.all.subjs(
    tid=xid,
    do.prep.call="prep.son1",
    do.prep.arg=list(son1_single=singlesub),
    cfgpath="/Volumes/bek/autopreprocessing_pipeline/Neurofeedback/nfb.cfg",
    regpath="/Volumes/bek/neurofeedback/sonrisa1/nfb/regs/R_fsl_reg/",
    gridpath="grid.csv",
    func.nii.name="swudktm*[0-9].nii.gz",
    proc_id_subs="_a",    #Put "" for nothing.
    wrt.timing=c("convolved", "FSL"),
    model.name="PE_8C_reg_by_vol",
    model.varinames=c("infusion",         
                      "noinfusion",
                      "feedback",
                      "nofeedback",
                      "twoLRPE_CS_reinf_cont",
                      "twoLRPE_CS_reinf_cont_r",
                      "twoLRValueShifted_CS_plac_ctrl",
                      "twoLRValueShifted_CS_plac_ctrl_r"),
    assigntoenvir=allsub.design)
  },error=function(x) {}
)
}
  
"/Volumes/bek/neurofeedback/scripts/fsl/fsl_8C_per_run_PE.fsf"


