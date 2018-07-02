############################################
####### New Functions for regressor ########
############################################
#Do a source script from git function
source_script_github <- function(gitscripturl) {
  # load package
  require(RCurl)
  # read script lines from website
  script <- getURL(gitscripturl, ssl.verifypeer = FALSE, followlocation = TRUE)
  # parse lines and evaluate in the global environment
  eval(parse(text = script), envir= .GlobalEnv)
}

#Source the general generate evenet signals functions
source_script_github("https://raw.githubusercontent.com/Jiazhouchen/fMRI_R/master/gen_eventsignal.R")

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

#Here will start the single subject loop; but for now, temporaily do SON1_002;
"SON1_002"->tid
son1_single<-son1_all[which(son1_all$Participant %in% tid),]

#Prep the data into generally acceptable output object;
output<-prep.son1(son1_single = son1_single)

#Generate signal with make signal with grid function (grid.csv need to be in working directory or specified otherwise)
signals<-makesignalwithgrid(outputdata = output,add_taskness = T)

#Create this model using centered scaled twoLR vba output and taskness regressors

model.cs.twoLR<-list(
  #Add Taskness Regressors:
  infusion=signals$infusion, 
  noinfusion=signals$noinfusion,
  feedback=signals$feedback,
  nofeedback=signals$nofeedback,
  #Add VBA output Regressors:
  PEreinf=signals$twoLRPE_CS_reinf_cont,
  PEnoreinf=signals$twoLRPE_CS_reinf_cont_r,
  ValueInfus=signals$twoLRValueShifted_CS_plac_ctrl,
  ValueNoInfus=signals$twoLRValueShifted_CS_plac_ctrl_r)

model.cs.twoLR.alt<-list(
  #Add Taskness Regressors:
  Infusion=signals$infusion_evt, 
  ifinfusion=signals$infusion,
  Feedback=signals$feedback_evt,
  iffeedback=signals$feedback,
  #Add VBA output Regressors:
  PE=signals$twoLRPE_CS,
  Value=signals$twoLRValueShifted_CS)

model.cs.twoLR.all<-list(
  #Add Taskness Regressors:
  yesinfusion=signals$infusion, 
  noinfusion=signals$noinfusion,
  yesfeedback=signals$feedback,
  nofeedback=signals$nofeedback,
  Infusion=signals$infusion_evt,
  Feedback=signals$feedback_evt,
  #Add VBA output Regressors:
  PEreinf=signals$twoLRPE_CS_reinf_cont,
  PEnoreinf=signals$twoLRPE_CS_reinf_cont_r,
  ValueInfus=signals$twoLRValueShifted_CS_plac_ctrl,
  ValueNoInfus=signals$twoLRValueShifted_CS_plac_ctrl_r)

#Use Michael's package to generate design matrix and correlation graph;
design.cs.twoLR.all<-dependlab::build_design_matrix(
                                       events = output$event.list$allconcat, #Load the task info
                                       signals = model.cs.twoLR.all,     #Load the Model
                                       write_timing_files = c("convolved", "FSL"), #Output timing files to FSL style
                                       tr=1.0,                      #tr=1 second, maybe need to double check, I'm kinda sure....
                                       output_directory = getwd(), #Where to output the timing files, default is the working directory
                                       nuisance_regressors = NULL #Maybe could add in nuisance_regressors from pre-proc
                                       )







