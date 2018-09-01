###

source_script_github <- function(gitscripturl) {
  # load package
  require(RCurl)
  # read script lines from website
  script <- getURL(gitscripturl, ssl.verifypeer = FALSE, followlocation = TRUE)
  # parse lines and evaluate in the global environment
  eval(parse(text = script), envir= .GlobalEnv)
}

###SON1 single sub Behavr Proc function:
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
  son1_single$plac_ctrl <- NA
  son1_single$plac_ctrl[son1_single$InfusionNum==1 | son1_single$InfusionNum==2] <- TRUE
  son1_single$plac_ctrl[son1_single$InfusionNum==3 | son1_single$InfusionNum==4] <- FALSE
  son1_single$plac_ctrl_r<-!son1_single$plac_ctrl
  #Creating a binary variable
  son1_single$plac_ctrl_bin<-son1_single$plac_ctrl
  son1_single$plac_ctrl_bin[which(son1_single$plac_ctrl_bin)]<-1
  son1_single$plac_ctrl_bin[which(!son1_single$plac_ctrl_bin)]<-(-1)

  
  son1_single$signal_baseline <- NA
  son1_single$signal_baseline[son1_single$Feedback=="Signal"] <- TRUE
  son1_single$signal_baseline[son1_single$Feedback=="Baseline"] <- FALSE
  son1_single$signal_baseline_r<-!son1_single$signal_baseline
  #Creating a binary variable
  son1_single$signal_baseline_bin<-son1_single$signal_baseline
  son1_single$signal_baseline_bin[which(son1_single$signal_baseline_bin)]<-1
  son1_single$signal_baseline_bin[which(!son1_single$signal_baseline_bin)]<-(-1)
  vba<-as.list(son1_single[c(!names(son1_all) %in% regualrvarinames)])
  vba<-addcenterscaletolist(vba)  ##Function Coming from fMRI_Dev Script
  #Add taskness variables to value
  vba$plac_ctrl<-son1_single$plac_ctrl
  vba$plac_ctrl_r<-son1_single$plac_ctrl_r
  vba$plac_ctrl_bin<-son1_single$plac_ctrl_bin
  vba$signal_baseline<-son1_single$signal_baseline
  vba$signal_baseline_r<-son1_single$signal_baseline_r
  vba$signal_baseline_bin<-son1_single$signal_baseline_bin
  
  vba$ExpRat_bin<-plyr::mapvalues(x = son1_single$WillImpRespBin,from = c(0:1),to = c(-1,1),warn_missing = F)
  vba$MoodRat_bin<-plyr::mapvalues(x = son1_single$ImprovedRespBin,from = c(0:1),to = c(-1,1),warn_missing = F)
  #Safe guard this from NaNs:
  son1_single$WillImpRt[which(is.na(son1_single$WillImpRt))]<-2
  son1_single$ImprovedRt[which(is.na(son1_single$ImprovedRt))]<-2
  
  vba$ExpRat_bin[which(is.na(vba$ExpRat_bin))]<-0
  vba$MoodRat_bin[which(is.na(vba$MoodRat_bin))]<-0
  
  finalist<-list(infusion=data.frame(event="infusion",
                                     onset=son1_single$InfOnset,
                                     duration=son1_single$WillImpOnset - son1_single$InfOnset,
                                     run=son1_single$Run,
                                     trial=son1_single$TrialNum),
                 feedback=data.frame(event="feedback",
                                     onset=son1_single$Feed2Onset,
                                     duration=son1_single$ImprovedOnset - son1_single$Feed2Onset,
                                     run=son1_single$Run,
                                     trial=son1_single$TrialNum),
                 feedback_up=data.frame(event="feedback_up",
                                     onset=son1_single$Feed2Onset,
                                     duration=son1_single$Feed3Onset - son1_single$Feed2Onset,
                                     run=son1_single$Run,
                                     trial=son1_single$TrialNum),
                 infusion_whole=data.frame(event="infusion_whole",
                                     onset=son1_single$InfOnset,
                                     duration=son1_single$WillImpOnset - son1_single$InfOnset + son1_single$WillImpRt,
                                     run=son1_single$Run,
                                     trial=son1_single$TrialNum),
                 feedback_whole=data.frame(event="feedback_whole",
                                     onset=son1_single$Feed2Onset,
                                     duration=son1_single$ImprovedOnset - son1_single$Feed2Onset + son1_single$ImprovedRt,
                                     run=son1_single$Run,
                                     trial=son1_single$TrialNum),
                 ExpRat=data.frame(event="ExpRat",
                                     onset=son1_single$WillImpOnset,
                                     duration=son1_single$WillImpRt,
                                     run=son1_single$Run,
                                     trial=son1_single$TrialNum),
                 MoodRat=data.frame(event="MoodRat",
                                   onset=son1_single$ImprovedOnset,
                                   duration=son1_single$ImprovedRt,
                                   run=son1_single$Run,
                                   trial=son1_single$TrialNum))
  for (i in 1:length(finalist)) {
    if (i==1) {ktz<-finalist[[i]]} else {
      ktz<-rbind(ktz,finalist[[i]])}
  }
  finalist[["allconcat"]]<-ktz
  output<-list(event.list=finalist,output.df=son1_single,value=vba)
}


##Con_Frame Prep
prep.confram<-function(singlesub=NULL) {
  conframe<-rbind(singlesub[[1]],singlesub[[2]])
  
  #PxH
  conframe$PxH<-FALSE
  conframe$PxH[conframe$Context=="Pleasant" & conframe$Emotion=="Happy"]<-TRUE
  #PxF
  conframe$PxF<-FALSE
  conframe$PxF[conframe$Context=="Pleasant" & conframe$Emotion=="Fearful"]<-TRUE
  #PxN
  conframe$PxN<-FALSE
  conframe$PxN[conframe$Context=="Pleasant" & conframe$Emotion=="Neutral"]<-TRUE
  #UxH
  conframe$UxH<-FALSE
  conframe$UxH[conframe$Context=="Unpleasant" & conframe$Emotion=="Happy"]<-TRUE
  #UxF
  conframe$UxF<-FALSE
  conframe$UxF[conframe$Context=="Unpleasant" & conframe$Emotion=="Fearful"]<-TRUE
  #UxN
  conframe$UxN<-FALSE
  conframe$UxN[conframe$Context=="Unpleasant" & conframe$Emotion=="Neutral"]<-TRUE
  
  finalist<-list(trial=data.frame(event="trial",
                                  onset=conframe$ContextOnset,
                                  duration=conframe$Duration,
                                  run=conframe$Order,
                                  trial=conframe$Trial),
                 face=data.frame(event="face",
                                 onset=conframe$FaceOnset,
                                 duration=conframe$FaceRt,
                                 run=conframe$Order,
                                 trial=conframe$Trial)
  )
  finalist$allconcat<-finalist$trial
  vba<-list(PxH=conframe$PxH,
            PxF=conframe$PxF,
            PxN=conframe$PxN,
            UxH=conframe$UxH,
            UxF=conframe$UxF,
            UxN=conframe$UxN)
  
  
  output<-list(event.list=finalist,output.df=conframe,value=vba)
  return(output)
}




