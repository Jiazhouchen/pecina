###

source_script_github <- function(gitscripturl) {
  # load package
  require(RCurl)
  # read script lines from website
  script <- getURL(gitscripturl, ssl.verifypeer = FALSE, followlocation = TRUE)
  # parse lines and evaluate in the global environment
  eval(parse(text = script), envir= .GlobalEnv)
}

nfb_getdata<-function(boxdir="~/Box",grp_sep=argu$group_id_sep,proc_id_sub=argu$proc_id_subs,QCflag=runQC,verbose=T){
  son_all<-as.environment(list())
  load(file.path(boxdir,"GitHub","Nfb_task","NFB_response","SON1&2_behav_results","nfb_behav.rdata"),envir = son_all)
  son1_all<-son_all$bothSONs$SON1$df
  #Split them into mulitiple participants
  if(is.null(grp_sep)){
    son1_split<-split(son1_all,son1_all$Participant)
  } else {
    son1_split<-split(son1_all,son1_all$FullID)
    names(son1_split)<-gsub("_2$","_b",gsub("_1$","_a",names(son1_split)))
  }
  
  son1_rework<-lapply(names(son1_split),function(x) {
    son1_split[[x]]->y
    if(!is.null(grp_sep)){
      if (grepl("_a",x)){adminifilter=1}else if (grepl("_b",x)){adminifilter=2}
      
    } else if (!is.null(proc_id_sub)) {
      if (argu$proc_id_subs=="_a"){adminifilter=1}else if (argu$proc_id_subs=="_b"){adminifilter=2}
    }else {
      adminifilter=NULL
    }
    return(list(son1_single=y,adminfilter=adminifilter,QC=QCflag))
    
    
  })
  names(son1_rework)<-names(son1_split)
  if(verbose){
  message("Will run with ",length(son1_rework), " subjects.")
  message(paste(names(son1_rework),collapse = ", "))
  }
  return(son1_rework)
}

###SON1 single sub Behavr Proc function:
prep.son1<-function(son1_single = NULL,QC=F,
                    regualrvarinames=c('Participant','ColorSet','Feed1Onset','Feed2Onset','Feed3Onset','Feedback',
                                       'ImprovedOnset','ImprovedRespBin','ImprovedRespNum','ImprovedRespText','ImprovedRt',
                                       'InfOnset','Infusion','InfusionNum','J1Onset','J1Seconds','J2Onset','J2Seconds',
                                       'Jitter1','Jitter2','Run','TrialColor','TrialNum','Version','Waveform',
                                       'WillImpOnset','WillImpRespBin','WillImpRespNum','WillImpRespText',
                                       'WillImpRt','administration','subject_id','plac_ctrl','reinf_cont','plac','plac_ctrl_r','reinf_cont_r'),
                    adminfilter=1) {
  if (is.null(son1_single)) {stop("NO INPUT")}
  #print("This is current Ver.")
  if(is.null(son1_single$administration)){son1_single$administration<-son1_single$VisitType}
  
  if(QC){
    son1_single$duration<-4+2+(son1_single$Jitter1/100)+10+2+(son1_single$Jitter2/100)
    son1_single$QC_OUT<-sample(1:0,length(son1_single$TrialNum),replace = T)
    finalist<-list(QC=data.frame(event="QC",
                                 onset=son1_single$InfOnset,
                                 duration=son1_single$duration,
                                 run=son1_single$Run,
                                 trial=son1_single$TrialNum
                                 ))
    finalist[["allconcat"]]<-do.call(rbind,finalist)
    output<-list(event.list=finalist,output.df=son1_single,value=son1_single)
  } else {
  if(!is.null(adminfilter)){
  son1_single<-son1_single[which(son1_single$administration==adminfilter),]
  }
  vba<-as.list(son1_single[c(!names(son1_single) %in% regualrvarinames)])
  #vba<-addcenterscaletolist(vba)  ##Function Coming from fMRI_Dev Script
  if(nrow(son1_single)<1){return(NULL)}
  son1_single$plac_ctrl <- NA
  son1_single$plac_ctrl[son1_single$InfusionNum==1 | son1_single$InfusionNum==2] <- TRUE
  son1_single$plac_ctrl[son1_single$InfusionNum==3 | son1_single$InfusionNum==4] <- FALSE
  son1_single$plac_ctrl_r<-!son1_single$plac_ctrl
  
  #Creating a binary variable
  #son1_single$plac_ctrl_bin<-son1_single$plac_ctrl
  son1_single$plac_ctrl_bin <- NA
  son1_single$plac_ctrl_bin[which(is.na(son1_single$plac_ctrl))]<-0
  son1_single$plac_ctrl_bin[which(son1_single$plac_ctrl)]<-1
  son1_single$plac_ctrl_bin[which(!son1_single$plac_ctrl)]<-(-1)

  son1_single$inf_cali<-NA
  son1_single$inf_cali[which(son1_single$plac_ctrl)]<-1
  son1_single$inf_cali[which(!son1_single$plac_ctrl)]<-0
  
  son1_single$signal_baseline <- NA
  son1_single$signal_baseline[son1_single$Feedback=="Signal"] <- TRUE
  son1_single$signal_baseline[son1_single$Feedback=="Baseline"] <- FALSE
  son1_single$signal_baseline_r<-!son1_single$signal_baseline
  #Creating a binary variable
  son1_single$signal_baseline_bin<-NA
  son1_single$signal_baseline_bin[which(son1_single$signal_baseline)]<-1
  son1_single$signal_baseline_bin[which(!son1_single$signal_baseline)]<-(-1)
  
  son1_single$contingency<-0
  son1_single$contingency[which(son1_single$InfusionNum %in% c("1","3"))]<-1
  son1_single$contingency[which(son1_single$InfusionNum %in% c("2","4"))]<-(-1)
  
  son1_single$condizeroone<-NA
  son1_single$condizeroone[which(son1_single$signal_baseline)]<-1
  son1_single$condizeroone[which(!son1_single$signal_baseline)]<-0
  
  son1_single$ExpRat<-NA
  son1_single$ExpRat[son1_single$WillImpRespText=="Yes"] <- TRUE
  son1_single$ExpRat[son1_single$WillImpRespText=="No"] <- FALSE
  
  son1_single$ExpRat_pm<-NA
  son1_single$ExpRat_pm[son1_single$WillImpRespText=="Yes"] <- (1)
  son1_single$ExpRat_pm[son1_single$WillImpRespText=="No"] <- (-1)
  
  son1_single$ExpRat_bin<-NA
  son1_single$ExpRat_bin[which(is.na(son1_single$ExpRat))]<-0
  son1_single$ExpRat_bin[which(son1_single$ExpRat)]<-1
  son1_single$ExpRat_bin[which(!son1_single$ExpRat)]<-(-1)

  son1_single$MoodRat<-NA
  son1_single$MoodRat[son1_single$ImprovedRespText=="Yes"] <- TRUE
  son1_single$MoodRat[son1_single$ImprovedRespText=="No"] <- FALSE
  
  son1_single$MoodRat_bin<-NA
  son1_single$MoodRat_bin[which(is.na(son1_single$MoodRat))]<-0
  son1_single$MoodRat_bin[which(son1_single$MoodRat)]<-1
  son1_single$MoodRat_bin[which(!son1_single$MoodRat)]<-(-1)
  
  son1_single$PE_correct<-NA
  son1_single$PE_correct[which(is.na(son1_single$ExpRat))]<-0
  son1_single$PE_correct[which(son1_single$ExpRat == son1_single$signal_baseline)] <- 0
  son1_single$PE_correct[which(!son1_single$ExpRat & son1_single$signal_baseline)]<-1
  son1_single$PE_correct[which(son1_single$ExpRat & !son1_single$signal_baseline)]<- (-1)
  
  son1_single$PE_congruent<-NA
  son1_single$PE_congruent[which(is.na(son1_single$ExpRat))]<-0
  son1_single$PE_congruent[which(son1_single$ExpRat == son1_single$signal_baseline)] <- 1
  son1_single$PE_congruent[which(son1_single$ExpRat != son1_single$signal_baseline)]<- (-1)
  
  son1_single$MoodRat_Miss<-FALSE
  son1_single$MoodRat_Miss[which(is.na(son1_single$MoodRat))]<-TRUE
  
  son1_single$ExpRat_Miss<-FALSE
  son1_single$ExpRat_Miss[which(is.na(son1_single$ExpRat))]<-TRUE
  
  son1_single$true_plac<- (0)
  son1_single$true_plac[son1_single$plac_ctrl & son1_single$signal_baseline]<-1
  
  son1_single$plac_highreinf<- (0)
  son1_single$plac_highreinf[son1_single$plac_ctrl & son1_single$contingency==1]<-1
  son1_single$plac_highreinf[!son1_single$plac_ctrl & son1_single$contingency==1]<-(-1)
  
  son1_single$reinf_withplac<- (0)
  son1_single$reinf_withplac[son1_single$plac_ctrl & son1_single$contingency==1]<-1
  son1_single$reinf_withplac[son1_single$plac_ctrl & !son1_single$contingency==1]<-(-1)
  
  son1_single$plac_contin<- (0)
  son1_single$plac_contin<-(son1_single$plac_ctrl_bin*son1_single$contingency)*(-1)
  
  
  son1_single$plac_continson1_single$Inf_FbOnly<- (0)
  son1_single$Inf_FbOnly[son1_single$plac_ctrl & son1_single$signal_baseline]<-1
  son1_single$Inf_FbOnly[(!son1_single$plac_ctrl) & son1_single$signal_baseline]<- (-1)
  
  son1_single$Inf_NoFbOnly<- (0)
  son1_single$Inf_NoFbOnly[son1_single$plac_ctrl & (!son1_single$signal_baseline)]<-1
  son1_single$Inf_NoFbOnly[(!son1_single$plac_ctrl) & (!son1_single$signal_baseline)]<- (-1)
  
  
  son1_single$Fb_InfOnly<- (0)
  son1_single$Fb_InfOnly[son1_single$plac_ctrl & son1_single$signal_baseline]<-1
  son1_single$Fb_InfOnly[son1_single$plac_ctrl & (!son1_single$signal_baseline)]<- (-1)
  
  son1_single$Fb_NoInfOnly<- (0)
  son1_single$Fb_NoInfOnly[(!son1_single$plac_ctrl) & son1_single$signal_baseline]<-1
  son1_single$Fb_NoInfOnly[(!son1_single$plac_ctrl) & (!son1_single$signal_baseline)]<- (-1)
  
  for(cond in 1:4){
    son1_single[[paste0("cond",cond)]]<-0
    son1_single[[paste0("cond",cond)]][as.character(son1_single$InfusionNum)==as.character(cond)]<-1
  }
  #son1_single$LRPE<- as.numeric(as.character(son1_single$oneLR_fixD_oneK_LR_1)) * as.numeric(as.character(son1_single$oneLR_fixD_oneK_PE))
  #son1_single$oneLR_fixD_oneK_PE_abs<-abs(son1_single$oneLR_fixD_oneK_PE)
  #son1_single$oneLR_fixD_oneK_PE_abs_neg<-abs(son1_single$oneLR_fixD_oneK_PE)
  vba<-as.list(son1_single[c(!names(son1_single) %in% regualrvarinames)])
  #Add taskness variables to value
  vba$plac_ctrl<-son1_single$plac_ctrl
  vba$plac_ctrl_r<-son1_single$plac_ctrl_r
  vba$plac_ctrl_bin<-son1_single$plac_ctrl_bin
  vba$signal_baseline<-son1_single$signal_baseline
  vba$signal_baseline_r<-son1_single$signal_baseline_r
  vba$signal_baseline_bin<-son1_single$signal_baseline_bin
  vba$MoodRat<-son1_single$MoodRat
  vba$MoodRat_bin<-son1_single$MoodRat_bin
  vba$ExpRat<-son1_single$ExpRat
  vba$ExpRat_bin<-son1_single$ExpRat_bin
  #vba$twoLR_fixD_oneK_vt1_centerscaled<-as.numeric(scale(son1_single$twoLR_fixD_oneK_vt1,center=T))
  #vba$twoLR_fixD_oneK_PEshifted_centerscaled<-as.numeric(scale(son1_single$twoLR_fixD_oneK_PEshifted,center=T))
  #vba$oneLR_fixD_oneK_vt1_centerscaled<-as.numeric(scale(son1_single$oneLR_fixD_oneK_vt1,center=T))
  #vba$oneLR_fixD_oneK_PEshifted_centerscaled<-as.numeric(scale(son1_single$oneLR_fixD_oneK_PEshifted,center=T))
  #vba$PE_congruent_centerscaled<-scale(son1_single$PE_congruent,center = T)
  
  #vba$ExpRat_bin<-plyr::mapvalues(x = son1_single$WillImpRespBin,from = c(0:1),to = c(-1,1),warn_missing = F)
  #vba$MoodRat_bin<-plyr::mapvalues(x = son1_single$ImprovedRespBin,from = c(0:1),to = c(-1,1),warn_missing = F)
  
  #Safe guard this from NaNs:
  son1_single$WillImpRt[which(is.na(son1_single$WillImpRt))]<-0
  son1_single$ImprovedRt[which(is.na(son1_single$ImprovedRt))]<-0
  
  if(any(grepl("TD",names(son1_single)))){
    #Let's get the PE;
    son1_single$Acculated_height<-son1_single$oneLR_fixD_oneK_TD_TDError + son1_single$oneLR_fixD_oneK_TD_Value_9
  }
  
  
  finalist<-list(infusion=data.frame(event="infusion",
                                     onset=son1_single$InfOnset,
                                     duration=4, #Since it's designed for 4 seconds let's just do 4 seconds
                                     run=son1_single$Run,
                                     trial=son1_single$TrialNum),
                 TD=data.frame(event="TD",
                                     onset=son1_single$InfOnset,
                                     duration=son1_single$Feed2Onset - son1_single$InfOnset, #Since it's designed for 4 seconds let's just do 4 seconds
                                     run=son1_single$Run,
                                     trial=son1_single$TrialNum),
                 feedback_PE = data.frame(event="feedback_PE",
                               onset=son1_single$Feed2Onset,
                               duration=(son1_single$Feed2Onset - son1_single$Feed1Onset)+1, #Since it's designed for 4 seconds let's just do 4 seconds
                               run=son1_single$Run,
                               trial=son1_single$TrialNum),
                 TD_error = data.frame(event="TD_error",
                                       onset=son1_single$Feed1Onset,
                                       duration=1, #Since it's designed for 4 seconds let's just do 4 seconds
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
                                   trial=son1_single$TrialNum),
                 wholetrial=data.frame(event="wholetrial",
                                     onset=son1_single$InfOnset,
                                     duration=son1_single$WillImpOnset - son1_single$InfOnset,
                                     run=son1_single$Run,
                                     trial=son1_single$TrialNum),
                 feedback_bl=data.frame(event="feedback_bl",
                                       onset=son1_single$Feed1Onset,
                                       duration=son1_single$Feed2Onset - son1_single$Feed1Onset,
                                       run=son1_single$Run,
                                       trial=son1_single$TrialNum)
                 )
  for (i in 1:length(finalist)) {
    if (i==1) {ktz<-finalist[[i]]} else {
      ktz<-rbind(ktz,finalist[[i]])}
  }
  finalist[["allconcat"]]<-ktz
  output<-list(event.list=finalist,output.df=son1_single,value=vba)
  }
  return(output)
}


##Con_Frame Prep
prep.confram<-function(singlesub=NULL) {
  tobind<-singlesub[grepl("Order[0-9]",names(singlesub))]
  conframe<-do.call(rbind,tobind)
  
  #For Rating bias model no use;
  #conframe$Rating_w_bias[is.na(conframe$Rating_w_bias)]<-0
  
  #For 6 regs model
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
  
  #For the GM Models:
  conframe$EmotionReg<- 0
  conframe$EmotionReg[conframe$Emotion=="Happy"]<- 1
  conframe$EmotionReg[conframe$Emotion=="Neutral"]<- (-1)
  
  conframe$EmotionHappy<- 0
  conframe$EmotionHappy[conframe$Emotion=="Happy"]<- 1
  
  conframe$EmotionFearful<- 0
  conframe$EmotionFearful[conframe$Emotion=="Fearful"]<- 1
  
  conframe$ContextReg<- 0
  conframe$ContextReg[conframe$Context=="Pleasant"]<- 1
  conframe$ContextReg[conframe$Context=="Unpleasant"]<- (-1)
  
  conframe$ContextHappy<- 0
  conframe$ContextHappy[conframe$Context=="Pleasant" & conframe$Emotion=="Happy"]<- 1
  conframe$ContextHappy[conframe$Context=="Unpleasant" & conframe$Emotion=="Happy"]<- (-1)
  
  conframe$ContextFearful<- 0
  conframe$ContextFearful[conframe$Context=="Pleasant" & conframe$Emotion=="Fearful"]<- 1
  conframe$ContextFearful[conframe$Context=="Unpleasant" & conframe$Emotion=="Fearful"]<- (-1)
  



  conframe$Congruent<-0
  conframe$Congruent[conframe$Context=="Unpleasant" & conframe$Emotion=="Fearful"]<- 1
  conframe$Congruent[conframe$Context=="Pleasant" & conframe$Emotion=="Happy"]<- 1
  
  conframe$Incongruent<-0
  conframe$Incongruent[conframe$Context=="Unpleasant" & conframe$Emotion=="Happy"]<- 1
  conframe$Incongruent[conframe$Context=="Pleasant" & conframe$Emotion=="Fearful"]<- 1

  conframe$NewtwoEmotion<- 0
  conframe$NewtwoEmotion[conframe$Emotion %in% "Happy"]<- 1
  conframe$NewtwoEmotion[conframe$Emotion %in% c("Neutral","Fearful")]<- (-1)

  conframe$Incongruent <- 1 
  conframe$Incongruent[conframe$Context=="Unpleasant" & conframe$Emotion=="Fearful"]<- 0
  conframe$Incongruent[conframe$Context=="Pleasant" & conframe$Emotion=="Happy"]<- 0
  
  conframe$IncongruentPN <- 1
  conframe$IncongruentPN[conframe$Context=="Unpleasant" & conframe$Emotion=="Fearful"]<- (-1)
  conframe$IncongruentPN[conframe$Context=="Pleasant" & conframe$Emotion=="Happy"]<- (-1)
  
  conframe$NewtwoEmotionZO<- 0
  conframe$NewtwoEmotionZO[conframe$Emotion %in% "Happy"]<- 1
  #conframe$NewtwoEmotionZO[conframe$Emotion %in% c("Neutral","Fearful")]<- (0)

  conframe$InteractionNewEmoInc <- conframe$Incongruent * conframe$NewtwoEmotion
  conframe$InteractionNewEmoIncNG <- conframe$IncongruentPN * conframe$NewtwoEmotionZO
  
  finalist<-list(trial=data.frame(event="trial",
                                  onset=conframe$ContextOnset,
                                  duration=5,
                                  run=conframe$Order,
                                  trial=conframe$Trial),
                 face=data.frame(event="face",
                                 onset=conframe$FaceOnset,
                                 duration=conframe$FaceRt,
                                 run=conframe$Order,
                                 trial=conframe$Trial)
  )
  
  finalist[["allconcat"]]<-finalist$trial
  vba<-as.list(conframe)
  
  
  output<-list(event.list=finalist,output.df=conframe,value=vba)
  return(output)
}




