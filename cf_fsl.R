#####Con_Frame Script

#Prepare, setting up resources
rm(list = ls())
require("devtools")
devtools::install_github("DecisionNeurosciencePsychopathology/fMRI_R")
library(fslpipe)
if("dependlab" %in% installed.packages()){"GREAT, DEPENDLAB PACK IS INSTALLED"}else{devtools::install_github("PennStateDEPENdLab/dependlab")}
#Load utility functions from both sources
source('pecina_R_utility_function.R')
#devtools::source_url("https://raw.githubusercontent.com/Jiazhouchen/con_frame/master/cf_behav.R")
source("cf_behav.R")
#Setting up FSL global enviroment variables in case we are using RStudio 
fsl_2_sys_env()

#Load in behavioral data:
#boxdir<-findbox()
boxdir <- "/Volumes/bek/Box Sync"
#Setting default options
argu<-as.environment(list(nprocess=12,onlyrun=NULL,proc_id_subs=NULL,regtype=".1D",ifnuisa=FALSE,adaptive_gfeat=TRUE,
                          hig_lvl_path_filter=NULL,cluster_thresh = 3,whichttest = c("paired","onesample"),
                          group_id_sep=c('Nalt','Plac'),graphic.threshold=0.95,forcereg=FALSE,ifoverwrite_secondlvl=F,
                          cfgpath="/Volumes/bek/autopreprocessing_pipeline/Neurofeedback/con_framing.cfg",
                          regpath="/Volumes/bek/neurofeedback/sonrisa2/con_framing/regs/R_fsl_reg",func.nii.name="nfswudktm*[0-9]_[0-9].nii.gz",
                          gsub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_gfeat_general_adaptive_template.fsf",
                          ssub_outputroot="/Volumes/bek/neurofeedback/sonrisa2/con_framing/ssanalysis/fsl",
                          glvl_outputroot="/Volumes/bek/neurofeedback/sonrisa2/con_framing/grpanal/fsl",
                          templatedir="/Volumes/bek/Newtemplate_may18/fsl_mni152/MNI152_T1_2mm_brain.nii",
                          nuisa_motion=c("nuisance","motion_par"),motion_type="fd",motion_threshold="default",convlv_nuisa=F
))

#Which model to run:
M_base=FALSE
M_RTConv=FALSE
M_Value=TRUE

#Differentiate argument for different models here:
if (M_base) {
  argu$gridpath<-"grid_sc.csv"
  argu$model.name="con_framing_new_cope"
  argu$model.varinames=c("PxH",        
                         "PxF",
                         "PxN",
                         "UxH",
                         "UxF",
                         "UxN")
  argu$ssub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_cf2_template_R.fsf"
}
if (M_Value) {
  argu$gridpath<-"grid_xd.csv"
  argu$model.name="con_framing_Bias_value"
  argu$model.varinames=c("PxH",        
                         "PxF",
                         "PxN",
                         "UxH",
                         "UxF",
                         "UxN",
                         "PxH_v",        
                         "PxF_v",
                         "PxN_v",
                         "UxH_v",
                         "UxF_v",
                         "UxN_v")
  argu$ssub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_cf3_template_R.fsf"
}

#Get behavioral data
datalist<-proc_behav_cf(boxdir = boxdir,fmriproc = T)
CF_outscan<-lapply(proc_behav_cf(boxdir = boxdir,behav.list = T,inscan = F),proc_outscan_cf)
CF_P_outscan<-lapply(proc_behav_cf(boxdir = boxdir,behav.list = T,inscan = F), genProbability, 
                     condition=c("Condition"),response=c("ConditionResposne"),missresp="")
CF_P_outscan_ALL<-do.call(rbind,CF_P_outscan)
CF_P_pos<-CF_P_outscan_ALL[CF_P_outscan_ALL$resp=="7&",]
CF_prc2<-lapply(datalist, function(xz) {
  xz$singlesub->xj
  ID<-xz$singlesub$ID_CON
  ID<-gsub("_Nalt","",gsub("_Plac","",ID))
  if (length(xj)>2) {
  for (xi in 1:2) {
    x<-xj[[xi]]
    emobu<-x$Emotion
    x$Emotion<-plyr::mapvalues(x$Emotion,from = c("Happy","Neutral","Fearful"),to = c("positive","neutral","negative"))
    x$Rating_w_bias<-NA
    x$Rating<-NA
    x$Rating[x$FaceResponseText=='Positive'] <-1
    x$Rating[x$FaceResponseText=='Negative'] <-0
    for (emo in unique(CF_P_pos$Condition)) {
      if (length(CF_P_pos$p[CF_P_pos$ID==ID & CF_P_pos$Condition==emo])>0) {
        x$Rating_w_bias[which(tolower(x$Emotion)==emo)]<-as.numeric(as.character(x$Rating[which(tolower(x$Emotion)==emo)])) - CF_P_pos$p[CF_P_pos$ID==ID & CF_P_pos$Condition==emo]
      }
    }
    emobu->x$Emotion
    x->xj[[xi]]
      if (all(is.na(x$Rating_w_bias))){noscore<-T}else {noscore<-F}
  } 
  }else {noscore<-T
      xj<-NULL}
  
  if (noscore) {return(NULL)} else {
    xz$singlesub<-xj
    return(xz)}
    # else {
    #   t<-sample(x = CF_P_pos$p[CF_P_pos$Condition==emo],size = 1)
    #   x$Rating_w_bias[which(tolower(x$Emotion)==emo)]<-as.numeric(as.character(x$Rating[which(tolower(x$Emotion)==emo)])) - t
    # }
  
  
})

CF_prc2<-cleanuplist(CF_prc2)

#stop()
#Run fsl_pipe
fslpipe::fsl_pipe(argu=argu,
         prep.call.func="prep.confram", #This should be a character string that's the name of the prep proc function
         prep.call.allsub=CF_prc2 #List of ID list of arguments for prep.call.
)