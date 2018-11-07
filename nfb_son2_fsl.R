rm(list = ls())
require("devtools")
devtools::install_github("DecisionNeurosciencePsychopathology/fMRI_R",force = T)
library(fslpipe)
if("dependlab" %in% installed.packages()){"GREAT, DEPENDLAB PACK IS INSTALLED"}else{devtools::install_github("PennStateDEPENdLab/dependlab")}
source('pecina_R_utility_function.R')

#Setting up FSL global enviroment variables in case we are using RStudio
fsl_2_sys_env()

singlesub<-FALSE
######
#Actual arguments for each model. Should follow template: github.com/DecisionNeurosciencePsychopathology/fMRI_R
####BE AWARE!
argu<-as.environment(list(nprocess=4,onlyrun=NULL,forcereg=F,cfgpath="/Volumes/bek/autopreprocessing_pipeline/Neurofeedback/nfb_son2.cfg",
                          regpath="/Volumes/bek/neurofeedback/sonrisa2/nfb/regs/R_fsl_reg",func.nii.name="nfswudktm*[0-9]_[0-9].nii.gz",
                          group_id_sep=c('Nalt','Plac'),regtype=".1D", convlv_nuisa=FALSE,adaptive_gfeat=TRUE,adaptive_ssfeat=TRUE,randomize_demean=FALSE,
                          gsub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_gfeat_general_adaptive_template.fsf",
                          ssub_outputroot="/Volumes/bek/neurofeedback/sonrisa2/nfb/ssanalysis/fsl",whichttest = c("paired","onesample"),
                          glvl_outputroot="/Volumes/bek/neurofeedback/sonrisa2/nfb/grpanal/fsl",
                          templatedir="/Volumes/bek/Newtemplate_may18/fsl_mni152/MNI152_T1_2mm_brain.nii",
                          ssub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_ssfeat_general_adaptive_template_R.fsf",
                          glvl_output="/Volumes/bek/neurofeedback/sonrisa2/nfb/grpanal/fsl",ifoverwrite_secondlvl=FALSE,hig_lvl_path_filter=NULL,
                          graphic.threshold=0.95,nuisa_motion=c("nuisance","motion_par"),motion_type="fd", motion_threshold="default",convlv_nuisa=F))
#DO NOT PROVIDE THOSE TWO AND IT WILL BE FINE;
#argu$thresh_cluster_extent<-3.1 
#argu$thresh_cluster_mass<-3.1
argu$randomize_p_threshold<-0.001
argu$randomize_thresholdingways<-c("tfce","voxel-based","cluster-based-mass","cluster-based-extent")
argu$ss_zthreshold<-3.2  #This controls the single subject z threshold (if enabled in template)
argu$ss_pthreshold<-0.05 #This controls the single subject p threshold (if enabled in template)

alignment1<-F
alignment2<-F
alignment3<-T
alignment4<-F
alignment5<-F
alignment6<-F
if (alignment1) {
  argu$model.name="alignment1_evtmax"
  argu$gridpath="/Volumes/bek/neurofeedback/scripts/pecina/grid_alignment1.csv"
}
if (alignment2) {
  argu$model.name="alignment2"
  argu$gridpath="/Volumes/bek/neurofeedback/scripts/pecina/grid_alignment2.csv"
}
if (alignment3) {
  argu$model.name="alignment3a"
  argu$gridpath="/Volumes/bek/neurofeedback/scripts/pecina/grid_alignment3a.csv"
}
if (alignment4) {
  argu$model.name="alignment4"
  argu$gridpath="/Volumes/bek/neurofeedback/scripts/pecina/grid_alignment4.csv"
}
if (alignment5) {
  argu$model.name="alignment5"
  argu$gridpath="/Volumes/bek/neurofeedback/scripts/pecina/grid_alignment5.csv"
}
if (alignment6) {
  argu$model.name="alignment6"
  argu$gridpath="/Volumes/bek/neurofeedback/scripts/pecina/grid_alignment6.csv"
}

###################
##Official Start:##
#Supposedly you shouldn't need to change anything down below:
#If you are just switching models
###################

boxdir <- "/Volumes/bek/Box Sync"

son2_all <- read.csv(file.path(boxdir,"GitHub","Nfb_task","NFB_response","SON1&2_behav_results","son2_all.csv"))
son2_all$FULLID<-paste(son2_all$Participant,son2_all$administration,sep = "_")
#Split them into mulitiple participants
son2_split<-split(son2_all,son2_all$FULLID)
son2_rework<-lapply(names(son2_split),function(x) {
  son2_split[[x]]->y
  return(list(son1_single=y,adminfilter=NULL))
})
names(son2_rework)<-names(son2_split)

if(singlesub){
  son2_rework<-son2_rework["SON2_018_Plac"]
}

print(names(son2_rework))
fsl_pipe(
  argu=argu, #This is the arguments environment, each model should have a different one;
  prep.call.func="prep.son1", #This should be a character string that's the name of the prep proc function
  prep.call.allsub=son2_rework #List of ID list of arguments for prep.call.
)
