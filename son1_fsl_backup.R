############################################
####### New Functions for regressor ########
############################################
#Do a source utility scripts from git (when we have enough we will make a function out of it...)
#Check required packages:
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
argu<-as.environment(list(nprocess=8,onlyrun=NULL,forcereg=F,cfgpath="/Volumes/bek/autopreprocessing_pipeline/Neurofeedback/nfb.cfg",
                          regpath="/Volumes/bek/neurofeedback/sonrisa1/nfb/regs/R_fsl_reg",func.nii.name="nfswudktm*[0-9]_[0-9].nii.gz",
                          proc_id_subs="_a",regtype=".1D", convlv_nuisa=FALSE,adaptive_gfeat=TRUE,adaptive_ssfeat=TRUE,randomize_demean=FALSE,
                          gsub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_gfeat_general_adaptive_template.fsf",
                          ssub_outputroot="/Volumes/bek/neurofeedback/sonrisa1/nfb/ssanalysis/fsl",
                          glvl_outputroot="/Volumes/bek/neurofeedback/sonrisa1/nfb/grpanal/fsl",
                          templatedir="/Volumes/bek/Newtemplate_may18/fsl_mni152/MNI152_T1_2mm_brain.nii",
                          ssub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_ssfeat_general_adaptive_template_R.fsf",
                          glvl_output="/Volumes/bek/neurofeedback/sonrisa1/nfb/grpanal/fsl",ifoverwrite_secondlvl=FALSE,hig_lvl_path_filter=NULL,
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
  argu$model.name="alignment3c"
  argu$gridpath="/Volumes/bek/neurofeedback/scripts/pecina/grid_alignment3c.csv"
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

son1_all <- read.csv(file.path(boxdir,"GitHub","Nfb_task","NFB_response","SON1&2_behav_results","son1_all.csv"))
#Split them into mulitiple participants
son1_split<-split(son1_all,son1_all$Participant)
son1_rework<-lapply(names(son1_split),function(x) {
  son1_split[[x]]->y
  return(list(son1_single=y))
})
names(son1_rework)<-names(son1_split)

if(singlesub){
  son1_rework<-son1_rework["SON1_018"]
}
#stop()
 print(names(son1_rework))
fsl_pipe(
    argu=argu, #This is the arguments environment, each model should have a different one;
    prep.call.func="prep.son1", #This should be a character string that's the name of the prep proc function
    prep.call.allsub=son1_rework #List of ID list of arguments for prep.call.
    )

##ROI
if(F) {
  alignment1_roi<-roi_getvalue(rootdir=argu$ssub_outputroot,grproot=argu$glvl_outputroot,modelname="alignment1_evtmax",
                               basemask="tstat",corrp_mask="tfce",saveclustermap=TRUE,Version="tfce0.95",corrmaskthreshold=0.95,
                               roimaskthreshold=0.0001, voxelnumthres=10, clustertoget=NULL,copetoget=NULL,maxcore=6)
  alignment6_roi<-roi_getvalue(rootdir=argu$ssub_outputroot,grproot=argu$glvl_outputroot,modelname="alignment6",
                               basemask="tstat",corrp_mask="tfce",saveclustermap=TRUE,Version="tfce0.95",corrmaskthreshold=0.95,
                               roimaskthreshold=0.0001, voxelnumthres=10, clustertoget=NULL,copetoget=NULL,maxcore=6)
  alignment3a_roi<-roi_getvalue(rootdir="/Volumes/bek/neurofeedback/sonrisa1/nfb/ssanalysis/fsl",
                                grproot="/Volumes/bek/neurofeedback/sonrisa1/nfb/grpanal/fsl",
                                modelname="alignment3a",
                                basemask="tstat",corrp_mask="tfce",saveclustermap=TRUE,Version="tfce0.95",corrmaskthreshold=0.965,
                                roimaskthreshold=0.0001, voxelnumthres=10, clustertoget=NULL,copetoget=NULL,maxcore=6)
  alignment3b_roi<-roi_getvalue(rootdir="/Volumes/bek/neurofeedback/sonrisa1/nfb/ssanalysis/fsl",
                                grproot="/Volumes/bek/neurofeedback/sonrisa1/nfb/grpanal/fsl",
                                modelname="alignment3b",
                                basemask="tstat",corrp_mask="tfce",saveclustermap=TRUE,Version="tfce0.95",corrmaskthreshold=0.965,
                                roimaskthreshold=0.0001, voxelnumthres=10, clustertoget=NULL,copetoget=NULL,maxcore=6)
  
  #Example:
  df<-alignment3b_tfce0.965_roi$cope_17$roivalues
  df[c("cluster_4","ID")]
}

