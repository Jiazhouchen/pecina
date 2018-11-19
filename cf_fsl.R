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
argu<-as.environment(list(nprocess=4,onlyrun=NULL,proc_id_subs=NULL,regtype=".1D",ifnuisa=FALSE,adaptive_gfeat=TRUE,
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
M_Value=FALSE
M_GM<-TRUE
M_GMa<-FALSE
M_inC<-FALSE
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
if (M_GM) {
  argu$gridpath<-"grid_sc_gm.csv"
  argu$model.name="cf_gm"
  argu$model.varinames=c("Emotion",        
                         "Context",
                         "ContextHappy",
                         "ContextFearful",
                         "Trial")
  argu$ssub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_cf_gm_template_R.fsf"
}
if (M_GMa) { #none ordinal
  argu$gridpath<-"grid_sc_gma.csv"
  argu$model.name="cf_gma"
  argu$model.varinames=c("EmotionHappy",
                         "EmotionFearful",
                         "Context",
                         "ContextHappy",
                         "ContextFearful",
                         "Trial")
  argu$ssub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_cf_gma_template_R.fsf"
}
if (M_inC) {
  argu$gridpath<-"grid_sc_inc.csv"
  argu$model.name="cf_inC"
  argu$model.varinames=c("Congruent",        
                         "Incongruent",
                         "Trial")
  argu$ssub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_cf_inc_template_R.fsf"
}
#Get behavioral data
datalist<-proc_behav_cf(boxdir = boxdir,fmriproc = T)


#Run fsl_pipe
fslpipe::fsl_pipe(argu=argu,
         prep.call.func="prep.confram", #This should be a character string that's the name of the prep proc function
         prep.call.allsub=datalist #List of ID list of arguments for prep.call.
)


if(F){
  
  roi_extraction_pairedtmask<-roi_getvalue(rootdir=argu$ssub_outputroot,grproot=argu$glvl_outputroot,modelname=argu$model.name,
                               basemask="tstat",corrp_mask="tfce",saveclustermap=TRUE,Version="tfce0.95",corrmaskthreshold=0.95,
                               roimaskthreshold=0.0001, voxelnumthres=10, clustertoget=NULL,copetoget=NULL,maxcore=6)
  roi_extraction_placmask<-roi_getvalue(rootdir=argu$ssub_outputroot,grproot=argu$glvl_outputroot,modelname=argu$model.name,grp_identif="Plac",
                                           basemask="tstat",corrp_mask="tfce",saveclustermap=TRUE,Version="tfce0.95",corrmaskthreshold=0.95,
                                           roimaskthreshold=0.0001, voxelnumthres=10, clustertoget=NULL,copetoget=NULL,maxcore=6)
  roi_extraction_naltmask<-roi_getvalue(rootdir=argu$ssub_outputroot,grproot=argu$glvl_outputroot,modelname=argu$model.name,grp_identif="Nalt",
                                        basemask="tstat",corrp_mask="tfce",saveclustermap=TRUE,Version="tfce0.95",corrmaskthreshold=0.95,
                                        roimaskthreshold=0.0001, voxelnumthres=10, clustertoget=NULL,copetoget=NULL,maxcore=6)
}