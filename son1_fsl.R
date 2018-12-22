############################################
####### New Functions for regressor ########
############################################
#Do a source utility scripts from git (when we have enough we will make a function out of it...)
#Check required packages:
rm(list = ls())
require("devtools")
if("fslpipe" %in% installed.packages()){"GREAT, FSLPIPE PACK IS INSTALLED"}else{devtools::install_github("DecisionNeurosciencePsychopathology/fMRI_R")}
library(fslpipe)
if("dependlab" %in% installed.packages()){"GREAT, DEPENDLAB PACK IS INSTALLED"}else{devtools::install_github("PennStateDEPENdLab/dependlab")}
source('pecina_R_utility_function.R')

#Setting up FSL global enviroment variables in case we are using RStudio
fsl_2_sys_env()

singlesub<-F
######
#Actual arguments for each model. Should follow template: github.com/DecisionNeurosciencePsychopathology/fMRI_R
####BE AWARE!
argu<-as.environment(list(nprocess=10,onlyrun=NULL,forcereg=F,cfgpath="/Volumes/bek/autopreprocessing_pipeline/Neurofeedback/nfb.cfg",
                          regpath="/Volumes/bek/neurofeedback/sonrisa1/nfb/regs/R_fsl_reg",func.nii.name="nfswudktm*[0-9]_[0-9].nii.gz",
                          group_id_sep=c("a","b"),regtype=".1D", convlv_nuisa=FALSE,adaptive_gfeat=TRUE,adaptive_ssfeat=TRUE,randomize_demean=FALSE,
                          gsub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_gfeat_general_adaptive_template.fsf",
                          ssub_outputroot="/Volumes/bek/neurofeedback/sonrisa1/nfb/ssanalysis/fsl",centerscaleall=FALSE,
                          glvl_outputroot="/Volumes/bek/neurofeedback/sonrisa1/nfb/grpanal/fsl",
                          templatedir="/Volumes/bek/Newtemplate_may18/fsl_mni152/MNI152_T1_2mm_brain.nii",whichttest = c("paired","onesample"),
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

PE1<-F
Value1<-T
alignment1<-F
alignment2<-F
alignment3<-F
alignment3c2<-F
alignment3c3<-F
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
if (alignment3c2) {
  argu$model.name="alignment3c_light"
  argu$gridpath="/Volumes/bek/neurofeedback/scripts/pecina/grid_alignment3c_light.csv"
}
if (alignment3c3) {
  argu$model.name="alignment3c3"
  argu$gridpath="/Volumes/bek/neurofeedback/scripts/pecina/grid_alignment3c3.csv"
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
if (PE1) {
  argu$model.name="PE1"
  argu$gridpath="/Volumes/bek/neurofeedback/scripts/pecina/grid_PE1.csv"
}
if (Value1) {
  argu$model.name="Value1"
  argu$gridpath="/Volumes/bek/neurofeedback/scripts/pecina/grid_Value1.csv"
  argu$centerscaleall=TRUE
}

###########Official Start:###########
#Supposedly you shouldn't need to change anything down below:
#If you are just switching models


boxdir <- "/Volumes/bek/Box Sync"

son_all<-as.environment(list())
load(file.path(boxdir,"GitHub","Nfb_task","NFB_response","SON1&2_behav_results","son_behav.rdata"),envir = son_all)
son1_all<-son_all$bothSONs$SON1$df
#Split them into mulitiple participants
if(is.null(argu$group_id_sep)){
son1_split<-split(son1_all,son1_all$Participant)
} else {
son1_split<-split(son1_all,son1_all$FullID)
names(son1_split)<-gsub("_2$","_b",gsub("_1$","_a",names(son1_split)))
}
son1_rework<-lapply(names(son1_split),function(x) {
  son1_split[[x]]->y
  return(list(son1_single=y,adminfilter=NULL))
})
names(son1_rework)<-names(son1_split)
#son1_split$SON1_018->son1_single
if(singlesub){
  son1_rework<-son1_rework["SON1_031"]
}
#stop()
 print(names(son1_rework))
 
if(!is.null(argu$group_id_sep) & dir.exists(file.path(argu$ssub_outputroot,argu$model.name))) {
  ogdir<-file.path(argu$ssub_outputroot,argu$model.name)
  argu$model.name<-paste0(argu$model.name,"_ABCompare")
  newdir<-file.path(argu$ssub_outputroot,argu$model.name)
  dir.create(newdir,recursive = T,showWarnings = F)
  if(!dir.exists(newdir)){
  for (idx in list.dirs(ogdir,recursive = F,full.names = F)) {
    file.symlink(from = file.path(ogdir,idx),to = file.path(newdir,paste0(idx,"_a")))
  }
  oldds<-new.env()
  load(file.path(ogdir,"design.rdata"),envir = oldds)
  oldds$ls<-as.list(oldds$allsub.design)
  names(oldds$ls)<-paste0(names(oldds$ls),"_a")
  oldds$ls<-lapply(oldds$ls,function(x){x$ID<-paste0(x$ID,"_a");return(x)})
  oldds$allsub.design<-as.environment(oldds$ls)
  save(allsub.design,file = file.path(newdir,"design.rdata"),envir = oldds)
  }
}
 
 
fsl_pipe(
    argu=argu, #This is the arguments environment, each model should have a different one;
    prep.call.func="prep.son1", #This should be a character string that's the name of the prep proc function
    prep.call.allsub=son1_rework #List of ID list of arguments for prep.call.
    )





#########Additional Functions##########


####HEAT HEATMAP: #####
if(F){
load(file.path(argu$ssub_outputroot,argu$model.name,"design.rdata"))
fslpipe::make_heatmap_with_design( allsub.design$SON1_018)
}
####ROI Extraction#####
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

