############################################
####### New Functions for regressor ########
############################################
#Do a source utility scripts from git (when we have enough we will make a function out of it...)
#Check required packages:
rm(list = ls())
require("devtools")
devtools::install_github("DecisionNeurosciencePsychopathology/fMRI_R",upgrade = "ask",force = F)
library(fslpipe)
if("dependlab" %in% installed.packages()){"GREAT, DEPENDLAB PACK IS INSTALLED"}else{devtools::install_github("PennStateDEPENdLab/dependlab")}
source('pecina_R_utility_function.R')

#Setting up FSL global enviroment variables in case we are using RStudio
fsl_2_sys_env()

singlesub<-F
runQC<-F
######
#Actual arguments for each model. Should follow template: github.com/DecisionNeurosciencePsychopathology/fMRI_R
####BE AWARE!
argu<-as.environment(list(nprocess=10,onlyrun=NULL,forcereg=F,cfgpath="/Volumes/bek/autopreprocessing_pipeline/Neurofeedback/nfb.cfg",
                          regpath="/Volumes/bek/neurofeedback/sonrisa1/nfb/regs/R_fsl_reg",func.nii.name="nfswudktm*[0-9]_[0-9].nii.gz",
                          group_id_sep=NULL,regtype=".1D", convlv_nuisa=FALSE,adaptive_gfeat=TRUE,adaptive_ssfeat=TRUE,randomize_demean=FALSE,
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

Vt_PE_Plac<-F
PE<-F
PE_abs<-F
Value1<-T
LRPE<-F
LRPE_lite<-F
alignment1<-F
alignment2<-F
alignment3<-F
alignment3c2<-F
alignment3c3<-F
alignment3cx<-T
alignment4<-F
alignment5<-F
alignment6<-F


if (PE) {
  argu$model.name="PE1n"
  argu$gridpath="/Volumes/bek/neurofeedback/scripts/pecina/grid_PE1.csv"
  argu$centerscaleall=TRUE
  argu$proc_id_subs="_a"
  argu$adminfilter=1
}

if (PE_abs) {
  argu$model.name="PE_abs"
  argu$gridpath="/Volumes/bek/neurofeedback/scripts/pecina/grid_PE_abs.csv"
  argu$centerscaleall=TRUE
  argu$proc_id_subs="_a"
  argu$adminfilter=1
}

if (Value1) {
  argu$model.name="Value1n"
  argu$gridpath="/Volumes/bek/neurofeedback/scripts/pecina/grid_Value1.csv"
  argu$centerscaleall=TRUE
  argu$proc_id_subs="_a"
  argu$adminfilter=1
}

if(LRPE){
  argu$model.name="LRPE"
  argu$gridpath="/Volumes/bek/neurofeedback/scripts/pecina/grid_vt_pe.csv"
  argu$centerscaleall=TRUE
  argu$proc_id_subs="_a"
  argu$adminfilter=1
}

if(LRPE_lite){
  argu$model.name="LRPE_lite"
  argu$gridpath="/Volumes/bek/neurofeedback/scripts/pecina/grid_vt_pe_lite.csv"
  argu$centerscaleall=TRUE
  argu$proc_id_subs="_a"
  argu$adminfilter=1
}
if(Vt_PE_Plac){
  argu$model.name="Vt_PE_Plac"
  argu$gridpath="/Volumes/bek/neurofeedback/scripts/pecina/grid_value_pe_plac.csv"
  argu$centerscaleall=TRUE
  argu$proc_id_subs="_a"
  argu$adminfilter=1
}

if(alignment3cx){
  argu$model.name="alignment3cx"
  argu$gridpath="/Volumes/bek/neurofeedback/scripts/pecina/grid_alignment3c.csv"
  argu$centerscaleall=TRUE
  argu$proc_id_subs="_a"
  argu$adminfilter=1
}
###########Official Start:###########
#Supposedly you shouldn't need to change anything down below:
#If you are just switching models


boxdir <- "~/Box/"

son_all<-as.environment(list())
load(file.path(boxdir,"GitHub","Nfb_task","NFB_response","SON1&2_behav_results","nfb_behav.rdata"),envir = son_all)
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
  if(exists("proc_id_subs",envir = argu)){
    if (argu$proc_id_subs=="_a"){adminifilter=1}else if (argu$proc_id_subs=="_b"){adminifilter=2}

  } else {
    adminifilter=NULL
  }
  return(list(son1_single=y,adminfilter=adminifilter,QC=runQC))

  
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
 
 
if(runQC){
  cfg<-cfg_info(cfgpath = argu$cfgpath)
  info_qc<-QC_pipe(cfgpath=argu$cfgpath,QC_func="prep.son1",supplylist = son1_rework,hdtemplate=argu$templatedir,
                   QC_auxdir="/Volumes/bek/QC_fsl",nparalle=argu$nprocess)
  info_qc$study<-"SON1_NFB"
} else {
  fsl_pipe(
    argu=argu, #This is the arguments environment, each model should have a different one;
    prep.call.func="prep.son1", #This should be a character string that's the name of the prep proc function
    prep.call.allsub=son1_rework #List of ID list of arguments for prep.call.
  )
}
 





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
  value1n_roi<-roi_getvalue(rootdir="/Volumes/bek/neurofeedback/sonrisa1/nfb/ssanalysis/fsl",
                                grproot="/Volumes/bek/neurofeedback/sonrisa1/nfb/grpanal/fsl",
                                modelname="Value1n",
                                basemask="tstat",corrp_mask="tfce",saveclustermap=TRUE,Version="tfce0.95",corrmaskthreshold=0.95,
                                roimaskthreshold=0.0001, voxelnumthres=10, clustertoget=NULL,copetoget=NULL,maxcore=6)
  
  #Example:
  df<-value1n_roi$cope_7$roivalues #take the output, cope 7, 
  #df<-df[c("cluster_3","ID")] #take only cluster 3 and ID
  df$FullID<-paste0(df$ID,"_1") #make full ID
  df1_admin1_wroi<-merge(df1_admin1,df,by = "FullID")
  
}

