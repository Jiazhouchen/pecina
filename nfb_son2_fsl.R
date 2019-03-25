rm(list = ls())
require("devtools")
#devtools::install_github("DecisionNeurosciencePsychopathology/fMRI_R",force = F)
library(fslpipe)
#if("dependlab" %in% installed.packages()){"GREAT, DEPENDLAB PACK IS INSTALLED"}else{devtools::install_github("PennStateDEPENdLab/dependlab")}
source('pecina_R_utility_function.R')

#Setting up FSL global enviroment variables in case we are using RStudio
fsl_2_sys_env()

singlesub<-FALSE
######
#Actual arguments for each model. Should follow template: github.com/DecisionNeurosciencePsychopathology/fMRI_R
####BE AWARE!
argu<-as.environment(list(nprocess=10,onlyrun=c(5,6),forcereg=F,cfgpath="/Volumes/bek/autopreprocessing_pipeline/Neurofeedback/nfb_son2.cfg",
                          regpath="/Volumes/bek/neurofeedback/sonrisa2/nfb/regs/R_fsl_reg",func.nii.name="nfswudktm*[0-9]_[0-9].nii.gz", #c(a,b) = b>a
                          group_id_sep=c('Nalt','Plac'),regtype=".1D", convlv_nuisa=FALSE,adaptive_gfeat=TRUE,adaptive_ssfeat=TRUE,randomize_demean=FALSE,
                          gsub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_gfeat_general_adaptive_template.fsf",
                          ssub_outputroot="/Volumes/bek/neurofeedback/sonrisa2/nfb/ssanalysis/fsl",whichttest = c("paired","onesample"),
                          glvl_outputroot="/Volumes/bek/neurofeedback/sonrisa2/nfb/grpanal/fsl",centerscaleall=FALSE,
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


ValuePE<-T
Value1<-F
alignment1<-F
alignment2<-F
alignment3<-F
alignment4<-F
alignment5<-F
alignment6<-F
LRPE<-F

if (ValuePE) {
  argu$model.name="ValuePE"
  argu$gridpath="/Volumes/bek/neurofeedback/scripts/pecina/grid_ValuePE.csv"
  argu$centerscaleall=TRUE
}

if (alignment1) {
  argu$model.name="alignment1_evtmax"
  argu$gridpath="/Volumes/bek/neurofeedback/scripts/pecina/grid_alignment1.csv"
}
if (alignment2) {
  argu$model.name="alignment2"
  argu$gridpath="/Volumes/bek/neurofeedback/scripts/pecina/grid_alignment2.csv"
}
if (alignment3) {
    argu$model.name="alignment3c_light"
    argu$gridpath="/Volumes/bek/neurofeedback/scripts/pecina/grid_alignment3c_light.csv"
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
if (Value1) {
  argu$model.name="Value1n"
  argu$gridpath="/Volumes/bek/neurofeedback/scripts/pecina/grid_Value1.csv"
  argu$centerscaleall=TRUE
  argu$adminfilter=NULL
}
if(LRPE){
  argu$model.name="LRPE"
  argu$gridpath="/Volumes/bek/neurofeedback/scripts/pecina/grid_vt_pe.csv"
  argu$centerscaleall=TRUE
}
###################
##Official Start:##
#Supposedly you shouldn't need to change anything down below:
#If you are just switching models
###################

boxdir <- "~/Box/"

son_all<-as.environment(list())
load(file.path(boxdir,"GitHub","Nfb_task","NFB_response","SON1&2_behav_results","nfb_behav.rdata"),envir = son_all)
son2_all<-son_all$bothSONs$SON2$df
#Split them into mulitiple participants
if(is.null(argu$group_id_sep)){
son2_split<-split(son2_all,son2_all$Participant)
} else {
son2_split<-split(son2_all,son2_all$FullID)
#names(son1_split)<-gsub("_2$","_b",gsub("_1$","_a",names(son1_split)))
}
son2_rework<-lapply(names(son2_split),function(x) {
  son2_split[[x]]->y
  return(list(son1_single=y,adminfilter=NULL))
})
names(son2_rework)<-names(son2_split)
#son1_split$SON1_018->son1_single
if(singlesub){
  son2_rework<-son2_rework["SON2_018"]
}

print(names(son2_rework))
fsl_pipe(
  argu=argu, #This is the arguments environment, each model should have a different one;
  prep.call.func="prep.son1", #This should be a character string that's the name of the prep proc function
  prep.call.allsub=son2_rework #List of ID list of arguments for prep.call.
)


#ROI###
if(F){

a3c_light<-fslpipe::roi_getvalue(rootdir="/Volumes/bek/neurofeedback/sonrisa2/nfb/ssanalysis/fsl",
                                      grproot="/Volumes/bek/neurofeedback/sonrisa2/nfb/grpanal/fsl",
                                      modelname="alignment3c_light",basemask = "tstat",corrp_mask="cluster-based-extent",
                                      saveclustermap=TRUE,Version=NULL,corrmaskthreshold=0.95,saverdata = T,
                                      roimaskthreshold=0.001, voxelnumthres=1, clustertoget=NULL,copetoget=10,maxcore=6)

ValuePE_roi_son2<-fslpipe::roi_getvalue(rootdir="/Volumes/bek/neurofeedback/sonrisa2/nfb/ssanalysis/fsl",
                                   grproot ="/Volumes/bek/neurofeedback/sonrisa2/nfb/grpanal/fsl",grp_identif = "Plac",
                                   modelname="ValuePE",saverdata = T,
                                   basemask="tstat",corrp_mask="tstat",saveclustermap=TRUE,Version="stat_2.6_0",corrmaskthreshold=2.6,
                                   roimaskthreshold=0.001, voxelnumthres=0, clustertoget=NULL,copetoget=7,maxcore=6)

roi_list<-ValuePE_roi_son2

for (xr in names(roi_list)){
  roi_list[[xr]]$copename<-xr
}


proc_son2_roi<-function(roi_list) {
  if(is.null(roi_list$roivalues)){return(NULL)}else{
    names(roi_list$roivalues)[!grepl("ID",names(roi_list$roivalues))]<-paste(names(roi_list$roivalues)[!grepl("ID",names(roi_list$roivalues))],roi_list$copename,sep = "_")
    roi_list$roivalues$Drug<-NA
    roi_list$roivalues$Drug[grepl("Nalt",roi_list$roivalues$ID)]<-"Nalt"
    roi_list$roivalues$Drug[grepl("Plac",roi_list$roivalues$ID)]<-"Plac"
    roi_list$roivalues$uID<-gsub("_Plac","",gsub("_Nalt","",roi_list$roivalues$ID))
  }
  return(roi_list)
}

roi_list_proc<-cleanuplist(lapply(roi_list,proc_son2_roi))

t.test(x, y, paired = TRUE, alternative = "two.sided")

allpairedt<-do.call(rbind,lapply(roi_list_proc,function(xr){
  xrnames<-names(xr$roivalues)[grepl("cluster",names(xr$roivalues))]
  DRLS<-split(xr$roivalues,xr$roivalues$Drug)
  dfa<-merge(x = DRLS$Nalt,y = DRLS$Plac,by = "uID",all = T)
  tresults<-do.call(rbind,lapply(xrnames,function(xname){
    for(xz in names(dfa)[grepl(xname,names(dfa))]) {dfa[[xz]]<-as.numeric(as.character(dfa[[xz]]))}
    xrz<-dfa[c("uID",names(dfa)[grepl(xname,names(dfa))])]
    xrj<-t.test(xrz[[paste0(xname,".x")]], xrz[[paste0(xname,".y")]], paired = TRUE, alternative = "two.sided")
    return(data.frame(clustername=xname,t_stat=xrj$statistic,est=xrj$estimate,p_value=xrj$p.value))
  }))
  return(tresults)
}))

allpairedt[allpairedt$p_value<0.05,]
allpairedt[allpairedt$p_value<0.1,]


roidfx<-do.call(rbind,lapply(unique(roidf$uID),function(id){
  Naltvalue<-(as.numeric(as.character(roidf[roidf$uID==id & roidf$Drug=="Nalt","cluster_1"])))
  Placvalue<-(as.numeric(as.character(roidf[roidf$uID==id & roidf$Drug=="Plac","cluster_1"])))

  data.frame(uID=id, NaltValue=Naltvalue,PlacValue=Placvalue)
}))

}
