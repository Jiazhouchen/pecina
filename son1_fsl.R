############################################
####### New Functions for regressor ########
############################################
#Do a source utility scripts from git (when we have enough we will make a function out of it...)
#Check required packages:
rm(list = ls())
require("devtools")
if("dependlab" %in% installed.packages()){"GREAT, DEPENDLAB PACK IS INSTALLED"}else{devtools::install_github("PennStateDEPENdLab/dependlab")}
#Load utility functions from both sources

#devtools::source_url("https://raw.githubusercontent.com/Jiazhouchen/pecina/master/pecina_R_utility_function.R")
source('pecina_R_utility_function.R')
devtools::source_url("https://raw.githubusercontent.com/DecisionNeurosciencePsychopathology/fMRI_R/master/dnpl_utility.R")
#Setting up FSL global enviroment variables in case we are using RStudio 
fsl_2_sys_env()

#Model selection
argu_8c<-FALSE
argu_4c<-FALSE
argu_6c<-FALSE
argu_8c_resp<-FALSE
model0<-F
model1<-F
model1a<-F
model2<-F
model1aa<-T

######
if(length(which(sapply(Filter( function(x) 'logical' %in% class( get(x) ), ls() ),function(x) {get(x)})))>1) {multimodels<-TRUE} else {multimodels<-FALSE}

#Actual arguments for each model. Should follow template: github.com/DecisionNeurosciencePsychopathology/fMRI_R
if (argu_8c) {
#Setting some global options (Putting moving variables here so the function down there could just grab them)
argu_8c<-as.environment(list(
#Number of processes to allow for paralle processing
nprocess=3,
#Do only these steps, if NULL then do all. 
onlyrun=NULL,
#Force Reg gen restart:
forcereg=FALSE,
#Where is the cfg config file:
cfgpath="/Volumes/bek/autopreprocessing_pipeline/Neurofeedback/nfb.cfg",
#Where to put/are the regressors 
regpath="/Volumes/bek/neurofeedback/sonrisa1/nfb/regs/R_fsl_reg",
#Where is the grid to make signal?
gridpath="grid.csv",
#What pre-proc data to grab:
func.nii.name="nfswudktm*[0-9].nii.gz",
#Does the ID have a tails:
proc_id_subs="_a",
#Now set up the model:
model.name="PE_8C_reg_by_vol",
#Look at the grid! 
model.varinames=c("inf",         
                  "noinf",
                  "fb",
                  "nofb",
                  "inf_value",
                  "noinf_value",
                  "fb_PE",
                  "nofb_PE"),
regtype=".1D",
#If to convolve with nuisance regressors with dependlab package:
ifnuisa=FALSE,
#Single subject FSL template path
ssub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_8C_by_run_usedby_R.fsf",
#Group level FSL template path
gsub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_gfeat_8Cbyrun_average_R.fsf",
#Single Subject output root path (before model name folder)
ssub_outputroot="/Volumes/bek/neurofeedback/sonrisa1/nfb/ssanalysis/fsl",
#Group lvl output rootpath (before model name folder)
glvl_outputroot="/Volumes/bek/neurofeedback/sonrisa1/nfb/grpanal/fsl",
#Brain template path
templatedir="/Volumes/bek/Newtemplate_may18/fsl_mni152/MNI152_T1_2mm_brain.nii",
#Group level analysis output path
glvl_output="/Volumes/bek/neurofeedback/sonrisa1/nfb/grpanal/fsl",
#If to redo all the linking for 2nd level
ifoverwrite_secondlvl=FALSE,
#If there's anyother folder within $output/$model.name that contains *.feat, please remove it from here
hig_lvl_path_filter=NULL,
#Threshold for graphic purposes
graphic.threshold=0.95
))
argu<-argu_8c
}
if (argu_4c) {
argu_4c<-as.environment(list(
  #Number of processes to allow for paralle processing
  nprocess=3,
  #Do only these steps, if NULL then do all. 
  onlyrun=NULL,
  #Force Reg gen restart:
  forcereg=FALSE,
  #Where is the cfg config file:
  cfgpath="/Volumes/bek/autopreprocessing_pipeline/Neurofeedback/nfb.cfg",
  #Where to put/are the regressors 
  regpath="/Volumes/bek/neurofeedback/sonrisa1/nfb/regs/R_fsl_reg",
  #Where is the grid to make signal?
  gridpath="/Volumes/bek/neurofeedback/scripts/pecina_R/grid_4c_new.csv",
  #What pre-proc data to grab:
  func.nii.name="nfswudktm*[0-9]_[0-9].nii.gz",
  #Does the ID have a tails:
  proc_id_subs="_a",
  #Now set up the model:
  model.name="PE_4C_reg_by_vol",
  #Look at the grid! 
  model.varinames=c("inf",         
                    "noinf",
                    "fb",
                    "nofb"),
  regtype=".1D", #To use fsl 3 col, do '_FSL3col.txt'
  #If to convolve with nuisance regressors with dependlab package:
  ifnuisa=FALSE,
  #Single subject FSL template path
  ssub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_4C_by_run_usedby_R.fsf",
  #Group level FSL template path
  gsub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_gfeat_4Cbyrun_average_R.fsf",
  #Single Subject output root path (before model name folder)
  ssub_outputroot="/Volumes/bek/neurofeedback/sonrisa1/nfb/ssanalysis/fsl",
  #Group lvl output rootpath (before model name folder)
  glvl_outputroot="/Volumes/bek/neurofeedback/sonrisa1/nfb/grpanal/fsl",
  #Brain template path
  templatedir="/Volumes/bek/Newtemplate_may18/fsl_mni152/MNI152_T1_2mm_brain.nii",
  #Group level analysis output path
  glvl_output="/Volumes/bek/neurofeedback/sonrisa1/nfb/grpanal/fsl",
  #If to redo all the linking for 2nd level
  ifoverwrite_secondlvl=FALSE,
  #If there's anyother folder within $output/$model.name that contains *.feat, please remove it from here
  hig_lvl_path_filter=NULL,
  #Threshold for graphic purposes
  graphic.threshold=0.95
  
  #Add more universal arguements in here: 
))
argu<-argu_4c
}
if (argu_6c) {
  argu_6c<-as.environment(list(
    #Number of processes to allow for paralle processing
    nprocess=NULL,
    #Do only these steps, if NULL then do all. 
    onlyrun=NULL,
    #Force Reg gen restart:
    forcereg=FALSE,
    #Where is the cfg config file:
    cfgpath="/Volumes/bek/autopreprocessing_pipeline/Neurofeedback/nfb.cfg",
    #Where to put/are the regressors 
    regpath="/Volumes/bek/neurofeedback/sonrisa1/nfb/regs/R_fsl_reg",
    #Where is the grid to make signal?
    gridpath="/Volumes/bek/neurofeedback/scripts/pecina_R/grid_6c_new.csv",
    #What pre-proc data to grab:
    func.nii.name="nfswudktm*[0-9]_[0-9].nii.gz",
    #Does the ID have a tails:
    proc_id_subs="_a",
    #Now set up the model:
    model.name="PE_6C_reg_by_vol",
    #Look at the grid! 
    model.varinames=c("inf",         
                      "noinf",
                      "value",
                      "fb",
                      "nofb",
                      "PE"),
    regtype=".1D", #To use fsl 3 col, do '_FSL3col.txt'
    #If to convolve with nuisance regressors with dependlab package:
    ifnuisa=FALSE,
    #Single subject FSL template path
    ssub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_6C_by_run_usedby_R.fsf",
    #Group level FSL template path
    gsub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_gfeat_4Cbyrun_average_R.fsf",
    #Single Subject output root path (before model name folder)
    ssub_outputroot="/Volumes/bek/neurofeedback/sonrisa1/nfb/ssanalysis/fsl",
    #Group lvl output rootpath (before model name folder)
    glvl_outputroot="/Volumes/bek/neurofeedback/sonrisa1/nfb/grpanal/fsl",
    #Brain template path
    templatedir="/Volumes/bek/Newtemplate_may18/fsl_mni152/MNI152_T1_2mm_brain.nii",
    #Group level analysis output path
    glvl_output="/Volumes/bek/neurofeedback/sonrisa1/nfb/grpanal/fsl",
    #If to redo all the linking for 2nd level
    ifoverwrite_secondlvl=FALSE,
    #If there's anyother folder within $output/$model.name that contains *.feat, please remove it from here
    hig_lvl_path_filter=NULL,
    #Threshold for graphic purposes
    graphic.threshold=0.95
    
    #Add more universal arguements in here: 
  ))
  argu<-argu_6c
}
if (argu_8c_resp) {
  argu_8c_resp<-as.environment(list(
    #Number of processes to allow for paralle processing
    nprocess=NULL,
    #Do only these steps, if NULL then do all. 
    onlyrun=NULL,
    #Force Reg gen restart:
    forcereg=FALSE,
    #Where is the cfg config file:
    cfgpath="/Volumes/bek/autopreprocessing_pipeline/Neurofeedback/nfb.cfg",
    #Where to put/are the regressors 
    regpath="/Volumes/bek/neurofeedback/sonrisa1/nfb/regs/R_fsl_reg",
    #Where is the grid to make signal?
    gridpath="/Volumes/bek/neurofeedback/scripts/pecina_R/grid_8c_resp.csv",
    #What pre-proc data to grab:
    func.nii.name="nfswudktm*[0-9]_[0-9].nii.gz",
    #Does the ID have a tails:
    proc_id_subs="_a",
    #Now set up the model:
    model.name="PE_8C_reg_by_vol_resp",
    #Look at the grid! 
    model.varinames=c("inf",         
                      "noinf",
                      "fb",
                      "nofb",
                      "infresp",
                      "noinfresp",
                      "fbresp",
                      "nofbresp"),
    regtype=".1D", #To use fsl 3 col, do '_FSL3col.txt'
    #If to convolve with nuisance regressors with dependlab package:
    ifnuisa=FALSE,
    #Single subject FSL template path
    ssub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_8C_resp.fsf",
    #Group level FSL template path
    gsub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_gfeat_4Cbyrun_average_R.fsf",
    #Single Subject output root path (before model name folder)
    ssub_outputroot="/Volumes/bek/neurofeedback/sonrisa1/nfb/ssanalysis/fsl",
    #Group lvl output rootpath (before model name folder)
    glvl_outputroot="/Volumes/bek/neurofeedback/sonrisa1/nfb/grpanal/fsl",
    #Brain template path
    templatedir="/Volumes/bek/Newtemplate_may18/fsl_mni152/MNI152_T1_2mm_brain.nii",
    #Group level analysis output path
    glvl_output="/Volumes/bek/neurofeedback/sonrisa1/nfb/grpanal/fsl",
    #If to redo all the linking for 2nd level
    ifoverwrite_secondlvl=FALSE,
    #If there's anyother folder within $output/$model.name that contains *.feat, please remove it from here
    hig_lvl_path_filter=NULL,
    #Threshold for graphic purposes
    graphic.threshold=0.95
    
    #Add more universal arguements in here: 
  ))
  argu<-argu_8c_resp
}
if (model0) {
  model0<-as.environment(list(
    #Number of processes to allow for paralle processing
    nprocess=2,
    #Do only these steps, if NULL then do all. 
    onlyrun=NULL,
    #Force Reg gen restart:
    forcereg=FALSE,
    #Where is the cfg config file:
    cfgpath="/Volumes/bek/autopreprocessing_pipeline/Neurofeedback/nfb.cfg",
    #Where to put/are the regressors 
    regpath="/Volumes/bek/neurofeedback/sonrisa1/nfb/regs/R_fsl_reg",
    #Where is the grid to make signal?
    gridpath="/Volumes/bek/neurofeedback/scripts/pecina_R/grid_m0.csv",
    #What pre-proc data to grab:
    func.nii.name="nfswudktm*[0-9]_[0-9].nii.gz",
    #Does the ID have a tails:
    proc_id_subs="_a",
    #Now set up the model:
    model.name="M0",
    #Look at the grid! 
    model.varinames=c("inf",         
                      "noinf",
                      "fb",
                      "nofb"),
    regtype=".1D", #To use fsl 3 col, do '_FSL3col.txt'
    #If to convolve with nuisance regressors with dependlab package:
    ifnuisa=FALSE,
    #Single subject FSL template path
    ssub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_4C_by_run_usedby_R.fsf",
    #Group level FSL template path
    gsub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_gfeat_4Cbyrun_average_R.fsf",
    #Single Subject output root path (before model name folder)
    ssub_outputroot="/Volumes/bek/neurofeedback/sonrisa1/nfb/ssanalysis/fsl",
    #Group lvl output rootpath (before model name folder)
    glvl_outputroot="/Volumes/bek/neurofeedback/sonrisa1/nfb/grpanal/fsl",
    #Brain template path
    templatedir="/Volumes/bek/Newtemplate_may18/fsl_mni152/MNI152_T1_2mm_brain.nii",
    #Group level analysis output path
    glvl_output="/Volumes/bek/neurofeedback/sonrisa1/nfb/grpanal/fsl",
    #If to redo all the linking for 2nd level
    ifoverwrite_secondlvl=FALSE,
    #If there's anyother folder within $output/$model.name that contains *.feat, please remove it from here
    hig_lvl_path_filter=NULL,
    #Threshold for graphic purposes
    graphic.threshold=0.95
    
    #Add more universal arguements in here: 
  ))
  argu<-model0
}
if (model1) {
  model1<-as.environment(list(
    #Number of processes to allow for paralle processing
    nprocess=2,
    #Do only these steps, if NULL then do all. 
    onlyrun=NULL,
    #Force Reg gen restart:
    forcereg=FALSE,
    #Where is the cfg config file:
    cfgpath="/Volumes/bek/autopreprocessing_pipeline/Neurofeedback/nfb.cfg",
    #Where to put/are the regressors 
    regpath="/Volumes/bek/neurofeedback/sonrisa1/nfb/regs/R_fsl_reg",
    #Where is the grid to make signal?
    gridpath="/Volumes/bek/neurofeedback/scripts/pecina_R/grid_m1.csv",
    #What pre-proc data to grab:
    func.nii.name="nfswudktm*[0-9]_[0-9].nii.gz",
    #Does the ID have a tails:
    proc_id_subs="_a",
    #Now set up the model:
    model.name="M1",
    #Look at the grid! 
    model.varinames=c("inf_evt",         
                      "inf_noinf",
                      "fb_evt",
                      "fb_nofb"),
    regtype=".1D", #To use fsl 3 col, do '_FSL3col.txt'
    #If to convolve with nuisance regressors with dependlab package:
    ifnuisa=FALSE,
    #Single subject FSL template path
    ssub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_m1_usedby_R.fsf",
    #Group level FSL template path
    gsub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_m1_average_R.fsf",
    #Single Subject output root path (before model name folder)
    ssub_outputroot="/Volumes/bek/neurofeedback/sonrisa1/nfb/ssanalysis/fsl",
    #Group lvl output rootpath (before model name folder)
    glvl_outputroot="/Volumes/bek/neurofeedback/sonrisa1/nfb/grpanal/fsl",
    #Brain template path
    templatedir="/Volumes/bek/Newtemplate_may18/fsl_mni152/MNI152_T1_2mm_brain.nii",
    #Group level analysis output path
    glvl_output="/Volumes/bek/neurofeedback/sonrisa1/nfb/grpanal/fsl",
    #If to redo all the linking for 2nd level
    ifoverwrite_secondlvl=FALSE,
    #If there's anyother folder within $output/$model.name that contains *.feat, please remove it from here
    hig_lvl_path_filter=NULL,
    #Threshold for graphic purposes
    graphic.threshold=0.95
    
    #Add more universal arguements in here: 
  ))
  argu<-model1
}
if (model1a) {
  model1a<-as.environment(list(
    #Number of processes to allow for paralle processing
    nprocess=2,
    #Do only these steps, if NULL then do all. 
    onlyrun=NULL,
    #Force Reg gen restart:
    forcereg=FALSE,
    #Where is the cfg config file:
    cfgpath="/Volumes/bek/autopreprocessing_pipeline/Neurofeedback/nfb.cfg",
    #Where to put/are the regressors 
    regpath="/Volumes/bek/neurofeedback/sonrisa1/nfb/regs/R_fsl_reg",
    #Where is the grid to make signal?
    gridpath="/Volumes/bek/neurofeedback/scripts/pecina_R/grid_m1a.csv",
    #What pre-proc data to grab:
    func.nii.name="nfswudktm*[0-9]_[0-9].nii.gz",
    #Does the ID have a tails:
    proc_id_subs="_a",
    #Now set up the model:
    model.name="M1a",
    #Look at the grid! 
    model.varinames=c("inf_evt",         
                      "inf_noinf",
                      "fb_evt",
                      "fb_nofb",
                      "exprat",
                      "moodrat"),
    regtype=".1D", #To use fsl 3 col, do '_FSL3col.txt'
    #If to convolve with nuisance regressors with dependlab package:
    ifnuisa=FALSE,
    #Single subject FSL template path
    ssub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_m1a_usedby_R.fsf",
    #Group level FSL template path
    gsub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_m1a_average_R.fsf",
    #Single Subject output root path (before model name folder)
    ssub_outputroot="/Volumes/bek/neurofeedback/sonrisa1/nfb/ssanalysis/fsl",
    #Group lvl output rootpath (before model name folder)
    glvl_outputroot="/Volumes/bek/neurofeedback/sonrisa1/nfb/grpanal/fsl",
    #Brain template path
    templatedir="/Volumes/bek/Newtemplate_may18/fsl_mni152/MNI152_T1_2mm_brain.nii",
    #Group level analysis output path
    glvl_output="/Volumes/bek/neurofeedback/sonrisa1/nfb/grpanal/fsl",
    #If to redo all the linking for 2nd level
    ifoverwrite_secondlvl=FALSE,
    #If there's anyother folder within $output/$model.name that contains *.feat, please remove it from here
    hig_lvl_path_filter=NULL,
    #Threshold for graphic purposes
    graphic.threshold=0.95
    
    #Add more universal arguements in here: 
  ))
  argu<-model1a
}
if (model2) {
  model2<-as.environment(list(
    #Number of processes to allow for paralle processing
    nprocess=12,
    #Do only these steps, if NULL then do all. 
    onlyrun=NULL,
    #Force Reg gen restart:
    forcereg=FALSE,
    #Where is the cfg config file:
    cfgpath="/Volumes/bek/autopreprocessing_pipeline/Neurofeedback/nfb.cfg",
    #Where to put/are the regressors 
    regpath="/Volumes/bek/neurofeedback/sonrisa1/nfb/regs/R_fsl_reg",
    #Where is the grid to make signal?
    gridpath="/Volumes/bek/neurofeedback/scripts/pecina_R/grid_m2.csv",
    #What pre-proc data to grab:
    func.nii.name="nfswudktm*[0-9]_[0-9].nii.gz",
    #Does the ID have a tails:
    proc_id_subs="_a",
    #Now set up the model:
    model.name="M2",
    #Look at the grid! 
    model.varinames=c("inf_evt",         
                      "inf_value",
                      "fb_evt",
                      "fb_PE",
                      "exprat",
                      "moodrat"),
    regtype=".1D", #To use fsl 3 col, do '_FSL3col.txt'
    #If to convolve with nuisance regressors with dependlab package:
    ifnuisa=FALSE,
    #Single subject FSL template path
    ssub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_m2_usedby_R.fsf",
    #Group level FSL template path
    gsub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_m1a_average_R.fsf",
    #Single Subject output root path (before model name folder)
    ssub_outputroot="/Volumes/bek/neurofeedback/sonrisa1/nfb/ssanalysis/fsl",
    #Group lvl output rootpath (before model name folder)
    glvl_outputroot="/Volumes/bek/neurofeedback/sonrisa1/nfb/grpanal/fsl",
    #Brain template path
    templatedir="/Volumes/bek/Newtemplate_may18/fsl_mni152/MNI152_T1_2mm_brain.nii",
    #Group level analysis output path
    glvl_output="/Volumes/bek/neurofeedback/sonrisa1/nfb/grpanal/fsl",
    #If to redo all the linking for 2nd level
    ifoverwrite_secondlvl=FALSE,
    #If there's anyother folder within $output/$model.name that contains *.feat, please remove it from here
    hig_lvl_path_filter=NULL,
    #Threshold for graphic purposes
    graphic.threshold=0.95
    
    #Add more universal arguements in here: 
  ))
  argu<-model2
}
if (model1aa) {
  model1aa<-as.environment(list(
    #Number of processes to allow for paralle processing
    nprocess=4,
    #Do only these steps, if NULL then do all. 
    onlyrun=NULL,
    #Force Reg gen restart:
    forcereg=FALSE,
    #Where is the cfg config file:
    cfgpath="/Volumes/bek/autopreprocessing_pipeline/Neurofeedback/nfb.cfg",
    #Where to put/are the regressors 
    regpath="/Volumes/bek/neurofeedback/sonrisa1/nfb/regs/R_fsl_reg",
    #Where is the grid to make signal?
    gridpath="/Volumes/bek/neurofeedback/scripts/pecina/grid_m1aa.csv",
    #What pre-proc data to grab:
    func.nii.name="nfswudktm*[0-9]_[0-9].nii.gz",
    #Does the ID have a tails:
    proc_id_subs="_a",
    #Now set up the model:
    model.name="M1aa",
    #Look at the grid! 
    model.varinames=c("inf_evt",         
                      "inf_noinf",
                      "fb_evt",
                      "fb_nofb",
                      "exprat_evt",
                      "exprat",
                      "moodrat_evt",
                      "moodrat"),
    regtype=".1D", #To use fsl 3 col, do '_FSL3col.txt'
    #If to convolve with nuisance regressors with dependlab package:
    ifnuisa=FALSE,
    #Single subject FSL template path
    ssub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_m1aa_usedby_R.fsf",
    #Group level FSL template path
    gsub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_m1aa_average_R.fsf",
    #Single Subject output root path (before model name folder)
    ssub_outputroot="/Volumes/bek/neurofeedback/sonrisa1/nfb/ssanalysis/fsl",
    #Group lvl output rootpath (before model name folder)
    glvl_outputroot="/Volumes/bek/neurofeedback/sonrisa1/nfb/grpanal/fsl",
    #Brain template path
    templatedir="/Volumes/bek/Newtemplate_may18/fsl_mni152/MNI152_T1_2mm_brain.nii",
    #Group level analysis output path
    glvl_output="/Volumes/bek/neurofeedback/sonrisa1/nfb/grpanal/fsl",
    #If to redo all the linking for 2nd level
    ifoverwrite_secondlvl=FALSE,
    #If there's anyother folder within $output/$model.name that contains *.feat, please remove it from here
    hig_lvl_path_filter=NULL,
    #Threshold for graphic purposes
    graphic.threshold=0.95
    
    #Add more universal arguements in here: 
  ))
  argu<-model1aa
}
###################
##Official Start:##
#Supposedly you shouldn't need to change anything down below:
#If you are just switching models
###################

#These are for future use of single function run:
#boxdir<-findbox()
#son1_all <- read.csv(file.path(boxdir,"GitHub","Nfb_task","NFB_response","SON1&2_behav_results","son1_all.csv"))
#prep.call.allsub<-lapply(unique(son1_all$Participant),function(x) {
#  emx<-list(
#    son1_single=son1_all[which(son1_all$Participant %in% x),]
#  )
#  return(emx)
#})
#names(prep.call.allsub)<-unique(son1_all$Participant)

#Run multiple models;
#if (Sys.getenv("USER")=="jiazhouchen") {
#boxdir <- "/Users/jiazhouchen/Box Sync"
#} else if (Sys.getenv("USER")=="jiazhou") {
boxdir <- "/Volumes/bek/Box Sync"
#} else {
#boxdir<-system("find ~ -iname 'Box*' -maxdepth 2 -type d",intern = T)
#}

son1_all <- read.csv(file.path(boxdir,"GitHub","Nfb_task","NFB_response","SON1&2_behav_results","son1_all.csv"))
#Split them into mulitiple participants
son1_split<-split(son1_all,son1_all$Participant)
son1_rework<-lapply(names(son1_split),function(x) {
  son1_split[[x]]->y
  return(list(son1_single=y))
})
names(son1_rework)<-names(son1_split)

if (multimodels) {
  allargulist <- Filter( function(x) 'environment' %in% class( get(x) ), ls() )
  allargulist <- allargulist[-grep("argu",allargulist)]
  for (modelargu in allargulist) {
    print(modelargu)
    get(modelargu)->argu
    tryCatch(
    {
    fsl_pipe(
      argu=argu, #This is the arguments environment, each model should have a different one;
      prep.call.func="prep.son1", #This should be a character string that's the name of the prep proc function
      prep.call.allsub=son1_rework #List of ID list of arguments for prep.call.
      )
    },error=function(x) {paste0(modelargu," Failed")}
    )
  }
} else {
  fsl_pipe(
    argu=argu, #This is the arguments environment, each model should have a different one;
    prep.call.func="prep.son1", #This should be a character string that's the name of the prep proc function
    prep.call.allsub=son1_rework #List of ID list of arguments for prep.call.
  )
}















