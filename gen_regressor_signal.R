############################################
####### New Functions for regressor ########
############################################
#Do a source script from git function
#Check required packages:
require("devtools")
if("dependlab" %in% installed.packages()){}else{devtools::install_github("PennStateDEPENdLab/dependlab")}
#Load utility functions from both sources
if (file.exists("pecina_R_utility_function.R")){
  source("pecina_R_utility_function.R")
} else {
  devtools::source_url("https://raw.githubusercontent.com/Jiazhouchen/pecina/master/pecina_R_utility_function.R")
}
devtools::source_url("https://raw.githubusercontent.com/DecisionNeurosciencePsychopathology/fMRI_R/master/dnpl_utility.R")

#Setting up FSL in case we are using RStudio 
fsl_2_sys_env()

#Setting some global options (Putting moving variables here so the function down there could just grab them)
argu<-as.environment(list(
#Put them in a enviroment so that they can be access everywhere by get function
#Where is the cfg config file:
cfgpath="/Volumes/bek/autopreprocessing_pipeline/Neurofeedback/nfb.cfg",
#Where to put/are the regressors 
regpath="/Volumes/bek/neurofeedback/sonrisa1/nfb/regs/R_fsl_reg",
#Where is the grid to make signal?
gridpath="grid.csv",
#What pre-proc data to grab:
func.nii.name="swudktm*[0-9].nii.gz",
#Does the ID have a tails:
proc_id_subs="_a",
#Now set up the model:
model.name="PE_8C_reg_by_vol",
#Look at the grid! 
model.varinames=c("infusion",         
                  "noinfusion",
                  "feedback",
                  "nofeedback",
                  "twoLRPE_CS_reinf_cont",
                  "twoLRPE_CS_reinf_cont_r",
                  "twoLRValueShifted_CS_plac_ctrl",
                  "twoLRValueShifted_CS_plac_ctrl_r"),
ssub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_8C_by_run_usedby_R.fsf",
ssub_outputroot="/Volumes/bek/neurofeedback/sonrisa1/nfb/ssanalysis/fsl"

#Add more universal arguements in here: 
))


#Load son1's sepcific functions here:
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
  son1_single$plac_ctrl[son1_single$InfusionNum==1 | son1_single$InfusionNum==2] <- TRUE
  son1_single$plac_ctrl[son1_single$InfusionNum==3 | son1_single$InfusionNum==4] <- FALSE
  son1_single$plac_ctrl_r<-!son1_single$plac_ctrl
  
  son1_single$reinf_cont <- NA
  son1_single$reinf_cont[son1_single$InfusionNum==1 | son1_single$InfusionNum==3] <- TRUE
  son1_single$reinf_cont[son1_single$InfusionNum==2 | son1_single$InfusionNum==4] <- FALSE
  son1_single$reinf_cont_r<-!son1_single$reinf_cont
  
  son1_single$InfDur<-son1_single$WillImpOnset - son1_single$InfOnset
  son1_single$FeedDur<-son1_single$ImprovedOnset - son1_single$Feed2Onset
  
  vba<-as.list(son1_single[c(!names(son1_all) %in% regualrvarinames)])
  vba<-addcenterscaletolist(vba)  ##Function Coming from fMRI_Dev Script
  #Add taskness variables to value
  vba$plac_ctrl<-son1_single$plac_ctrl
  vba$plac_ctrl_r<-son1_single$plac_ctrl_r
  vba$reinf_cont<-son1_single$reinf_cont
  vba$reinf_cont_r<-son1_single$reinf_cont_r
  
  finalist<-list(infusion=data.frame(event="infusion",
                                     onset=son1_single$InfOnset,
                                     duration=son1_single$WillImpOnset - son1_single$InfOnset,
                                     run=son1_single$Run,
                                     trial=son1_single$TrialNum),
                 feedback=data.frame(event="feedback",
                                     onset=son1_single$Feed2Onset,
                                     duration=son1_single$ImprovedOnset - son1_single$Feed2Onset,
                                     run=son1_single$Run,
                                     trial=son1_single$TrialNum))
  for (i in 1:length(finalist)) {
    if (i==1) {ktz<-finalist[[i]]} else {
      ktz<-rbind(ktz,finalist[[i]])}
  }
  finalist[["allconcat"]]<-ktz
  output<-list(event.list=finalist,output.df=son1_single,value=vba)
}
#Get Data
if (Sys.getenv("RSTUDIO_USER_IDENTITY")=="jiazhouchen") {boxdir <- "/Users/jiazhouchen/Box Sync"
} else if (Sys.getenv("RSTUDIO_USER_IDENTITY")=="jiazhou") {boxdir <- "/Volumes/bek/Box Sync"} else {
boxdir<-system("find ~ -iname 'Box*' -maxdepth 2 -type d",intern = T)}

son1_all <- read.csv(file.path(boxdir,"GitHub","Nfb_task","NFB_response","SON1&2_behav_results","son1_all.csv"))

allsub.design<-as.environment(list())

for (xid in unique(son1_all$Participant)) {
  singlesub<-son1_all[which(son1_all$Participant %in% xid),]
tryCatch(
  {
    do.all.subjs(
    tid=xid,
    do.prep.call="prep.son1",
    do.prep.arg=list(son1_single=singlesub),
    cfgpath=argu$cfgpath,
    regpath=argu$regpath,
    gridpath=argu$gridpath,
    func.nii.name=argu$func.nii.name,
    proc_id_subs=argu$proc_id_subs,    #Put "" for nothing.
    wrt.timing=c("convolved", "FSL"),
    model.name=argu$model.name,
    model.varinames=argu$model.varinames,
    assigntoenvir=allsub.design)
  },error=function(x) {paste0(xid,": This person failed regressor generation...go investigate")}
)
}



#Now we do the single sub processinggggggggggggg 

#let's subset this 
small.sub<-eapply(allsub.design, function(x) {
  list(
  ID=x$ID,
  run_volumes=x$run_volumes)
})

#This part takes a long time...Let's paralle it:
require("parallel")
if (detectCores()>12){
num_cores<-8 #Use 8 cores to minimize burden; if on throndike 
} else {num_cores<-detectCores()-2} #Or if you are running this on laptop; whatever cores minus 2; 

clusterjobs<-makeCluster(num_cores)
clusterExport(clusterjobs,c("argu","small.sub","get_volume_run","cfg_info","change_fsl_template","fsl_2_sys_env"),envir = environment())

NU<-parSapply(clusterjobs,small.sub,function(x) {
  fsl_2_sys_env()
  idx<-x$ID
  for (runnum in 1:length(x$run_volumes)) {
    xarg<-as.environment(list())
    xarg$runnum<-runnum    
    xarg$outputpath<-file.path(argu$ssub_outputroot,argu$model.name,idx,paste0("run",runnum,"_output"))
    xarg$volumes<-x$run_volumes[runnum]
    xarg$funcfile<-get_volume_run(id=paste0(idx,argu$proc_id_subs),cfgfilepath = argu$cfgpath,reg.nii.name = argu$func.nii.name,returnas = "path")[runnum]
    xarg$nuisa<-file.path(argu$regpath,argu$model.name,idx,paste0("run",runnum,"_nuisance_regressor_with_motion.txt"))
    xarg$infreg<-file.path(argu$regpath,argu$model.name,idx,paste0("run",runnum,"_infusion.1D"))
    xarg$noinfreg<-file.path(argu$regpath,argu$model.name,idx,paste0("run",runnum,"_noinfusion.1D"))
    xarg$fbreg<-file.path(argu$regpath,argu$model.name,idx,paste0("run",runnum,"_feedback.1D"))
    xarg$nofbreg<-file.path(argu$regpath,argu$model.name,idx,paste0("run",runnum,"_nofeedback.1D"))
    xarg$inf_value<-file.path(argu$regpath,argu$model.name,idx,paste0("run",runnum,"_twoLRValueShifted_CS_plac_ctrl.1D"))
    xarg$noinf_value<-file.path(argu$regpath,argu$model.name,idx,paste0("run",runnum,"_twoLRValueShifted_CS_plac_ctrl_r.1D"))
    xarg$fb_PE<-file.path(argu$regpath,argu$model.name,idx,paste0("run",runnum,"_twoLRPE_CS_reinf_cont.1D"))
    xarg$nofb_PE<-file.path(argu$regpath,argu$model.name,idx,paste0("run",runnum,"_twoLRPE_CS_reinf_cont_r.1D"))
    
    fsltemplate<-readLines(argu$ssub_fsl_templatepath)
    subbyrunfeat<-change_fsl_template(fsltemplate = readLines(argu$ssub_fsl_templatepath),begin = "ARG_",end="_END",searchenvir = xarg)
    fsfpath<-file.path(argu$regpath,argu$model.name,idx,paste0("run",runnum,"_",argu$model.name,".fsf"))
    writeLines(subbyrunfeat,fsfpath)
    system(paste0("feat ",fsfpath),intern = T)
  }
  
})

stopCluster(clusterjobs)

#In development:

















