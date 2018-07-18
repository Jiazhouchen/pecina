############################################
####### New Functions for regressor ########
############################################
#Do a source utility scripts from git (when we have enough we will make a function out of it...)
#Check required packages:
require("devtools")
if("dependlab" %in% installed.packages()){"GREAT, DEPENDLAB PACK IS INSTALLED"}else{devtools::install_github("PennStateDEPENdLab/dependlab")}
#Load utility functions from both sources
if (file.exists("pecina_R_utility_function.R")){
  source("pecina_R_utility_function.R")
} else {
  devtools::source_url("https://raw.githubusercontent.com/Jiazhouchen/pecina/master/pecina_R_utility_function.R")
}
devtools::source_url("https://raw.githubusercontent.com/DecisionNeurosciencePsychopathology/fMRI_R/master/dnpl_utility.R")

#Setting up FSL global enviroment variables in case we are using RStudio 
fsl_2_sys_env()

#Model selection
argu_8c<-FALSE
argu_4c<-TRUE

if (argu_8c) {
#Setting some global options (Putting moving variables here so the function down there could just grab them)
argu_8c<-as.environment(list(
#Number of processes to allow for paralle processing
nprocess=12,
#If at any point you wish to stop the function, input step number here: ; if NULL then will be ignored.
stop=NULL,
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
#If there's anyother folder within $output/$model.name that contains *.feat, please remove it from here
hig_lvl_path_filter=NULL 

#Add more universal arguements in here: 
))
argu<-argu_8c
}
if (argu_4c) {
argu_4c<-as.environment(list(
  #Number of processes to allow for paralle processing
  nprocess=3,
  #If at any point you wish to stop the function, input step number here: ; if NULL then will be ignored.
  stop=NULL,
  #Where is the cfg config file:
  cfgpath="/Volumes/bek/autopreprocessing_pipeline/Neurofeedback/nfb.cfg",
  #Where to put/are the regressors 
  regpath="/Volumes/bek/neurofeedback/sonrisa1/nfb/regs/R_fsl_reg",
  #Where is the grid to make signal?
  gridpath="grid_4c_new.csv",
  #What pre-proc data to grab:
  func.nii.name="swudktm*[0-9].nii.gz",
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
  #If there's anyother folder within $output/$model.name that contains *.feat, please remove it from here
  hig_lvl_path_filter=NULL,
  #Threshold for graphic purposes
  graphic.threshold=0.95
  
  #Add more universal arguements in here: 
))
argu<-argu_4c
}

###################
##Official Start:##
#Supposedly you shouldn't need to change anything down below:
#If you are just switching models
###################

#Step 1: 
#Get Behavioral and VBA output Data
if (Sys.getenv("USER")=="jiazhouchen") {boxdir <- "/Users/jiazhouchen/Box Sync"
} else if (Sys.getenv("USER")=="jiazhou") {boxdir <- "/Volumes/bek/Box Sync"} else {
boxdir<-system("find ~ -iname 'Box*' -maxdepth 2 -type d",intern = T)}

son1_all <- read.csv(file.path(boxdir,"GitHub","Nfb_task","NFB_response","SON1&2_behav_results","son1_all.csv"))

stepnow<-1
if (!is.null(argu$stop)) {if(argu$stop<stepnow+1) {stop(paste0("Made to stop at step ",stepnow))}}


#Step 2:
#Now we process the data and convolve them using the argument provided up there; with help of 'dependlab' package
if (file.exists(file.path(argu$ssub_outputroot,argu$model.name,"design.rdata"))) {
  load(file.path(argu$ssub_outputroot,argu$model.name,"design.rdata"))
} else {allsub.design<-as.environment(list())}

if (length(names(allsub.design))>0) {
  idtodo<-as.character(unique(son1_all$Participant)[which(! unique(son1_all$Participant) %in% names(allsub.design))])
} else {idtodo<-unique(son1_all$Participant)}

if (length(idtodo)>0) {
for (xid in idtodo) {
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
    add.nuisa=argu$ifnuisa,
    assigntoenvir=allsub.design)
  },error=function(x) {paste0(xid,": This person failed regressor generation...go investigate")}
)
}

save(list = "allsub.design",file = file.path(argu$ssub_outputroot,argu$model.name,"design.rdata"))
} else {message("NO NEW DATA NEEDED TO BE PROCESSED")}


allstepnow<-stepnow+1
if (!is.null(argu$stop)) {if(argu$stop<stepnow+1) {stop(paste0("Made to stop at step ",stepnow))}}


#Step 3: 
#Now we do the single sub processinggggggggggggg 

#let's subset this 
small.sub<-eapply(allsub.design, function(x) {
  list(
  ID=x$ID,
  run_volumes=x$run_volumes,
  regpath=x$regpath,
  preprocID=x$preprocID)
})

#This part takes a long time...Let's paralle it:
require("parallel")
if (is.null(argu$nprocess)){
if (detectCores()>12){
num_cores<-8 #Use 8 cores to minimize burden; if on throndike 
#Or if you are running this on laptop; whatever cores minus 2; I guess if it's a dual core...let's just don't do that (zero core will not paralle anything)
} else {num_cores<-detectCores()-2} 
} else {argu$nprocess->num_cores}
clusterjobs<-makeCluster(num_cores,outfile="")
clusterExport(clusterjobs,c("argu","gen_reg","small.sub","get_volume_run",
                            "cfg_info","change_fsl_template","fsl_2_sys_env",
                            "feat_w_template","info_to_sysenv"),envir = environment())

NU<-parSapply(clusterjobs,small.sub,function(x) {
  fsl_2_sys_env()
  idx<-x$ID
  for (runnum in 1:length(x$run_volumes)) {
    xarg<-as.environment(list())
    xarg$runnum<-runnum    
    xarg$outputpath<-file.path(argu$ssub_outputroot,argu$model.name,idx,paste0("run",runnum,"_output"))
    xarg$templatebrain<-argu$templatedir
    if (!file.exists(paste0(xarg$outputpath,".feat")) ) {
    xarg$volumes<-x$run_volumes[runnum]
    xarg$funcfile<-get_volume_run(id=paste0(idx,argu$proc_id_subs),cfgfilepath = argu$cfgpath,reg.nii.name = argu$func.nii.name,returnas = "path")[runnum]
    xarg$nuisa<-file.path(argu$regpath,argu$model.name,idx,paste0("run",runnum,"_nuisance_regressor_with_motion.txt"))
    #Could do better on the regressor thing here; it's hard coded but it could be not hard coded.
    #Also could've just use the regpath in small.sub
    gen_reg(vmodel=argu$model.varinames,regpath=file.path(argu$regpath,argu$model.name),idx=idx,runnum=runnum,env=xarg,regtype = argu$regtype)
    #xarg$infreg<-file.path(argu$regpath,argu$model.name,idx,paste0("run",runnum,"_infusion.1D"))
    #xarg$noinfreg<-file.path(argu$regpath,argu$model.name,idx,paste0("run",runnum,"_noinfusion.1D"))
    #xarg$fbreg<-file.path(argu$regpath,argu$model.name,idx,paste0("run",runnum,"_feedback.1D"))
    #xarg$nofbreg<-file.path(argu$regpath,argu$model.name,idx,paste0("run",runnum,"_nofeedback.1D"))
    #xarg$inf_value<-file.path(argu$regpath,argu$model.name,idx,paste0("run",runnum,"_twoLRValueShifted_CS_plac_ctrl.1D"))
    #xarg$noinf_value<-file.path(argu$regpath,argu$model.name,idx,paste0("run",runnum,"_twoLRValueShifted_CS_plac_ctrl_r.1D"))
    #xarg$fb_PE<-file.path(argu$regpath,argu$model.name,idx,paste0("run",runnum,"_twoLRPE_CS_reinf_cont.1D"))
    #xarg$nofb_PE<-file.path(argu$regpath,argu$model.name,idx,paste0("run",runnum,"_twoLRPE_CS_reinf_cont_r.1D"))
    
    
    fsltemplate<-readLines(argu$ssub_fsl_templatepath)
    
    
    feat_w_template(fsltemplate = fsltemplate,
                    beg = "ARG_",
                    end = "_END",
                    fsf.path = file.path(argu$regpath,argu$model.name,idx,paste0("run",runnum,"_",argu$model.name,".fsf")),
                    envir = xarg)
  
    #fsltemplate<-readLines(argu$ssub_fsl_templatepath)
    #subbyrunfeat<-change_fsl_template(fsltemplate = readLines(argu$ssub_fsl_templatepath),begin = "ARG_",end="_END",searchenvir = xarg)
    #fsfpath<-
    #writeLines(subbyrunfeat,fsfpath)
    #system(paste0("feat ",fsfpath),intern = T)
    } else {message(paste("ID:",idx,"RUN:",runnum,",already exists,","to re-run, remove the directory."))}
  }
  
})

stopCluster(clusterjobs)



stepnow<-stepnow+1
if (!is.null(argu$stop)) {if(argu$stop<stepnow+1) {stop(paste0("Made to stop at step ",stepnow))}}

#Step 4: 
#Now we make the symbolic link for template matching...so they are not misaligned anymore...
#source the script: 
#This one runs fast enough that it should be fine to not parallel it
devtools::source_url("https://raw.githubusercontent.com/Jiazhouchen/pecina/master/prep_for_second_lvl.R")
cfg<-cfg_info(cfgpath = argu$cfgpath)
prepmap<-son.prepare4secondlvl(
  ssana.path=file.path(argu$ssub_outputroot,argu$model.name),            
  preproc.path=cfg$loc_mrproc_root,                                
  standardbarin.path=argu$templatedir, 
  dir.filter=argu$hig_lvl_path_filter,                                                
  proc.name=cfg$paradigm_name,                                                                         
  taskname=cfg$preprocessed_dirname,                                                                   
  overwrite=TRUE,
  outputmap=TRUE)           



#Should set up another paralle here:
#outputpath average
#feat# 

cfg$n_expected_funcruns->runnum
featlist<-lapply(small.sub,function(x) {
  x$ID->idz
  emp<-list()
  for (runnum in 1:length(x$run_volumes)) {
    emp[[paste0("feat",runnum)]]<-file.path(argu$ssub_outputroot,argu$model.name,idz,paste0("run",runnum,"_output.feat"))
  }
  small.sub[[idz]]$featlist<-emp
  assign("small.sub",small.sub,envir = globalenv())
  return(emp)
  })
clusterjobs1<-makeCluster(num_cores,outfile="")
clusterExport(clusterjobs1,c("argu","small.sub","get_volume_run","cfg_info","change_fsl_template","fsl_2_sys_env","feat_w_template"),envir = environment())

NU<-parSapply(clusterjobs1,small.sub, function(y) {

  larg<-as.environment(list())
  y$ID->larg$idx
  larg$outputpath<-file.path(argu$ssub_outputroot,argu$model.name,larg$idx,"average")
  larg<-list2env(y$featlist,envir = larg)
  if (!file.exists(file.path(larg$outputpath,".gfeat"))) {
    feat_w_template(templatepath = argu$gsub_fsl_templatepath,
                    beg = "ARG_",
                    end = "_END",
                    fsf.path = file.path(argu$regpath,argu$model.name,larg$idx,paste0("gfeat_temp",".fsf")),
                    envir = larg)
  } else {message("This person already got average done!")}
  
})
stopCluster(clusterjobs1)


fsltemplate.GL<-readLines(argu$gsub_fsl_templatepath)
#Start Group Level Analysis:
glvl_all_cope(rootdir=argu$ssub_outputroot,
                        outputdir=argu$glvl_outputroot,
                        modelname=argu$model.name,
                        copestorun=1:as.numeric(gsub(".*?([0-9]+).*", "\\1", fsltemplate.GL[grep("ncopeinputs",fsltemplate.GL)])),
                        paralleln = num_cores
)

library(oro.nifti)
plot_image_all(rootpath=argu$glvl_outputroot,
                         templatedir=argu$templatedir,
                         model.name=argu$model.name,
                         patt="OneSampT_tfce_corrp_tstat1.nii.gz",
                         threshold=argu$graphic.threshold,
                         outputdir=file.path(argu$glvl_outputroot,argu$model.name),
                         colour="red")


#In development:
if (FALSE) {
  #To see if make sense to just create a new 
  splitAt <- function(x, pos) unname(split(x, cumsum(seq_along(x) %in% pos)))
  chuckofev<-fsltemplate[min(grep("EV [0-9]* title",fsltemplate)):grep("# Contrast & F-tests mode",fsltemplate,fixed = T)]
  byev<-splitAt(chuckofev,grep("EV [0-9]* title",chuckofev))
  length(byev)->nev
    
  
  
                        
  
}












