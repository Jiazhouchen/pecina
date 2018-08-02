#####Con_Frame Script

#Prepare, setting up resources
rm(list = ls())
require("devtools")
if("dependlab" %in% installed.packages()){"GREAT, DEPENDLAB PACK IS INSTALLED"}else{devtools::install_github("PennStateDEPENdLab/dependlab")}
#Load utility functions from both sources
devtools::source_url("https://raw.githubusercontent.com/Jiazhouchen/pecina/master/pecina_R_utility_function.R")
devtools::source_url("https://raw.githubusercontent.com/DecisionNeurosciencePsychopathology/fMRI_R/master/dnpl_utility.R")
#devtools::source_url("https://raw.githubusercontent.com/DecisionNeurosciencePsychopathology/fMRI_R/master/fslpipe.R")
#Setting up FSL global enviroment variables in case we are using RStudio 
fsl_2_sys_env()

#Load in behavioral data:
boxdir<-findbox()
#boxdir <- "/Volumes/bek/Box Sync"
if (grepl(" ",boxdir)) {
  sub("Box Sync","/'Box Sync/'",boxdir)->boxdir
}

#The goal is to create a structure that is similar to what to be produced by step 2;



#We also need to move the regressors;


#Sort data into right format: 
#Remember when creating new model:
##ONLY RUN STEP 2 to STEP 7!
model_basic<-TRUE

if (model_basic) {
  #Setting some global options (Putting moving variables here so the function down there could just grab them)
  model_basic<-as.environment(list(
    #Number of processes to allow for paralle processing
    nprocess=4, #Running on Jiazhou's do 4, Thorndike can handle 12; 
    #If at any point you wish to stop the function, input step number here: ; if NULL then will be ignored.
    onlyrun=2:7,
    #Where is the cfg config file:
    cfgpath="/Volumes/bek/autopreprocessing_pipeline/Neurofeedback/reststate_son1.cfg",
    #Where to put/are the regressors 
    regpath="/Volumes/bek/neurofeedback/sonrisa1/resting/regs/R_fsl_reg",
    #Where is the grid to make signal?
    gridpath="grid_sc.csv",
    #What pre-proc data to grab:
    func.nii.name="nfswudktm*[0-9]_[0-9].nii.gz",
    #Does the ID have a tails:
    proc_id_subs=NULL,
    #Now set up the model:
    model.name="rest_basic",
    #Look at the grid! 
    model.varinames=c("roi"),
    regtype=".1D",
    #If to convolve with nuisance regressors with dependlab package:
    ifnuisa=FALSE,
    #Single subject FSL template path
    ssub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_ssa_rs_R.fsf",
    #Group level FSL template path
    gsub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_gfeat_rs_template_average_R.fsf",
    #Single Subject output root path (before model name folder)
    ssub_outputroot="/Volumes/bek/neurofeedback/sonrisa1/resting/ssanalysis/fsl",
    #Group lvl output rootpath (before model name folder)
    glvl_outputroot="/Volumes/bek/neurofeedback/sonrisa1/resting/grpanal/fsl",
    #Brain template path
    templatedir="/Volumes/bek/Newtemplate_may18/fsl_mni152/MNI152_T1_2mm_brain.nii",
    #If there's anyother folder within $output/$model.name that contains *.feat, please remove it from here
    hig_lvl_path_filter=NULL,
    graphic.threshold=0.95,
    forcereg=FALSE,
    ifoverwrite_secondlvl=T
    #Add more universal arguements in here: 
  ))
  argu<-model_basic
}

list.dirs("/Volumes/bek/neurofeedback/sonrisa1/proc/",recursive = F)->allpd
xj<-lapply(allpd, function(x) {
  y<-list.files(file.path(x,"rest_proc","rest1"),pattern = "NAcc.txt",recursive = T)
  if (length(y)>0) {
    id<-strsplit(x,split = .Platform$file.sep)[[1]][8]
    dir.create(file.path(argu$regpath,argu$model.name,id),recursive = T,showWarnings = F)
    file.copy(from = file.path(file.path(x,"rest_proc","rest1"),y),to = file.path(argu$regpath,argu$model.name,id,"run1_roi.1D"))
    nuisa<-NULL
    tryCatch({
    nuisa<-get_nuisance_preproc(id=id,cfgfilepath = argu$cfgpath,returnas = "data.frame") },error=function(e){})
    if (!is.null(nuisa)){
      for (k in 1:length(nuisa)) {
        write.table(as.matrix(nuisa[[k]]),file.path(regpath,model.name,id,
                                                    paste0("run",1,"_nuisance_regressor_with_motion.txt")),
                    row.names = F,col.names = FALSE)
      }}
    
    return(file.path(file.path(x,"rest_proc","rest1"),y))
  } else {return(NULL)}
})
xj[sapply(xj, is.null)] <- NULL

#Move the regressors and made them the right name:
allidz<-sapply(xj,function(x) {strsplit(x,split = .Platform$file.sep)[[1]][8]})
allvolumez<-sapply(allidz,function(id) {length(readLines(file.path(argu$regpath,argu$model.name,id,"run1_roi.1D")))})
allsub<-list()
for (ix in 1:length(allidz)) {
  idy<-allidz[ix]
  allsub[[idy]]<-list(ID=idy,run_volumes=allvolumez[ix],regpath=file.path(argu$regpath,argu$model.name,idy),preprocID=idy)
}
as.environment(allsub)->allsub.design

fsl_pipe(
        argu=argu, #This is the arguments environment, each model should have a different one;
        prep.call.func=NULL, #This should be a character string that's the name of the prep proc function
        prep.call.allsub=NULL #List of ID list of arguments for prep.call.
)










