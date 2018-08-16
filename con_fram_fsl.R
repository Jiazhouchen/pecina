#####Con_Frame Script

#Prepare, setting up resources
rm(list = ls())
require("devtools")
if("dependlab" %in% installed.packages()){"GREAT, DEPENDLAB PACK IS INSTALLED"}else{devtools::install_github("PennStateDEPENdLab/dependlab")}
#Load utility functions from both sources
devtools::source_url("https://raw.githubusercontent.com/Jiazhouchen/pecina/master/pecina_R_utility_function.R")
devtools::source_url("https://raw.githubusercontent.com/DecisionNeurosciencePsychopathology/fMRI_R/master/dnpl_utility.R")
devtools::source_url("https://raw.githubusercontent.com/DecisionNeurosciencePsychopathology/fMRI_R/master/fslpipe.R")
#Setting up FSL global enviroment variables in case we are using RStudio 
fsl_2_sys_env()

#Load in behavioral data:
#boxdir<-findbox()
boxdir <- "/Volumes/bek/Box Sync"
if (grepl(" ",boxdir)) {
  sub("Box Sync","/'Box Sync/'",boxdir)->boxdir
}

#Sort data into right format: 
datafolder<-file.path(boxdir,"GitHub","SC_task","SC_responses")
lfilepath<-system(paste0("find ",datafolder," -name '*.csv' -maxdepth 2 -mindepth 1 -type f"),intern = T)
sapply(strsplit(lfilepath,split = "/"), "[[",length(strsplit(lfilepath,split = "/")[[1]])-1)->IDCON
IDCON<-sub("SC_","_",IDCON)
split(lfilepath,IDCON)->filexsplit
tempenvir<-as.environment(list())

for (i in 1:length(filexsplit)) {
  xz<-filexsplit[[i]]
  aal<-as.environment(list())
  for (xj in xz){
    tep<-read.csv(xj)
    assign(paste0("xd",xj),tep,envir = aal)  
  }
  aal<-as.list(aal)
  xzj<-list(singlesub=list())
  for (z in aal) {
    unique(z$Order)->ordernum
    xzj$singlesub[[paste0("Order",ordernum)]]<-z
  }
  xzj$singlesub$ID_CON<-names(filexsplit)[i]
  assign(names(filexsplit)[i],xzj,envir = tempenvir)
}

argu_sc<-TRUE

if (argu_sc) {
  #Setting some global options (Putting moving variables here so the function down there could just grab them)
  argu_sc<-as.environment(list(
    #Number of processes to allow for paralle processing
    nprocess=2,
    #If at any point you wish to stop the function, input step number here: ; if NULL then will be ignored.
    onlyrun=5:7,
    #Where is the cfg config file:
    cfgpath="/Volumes/bek/autopreprocessing_pipeline/Neurofeedback/con_framing.cfg",
    #Where to put/are the regressors 
    regpath="/Volumes/bek/neurofeedback/sonrisa2/con_framing/regs/R_fsl_reg",
    #Where is the grid to make signal?
    gridpath="grid_sc.csv",
    #What pre-proc data to grab:
    func.nii.name="nfswudktm*[0-9]_[0-9].nii.gz",
    #Does the ID have a tails:
    proc_id_subs=NULL,
    #Now set up the model:
    model.name="con_framing_basic_nalt",
    #Look at the grid! 
    model.varinames=c("PxH",         
                      "PxF",
                      "PxN",
                      "UxH",
                      "UxF",
                      "UxN"),
    regtype=".1D",
    #If to convolve with nuisance regressors with dependlab package:
    ifnuisa=FALSE,
    #Single subject FSL template path
    ssub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_cf_template_R.fsf",
    #Group level FSL template path
    gsub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_gfeat_cf_template_average_R.fsf",
    #Single Subject output root path (before model name folder)
    ssub_outputroot="/Volumes/bek/neurofeedback/sonrisa2/con_framing/ssanalysis/fsl",
    #Group lvl output rootpath (before model name folder)
    glvl_outputroot="/Volumes/bek/neurofeedback/sonrisa2/con_framing/grpanal/fsl",
    #Brain template path
    templatedir="/Volumes/bek/Newtemplate_may18/fsl_mni152/MNI152_T1_2mm_brain.nii",
    #If there's anyother folder within $output/$model.name that contains *.feat, please remove it from here
    hig_lvl_path_filter=NULL,
    graphic.threshold=0.95,
    forcereg=FALSE,
    ifoverwrite_secondlvl=T
    #Add more universal arguements in here: 
  ))
  argu<-argu_sc
}



fsl_pipe(argu=argu,
        prep.call.func="prep.confram", #This should be a character string that's the name of the prep proc function
        prep.call.allsub=as.list(tempenvir) #List of ID list of arguments for prep.call.
)










