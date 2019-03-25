
if (alignment1) {
  argu$model.name="alignment1ar"
  argu$gridpath="/Volumes/bek/neurofeedback/scripts/pecina/grid_alignment1ar.csv"
  argu$centerscaleall=TRUE
  argu$proc_id_subs="_a"
  argu$adminfilter=1
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


if (PE_abs) {
  argu$model.name="PE_abs"
  argu$gridpath="/Volumes/bek/neurofeedback/scripts/pecina/grid_PE_abs.csv"
  argu$centerscaleall=TRUE
  argu$proc_id_subs="_a"
  argu$adminfilter=1
}

if (Value1) {
  argu$model.name="Value1"
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
  argu$gridpath="/Volumes/bek/neurofeedback/scripts/pecina/grid_alignment3cx.csv"
  argu$centerscaleall=TRUE
  argu$proc_id_subs="_a"
  argu$adminfilter=1
}


if (F){
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
  
  if (model1aa_vba) {
    model1aa_vba<-as.environment(list(
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
      gridpath="/Volumes/bek/neurofeedback/scripts/pecina/grid_m1aa_vba.csv",
      #What pre-proc data to grab:
      func.nii.name="nfswudktm*[0-9]_[0-9].nii.gz",
      #Does the ID have a tails:
      proc_id_subs="_a",
      #Now set up the model:
      model.name="M1aa_vba",
      #Look at the grid!
      model.varinames=c("inf_evt",
                        "inf_value",
                        "fb_evt",
                        "fb_PE",
                        "exprat_evt",
                        "exprat",
                        "moodrat_evt",
                        "moodrat",
                        "exprat_miss",
                        "moodrat_miss"),
      regtype=".1D", #To use fsl 3 col, do '_FSL3col.txt'
      #If to convolve with nuisance regressors with dependlab package:
      convlv_nuisa=FALSE,
      #Single subject FSL template path
      ssub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_m1aa_vba_usedby_R.fsf",
      #ADAPTIVE GROUP LEVEL GFEAT TEMPLATE;
      adaptive_gfeat=TRUE,
      #Group level FSL template path [MAYBE LATER DEPRECIATE THIS COMAND AND JUST PUT IT TO GITHUB / PACK IT]
      gsub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_gfeat_general_adaptive_template.fsf",
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
      graphic.threshold=0.95,
      nuisa_motion=c("nuisance","motion_par"),
      motion_type="fd",
      motion_threshold="default",
      convlv_nuisa=F
      
      #Add more universal arguements in here:
    ))
    argu<-model1aa_vba
  }
  if (r_model1ab) {
    r_model1ab<-as.environment(list(
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
      gridpath="/Volumes/bek/neurofeedback/scripts/pecina/grid_m1ab.csv",
      #What pre-proc data to grab:
      func.nii.name="nfswudktm*[0-9]_[0-9].nii.gz",
      #Does the ID have a tails:
      proc_id_subs="_a",
      #Now set up the model:
      model.name="M1ab_re",
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
      ssub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_m1ab_usedby_R.fsf",
      #ADAPTIVE GROUP LEVEL GFEAT TEMPLATE;
      adaptive_gfeat=TRUE,
      #Group level FSL template path
      gsub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_gfeat_general_adaptive_template.fsf",
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
    argu<-r_model1ab
  }
  if (model1ac) {
    model1ac<-as.environment(list(
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
      gridpath="/Volumes/bek/neurofeedback/scripts/pecina/grid_m1ac.csv",
      #What pre-proc data to grab:
      func.nii.name="nfswudktm*[0-9]_[0-9].nii.gz",
      #Does the ID have a tails:
      proc_id_subs="_a",
      #Now set up the model:
      model.name="M1ac",
      #Look at the grid!
      model.varinames=c("inf_noinf",
                        "fb_nofb",
                        "exprat_evt",
                        "exprat",
                        "moodrat_evt",
                        "moodrat"),
      regtype=".1D", #To use fsl 3 col, do '_FSL3col.txt'
      #If to convolve with nuisance regressors with dependlab package:
      ifnuisa=FALSE,
      #Single subject FSL template path
      ssub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_m1ac_usedby_R.fsf",
      #ADAPTIVE GROUP LEVEL GFEAT TEMPLATE;
      adaptive_gfeat=TRUE,
      #Group level FSL template path
      gsub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_gfeat_general_adaptive_template.fsf",
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
    argu<-model1ac
  }
  if (model1ad) {
    model1ad<-as.environment(list(
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
      gridpath="/Volumes/bek/neurofeedback/scripts/pecina/grid_m1ad.csv",
      #What pre-proc data to grab:
      func.nii.name="nfswudktm*[0-9]_[0-9].nii.gz",
      #Does the ID have a tails:
      proc_id_subs="_a",
      #Now set up the model:
      model.name="M1ad",
      #Look at the grid!
      model.varinames=c("inf_evt",
                        "inf_noinf_expalign",
                        "fb_evt",
                        "fb_nofb_moodalign",
                        "exprat_evt",
                        "exprat",
                        "moodrat_evt",
                        "moodrat"),
      regtype=".1D", #To use fsl 3 col, do '_FSL3col.txt'
      #If to convolve with nuisance regressors with dependlab package:
      ifnuisa=FALSE,
      #Single subject FSL template path
      ssub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_m1ad_usedby_R.fsf",
      #ADAPTIVE GROUP LEVEL GFEAT TEMPLATE;
      adaptive_gfeat=TRUE,
      #Group level FSL template path [MAYBE LATER DEPRECIATE THIS COMAND AND JUST PUT IT TO GITHUB / PACK IT]
      gsub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_gfeat_general_adaptive_template.fsf",
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
    argu<-model1ad
  }
  if (model3) {
    model3<-as.environment(list(
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
      gridpath="/Volumes/bek/neurofeedback/scripts/pecina/grid_m3.csv",
      #What pre-proc data to grab:
      func.nii.name="nfswudktm*[0-9]_[0-9].nii.gz",
      #Does the ID have a tails:
      proc_id_subs="_a",
      #Now set up the model:
      model.name="M3",
      #Look at the grid!
      model.varinames=c("inf_evt",
                        "inf_noinf",
                        "fb_evt",
                        "fb_nofb"),
      regtype=".1D", #To use fsl 3 col, do '_FSL3col.txt'
      #If to convolve with nuisance regressors with dependlab package:
      ifnuisa=FALSE,
      #Single subject FSL template path
      ssub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_m3_usedby_R.fsf",
      #ADAPTIVE GROUP LEVEL GFEAT TEMPLATE;
      adaptive_gfeat=TRUE,
      #Group level FSL template path [MAYBE LATER DEPRECIATE THIS COMAND AND JUST PUT IT TO GITHUB / PACK IT]
      gsub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_gfeat_general_adaptive_template.fsf",
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
    argu<-model3
  }
  if (model2a) {
    model2a<-as.environment(list(
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
      gridpath="/Volumes/bek/neurofeedback/scripts/pecina/grid_m2a.csv",
      #What pre-proc data to grab:
      func.nii.name="nfswudktm*[0-9]_[0-9].nii.gz",
      #Does the ID have a tails:
      proc_id_subs="_a",
      #Now set up the model:
      model.name="M2a",
      #Look at the grid!
      model.varinames=c("inf_evt",
                        "inf_value",
                        "fb_evt",
                        "fb_PE"),
      regtype=".1D", #To use fsl 3 col, do '_FSL3col.txt'
      #If to convolve with nuisance regressors with dependlab package:
      ifnuisa=FALSE,
      #Single subject FSL template path
      ssub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_m2a_usedby_R.fsf",
      #ADAPTIVE GROUP LEVEL GFEAT TEMPLATE;
      adaptive_gfeat=TRUE,
      #Group level FSL template path [MAYBE LATER DEPRECIATE THIS COMAND AND JUST PUT IT TO GITHUB / PACK IT]
      gsub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_gfeat_general_adaptive_template.fsf",
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
    argu<-model2a
  }
  if (model4) {
    model4<-as.environment(list(
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
      gridpath="/Volumes/bek/neurofeedback/scripts/pecina/grid_m4.csv",
      #What pre-proc data to grab:
      func.nii.name="nfswudktm*[0-9]_[0-9].nii.gz",
      #Does the ID have a tails:
      proc_id_subs="_a",
      #Now set up the model:
      model.name="M4",
      #Look at the grid!
      model.varinames=c("inf_evt",
                        "inf_noinf",
                        "fb_evt",
                        "fb_nofb"),
      regtype=".1D", #To use fsl 3 col, do '_FSL3col.txt'
      #If to convolve with nuisance regressors with dependlab package:
      ifnuisa=FALSE,
      #Single subject FSL template path
      ssub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_m4_usedby_R.fsf",
      #ADAPTIVE GROUP LEVEL GFEAT TEMPLATE;
      adaptive_gfeat=TRUE,
      #Group level FSL template path [MAYBE LATER DEPRECIATE THIS COMAND AND JUST PUT IT TO GITHUB / PACK IT]
      gsub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_gfeat_general_adaptive_template.fsf",
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
    argu<-model4
  }
  if (model1aa_mc) {
    model1aa_mc<-as.environment(list(
      #Number of processes to allow for paralle processing
      nprocess=4,
      #Do only these steps, if NULL then do all.
      onlyrun=(NULL),
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
      model.name="M1aa_MC",
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
      #ADAPTIVE GROUP LEVEL GFEAT TEMPLATE;
      adaptive_gfeat=TRUE,
      #Group level FSL template path [MAYBE LATER DEPRECIATE THIS COMAND AND JUST PUT IT TO GITHUB / PACK IT]
      gsub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_gfeat_general_adaptive_template.fsf",
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
      graphic.threshold=0.95,
      #New sets of arguments for motion sensoring...
      nuisa_motion=c("nuisance","motion_par","motion_outlier"),
      motion_type="fd",
      motion_threshold="default",
      convlv_nuisa=F
      #Add more universal arguements in here:
    ))
    argu<-model1aa_mc
  }
  if (model1aa_up) {
    model1aa_up<-as.environment(list(
      #Number of processes to allow for paralle processing
      nprocess=4,
      #Do only these steps, if NULL then do all.
      onlyrun=(NULL),
      #Force Reg gen restart:
      forcereg=FALSE,
      #Where is the cfg config file:
      cfgpath="/Volumes/bek/autopreprocessing_pipeline/Neurofeedback/nfb.cfg",
      #Where to put/are the regressors
      regpath="/Volumes/bek/neurofeedback/sonrisa1/nfb/regs/R_fsl_reg",
      #Where is the grid to make signal?
      gridpath="/Volumes/bek/neurofeedback/scripts/pecina/grid_m1aa_up.csv",
      #What pre-proc data to grab:
      func.nii.name="nfswudktm*[0-9]_[0-9].nii.gz",
      #Does the ID have a tails:
      proc_id_subs="_a",
      #Now set up the model:
      model.name="M1aa_up",
      #Look at the grid!
      model.varinames=c("inf_evt",
                        "inf_noinf",
                        "fb_evt",
                        "fb_nofb",
                        "exprat_evt",
                        "moodrat_evt"),
      regtype=".1D", #To use fsl 3 col, do '_FSL3col.txt'
      #If to convolve with nuisance regressors with dependlab package:
      ifnuisa=FALSE,
      #Single subject FSL template path
      ssub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_m1aa_up_usedby_R.fsf",
      #ADAPTIVE GROUP LEVEL GFEAT TEMPLATE;
      adaptive_gfeat=TRUE,
      #Group level FSL template path [MAYBE LATER DEPRECIATE THIS COMAND AND JUST PUT IT TO GITHUB / PACK IT]
      gsub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_gfeat_general_adaptive_template.fsf",
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
      graphic.threshold=0.95,
      #New sets of arguments for motion sensoring...
      nuisa_motion=c("nuisance","motion_par"),
      motion_type="fd",
      motion_threshold="default",
      convlv_nuisa=F
      #Add more universal arguements in here:
    ))
    argu<-model1aa_up
  }
  if (model1aaa) {
    model1aaa<-as.environment(list(
      #Number of processes to allow for paralle processing
      nprocess=10,
      #Do only these steps, if NULL then do all.
      onlyrun=NULL,
      #Force Reg gen restart:
      forcereg=FALSE,
      #Where is the cfg config file:
      cfgpath="/Volumes/bek/autopreprocessing_pipeline/Neurofeedback/nfb.cfg",
      #Where to put/are the regressors
      regpath="/Volumes/bek/neurofeedback/sonrisa1/nfb/regs/R_fsl_reg",
      #Where is the grid to make signal?
      gridpath="/Volumes/bek/neurofeedback/scripts/pecina/grid_m1aaa.csv",
      #What pre-proc data to grab:
      func.nii.name="nfswudktm*[0-9]_[0-9].nii.gz",
      #Does the ID have a tails:
      proc_id_subs="_a",
      #Now set up the model:
      model.name="M1aaa",
      #Look at the grid!
      model.varinames=c("inf_evt",
                        "inf_noinf",
                        "fb_evt",
                        "fb_nofb",
                        "exprat_evt",
                        "exprat",
                        "moodrat_evt",
                        "moodrat",
                        "PE_congruent"),
      regtype=".1D", #To use fsl 3 col, do '_FSL3col.txt'
      #If to convolve with nuisance regressors with dependlab package:
      ifnuisa=FALSE,
      #Single subject FSL template path
      ssub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_m1aaa_usedby_R.fsf",
      #ADAPTIVE GROUP LEVEL GFEAT TEMPLATE;
      adaptive_gfeat=TRUE,
      #Group level FSL template path [MAYBE LATER DEPRECIATE THIS COMAND AND JUST PUT IT TO GITHUB / PACK IT]
      gsub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_gfeat_general_adaptive_template.fsf",
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
      graphic.threshold=0.95,
      #New sets of arguments for motion sensoring...
      nuisa_motion=c("nuisance","motion_par"),
      motion_type="fd",
      motion_threshold="default",
      convlv_nuisa=F
      #Add more universal arguements in here:
    ))
    argu<-model1aaa
  }
  if (argu_8c) {
    #Setting some global options (Putting moving variables here so the function down there could just grab them)
    argu_8c<-as.environment(list(
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
      onlyrun=(NULL),
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
    
    all_p<-lapply(bothSONs$SON1$list,function(dfx){
      #dfx<-dfx[dfx$VisitType=="1",]
      ID<-unique(dfx$FullID)
      dfx<-dfx[grep("oneLR_fixD_oneK_TD_Value_",names(dfx))]
      dfx$TrialNum<-1:nrow(dfx)
      mdfx<-melt(dfx,id.vars="TrialNum")
      mdfx$cat<-gsub("oneLR_fixD_oneK_TD_Value_","",mdfx$variable)
      mdfx$cat_s[mdfx$cat < 5]<-"Reinf"
      mdfx$cat_s[mdfx$cat > 4]<-"Plac"
      mdfx$cat[mdfx$cat_s=="Plac"]<-as.numeric(mdfx$cat[mdfx$cat_s=="Plac"])-4
      a<-ggplot(mdfx,aes(TrialNum,value,color=cat))+geom_smooth()+facet_wrap(~cat_s)
      ggsave(filename = paste0("./plots/oneK_TD",ID,".png"),plot = a)
    })
    
    
        
    
      
    ggplot(dfx,aes(TrialNum,oneLR_fixD_oneK_PE,color=Participant))+geom_smooth()
    ggplot(dfx,aes(TrialNum,oneLR_fixD_oneK_,color=InfusionNum))+geom_smooth()
  }
  if (YES) {
argu$adaptive_ssfeat<-FALSE
argu$model.name="YES_t"
argu$gridpath="/Volumes/bek/neurofeedback/scripts/pecina/grid_YES.csv"
argu$model.varinames=c("inf_evt","fb_evt","Inf_FbOnly","Inf_NoFbOnly")
argu$ssub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_nfb_YESt_usedby_R.fsf"
}

if (TP) {
  argu$adaptive_ssfeat<-FALSE
  argu$model.name="TP"
  argu$gridpath="/Volumes/bek/neurofeedback/scripts/pecina/grid_TP.csv"
  argu$model.varinames=c("inf_evt","inf_noinf","fb_evt","fb_nofb","true_plac","ExpRat_Miss","MoodRat_Miss")
  argu$ssub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_nfb_TP_usedby_R.fsf"
}
}




