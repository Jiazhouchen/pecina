############################################
####### New Functions for regressor ########
############################################
#Do a source utility scripts from git (when we have enough we will make a function out of it...)
#Check required packages:
rm(list = ls())
require("devtools")
devtools::install_github("DecisionNeurosciencePsychopathology/fMRI_R",force = F,quiet = T,ask = FALSE)
library(fslpipe)
if("dependlab" %in% installed.packages()){"GREAT, DEPENDLAB PACK IS INSTALLED"}else{devtools::install_github("PennStateDEPENdLab/dependlab",force=F)}
source('pecina_R_utility_function.R')

#Setting up FSL global enviroment variables in case we are using RStudio
fsl_2_sys_env()
if(Sys.getenv("runPIPE")==""){runPIPE<-TRUE}else{runPIPE<-Sys.getenv("runPIPE")}
singlesub<-F
runQC<-F
######
#Actual arguments for each model. Should follow template: github.com/DecisionNeurosciencePsychopathology/fMRI_R
####BE AWARE!
argu<-as.environment(list(nprocess=11,onlyrun=NULL,forcereg=F,cfgpath="/Volumes/bek/autopreprocessing_pipeline/Neurofeedback/nfb.cfg",
                          regpath="/Volumes/bek/neurofeedback/sonrisa1/nfb/regs/R_fsl_reg",func.nii.name="nfswudktm*[0-9]_[0-9].nii.gz",
                          group_id_sep=NULL,regtype=".1D", convlv_nuisa=FALSE,adaptive_gfeat=TRUE,adaptive_ssfeat=TRUE,randomize_demean=FALSE,
                          gsub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_gfeat_general_adaptive_template.fsf",
                          ssub_outputroot="/Volumes/bek/neurofeedback/sonrisa1/nfb/ssanalysis/fsl",centerscaleall=FALSE,
                          glvl_outputroot="/Volumes/bek/neurofeedback/sonrisa1/nfb/grpanal/fsl",
                          templatedir="/Volumes/bek/Newtemplate_may18/fsl_mni152/MNI152_T1_2mm_brain.nii",whichttest = c("onesample"),
                          ssub_fsl_templatepath="/Volumes/bek/neurofeedback/scripts/fsl/templates/fsl_ssfeat_general_adaptive_template_R.fsf",
                          glvl_output="/Volumes/bek/neurofeedback/sonrisa1/nfb/grpanal/fsl",ifoverwrite_secondlvl=FALSE,hig_lvl_path_filter=NULL,
                          graphic.threshold=0.95,nuisa_motion=c("nuisance","motion_par"),motion_type="fd", motion_threshold="default",convlv_nuisa=F))
#DO NOT PROVIDE THOSE TWO AND IT WILL BE FINE;
#argu$thresh_cluster_extent<-3.1 
#argu$thresh_cluster_mass<-3.1
argu$cfg<-cfg_info(cfgpath = argu$cfgpath)
argu$randomize_p_threshold<-0.001
argu$randomize_thresholdingways<-c("tfce","voxel-based","cluster-based-mass","cluster-based-extent")
argu$ss_zthreshold<-3.2  #This controls the single subject z threshold (if enabled in template)
argu$ss_pthreshold<-0.05 #This controls the single subject p threshold (if enabled in template)

ValuePE_FixPara_Int<-F
ValuePE_u_Int<-F
BehModel_GM_new<-F
ValuePE_FixPara<-F
Fb<-F
ValuePE<-F
BehModel_GM<-T
exp_rating<- F
PlacInReinf<-F
ReinfInPlac<-F
Value<-F
PE<-F
Value_RATEVT<-F
PE_RATEVT<-F

if(ValuePE_FixPara_Int){
  argu$model.name="ValuePE_FixPara_Int"
  argu$gridpath="/Volumes/bek/neurofeedback/scripts/pecina/grid_ValuePE_fixPara_Int.csv"
  argu$centerscaleall=TRUE
  argu$proc_id_subs="_a"
  argu$adminfilter=1
}

if(ValuePE_u_Int){
  argu$model.name="ValuePE_u_Int"
  argu$gridpath="/Volumes/bek/neurofeedback/scripts/pecina/grid_ValuePE_u_Int.csv"
  argu$centerscaleall=TRUE
  argu$proc_id_subs="_a"
  argu$adminfilter=1
}


if(ValuePE_FixPara){
  argu$model.name="ValuePE_FixPara"
  argu$gridpath="/Volumes/bek/neurofeedback/scripts/pecina/grid_ValuePE_fixPara.csv"
  argu$centerscaleall=TRUE
  argu$proc_id_subs="_a"
  argu$adminfilter=1
}

if(Fb){
  argu$model.name="Fb"
  argu$gridpath="/Volumes/bek/neurofeedback/scripts/pecina/grid_feedback.csv"
  argu$centerscaleall=TRUE
  argu$proc_id_subs="_a"
  argu$adminfilter=1
}

if(BehModel_GM){
  argu$model.name="BehModel_GM"
  argu$gridpath="/Volumes/bek/neurofeedback/scripts/pecina/grid_alignment3c.csv"
  argu$centerscaleall=TRUE
  argu$proc_id_subs="_a"
  argu$adminfilter=1
}

if(BehModel_GM_new){
  argu$model.name="BehModel_GM_new"
  argu$gridpath="/Volumes/bek/neurofeedback/scripts/pecina/grid_alignment3c_new.csv"
  argu$centerscaleall=TRUE
  argu$proc_id_subs="_a"
  argu$adminfilter=1
}

if (ValuePE) {
  argu$model.name="ValuePE"
  argu$gridpath="/Volumes/bek/neurofeedback/scripts/pecina/grid_ValuePE.csv"
  argu$centerscaleall=TRUE
  argu$proc_id_subs="_a"
  argu$adminfilter=1
}

if (exp_rating) {
  argu$model.name="exp_rating"
  argu$gridpath="/Volumes/bek/neurofeedback/scripts/pecina/grid_exprat.csv"
  argu$centerscaleall=TRUE
  argu$proc_id_subs="_a"
  argu$adminfilter=1
}

if (PlacInReinf) {
  argu$model.name="PlacInReinf"
  argu$gridpath="/Volumes/bek/neurofeedback/scripts/pecina/grid_PlacInReinf.csv"
  argu$centerscaleall=TRUE
  argu$proc_id_subs="_a"
  argu$adminfilter=1
}

if (ReinfInPlac) {
  argu$model.name="ReinfInPlac"
  argu$gridpath="/Volumes/bek/neurofeedback/scripts/pecina/grid_ReinfInPlac.csv"
  argu$centerscaleall=TRUE
  argu$proc_id_subs="_a"
  argu$adminfilter=1
}

if (Value_RATEVT) {
  argu$model.name="Value_RateEVT"
  argu$gridpath="/Volumes/bek/neurofeedback/scripts/pecina/grid_Value_RateEvt.csv"
  argu$centerscaleall=TRUE
  argu$proc_id_subs="_a"
  argu$adminfilter=1
}

if (PE_RATEVT) {
  argu$model.name="PE_RateEVT"
  argu$gridpath="/Volumes/bek/neurofeedback/scripts/pecina/grid_PE_RateEvt.csv"
  argu$centerscaleall=TRUE
  argu$proc_id_subs="_a"
  argu$adminfilter=1
}

###########Official Start:###########
#Supposedly you shouldn't need to change anything down below:
#If you are just switching models
son1_rework<-nfb_getdata(boxdir = "~/Box",grp_sep = argu$group_id_sep,proc_id_sub = argu$proc_id_subs,QCflag=runQC)

if(singlesub){
  son1_rework<-son1_rework["SON1_018"]
}

if(!is.null(argu$group_id_sep) && dir.exists(file.path(argu$ssub_outputroot,argu$model.name))) {
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

#####The new TD Models will come as two part;#######Temporary fix currently will be consdiered for next iteration of the pipeline
if(F){
  # fsl_pipe(
  #   argu=argu, #This is the arguments environment, each model should have a different one;
  #   prep.call.func="prep.son1", #This should be a character string that's the name of the prep proc function
  #   prep.call.allsub=son1_rework #List of ID list of arguments for prep.call.
  # )
  for(idpath in list.dirs(paste(argu$regpath,argu$model.name,sep = .Platform$file.sep),recursive = F) ) {
    for(currun in 1:length(list.files(path = idpath,pattern = "TDS_EV_flat.1D"))){
      x1<-as.numeric(readLines( file.path(idpath,paste0("run",currun,"_TDS_EV_flat.1D") )))
      x2<-as.numeric(readLines( file.path(idpath,paste0("run",currun,"_TDS_PE_up.1D") )))
      writeLines(text = as.character(x1+x2),file.path(idpath,paste0("run",currun,"_TDS_CONCAT.1D") ))
    }
  }
  argu$gridpath="/Volumes/bek/neurofeedback/scripts/pecina/grid_TDS_post.csv"
  argu$old_onlyrun -> argu$onlyrun
}
##################################
 if(runPIPE){
stop("DIDN'T WORK!")
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
 

}
#########Additional Functions##########


####HEAT HEATMAP: #####
if(F){
load(file.path(argu$ssub_outputroot,argu$model.name,"design.rdata"))
fslpipe::make_heatmap_with_design( allsub.design$SON1_018)
}
####ROI Extraction#####
if(F) {
  valuePE_roi<-roi_getvalue(rootdir="/Volumes/bek/neurofeedback/sonrisa1/nfb/ssanalysis/fsl",
                                grproot="/Volumes/bek/neurofeedback/sonrisa1/nfb/grpanal/fsl", forceconcat = T,
                                modelname="ValuePE",
                                basemask="tstat",corrp_mask="tstat",saveclustermap=TRUE,Version="stast2.7_20",corrmaskthreshold=2.7,
                                roimaskthreshold=0.0001, voxelnumthres=20, clustertoget=NULL,copetoget=NULL,maxcore=6)
  
  valuePE_noprior_roi<-roi_getvalue(rootdir="/Volumes/bek/neurofeedback/sonrisa1/nfb/ssanalysis/fsl",
                            grproot="/Volumes/bek/neurofeedback/sonrisa1/nfb/grpanal/fsl",forceconcat = T,
                            modelname="ValuePE_noprior",
                            basemask="tstat",corrp_mask="tstat",saveclustermap=TRUE,Version="tstat_2.5",corrmaskthreshold=2.5,
                            roimaskthreshold=0.0001, voxelnumthres=20, clustertoget=NULL,copetoget=NULL,maxcore=6)
  
  exp_rating_roi<-roi_getvalue(rootdir="/Volumes/bek/neurofeedback/sonrisa1/nfb/ssanalysis/fsl",
                            grproot="/Volumes/bek/neurofeedback/sonrisa1/nfb/grpanal/fsl", forceconcat = T,
                            modelname="exp_rating",
                            basemask="tstat",corrp_mask="tstat",saveclustermap=TRUE,Version="tstat_2.5_20",corrmaskthreshold=2.5,
                            roimaskthreshold=0.0001, voxelnumthres=20, clustertoget=NULL,copetoget=12,maxcore=6)
  
  BehModel_GM_roi<-roi_getvalue(rootdir="/Volumes/bek/neurofeedback/sonrisa1/nfb/ssanalysis/fsl",
                                    grproot="/Volumes/bek/neurofeedback/sonrisa1/nfb/grpanal/fsl",forceconcat = T,
                                    modelname="BehModel_GM",
                                    basemask="tstat",corrp_mask="tfce",saveclustermap=TRUE,Version="tfce_090",corrmaskthreshold=0.90,
                                    roimaskthreshold=0.0001, voxelnumthres=20, clustertoget=NULL,copetoget=c(12,7),maxcore=6)
  
  ValuePE_FixPara_roi<-roi_getvalue(rootdir="/Volumes/bek/neurofeedback/sonrisa1/nfb/ssanalysis/fsl",
                                grproot="/Volumes/bek/neurofeedback/sonrisa1/nfb/grpanal/fsl",forceconcat = T,
                                modelname="ValuePE_FixPara",
                                basemask="tstat",corrp_mask="tfce",saveclustermap=TRUE,Version="tfce_095_95",corrmaskthreshold=0.95,
                                roimaskthreshold=0.0001, voxelnumthres=0, clustertoget=NULL,copetoget=c(10,7),maxcore=6)
  
  # for(x in names(valuePE_roi)){
  #   names(valuePE_roi[[x]]$roivalues)<-paste0(x,"_",names(valuePE_roi[[x]]$roivalues))
  #   
  # }
  
  ValuePE_FixPara_roi<-lapply(ValuePE_FixPara_roi,function(xrz){
    xr<-xrz$roivalues
    if(!is.null(xr)){
   # print(names(xr))
    ID_var <- names(xr)[grepl("ID",names(xr))]
    xr_var <- names(xr)[!grepl("ID",names(xr))]
    xr_num <- xr[xr_var]
    ID <- xr[ID_var]
      xrz$roivalues<-cbind(ID,as.data.frame(apply(xr_num,2,as.numeric)))
    } else{xrz<-NULL}
    return(xrz)
  })
  #Example:
  df<-ValuePE_FixPara_roi$cope_7$roivalues #take the output, cope 7, 
  #df<-df[c("cluster_3","ID")] #take only cluster 3 and ID
  df$FullID<-paste0(df$ID,"_1") #make full ID
  df1_admin1_wroi<-merge(df1_admin1,df,by = "FullID",all = T)
 
  #Example:
  df1<-ValuePE_FixPara_roi$cope_10$roivalues #take the output, cope 7, 
  #df<-df[c("cluster_3","ID")] #take only cluster 3 and ID
  df1$FullID<-paste0(df$ID,"_1") #make full ID
  ValuePE_FixPara_roi<-merge(df1_admin1_wroi,df1,by = "FullID",all = T)
  names(df1_admin1_wroi)
  
  df2<-valuePE_roi$cope_10$roivalues #take the output, cope 7, 
  #df<-df[c("cluster_3","ID")] #take only cluster 3 and ID
  df1$FullID<-paste0(df$ID,"_1") #make full ID
  df1_admin1_wroi<-merge(df1_admin1_wroi,df1,by = "FullID",all = T)
  names(df1_admin1_wroi)
  
  #PCA ANALYSIS#################
  library(corrplot)
  library(factoextra)
  library(readxl)
  xr<-BehModel_GM_roi$cope_12$roivalues
  roiID<-xr[,grepl("ID",names(xr))]
  just_rois<-xr[,!grepl("ID",names(xr))]
  # feedROIcor <- cor(just_rois)
  # corrplot(feedROIcor,type = "upper", tl.pos = "td", cl.lim=c(0,1),
  #          method = "circle", tl.cex = 1, tl.col = 'black',
  #          order = "hclust", diag = FALSE, 
  #          addCoef.col="black", addCoefasPercent = FALSE,
  #          p.mat = 1-feedROIcor, sig.level=0.3, insig = "blank")
  # 
  feedroipca <- prcomp(just_rois, center = TRUE, scale = TRUE)
  # print(feedroipca)
  # summary(feedroipca)
  # plot(feedroipca)
  feed_pcs <- get_pca_ind(feedroipca)
  cope_7_feed1pc <- feed_pcs$coord[,1]

  xr<-valuePE_roi$cope_10$roivalues
  roiID<-xr[,grepl("ID",names(xr))]
  just_rois<-xr[,!grepl("ID",names(xr))]
  
  feedroipca <- prcomp(just_rois, center = TRUE, scale = TRUE)
  
  # print(feedroipca)
  # summary(feedroipca)
  # plot(feedroipca)
  feed_pcs <- get_pca_ind(feedroipca)
  cope_10_feed1pc <- feed_pcs$coord[,1]
  
  
  pcas<-data.frame(cope7_PC1=cope_7_feed1pc,cope10_PC1=cope_10_feed1pc,ID=roiID)
  pcas$FullID<-paste0(pcas$ID,"_1") #make full ID
  df1_admin1_wroi<-merge(df1_admin1_wroi,pcas,by = "FullID",all = T)
  #Create Table:
  #cluster -i OneSampT_tfce_corrp_tstat1.nii.gz -t 0.95 --mm > cluster_t1_95.txt
  #atlasquery -a "MNI Structural Atlas" -m "OneSampT_tfce_corrp_tstat1.nii.gz"
  
  
  #FA#########################
  whichcope<-"cope_7"
  xr<-valuePE_roi[[whichcope]]$roivalues
  roiID<-paste0(xr[,grepl("ID",names(xr))],"_1")
  just_rois<-xr[,!grepl("ID",names(xr))]
  just_rois<-data.matrix(just_rois, rownames.force = NA)
  clust_cor <- cor(just_rois,method = 'pearson')
  
  setwd('~/Desktop/')
  pdf("Value_cluster_corr_fixed.pdf", width=12, height=12)
  corrplot(clust_cor, cl.lim=c(-1,1),
           method = "circle", tl.cex = 1.5, type = "upper", tl.col = 'black',
           order = "hclust", diag = FALSE,
           addCoef.col="black", addCoefasPercent = FALSE,
           p.mat = 1-clust_cor, sig.level=0.75, insig = "blank")
  dev.off()
  library(psych)
  #test the number of factors
  fa.parallel(just_rois)
  vss(just_rois)
  #Factor analyze
  mvalue <- nfactors(clust_cor, n=5, rotate = "oblimin", diagonal = FALSE,fm = "mle", n.obs = 35, SMC = FALSE)
  value.fa = psych::fa(just_rois, nfactors=3, rotate = "oblimin", fm = "mle")
  fa.diagram(value.fa)
  fscores <- factor.scores(just_rois, pe.fa)$scores
  # write  factor scores to your dataframe
  FA_df<-as.data.frame(fscores)
  names(FA_df)<-paste(names(FA_df),whichcope,sep = "_")
  df1_admin1_wFA<-merge(df1_admin1_wroi,cbind(roiID,FA_df),by.x = "FullID",by.y = "roiID",all = T)
  
  FA_df<-as.data.frame(fscores)
  names(FA_df)<-paste(names(FA_df),whichcope,sep = "_")
  df1_admin1_wFA<-merge(df1_admin1_wFA,cbind(roiID,FA_df),by.x = "FullID",by.y = "roiID",all = T)
  
  
  ##!!!!!GETTING TIME SERIRES IS TIME CONSUMING AND SHOULD CONSIDER SAVING THE RESULTS!!!#
  ###Cope 7: 100 (VS), 86 (mPFC), 76 (rACC), 71 (vlPFC), 73 (lOFC).
  ###Cope 10: 87 (VS), 78 (lOFC)
  if(file.exists("allcpcl.rdata")){load("allcpcl.rdata")}else{
    allcpcl<-new.env()
    allcpcl$CP7CL100<-get_timeserires(ssub_root = "/Volumes/bek/neurofeedback/sonrisa1/proc/",templatepath = "/Volumes/bek/Newtemplate_may18/fsl_mni152/MNI152_T1_2mm_brain.nii",
                                      maskpath = "/Volumes/bek/neurofeedback/sonrisa1/nfb/grpanal/fsl/ValuePE/cope7_randomize_onesample_ttest/ROI_masks_stast2.7_20/cluster_mask_100.nii.gz",
                                      tarname = "nfswudktm_nfb[0-9]_7.nii.gz",submaskname = "subject_mask.nii.gz",parallen = 10,depthcontr = 3)
    
    allcpcl$CP7CL86<-get_timeserires(ssub_root = "/Volumes/bek/neurofeedback/sonrisa1/proc/",templatepath = "/Volumes/bek/Newtemplate_may18/fsl_mni152/MNI152_T1_2mm_brain.nii",
                                     maskpath = "/Volumes/bek/neurofeedback/sonrisa1/nfb/grpanal/fsl/ValuePE/cope7_randomize_onesample_ttest/ROI_masks_stast2.7_20/cluster_mask_86.nii.gz",
                                     tarname = "nfswudktm_nfb[0-9]_7.nii.gz",submaskname = "subject_mask.nii.gz",parallen = 10,depthcontr = 3)
    allcpcl$CP7CL76<-get_timeserires(ssub_root = "/Volumes/bek/neurofeedback/sonrisa1/proc/",templatepath = "/Volumes/bek/Newtemplate_may18/fsl_mni152/MNI152_T1_2mm_brain.nii",
                                     maskpath = "/Volumes/bek/neurofeedback/sonrisa1/nfb/grpanal/fsl/ValuePE/cope7_randomize_onesample_ttest/ROI_masks_stast2.7_20/cluster_mask_76.nii.gz",
                                     tarname = "nfswudktm_nfb[0-9]_7.nii.gz",submaskname = "subject_mask.nii.gz",parallen = 10,depthcontr = 3)
    allcpcl$CP7CL71<-get_timeserires(ssub_root = "/Volumes/bek/neurofeedback/sonrisa1/proc/",templatepath = "/Volumes/bek/Newtemplate_may18/fsl_mni152/MNI152_T1_2mm_brain.nii",
                                     maskpath = "/Volumes/bek/neurofeedback/sonrisa1/nfb/grpanal/fsl/ValuePE/cope7_randomize_onesample_ttest/ROI_masks_stast2.7_20/cluster_mask_76.nii.gz",
                                     tarname = "nfswudktm_nfb[0-9]_7.nii.gz",submaskname = "subject_mask.nii.gz",parallen = 10,depthcontr = 3)
    allcpcl$CP7CL73<-get_timeserires(ssub_root = "/Volumes/bek/neurofeedback/sonrisa1/proc/",templatepath = "/Volumes/bek/Newtemplate_may18/fsl_mni152/MNI152_T1_2mm_brain.nii",
                                     maskpath = "/Volumes/bek/neurofeedback/sonrisa1/nfb/grpanal/fsl/ValuePE/cope7_randomize_onesample_ttest/ROI_masks_stast2.7_20/cluster_mask_71.nii.gz",
                                     tarname = "nfswudktm_nfb[0-9]_7.nii.gz",submaskname = "subject_mask.nii.gz",parallen = 10,depthcontr = 3)
    allcpcl$CP10CL87<-get_timeserires(ssub_root = "/Volumes/bek/neurofeedback/sonrisa1/proc/",templatepath = "/Volumes/bek/Newtemplate_may18/fsl_mni152/MNI152_T1_2mm_brain.nii",
                                      maskpath = "/Volumes/bek/neurofeedback/sonrisa1/nfb/grpanal/fsl/ValuePE/cope10_randomize_onesample_ttest/ROI_masks_stast2.7_20/cluster_mask_87.nii.gz",
                                      tarname = "nfswudktm_nfb[0-9]_7.nii.gz",submaskname = "subject_mask.nii.gz",parallen = 10,depthcontr = 3)
    allcpcl$CP10CL78<-get_timeserires(ssub_root = "/Volumes/bek/neurofeedback/sonrisa1/proc/",templatepath = "/Volumes/bek/Newtemplate_may18/fsl_mni152/MNI152_T1_2mm_brain.nii",
                                      maskpath = "/Volumes/bek/neurofeedback/sonrisa1/nfb/grpanal/fsl/ValuePE/cope10_randomize_onesample_ttest/ROI_masks_stast2.7_20/cluster_mask_78.nii.gz",
                                      tarname = "nfswudktm_nfb[0-9]_7.nii.gz",submaskname = "subject_mask.nii.gz",parallen = 10,depthcontr = 3)
    save(allcpcl,file = "allcpcl.rdata")
  }
  
  
  
  son1_alltsbeta<-lapply(objects(allcpcl),function(nx){
    if(grepl("CP7",nx)){evtn="ExpRat"} else if (grepl("CP10",nx)){evtn="feedback"}else{stop("evt not supported")}
    deconv_timeseries(datalist = nfb_getdata(boxdir = "~/Box",grp_sep = c("a","b"),proc_id_sub = NULL,QCflag=F,verbose = F),
                              tslist =  get(nx,envir = allcpcl),
                              func.proc = prep.son1,evtname = evtn,tr = argu$cfg$preproc_call$tr,num.calibrate = 1,
                              variname = nx,func.deconv = mean)
    
  })
  df_to_merge<-df1_admin1
  for (ty in son1_alltsbeta) {
    aty<-do.call(rbind,ty)
    aty$FullID<-gsub("_b","_2",gsub("_a","_1",aty$ID))
    aty$Run<-aty$run; aty$TrialNum<-aty$trial; aty$run<-NULL;aty$ID<-NULL;aty$trial<-NULL;
    aty[[1]]<-as.numeric(scale(aty[[1]]))
    df_to_merge<-merge(df_to_merge,aty,by = c("FullID","Run","TrialNum"),all.x = T)
  }  
  df1_admin1_wTSBETA<-df_to_merge      
}
 
 
  
 
 
 
 

