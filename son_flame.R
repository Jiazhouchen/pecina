####DO FLAME;
Sys.setenv(runPIPE=FALSE)
source("son1_fsl.R")

fsltemplate<-readLines("/Volumes/bek/helper_scripts/fsl_pipe/templates/fsl_flame_general_adaptive_template.fsf")

grouplevel_df<-do.call(rbind,lapply(son1_rework,function(da){
  dfa<-da$son1_single
  dfa<-dfa[which(dfa$VisitType == da$adminfilter),]
  as.data.frame(as.list(apply(dfa,2,function(x){
    uxa<-unique(x)
    if(length(unique(uxa))==1 && !is.na(uxa)){
      return(uxa)
    } else {return(NA)}
    })
  ))
})
)
grouplevel_df<-grouplevel_df[names(grouplevel_df)[which(apply(grouplevel_df,2,function(x){any(!is.na(x))}))]]
grouplevel_df$Age<-rnorm(nrow(grouplevel_df),mean=24,2)
grouplevel_df$ID<-grouplevel_df$uID
argu$grouplevel_df<-grouplevel_df

argu$grouplevel_var<-c("Intercept")


argu$grid<-read.csv(argu$gridpath,stringsAsFactors = F)
########Outside of function


default_ls<-list(glvl_zthresh=3.09,glvl_prob_thresh=0.05,glvl_thresh_type=3,glvl_overwrite=T,lv2_copenum=1)
default_ls<-default_ls[!names(default_ls) %in% names(argu)]

for(lx in 1:length(default_ls)){
  message("Variable: '",names(default_ls)[lx],"' is not set, will use default value: ",default_ls[[lx]])
}
argu<-list2env(default_ls,envir = argu)

pasteFSF<-function(fsfvari="",value="",addComment=NULL,quotevalue=F,featfile=F){
  if(quotevalue) {value<-paste0("\"",value,"\"")}
  if(featfile) {syx<-"feat_files("} else {syx<-"fmri("}
  c(addComment,paste0("set ",syx,fsfvari,")"," ",value))
}


#Do the single entry ones;
Head_text<-c(
pasteFSF(fsfvari = "thresh",value = argu$glvl_thresh_type,
         addComment = "# Thresholding \n # 0 : None \n # 1 : Uncorrected \n# 2 : Voxel \n # 3 : Cluster \n",quotevalue = F),
pasteFSF(fsfvari = "prob_thresh",value = argu$glvl_prob_thresh,addComment = "# P threshold",quotevalue = F),
pasteFSF(fsfvari = "z_thresh",value = argu$glvl_zthresh,addComment = "# Z threshold",quotevalue = F),
pasteFSF(fsfvari = "regstandard",value = argu$templatedir,addComment = "# Standard image",quotevalue = F),
pasteFSF(fsfvari = c("ncopeinputs",paste("copeinput",1:argu$lv2_copenum,sep = ".")),value = 1,
         addComment = "# Number of lower-level copes feeding into higher-level analysis;
         # Change lv2_copenum to do more",quotevalue = F)
)



if("Intercept" %in% argu$grouplevel_var){
  gvar_df<-argu$grouplevel_df[c("ID",argu$grouplevel_var[!argu$grouplevel_var %in% "Intercept"])]
  gvar_df$Intercept <- 1
  if(is.null(gvar_df$Group_Membership)) {gvar_df$Group_Membership<-1}

}

#new func;
lvl2_featname="average.gfeat"
num_sess_feat = 1


raw<-system(paste0("find ",
                   file.path(argu$ssub_outputroot,argu$model.name,"*/",lvl2_featname),
                   " -iname '*.feat' -maxdepth 2 -mindepth 1 -type d"),intern = T)
raw.split <- strsplit(raw,split = .Platform$file.sep) 
allcope_df<-do.call(rbind,lapply(raw.split,function(x){
  data.frame(
  ID=x[grep(lvl2_featname,x)-1],
  COPENUM=gsub("cope","",gsub(".feat","",x[grep(lvl2_featname,x)+1],fixed = T)),
  PATH = paste(x,collapse = .Platform$file.sep),stringsAsFactors = F)
})
)

copenum = 1
alldf<-lapply(6:13,function(copenum){
  subcopedf <- allcope_df[which(allcope_df$COPENUM==copenum),]
  gvar_cope_df <- merge(gvar_df,subcopedf,by = "ID",all.x = T)
  
  gvar_cope_df<-na.omit(gvar_cope_df)
  
  message("Running FLAME on '",argu$grid$name[copenum],"'. Sample size of: ",nrow(gvar_cope_df),"\n","List of IDs included: \n",paste(gvar_cope_df$ID,collapse = ", "))
  
  numev<-length(argu$grouplevel_var) #For now, when doing group it's different. 
  
  #Do one sample here:
  ev_mat<-as.matrix(gvar_cope_df[argu$grouplevel_var])
  ct_mat<-  diag(x = 1,nrow = ncol(gvar_cope_df[argu$grouplevel_var]),ncol = ncol(gvar_cope_df[argu$grouplevel_var]),names = rep(argu$grouplevel_var,2))
  colnames(ct_mat)<-colnames(ev_mat)
  rownames(ct_mat)<-argu$grouplevel_var

  
  Ev_text_re<-unlist(lapply(1:ncol(ev_mat),function(evnum) {
    base_text<-c(paste0("# EV ",evnum),
                 pasteFSF(fsfvari = paste0("evtitle",evnum),value = colnames(ev_mat)[evnum],addComment =NULL,quotevalue = T),
                 pasteFSF(fsfvari = paste0("shape",evnum),value = 2,addComment =NULL,quotevalue = F),
                 pasteFSF(fsfvari = c(paste0("convolve",evnum),paste0("convolve_phase",evnum),paste0("tempfilt_yn",evnum),paste0("deriv_yn",evnum)
                 ),value = 0,addComment =NULL,quotevalue = F),
                 pasteFSF(fsfvari = paste0("custom",evnum),value = "dummy",addComment =NULL,quotevalue = T)
    )
    ortho_text<-unlist(lapply(0:ncol(ev_mat),function(nc){pasteFSF(fsfvari = paste0("ortho",evnum,".",nc),value = 0,addComment =NULL,quotevalue = F)}))
    input_text<-unlist(lapply(1:nrow(ev_mat),function(ns){
      pasteFSF(fsfvari = paste0("evg",ns,".",evnum),value = ev_mat[ns,evnum],addComment =NULL,quotevalue = F)
    }))
    return(c(base_text,ortho_text,input_text))
  })
  )
  Ev_text<-c(Ev_text_re,pasteFSF(fsfvari = c("evs_orig","evs_real"),value = ncol(ev_mat),addComment =NULL,quotevalue = F),
             pasteFSF(fsfvari = "evs_vox",value = 0,addComment =NULL,quotevalue = F)
             )
  #Contrast
  Contrast_text<-c(unlist(lapply(1:nrow(ct_mat),function(ctnum){
    c(paste0("# Contrast ",ctnum),
      pasteFSF(fsfvari = paste("conpic_real",ctnum,sep = "."),value = 1,addComment =NULL,quotevalue = F),
      pasteFSF(fsfvari = paste("conname_real",ctnum,sep = "."),value = colnames(ct_mat)[ctnum],addComment =NULL,quotevalue = T),
      unlist(lapply(1:ncol(ct_mat),function(ny){pasteFSF(fsfvari = paste0("con_real",ctnum,".",ny),value = ct_mat[ctnum,ny],addComment =NULL,quotevalue = F)})),
      pasteFSF(fsfvari = paste0("conmask",ctnum,"_",which(!1:nrow(ct_mat) %in% ctnum)),value = 0,addComment ="##F-Test Variables",quotevalue = F)
      )
  })),
  pasteFSF(fsfvari = c("con_mode_old","con_mode"),value = "real",addComment = "######### Display images for contrast_real",quotevalue = F),
  pasteFSF(fsfvari = c("ncon_orig","ncon_real"),value = nrow(ct_mat),addComment = "######### number of contrasts",quotevalue = F),
  pasteFSF(fsfvari = c("nftests_orig","nftests_real"),value = 0,addComment = "######### number of F tests",quotevalue = F)
  )
  
  #Group input
  Groupinput_text<-c(
    pasteFSF(fsfvari = paste("groupmem",1:nrow(gvar_cope_df),sep = "."),
             value = gvar_cope_df$Group_Membership,addComment = "######### # Group membership for input ",quotevalue = F),
    pasteFSF(fsfvari = c("npts","multiple"),
             value = nrow(gvar_cope_df),addComment = "######### Number of first-level analyses",quotevalue = F),
    pasteFSF(fsfvari = 1:nrow(gvar_cope_df),
             value = gvar_cope_df$PATH,addComment = "######### # 4D AVW data or FEAT directory ",quotevalue = T,featfile = T)
  )
 
  fsf_final<-c(fsltemplate,
               pasteFSF(fsfvari = "outputdir",value = file.path(argu$glvl_output,argu$model.name,argu$grid$name[copenum]),addComment = "# Output directory",quotevalue = T),
               Head_text,Groupinput_text,Ev_text,Contrast_text)
  dir.create(file.path(argu$glvl_output,argu$model.name,"fsf_files"),recursive = T,showWarnings = F)
  writeLines(fsf_final,file.path(argu$glvl_output,argu$model.name,"fsf_files",paste0("grouplvl_",argu$grid$name[copenum],"grouplvl.fsf")))
  return(gvar_cope_df)
})

library(parallel)
fsf_ls<-list.files(path = file.path(argu$glvl_output,argu$model.name,"fsf_files"),pattern = ".*.fsf",full.names = T,recursive = F)
cl_glvl<-parallel::makeCluster(argu$nprocess,type = "FORK")
NX<-parallel::parLapply(cl_glvl,fsf_ls,function(yx) {
  fsl_2_sys_env()
  message("starting to run: /n ",basename(yx))
  tryCatch(
    {system(command = paste("feat",yx),intern = T)
     message("DONE")
    }, error=function(e){stop(paste0("feat unsuccessful...error: ", e))}
  )
  
})
parallel::stopCluster(cl_glvl)






###Group


