###

source_script_github <- function(gitscripturl) {
  # load package
  require(RCurl)
  # read script lines from website
  script <- getURL(gitscripturl, ssl.verifypeer = FALSE, followlocation = TRUE)
  # parse lines and evaluate in the global environment
  eval(parse(text = script), envir= .GlobalEnv)
}

###SON1 single sub Behavr Proc function:
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
  son1_single$plac_ctrl <- NA
  son1_single$plac_ctrl[son1_single$InfusionNum==1 | son1_single$InfusionNum==2] <- TRUE
  son1_single$plac_ctrl[son1_single$InfusionNum==3 | son1_single$InfusionNum==4] <- FALSE
  son1_single$plac_ctrl_r<-!son1_single$plac_ctrl
  #Creating a binary variable
  son1_single$plac_ctrl_bin<-son1_single$plac_ctrl
  son1_single$plac_ctrl_bin[which(son1_single$plac_ctrl_bin)]<-1
  son1_single$plac_ctrl_bin[which(!son1_single$plac_ctrl_bin)]<-(-1)

  
  son1_single$signal_baseline <- NA
  son1_single$signal_baseline[son1_single$Feedback=="Signal"] <- TRUE
  son1_single$signal_baseline[son1_single$Feedback=="Baseline"] <- FALSE
  son1_single$signal_baseline_r<-!son1_single$signal_baseline
  #Creating a binary variable
  son1_single$signal_baseline_bin<-son1_single$signal_baseline
  son1_single$signal_baseline_bin[which(son1_single$signal_baseline_bin)]<-1
  son1_single$signal_baseline_bin[which(!son1_single$signal_baseline_bin)]<-(-1)
  
  vba<-as.list(son1_single[c(!names(son1_all) %in% regualrvarinames)])
  vba<-addcenterscaletolist(vba)  ##Function Coming from fMRI_Dev Script
  #Add taskness variables to value
  vba$plac_ctrl<-son1_single$plac_ctrl
  vba$plac_ctrl_r<-son1_single$plac_ctrl_r
  vba$plac_ctrl_bin<-son1_single$plac_ctrl_bin
  vba$signal_baseline<-son1_single$signal_baseline
  vba$signal_baseline_r<-son1_single$signal_baseline_r
  vba$signal_baseline_bin<-son1_single$signal_baseline_bin
  
  #Safe guard this from NaNs:
  son1_single$WillImpRt[which(is.na(son1_single$WillImpRt))]<-2
  son1_single$ImprovedRt[which(is.na(son1_single$ImprovedRt))]<-2
  
  finalist<-list(infusion=data.frame(event="infusion",
                                     onset=son1_single$InfOnset,
                                     duration=son1_single$WillImpOnset - son1_single$InfOnset,
                                     run=son1_single$Run,
                                     trial=son1_single$TrialNum),
                 feedback=data.frame(event="feedback",
                                     onset=son1_single$Feed2Onset,
                                     duration=son1_single$ImprovedOnset - son1_single$Feed2Onset,
                                     run=son1_single$Run,
                                     trial=son1_single$TrialNum),
                 ExpRat=data.frame(event="ExpRat",
                                     onset=son1_single$WillImpOnset,
                                     duration=son1_single$WillImpRt,
                                     run=son1_single$Run,
                                     trial=son1_single$TrialNum),
                 MoodRat=data.frame(event="MoodRat",
                                   onset=son1_single$ImprovedOnset,
                                   duration=son1_single$ImprovedRt,
                                   run=son1_single$Run,
                                   trial=son1_single$TrialNum))
  for (i in 1:length(finalist)) {
    if (i==1) {ktz<-finalist[[i]]} else {
      ktz<-rbind(ktz,finalist[[i]])}
  }
  finalist[["allconcat"]]<-ktz
  output<-list(event.list=finalist,output.df=son1_single,value=vba)
}


##Con_Frame Prep
prep.confram<-function(singlesub=NULL) {
  conframe<-rbind(singlesub[[1]],singlesub[[2]])
  
  #PxH
  conframe$PxH<-FALSE
  conframe$PxH[conframe$Context=="Pleasant" & conframe$Emotion=="Happy"]<-TRUE
  #PxF
  conframe$PxF<-FALSE
  conframe$PxF[conframe$Context=="Pleasant" & conframe$Emotion=="Fearful"]<-TRUE
  #PxN
  conframe$PxN<-FALSE
  conframe$PxN[conframe$Context=="Pleasant" & conframe$Emotion=="Neutral"]<-TRUE
  #UxH
  conframe$UxH<-FALSE
  conframe$UxH[conframe$Context=="Unpleasant" & conframe$Emotion=="Happy"]<-TRUE
  #UxF
  conframe$UxF<-FALSE
  conframe$UxF[conframe$Context=="Unpleasant" & conframe$Emotion=="Fearful"]<-TRUE
  #UxN
  conframe$UxN<-FALSE
  conframe$UxN[conframe$Context=="Unpleasant" & conframe$Emotion=="Neutral"]<-TRUE
  
  finalist<-list(trial=data.frame(event="trial",
                                  onset=conframe$ContextOnset,
                                  duration=conframe$Duration,
                                  run=conframe$Order,
                                  trial=conframe$Trial)
  )
  finalist$allconcat<-finalist$trial
  vba<-list(PxH=conframe$PxH,
            PxF=conframe$PxF,
            PxN=conframe$PxN,
            UxH=conframe$UxH,
            UxF=conframe$UxF,
            UxN=conframe$UxN)
  
  
  output<-list(event.list=finalist,output.df=conframe,value=vba)
  return(output)
}



### FSL Group Level Analysis: 

glvl_all_cope<-function(rootdir="/Volumes/bek/neurofeedback/sonrisa1/nfb/ssanalysis/fsl",
                       outputdir="/Volumes/bek/neurofeedback/sonrisa1/nfb/grpanal/fsl",
                       modelname="PE_8C_old",
                       copestorun=1:8,
                       paralleln=NULL
) {
  if ( is.null(modelname) ) {stop("Must specify a model name other wise it will be hard to find all copes")}

#Ensure fsl are in path:
fsl_2_sys_env()

raw<-system(paste0("find ",file.path(rootdir,modelname,"*/average.gfeat")," -iname '*.feat' -maxdepth 2 -mindepth 1 -type d"),intern = T)
strsplit(raw,split = "/") ->raw.split
df.ex<-data.frame(ID=unlist(lapply(raw.split,function(x) {
  x[grep("average.gfeat",x)-1]
})),
COPENUM=unlist(lapply(raw.split,function(x) {
  x[grep("average.gfeat",x)+1]
})),
PATH=file.path(raw,"stats","cope1.nii.gz")
)
df.ex$COPENUM<-substr(df.ex$COPENUM,start=regexpr("[0-9]",df.ex$COPENUM),stop = regexpr(".feat",df.ex$COPENUM)-1)
if (max(aggregate(COPENUM~ID,data = df.ex,max)$COPENUM)<max(copestorun)) {stop("HEY! There's not that many copes to run! Change argument!")}
noIDpos<-which(aggregate(COPENUM~ID,data = df.ex,max)$COPENUM!=max(aggregate(COPENUM~ID,data = df.ex,max)$COPENUM) & aggregate(COPENUM~ID,data = df.ex,max)$COPENUM<max(copestorun))
if (length(noIDpos)>0){
noID<-aggregate(COPENUM~ID,data = df.ex,max)$ID[noIDpos]
print(paste("This ID:",noID,"does not have enough COPES, will be removed from running...."))
df.ex[which(!df.ex$ID %in% noID),]->df.ex
} else {print("All Good!")}
print("Now will run fslmerge and randomise, function will fail if FSL ENVIR is not set up. (Should not happen since it's guarded by func)")

cope.fslmerge<-lapply(copestorun,function(x) {
  outputroot<-file.path(outputdir,modelname,paste0("cope",x,"randomize_onesample_ttest"))
  dir.create(outputroot, showWarnings = FALSE)
  copefileconcat<-paste(df.ex$PATH[which(df.ex$COPENUM==x)],collapse = " ")
  paste0("fslmerge -t ",outputroot,"/OneSamp4D"," ",
         copefileconcat
         ," \n ",
         "randomise -i ",outputroot,"/OneSamp4D -o ",outputroot,"/OneSampT -1 -T -x -c 3"
         )
  })
sink(file="log.txt",split=TRUE)
if (!is.null(paralleln)){
  cj1<-makeCluster(paralleln,outfile="")
  NU<-parSapply(cj1,cope.fslmerge,function(x) {
    print(paste0("Now running ",cope.fslmerge))
    system(command = x,intern = T,ignore.stdout = F,ignore.stderr = F)
  })
  stopCluster(cj1)
} else {lapply(cope.fslmerge,function(x) {
  print(paste0("Now running ",cope.fslmerge))
  system(command = x,intern = T,ignore.stdout = F,ignore.stderr = F)
})
}
print("DONE")
}

