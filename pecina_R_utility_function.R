###SON1 FSL Group Level Analysis: 

glvl_all_cope<-function(rootdir="/Volumes/bek/neurofeedback/sonrisa1/nfb/ssanalysis/fsl",
                       outputdir="/Volumes/bek/neurofeedback/sonrisa1/nfb/grpanal/fsl",
                       modelname="PE_8C_old",
                       copestorun=1:8
) {
  if ( is.null(modelname) ) {stop("Must specify a model name other wise it will be hard to find all copes")}
  
  
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
print("Now will run fslmerge and randomise, function will fail if FSLENVIR is not set up.")

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
lapply(cope.fslmerge,function(x) {
  print(paste0("Now running ",cope.fslmerge))
  system(command = x,intern = T,ignore.stdout = F,ignore.stderr = F)
})

print("DONE")
}

