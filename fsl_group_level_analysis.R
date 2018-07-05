###SON1 FSL Group Level Analysis: 

get_all_feat<-function(rootdir="/Volumes/bek/neurofeedback/sonrisa1/nfb/ssanalysis/fsl",
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
noIDpos<-which(aggregate(COPENUM~ID,data = df.ex,max)$COPENUM!=max(aggregate(COPENUM~ID,data = df.ex,max)$COPENUM))
if (length(noIDpos)>0){
noID<-aggregate(COPENUM~ID,data = df.ex,max)$ID [noIDpos]
print(paste("This ID:",noID,"does not have enough COPES, will be removed from running...."))
df.ex[which(!df.ex$ID %in% noID),]->df.ex
} else {print("All Good!")}




}
