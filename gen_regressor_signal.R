library(dplyr)
library(readr)
library(data.table)

#Download database
son1_all <- read_csv("~/Box/GitHub/Nfb_task/NFB_response/SON1&2_behav_results/son1_all.csv")
View(son1_all)

#Recode variables
son1_all$plac_ctrl<- NA
son1_all$plac_ctrl[son1_all$InfusionNum==1 | son1_all$InfusionNum==2] <- 1
son1_all$plac_ctrl[son1_all$InfusionNum==3 | son1_all$InfusionNum==4] <- 0
son1_all$plac_ctrl_r<-as.numeric(!son1_all$plac_ctrl)

son1_all$reinf_cont <- NA
son1_all$reinf_cont[son1_all$InfusionNum==1 | son1_all$InfusionNum==3] <- 1
son1_all$reinf_cont[son1_all$InfusionNum==2 | son1_all$InfusionNum==4] <- 0
son1_all$reinf_cont_r<-as.numeric(!son1_all$reinf_cont)

son1_all$InfDur<-son1_all$WillImpOnset - son1_all$InfOnset
son1_all$FeedDur<-son1_all$ImprovedOnset - son1_all$Feed2Onset

#Mean centering variables
center_scale <- function(x) {
  scale(x, scale = FALSE)
}

# apply it
son1_all$twoLR_S_fixD_oneK_vt1_centered<- center_scale(df$twoLR_S_fixD_oneK_vt1)
son1_all$twoLR_S_fixD_oneK_vt1shifted_centered<- center_scale(df$twoLR_S_fixD_oneK_vt1shifted)

son1_all$InfValue<-son1_all$twoLR_S_fixD_oneK_vt1shifted_centered * son1_all$plac_ctrl
son1_all$NoInfValue<-son1_all$twoLR_S_fixD_oneK_vt1shifted_centered * ! son1_all$plac_ctrl

son1_all$FeedValue<-son1_all$twoLR_S_fixD_oneK_vt1_centered * son1_all$reinf_cont
son1_all$NoFeedValue<-son1_all$twoLR_S_fixD_oneK_vt1_centered * ! son1_all$reinf_cont

son1_all$FeedPE<-son1_all$twoLR_S_fixD_oneK_PE * son1_all$reinf_cont
son1_all$NoFeedPE<-son1_all$twoLR_S_fixD_oneK_PE * ! son1_all$reinf_cont

#Rename database
df<- son1_all

#MODEL REGRESSORS

#Value_8C Model
# df.inf <- select(df, Participant, Run, administration, InfOnset, InfDur, plac_ctrl) %>% filter (administration=="1", plac_ctrl=="1")
# df.noinf <- select(df, Participant, Run, administration, InfOnset, InfDur, plac_ctrl_r) %>% filter (administration=="1", plac_ctrl_r=="1")
# df.fb <- select(df, Participant, Run, administration, Feed2Onset, FeedDur, reinf_cont) %>% filter (administration=="1", reinf_cont=="1")
# df.nofb <- select(df, Participant, Run, administration, Feed2Onset, FeedDur, reinf_cont_r) %>% filter (administration=="1", reinf_cont_r=="1")
# 
# df.inf.value <- select(df, Participant, Run, administration, InfOnset, InfDur, InfValue, plac_ctrl) %>% filter (administration=="1", plac_ctrl=="1")
# df.noinf.value <- select(df, Participant, Run, administration, InfOnset, InfDur, NoInfValue, plac_ctrl_r) %>% filter (administration=="1", plac_ctrl_r=="1")
# df.fb.value <- select(df, Participant, Run, administration, Feed2Onset, FeedDur, FeedValue, reinf_cont) %>% filter (administration=="1", reinf_cont=="1")
# df.nofb.value <- select(df, Participant, Run, administration, Feed2Onset, FeedDur, NoFeedValue, reinf_cont_r) %>% filter (administration=="1", reinf_cont_r=="1")

#PE_8C Model
# df.inf <- select(df, Participant, Run, administration, InfOnset, InfDur, plac_ctrl) %>% filter (administration=="1")
# df.noinf <- select(df, Participant, Run, administration, InfOnset, InfDur, plac_ctrl_r) %>% filter (administration=="1")
# df.fb <- select(df, Participant, Run, administration, Feed2Onset, FeedDur, reinf_cont) %>% filter (administration=="1")
# df.nofb <- select(df, Participant, Run, administration, Feed2Onset, FeedDur, reinf_cont_r) %>% filter (administration=="1")
# 
# df.inf.value <- select(df, Participant, Run, administration, InfOnset, InfDur, InfValue) %>% filter (administration=="1")
# df.noinf.value <- select(df, Participant, Run, administration, InfOnset, InfDur, NoInfValue) %>% filter (administration=="1")
# df.fb.PE <- select(df, Participant, Run, administration, Feed2Onset, FeedDur, FeedPE) %>% filter (administration=="1")
# df.nofb.PE <- select(df, Participant, Run, administration, Feed2Onset, FeedDur, NoFeedPE) %>% filter (administration=="1")

#PE_8C Model_new
df.inf <- select(df, Participant, Run, administration, InfOnset, InfDur, plac_ctrl) %>% filter (administration=="1", plac_ctrl=="1")
df.noinf <- select(df, Participant, Run, administration, InfOnset, InfDur, plac_ctrl_r) %>% filter (administration=="1", plac_ctrl_r=="1")
df.fb <- select(df, Participant, Run, administration, Feed2Onset, FeedDur, reinf_cont) %>% filter (administration=="1", reinf_cont=="1")
df.nofb <- select(df, Participant, Run, administration, Feed2Onset, FeedDur, reinf_cont_r) %>% filter (administration=="1", reinf_cont_r=="1")

df.inf.value <- select(df, Participant, Run, administration, InfOnset, InfDur, InfValue, plac_ctrl) %>% filter (administration=="1", plac_ctrl=="1")
df.noinf.value <- select(df, Participant, Run, administration, InfOnset, InfDur, NoInfValue, plac_ctrl_r) %>% filter (administration=="1",plac_ctrl_r=="1")
df.fb.PE <- select(df, Participant, Run, administration, Feed2Onset, FeedDur, FeedPE, reinf_cont) %>% filter (administration=="1", reinf_cont=="1")
df.nofb.PE <- select(df, Participant, Run, administration, Feed2Onset, FeedDur, NoFeedPE, reinf_cont_r) %>% filter (administration=="1", reinf_cont_r=="1")

#Write Regressors
setwd('~/Box/Github/Nfb_sonrisa/regs/R_fsl_reg/PE_8C/')

#interaction(df.fb$Participant,df.fb$Run)->df.fb$term

loopreg<-function(df=NULL,name.x=NULL,output.path="~/Box/Github/Nfb_sonrisa/regs/R_fsl_reg/PE_8C/",coltoget=c(4,5,6)) {
  if (is.null(df)) {stop("NO DF")}
  as.character(substitute(df))->dfname
  interaction(df.fb$Participant,df.fb$Run)->df$term
  df$term<-gsub(".","_a_run",df$term,fixed = T)
  for (i in unique(df$term)) {
    todo<-df[grep(i,df$term,fixed = T),coltoget]
    write.table(todo,paste(output.path,i,"_",name.x,".txt",sep = ""), row.names=FALSE, col.names = FALSE, sep = "\t")
  }
}

torun<-c("df.inf","df.noinf","df.fb", "df.nofb", "df.inf.value", "df.noinf.value", "df.fb.PE", "df.nofb.PE")

lapply(torun, function(x) {
  df.x<-get(x,envir = .GlobalEnv)
  loopreg(df = df.x,name.x=x,output.path = "~/Box/Github/Nfb_sonrisa/regs/R_fsl_reg/PE_8C/")
})






