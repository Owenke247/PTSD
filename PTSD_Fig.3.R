rm(list=ls())
library(vegan)
setwd("/Users/shanlin/HMS_shanlin/Project/PTSD/01_dat/Data_pool")
dat1<-read.table("PTSD_diet_score.txt",header=T)
dat2<-read.table("PTSD_metadata.txt",header=T)
dat3<-read.table("PTSD_species_1.txt",header=T)
dat4<-read.table("PTSD_pathway_1.txt",header=T)
#delete 453577 extreme samples
dat3<-dat3[-32,]
dat4<-dat4[-32,]

overlap = intersect(dat1$ID, dat2$ID)
overlap = intersect(overlap, dat3$ID)

dat1 = dat1[match(overlap,dat1$ID),]
dat2 = dat2[match(overlap,dat2$ID),]
dat3 = dat3[match(overlap,dat3$ID),]
dat4 = dat4[match(overlap,dat4$ID),]

#merge data
metadat<-cbind(dat1,dat2[,-1])
metadat<-metadat[,-c(1,9,10,11,14,15,17,20,36,37)]#filiter redundant features
#
dat3<-dat3[,-c(1:4)]
dat4<-dat4[,-c(1:4)]

mat3 <- decostand(dat3, "rclr")
mat4 <- decostand(dat4, "rclr")

#n1<-nrow(dat3)*0.01
#n2<-nrow(dat4)*0.01
#dat3<-dat3[colSums(dat3!=0) >= n1]
#dat4<-dat4[colSums(dat4!=0) >= n2]

dis1 <- vegdist(mat3, method = "euclidean")
dis2 <- vegdist(mat4, method = "euclidean")

pcoa1<-cmdscale(dis1)
pcoa2<-cmdscale(dis2)
#vare.mds<- metaMDS(df)
set.seed(200)
pcoaef1<-envfit(pcoa1, metadat, permu = 9999,na.rm = FALSE)
pcoaef2<-envfit(pcoa2, metadat, permu = 9999,na.rm = FALSE)

res1<-as.data.frame(cbind(pcoaef1[["vectors"]][["r"]], pcoaef1[["vectors"]][["pvals"]]))
res2<-as.data.frame(cbind(pcoaef1[["factors"]][["r"]], pcoaef1[["factors"]][["pvals"]]))

res<-rbind(res1,res2)
names(res)<-c("R2_s","P_s")
res$Q_s<-p.adjust (res$P_s, method = "BH")

res3<-as.data.frame(cbind(pcoaef2[["vectors"]][["r"]], pcoaef2[["vectors"]][["pvals"]]))
res4<-as.data.frame(cbind(pcoaef2[["factors"]][["r"]], pcoaef2[["factors"]][["pvals"]]))

ret<-rbind(res3,res4)
names(ret)<-c("R2_p","P_p")
ret$Q_p<-p.adjust (ret$P_p, method = "BH")


final<-cbind(res,ret)

name<-as.data.frame(row.names(final))
final<-cbind(name,final)
names(final)[1]<-c("ID")
