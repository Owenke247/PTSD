rm(list=ls())
library(vegan)
library(ggplot2)
library(Hmisc)
setwd("/Users/shanlin/HMS_shanlin/Project/PTSD/01_dat/Data_pool")

PTSD_score<-read.table("PTSD_score.txt",header=T)
PTSD_diet<-read.table("PTSD_diet_score.txt",header=T)
PTSD_meta<-read.table("PTSD_metadata.txt",header=T)
PTSD_alpha<-read.table("PTSD_Alpha1.txt",header=T)

## select contuinue factors
PTSD_score<-PTSD_score[,c(1,5)]
PTSD_meta<-PTSD_meta[,c(1,25,29,30,33)]
PTSD_alpha<-PTSD_alpha[,c(2,4,5,7,8)]
PTSD_diet<-PTSD_diet[,-c(5,6)]
## unredundant subjects
overlap1 = intersect(PTSD_score$ID, PTSD_meta$ID)
overlap1 = intersect(overlap1, PTSD_diet$ID)
overlap1 = intersect(overlap1, PTSD_alpha$ID)

PTSD_score = PTSD_score[match(overlap1,PTSD_score$ID),]
PTSD_meta = PTSD_meta[match(overlap1,PTSD_meta$ID),]
PTSD_diet = PTSD_diet[match(overlap1,PTSD_diet$ID),]
PTSD_alpha = PTSD_alpha[match(overlap1,PTSD_alpha$ID),]
##Combine data
df1<-cbind(PTSD_score,PTSD_meta)
df1<-cbind(df1,PTSD_alpha)
df2<-PTSD_diet

df1<-df1[,-c(3,8)]

row.names(df1)<-c(1:150)
row.names(df2)<-c(1:150)

df1<-df1[,-1]
df2<-df2[,-1]

n<-ncol(df1)
m<-ncol(df2)
ms<-numeric()
mtxCor<-matrix(NA,nrow = ncol(df1),ncol = ncol(df2));
mtxFdr<-matrix(NA,nrow = ncol(df1),ncol = ncol(df2));
for (i in 1:n){
  for (j in 1:m){
    cor.spe<-corr.test(df1[,i],df2[,j],method='spearman',adjust="BH")
    mtxCor[i,j]<-cor.spe$r
    mtxFdr[i,j]<-cor.spe$p
  }
}

#mtxFdr <- apply(mtxFdr,2,p.adjust,method="BH")
rownames(mtxCor)<-colnames(df1)
colnames(mtxCor)<-colnames(df2)
rownames(mtxFdr)<-colnames(df1)
colnames(mtxFdr)<-colnames(df2)
