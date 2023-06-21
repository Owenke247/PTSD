###### Stability ##########
### ALpha diversity
rm(list=ls())
library(vegan)
library(ape)
library(ggplot2)
library(ggpubr)
library(tidyverse)
setwd("/Users/shanlin/HMS_shanlin/Project/PTSD/00_stability/Stability")
dg<-read.table("PTSD_species.txt",header=T) ### We only included subjects provides all four stool samples. And three participants (n=12) were excluded due to extreme lowe number of identified microbial species and pathways.
#dg<-read.table("PTSD_pathway.txt",header=T)
df<-dg[,-c(1:4)]
Shannon<-diversity(df,"shannon") #计算shannon.wiener指数
Simpson<-diversity(df,"simpson") #
Invsimp<-diversity(df, "inv")
H<-diversity(df)
S <- specnumber(df)
J <- H/log(S)
ds<-cbind(Shannon,Simpson)
dss<-cbind(ds,Invsimp)
dsss<-cbind(dss,J)
dssss<-cbind(dsss,S)
#chao1<-estimateR(df)
#chao1<-t(chao1)
dd<-cbind(dg[,c(1:4)],dssss)

write.table(dd,"PTSD_Alpha_stabilitys.txt",sep="\t",col.names=NA)

### subjects dissimilarity 
rm(list=ls())
library(reshape2) 
library(vegan)
setwd("/Users/shanlin/HMS_shanlin/Project/PTSD/00_stability/Stability")
df1<-read.table("PTSD_species.txt",header=T)
df2<-read.table("PTSD_pathway.txt",header=T)
dat1<-df1[,-c(1:4)]
dat2<-df2[,-c(1:4)]

### check minmal value that large than 0

mat1 <- decostand(dat1, "rclr")
mat2 <- decostand(dat2, "rclr")

#BC
BC1<-vegdist(mat1,method="euclidean") 
BC2<-vegdist(mat2,method="euclidean") 

###Matrix transfer
BC1<-as.matrix(BC1)
BC2<-as.matrix(BC2)


#Half matrix 
BC1[lower.tri(BC1)]=NA
BC2[lower.tri(BC2)]=NA

#data format transfer
Species_bc<- melt(BC1, id.var = "subject1",variable.name = "subject2", value.name = "bc")
Pathway_bc<- melt(BC2, id.var = "subject1",variable.name = "subject2", value.name = "bc")

setwd("/Users/shanlin/HMS_shanlin/Project/PTSD/Revision")
write.table(Species_bc,"PTSD_species_rclr.txt",sep="\t",col.names=NA)
write.table(Pathway_bc,"PTSD_pathway_rclr.txt",sep="\t",col.names=NA)

############################Permonva analysis################
rm(list=ls())
library(reshape2) 
library(vegan)
setwd("/Users/shanlin/HMS_shanlin/Project/PTSD/00_stability/Stability")
df1<-read.table("PTSD_species.txt",header=T)
df2<-read.table("PTSD_pathway.txt",header=T)
dat1<-df1[,-c(1:4)]
dat2<-df2[,-c(1:4)]

mat1 <- decostand(dat1, "rclr")
mat2 <- decostand(dat2, "rclr")

#BC
BC1<-vegdist(mat1,method="euclidean") 
BC2<-vegdist(mat2,method="euclidean") 


matrix1<-as.matrix(BC1)
matrix2<-as.matrix(BC2)

dis1 <-as.dist(matrix1)
dis2 <-as.dist(matrix2)

#species
set.seed(123)
permanova <- adonis( dis1~ Time,
                     data = df1, permutations=9999)
summary(permanova)
permanova$aov.tab 


#pathway
set.seed(123)
permanova <- adonis( dis2~ Time,
                     data = df2, permutations=9999)
summary(permanova)
permanova$aov.tab 

######################################## visualiztion Fig 2 revised ############
#Fig 2
rm(list=ls())
library(ggplot2)
library(plyr)
library(cowplot)
library(extrafont)
loadfonts(quiet = T)
setwd("/Users/shanlin/HMS_shanlin/Project/PTSD/00_stability/Stability")
d1<-read.table("PTSD_Alpha_stabilitys.txt",header=T)
d2<-read.table("PTSD_Alpha_stabilityp.txt",header=T)


scale1 <- function(x) sprintf("%.1f", x)
scale2 <- function(x) sprintf("%.2f", x)
scale3 <- function(x) sprintf("%.3f", x)
############ Comparison between timepoints 
comp1 <- list( c("T1", "T2"))
comp2 <- list( c("T1", "T3"))
comp3 <- list( c("T1", "T4"))
comp4 <- list( c("T2", "T3"))
comp5 <- list( c("T2", "T4"))
comp6 <- list( c("T3", "T4"))

#Fig2a
p0 <- ggplot(data=d1, aes(x = Time,y=Shannon,fill=Time))
p0 <- p0 + geom_violin(trim=FALSE)
p0 <- p0 + stat_boxplot(geom ='errorbar', width = 0.15)
p0 <- p0 + geom_boxplot(size=0.3,width=0.23,outlier.shape= NA,fill="white")
#p0 <- p0 + geom_line(aes(group = Subjects), alpha = 0.4, colour = "grey88", data = dd) 
#p0 <- p0 + geom_point(aes(fill = Time), size = 0.1, shape = 21, position = position_jitterdodge())
#p <- p + geom_jitter(shape=16, position=position_jitter(0.2),dotsize=0.00001)
#p <- p + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5,binwidth = 0.13)#shannon0.13
p0 <- p0 + theme(plot.background=element_blank(),panel.background=element_blank())
p0 <- p0 + theme_bw()
p0 <- p0 + theme(axis.text.x=element_text(colour="black",family="Arial",size=14), #设置x轴刻度标签的字体显示倾斜角度为15度，并向下调整1(hjust = 1)，字体簇为Arial大小为20
                 axis.text.y=element_text(colour="black",family="Arial",size=14,face="plain"), #设置y轴刻度标签的字体簇，字体大小，字体样式为plain
                 axis.title.x=element_text(family="Arial",size = 16,face="plain"), #设置y轴标题的字体属性
                 axis.title.y=element_text(family="Arial",size = 16,face="plain"), #设置y轴标题的字体属性
                 panel.border = element_blank(),axis.line = element_line(colour = "black",size=0.3), #去除默认填充的灰色，并将x=0轴和y=0轴加粗显示(size=1)
                 legend.text=element_text(face="italic", family="Arial", colour="black",  #设置图例的子标题的字体属性
                                          size=6),
                 legend.title=element_text(face="italic", family="Arial", colour="black", #设置图例的总标题的字体属性
                                           size=6),
                 panel.grid.major = element_blank(),   #不显示网格线
                 panel.grid.minor = element_blank(),
                 panel.background = element_rect(colour = "black", size = 0.5))

p0 <- p0 + scale_fill_brewer(palette="Paired")
p0 <- p0 + ylab("Shannon")
p0 <- p0 + xlab("Group")
p0 <- p0 + theme(legend.position="none")
#p0 <- p0 + scale_y_continuous(labels=scale1)
p0 <- p0 + stat_compare_means(comparisons = comp1,label = "p.format",size=3.5,method = "wilcox.test",label.x = 1.2, label.y = 3.8)
p0 <- p0 + stat_compare_means(comparisons = comp2,label = "p.format",size=3.5,method = "wilcox.test",label.x = 1.2, label.y = 4.4)
p0 <- p0 + stat_compare_means(comparisons = comp3,label = "p.format",size=3.5,method = "wilcox.test",label.x = 1.2, label.y = 5.0)
p0 <- p0 + stat_compare_means(comparisons = comp4,label = "p.format",size=3.5,method = "wilcox.test",label.x = 1.2, label.y = 4.0)
p0 <- p0 + stat_compare_means(comparisons = comp5,label = "p.format",size=3.5,method = "wilcox.test",label.x = 1.2, label.y = 4.8)
p0 <- p0 + stat_compare_means(comparisons = comp6,label = "p.format",size=3.5,method = "wilcox.test",label.x = 1.2, label.y = 4.2)
#Fig2a
p1 <- ggplot(data=d1, aes(x = Time,y=Simpson,fill=Time))
p1 <- p1 + geom_violin(trim=FALSE)
p1 <- p1 +stat_boxplot(geom ='errorbar', width = 0.15)
p1 <- p1 + geom_boxplot(size=0.3,width=0.23,outlier.shape= NA,fill="white")
#p1 <- p1 + geom_line(aes(group = Subjects), alpha = 0.4, colour = "grey88", data = dd) 
#p0 <- p0 + geom_point(aes(fill = Time), size = 0.1, shape = 21, position = position_jitterdodge())
#p <- p + geom_jitter(shape=16, position=position_jitter(0.2),dotsize=0.00001)
#p <- p + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5,binwidth = 0.13)#shannon0.13
p1 <- p1 + theme(plot.background=element_blank(),panel.background=element_blank())
p1 <- p1 + theme_bw()
p1 <- p1 + theme(axis.text.x=element_text(colour="black",family="Arial",size=14), #设置x轴刻度标签的字体显示倾斜角度为15度，并向下调整1(hjust = 1)，字体簇为Arial大小为20
                 axis.text.y=element_text(colour="black",family="Arial",size=14,face="plain"), #设置y轴刻度标签的字体簇，字体大小，字体样式为plain
                 axis.title.x=element_text(family="Arial",size = 16,face="plain"), #设置y轴标题的字体属性
                 axis.title.y=element_text(family="Arial",size = 16,face="plain"), #设置y轴标题的字体属性
                 panel.border = element_blank(),axis.line = element_line(colour = "black",size=0.3), #去除默认填充的灰色，并将x=0轴和y=0轴加粗显示(size=1)
                 legend.text=element_text(face="italic", family="Arial", colour="black",  #设置图例的子标题的字体属性
                                          size=6),
                 legend.title=element_text(face="italic", family="Arial", colour="black", #设置图例的总标题的字体属性
                                           size=6),
                 panel.grid.major = element_blank(),   #不显示网格线
                 panel.grid.minor = element_blank(),
                 panel.background = element_rect(colour = "black", size = 0.5))

p1 <- p1 + scale_fill_brewer(palette="Paired")
p1 <- p1 + ylab("Simpson")
p1 <- p1 + xlab("Group")
#p1 <- p1 + scale_y_continuous(labels=scale3)
p1 <- p1 + theme(legend.position="none")
p1 <- p1 + stat_compare_means(comparisons = comp1,label = "p.format",size=4,method = "wilcox.test",label.x = 1.2, label.y = 1.0)
p1 <- p1 + stat_compare_means(comparisons = comp2,label = "p.format",size=4,method = "wilcox.test",label.x = 1.2, label.y = 1.15)
p1 <- p1 + stat_compare_means(comparisons = comp3,label = "p.format",size=4,method = "wilcox.test",label.x = 1.2, label.y = 1.28)
p1 <- p1 + stat_compare_means(comparisons = comp4,label = "p.format",size=4,method = "wilcox.test",label.x = 1.2, label.y = 1.05)
p1 <- p1 + stat_compare_means(comparisons = comp5,label = "p.format",size=4,method = "wilcox.test",label.x = 1.2, label.y = 1.23)
p1 <- p1 + stat_compare_means(comparisons = comp6,label = "p.format",size=4,method = "wilcox.test",label.x = 1.2, label.y = 1.1)

p2 <- ggplot(data=d2, aes(x = Time,y=Shannon,fill=Time))
p2 <- p2 + geom_violin(trim=FALSE)
p2 <- p2 +stat_boxplot(geom ='errorbar', width = 0.15)
p2 <- p2 + geom_boxplot(size=0.3,width=0.23,outlier.shape= NA,fill="white")
#p2 <- p2 + geom_line(aes(group = Subjects), alpha = 0.4, colour = "grey88", data = df) 
#p0 <- p0 + geom_point(aes(fill = Time), size = 0.1, shape = 21, position = position_jitterdodge())
#p <- p + geom_jitter(shape=16, position=position_jitter(0.2),dotsize=0.00001)
#p <- p + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5,binwidth = 0.13)#shannon0.13
p2 <- p2 + theme(plot.background=element_blank(),panel.background=element_blank())
p2 <- p2 + theme_bw()
p2 <- p2 + theme(axis.text.x=element_text(colour="black",family="Arial",size=14), #设置x轴刻度标签的字体显示倾斜角度为15度，并向下调整1(hjust = 1)，字体簇为Arial大小为20
                 axis.text.y=element_text(colour="black",family="Arial",size=14,face="plain"), #设置y轴刻度标签的字体簇，字体大小，字体样式为plain
                 axis.title.x=element_text(family="Arial",size = 16,face="plain"), #设置y轴标题的字体属性
                 axis.title.y=element_text(family="Arial",size = 16,face="plain"), #设置y轴标题的字体属性
                 panel.border = element_blank(),axis.line = element_line(colour = "black",size=0.3), #去除默认填充的灰色，并将x=0轴和y=0轴加粗显示(size=1)
                 legend.text=element_text(face="italic", family="Arial", colour="black",  #设置图例的子标题的字体属性
                                          size=6),
                 legend.title=element_text(face="italic", family="Arial", colour="black", #设置图例的总标题的字体属性
                                           size=6),
                 panel.grid.major = element_blank(),   #不显示网格线
                 panel.grid.minor = element_blank(),
                 panel.background = element_rect(colour = "black", size = 0.5))

p2 <- p2 + scale_fill_brewer(palette="Paired")
p2 <- p2 + ylab("Shannon")
p2 <- p2 + xlab("Group")
p2 <- p2 + theme(legend.position="none")
#p2 <- p2 + scale_y_continuous(labels=scale1)
p2 <- p2 + stat_compare_means(comparisons = comp1,label = "p.format",size=4,method = "wilcox.test",label.x = 1.2, label.y = 5.7)
p2 <- p2 + stat_compare_means(comparisons = comp2,label = "p.format",size=4,method = "wilcox.test",label.x = 1.2, label.y = 6.3)
p2 <- p2 + stat_compare_means(comparisons = comp3,label = "p.format",size=4,method = "wilcox.test",label.x = 1.2, label.y = 6.7)
p2 <- p2 + stat_compare_means(comparisons = comp4,label = "p.format",size=4,method = "wilcox.test",label.x = 1.2, label.y = 5.9)
p2 <- p2 + stat_compare_means(comparisons = comp5,label = "p.format",size=4,method = "wilcox.test",label.x = 1.2, label.y = 6.5)
p2 <- p2 + stat_compare_means(comparisons = comp6,label = "p.format",size=4,method = "wilcox.test",label.x = 1.2, label.y = 6.1)
#Fig2a
p3 <- ggplot(data=d2, aes(x = Time,y=Simpson,fill=Time))
p3 <- p3 + geom_violin(trim=FALSE)
p3 <- p3 +stat_boxplot(geom ='errorbar', width = 0.15)
p3 <- p3 + geom_boxplot(size=0.3,width=0.23,outlier.shape= NA,fill="white")
#p3 <- p3 + geom_line(aes(group = Subjects), alpha = 0.4, colour = "grey88", data = df) 
#p0 <- p0 + geom_point(aes(fill = Time), size = 0.1, shape = 21, position = position_jitterdodge())
#p <- p + geom_jitter(shape=16, position=position_jitter(0.2),dotsize=0.00001)
#p <- p + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5,binwidth = 0.13)#shannon0.13
p3 <- p3 + theme(plot.background=element_blank(),panel.background=element_blank())
p3 <- p3 + theme_bw()
p3 <- p3 + theme(axis.text.x=element_text(colour="black",family="Arial",size=14), #设置x轴刻度标签的字体显示倾斜角度为15度，并向下调整1(hjust = 1)，字体簇为Arial大小为20
                 axis.text.y=element_text(colour="black",family="Arial",size=14,face="plain"), #设置y轴刻度标签的字体簇，字体大小，字体样式为plain
                 axis.title.x=element_text(family="Arial",size = 16,face="plain"), #设置y轴标题的字体属性
                 axis.title.y=element_text(family="Arial",size = 16,face="plain"), #设置y轴标题的字体属性
                 panel.border = element_blank(),axis.line = element_line(colour = "black",size=0.3), #去除默认填充的灰色，并将x=0轴和y=0轴加粗显示(size=1)
                 legend.text=element_text(face="italic", family="Arial", colour="black",  #设置图例的子标题的字体属性
                                          size=6),
                 legend.title=element_text(face="italic", family="Arial", colour="black", #设置图例的总标题的字体属性
                                           size=6),
                 panel.grid.major = element_blank(),   #不显示网格线
                 panel.grid.minor = element_blank(),
                 panel.background = element_rect(colour = "black", size = 0.5))

p3 <- p3 + scale_fill_brewer(palette="Paired")
p3 <- p3 + ylab("Simpson")
p3 <- p3 + xlab("Group")
#p3 <- p3 + scale_y_continuous(labels=scale3)
p3 <- p3 + theme(legend.position="none")
p3 <- p3 + stat_compare_means(comparisons = comp1,label = "p.format",size=4,method = "wilcox.test",label.x = 1.2, label.y = 0.997)
p3 <- p3 + stat_compare_means(comparisons = comp2,label = "p.format",size=4,method = "wilcox.test",label.x = 1.2, label.y = 1.003)
p3 <- p3 + stat_compare_means(comparisons = comp3,label = "p.format",size=4,method = "wilcox.test",label.x = 1.2, label.y = 1.007)
p3 <- p3 + stat_compare_means(comparisons = comp4,label = "p.format",size=4,method = "wilcox.test",label.x = 1.2, label.y = 0.999)
p3 <- p3 + stat_compare_means(comparisons = comp5,label = "p.format",size=4,method = "wilcox.test",label.x = 1.2, label.y = 1.005)
p3 <- p3 + stat_compare_means(comparisons = comp6,label = "p.format",size=4,method = "wilcox.test",label.x = 1.2, label.y = 1.001)
######PCOA
library(vegan)
library(ggplot2)
library(plyr)
library(cowplot)
library(extrafont)
library(reshape2)
library(ggpubr)
library(ade4)
library(gclus)
library(ape)
df1<-read.table("PTSD_species.txt",header=T)
df2<-read.table("PTSD_pathway.txt",header=T)
dat1<-df1[,-c(1:4)]
dat2<-df2[,-c(1:4)]

mat1 <- decostand(dat1, "rclr")
mat2 <- decostand(dat2, "rclr")

#BC
Aitchison1<-vegdist(mat1,method="euclidean") 
Aitchison2<-vegdist(mat2,method="euclidean") 

PCoA1 <- pcoa(Aitchison1,correction = "none",rn=NULL)#
PCoA2 <- pcoa(Aitchison2,correction = "none",rn=NULL)#

pc1 = as.data.frame(PCoA1$vectors)
pc2 = as.data.frame(PCoA2$vectors)
PCoA.result1 <- cbind(df1[,c(1,4)],pc1[,c(1,2)])
PCoA.result2 <- cbind(df2[,c(1,4)],pc2[,c(1,2)])
names(PCoA.result1) <- c("ID","Time","PCoA1","PCoA2")
names(PCoA.result2) <- c("ID","Time","PCoA1","PCoA2")
b1 <-PCoA1$values[,"Relative_eig"]
b2 <-PCoA2$values[,"Relative_eig"]
pro11 = as.numeric(sprintf("%.3f",b1[1]))*100
pro21 = as.numeric(sprintf("%.3f",b2[1]))*100
pro12 = as.numeric(sprintf("%.3f",b1[2]))*100
pro22 = as.numeric(sprintf("%.3f",b2[2]))*100
xlab1=paste("PCoA1(",pro11,"%)",sep="") 
xlab2=paste("PCoA1(",pro21,"%)",sep="") 
ylab1=paste("PCoA2(",pro12,"%)",sep="")
ylab2=paste("PCoA2(",pro22,"%)",sep="")

p4<- ggplot(PCoA.result1, aes(PCoA1, PCoA2, group = Time, color = Time))
p4 <- p4 + geom_point(size=1.4)
#p1 <- p1 + geom_text(aes(label=ID),hjust=0, vjust=0,size=0.4,colour="black")
p4 <- p4 + theme_bw()
#geom_text(aes(label=ID),hjust=0, vjust=0,size=1.6)+
#p1 <- p1 + stat_ellipse(level = 0.95, show.legend = F,linetype = 2)
p4 <- p4 + xlab(xlab2)+ylab(ylab2)+theme(panel.grid=element_blank())
#scale_color_viridis(discrete = TRUE, option = "D")+
#scale_fill_viridis(discrete = TRUE) +
p4 <- p4 + scale_color_brewer(palette="Paired")
p4 <- p4 + theme(legend.key.size = unit(0.3, "cm"))
p4 <- p4 + theme(axis.text.x=element_text(colour="black",family="Arial",size=14), #设置x轴刻度标签的字体显示倾斜角度为15度，并向下调整1(hjust = 1)，字体簇为Arial大小为20
                 axis.text.y=element_text(colour="black",family="Arial",size=14,face="plain"), #设置y轴刻度标签的字体簇，字体大小，字体样式为plain
                 axis.title.x=element_text(family="Arial",size = 16,face="plain"),
                 axis.title.y=element_text(family="Arial",size = 16,face="plain"),
                 legend.position="none",
                 panel.border = element_blank(),axis.line = element_line(colour = "black",size=0.3), #去除默认填充的灰色，并将x=0轴和y=0轴加粗显示(size=1)
                 legend.text=element_text(face="italic", family="Arial", colour="black",  #设置图例的子标题的字体属性
                                          size=6),
                 legend.title=element_text(face="italic", family="Arial", colour="black", #设置图例的总标题的字体属性
                                           size=6),
                 panel.grid.major = element_blank(),   #不显示网格线
                 panel.grid.minor = element_blank(),
                 panel.background = element_rect(colour = "black", size = 0.5))
p4 <- p4 + theme(legend.position = c())
p4 <- p4 + theme(legend.position="none")
p4 <- p4 + scale_y_continuous(labels=scale1)

p5<- ggplot(PCoA.result2, aes(PCoA1, PCoA2, group = Time, color = Time))
p5 <- p5 + geom_point(size=1.4)
#p1 <- p1 + geom_text(aes(label=ID),hjust=0, vjust=0,size=0.4,colour="black")
p5 <- p5 + theme_bw()
#geom_text(aes(label=ID),hjust=0, vjust=0,size=1.6)+
#p1 <- p1 + stat_ellipse(level = 0.95, show.legend = F,linetype = 2)
p5 <- p5 + xlab(xlab1)+ylab(ylab1)+theme(panel.grid=element_blank())
#scale_color_viridis(discrete = TRUE, option = "D")+
#scale_fill_viridis(discrete = TRUE) +
p5 <- p5 + scale_color_brewer(palette="Paired")
p5 <- p5 + theme(legend.key.size = unit(0.3, "cm"))
p5 <- p5 + theme(axis.text.x=element_text(colour="black",family="Arial",size=14), #设置x轴刻度标签的字体显示倾斜角度为15度，并向下调整1(hjust = 1)，字体簇为Arial大小为20
                 axis.text.y=element_text(colour="black",family="Arial",size=14,face="plain"), #设置y轴刻度标签的字体簇，字体大小，字体样式为plain
                 axis.title.x=element_text(family="Arial",size = 16,face="plain"),
                 axis.title.y=element_text(family="Arial",size = 16,face="plain"),
                 legend.position="none",
                 panel.border = element_blank(),axis.line = element_line(colour = "black",size=0.3), #去除默认填充的灰色，并将x=0轴和y=0轴加粗显示(size=1)
                 legend.text=element_text(face="italic", family="Arial", colour="black",  #设置图例的子标题的字体属性
                                          size=6),
                 legend.title=element_text(face="italic", family="Arial", colour="black", #设置图例的总标题的字体属性
                                           size=6),
                 panel.grid.major = element_blank(),   #不显示网格线
                 panel.grid.minor = element_blank(),
                 panel.background = element_rect(colour = "black", size = 0.5))
p5 <- p5 + theme(legend.position = c())
p5 <- p5 + theme(legend.position="none")

###### ar Aitchison boxplot
library(readxl)
setwd("/Users/shanlin/HMS_shanlin/Project/PTSD/Revision")

Res1 <- "/Users/shanlin/HMS_shanlin/Project/PTSD/Revision/PTSD_stability_distance.xlsx"

dat6 <- read_excel(path = Res1,sheet = "Species_rclr")
dat7 <- read_excel(path = Res1,sheet = "Pathway_rclr")
my_comparison <- list( c("Inter_subject", "Intra_subject"))

p6 <- ggplot(data=dat6, aes(x = Type,y=Dis,fill=Type))
p6 <- p6 + geom_violin(trim=FALSE,width=0.45)
p6 <- p6 + stat_boxplot(geom ='errorbar', width = 0.15)
p6 <- p6 + geom_boxplot(size=0.3,width=0.12,outlier.shape= NA,fill="white")
#p3 <- p3 + geom_line(aes(group = Subjects), alpha = 0.4, colour = "grey88", data = df) 
#p0 <- p0 + geom_point(aes(fill = Time), size = 0.1, shape = 21, position = position_jitterdodge())
#p <- p + geom_jitter(shape=16, position=position_jitter(0.2),dotsize=0.00001)
#p <- p + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5,binwidth = 0.13)#shannon0.13
p6 <- p6 + theme(plot.background=element_blank(),panel.background=element_blank())
p6 <- p6 + theme_bw()
p6 <- p6 + theme(axis.text.x=element_text(colour="black",family="Arial",size=14), #设置x轴刻度标签的字体显示倾斜角度为15度，并向下调整1(hjust = 1)，字体簇为Arial大小为20
                 axis.text.y=element_text(colour="black",family="Arial",size=14,face="plain"), #设置y轴刻度标签的字体簇，字体大小，字体样式为plain
                 axis.title.x=element_text(family="Arial",size = 16,face="plain"), #设置y轴标题的字体属性
                 axis.title.y=element_text(family="Arial",size = 16,face="plain"), #设置y轴标题的字体属性
                 panel.border = element_blank(),axis.line = element_line(colour = "black",size=0.3), #去除默认填充的灰色，并将x=0轴和y=0轴加粗显示(size=1)
                 legend.text=element_text(face="italic", family="Arial", colour="black",  #设置图例的子标题的字体属性
                                          size=6),
                 legend.title=element_text(face="italic", family="Arial", colour="black", #设置图例的总标题的字体属性
                                           size=6),
                 panel.grid.major = element_blank(),   #不显示网格线
                 panel.grid.minor = element_blank(),
                 panel.background = element_rect(colour = "black", size = 0.5))

p6 <- p6 + scale_fill_brewer(palette="Paired")
p6 <- p6 + ylab("Robust Aitchison distance")
p6 <- p6 + xlab("Group")
p6 <- p6 + theme(legend.position="none")
#p6 <- p6 + scale_y_continuous(labels=scale2)
p6 <- p6 + stat_compare_means(comparisons = my_comparison,label = "p.format",size=3.5,method = "wilcox.test",label.x = 1.2, label.y = 38)

p7 <- ggplot(data=dat7, aes(x = Type,y=Dis,fill=Type))
p7 <- p7 + geom_violin(trim=FALSE,width=0.45)
p7 <- p7 +stat_boxplot(geom ='errorbar', width = 0.15)
p7 <- p7 + geom_boxplot(size=0.3,width=0.12,outlier.shape= NA,fill="white")
#p3 <- p3 + geom_line(aes(group = Subjects), alpha = 0.4, colour = "grey88", data = df) 
#p0 <- p0 + geom_point(aes(fill = Time), size = 0.1, shape = 21, position = position_jitterdodge())
#p <- p + geom_jitter(shape=16, position=position_jitter(0.2),dotsize=0.00001)
#p <- p + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5,binwidth = 0.13)#shannon0.13
p7 <- p7 + theme(plot.background=element_blank(),panel.background=element_blank())
p7 <- p7 + theme_bw()
p7 <- p7 + theme(axis.text.x=element_text(colour="black",family="Arial",size=14), #设置x轴刻度标签的字体显示倾斜角度为15度，并向下调整1(hjust = 1)，字体簇为Arial大小为20
                 axis.text.y=element_text(colour="black",family="Arial",size=14,face="plain"), #设置y轴刻度标签的字体簇，字体大小，字体样式为plain
                 axis.title.x=element_text(family="Arial",size = 16,face="plain"), #设置y轴标题的字体属性
                 axis.title.y=element_text(family="Arial",size = 16,face="plain"), #设置y轴标题的字体属性
                 panel.border = element_blank(),axis.line = element_line(colour = "black",size=0.3), #去除默认填充的灰色，并将x=0轴和y=0轴加粗显示(size=1)
                 legend.text=element_text(face="italic", family="Arial", colour="black",  #设置图例的子标题的字体属性
                                          size=6),
                 legend.title=element_text(face="italic", family="Arial", colour="black", #设置图例的总标题的字体属性
                                           size=6),
                 panel.grid.major = element_blank(),   #不显示网格线
                 panel.grid.minor = element_blank(),
                 panel.background = element_rect(colour = "black", size = 0.5))

p7 <- p7 + scale_fill_brewer(palette="Paired")
p7 <- p7 + ylab("Robust Aitchison distance")
p7 <- p7 + xlab("Group")
#p7 <- p7 + scale_y_continuous(labels=scale2)
p7 <- p7 + theme(legend.position="none")
p7 <- p7 + stat_compare_means(comparisons = my_comparison,label = "p.format",size=3.5,method = "wilcox.test",label.x = 1.2, label.y = 45)



p<-plot_grid(p0,p1,p4,p6,p2,p3,p5,p7, labels=c('a','b','c','d','e','f','g','h'),label_size = 16,ncol=4,rel_widths = c(0.95, 1, 1, 1, 0.95, 1, 1, 1))

save_plot("PTSD_Fig2_rclr.pdf", p, base_width =20,base_height =10)
