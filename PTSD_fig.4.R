rm(list=ls())
library(ggplot2)
library(plyr)
library(cowplot)
library(extrafont)
setwd("/Users/shanlin/HMS_shanlin/Project/PTSD/02_diversity/Diversity")
d1<-read.table("PTSD_Alpha_species.txt",header=T)
d2<-read.table("PTSD_Alpha_path.txt",header=T)
d1$Status <- factor(d1$Status, levels=c("No_trauma", "Trauma_no_PTSD", "PTSD"))
d2$Status <- factor(d2$Status, levels=c("No_trauma", "Trauma_no_PTSD", "PTSD"))

scaleFUN <- function(x) sprintf("%.2f", x)
scaleFUNs <- function(x) sprintf("%.1f", x)
scaleFUNss <- function(x) sprintf("%.3f", x)
############ Comparison between timepoints 
my_comparisons <- list( c("No_trauma", "Trauma_no_PTSD"), c("No_trauma", "PTSD"), c("Trauma_no_PTSD", "PTSD"))
#Fig2a
p0 <- ggplot(data=d1, aes(x = Status,y=Shannon,fill=Status))
p0 <- p0 + geom_violin(trim=FALSE)
p0 <- p0 + stat_boxplot(geom ='errorbar', width = 0.15)
p0 <- p0 + geom_boxplot(size=0.3,width=0.23,outlier.shape= NA,fill="white")
#p0 <- p0 + geom_line(aes(group = Subjects), alpha = 0.4, colour = "grey88", data = dd) 
#p0 <- p0 + geom_point(aes(fill = Time), size = 0.1, shape = 21, position = position_jitterdodge())
#p <- p + geom_jitter(shape=16, position=position_jitter(0.2),dotsize=0.00001)
#p <- p + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5,binwidth = 0.13)#shannon0.13
p0 <- p0 + theme(plot.background=element_blank(),panel.background=element_blank())
p0 <- p0 + theme_bw()
p0 <- p0 + theme(axis.text.x=element_text(colour="black",family="Times",size=8), #设置x轴刻度标签的字体显示倾斜角度为15度，并向下调整1(hjust = 1)，字体簇为Times大小为20
                 axis.text.y=element_text(colour="black",family="Times",size=8,face="plain"), #设置y轴刻度标签的字体簇，字体大小，字体样式为plain
                 axis.title.x=element_text(family="Times",size = 10,face="plain"), #设置y轴标题的字体属性
                 axis.title.y=element_text(family="Times",size = 10,face="plain"), #设置y轴标题的字体属性
                 panel.border = element_blank(),axis.line = element_line(colour = "black",size=0.3), #去除默认填充的灰色，并将x=0轴和y=0轴加粗显示(size=1)
                 legend.text=element_text(face="italic", family="Times", colour="black",  #设置图例的子标题的字体属性
                                          size=6),
                 legend.title=element_text(face="italic", family="Times", colour="black", #设置图例的总标题的字体属性
                                           size=6),
                 panel.grid.major = element_blank(),   #不显示网格线
                 panel.grid.minor = element_blank(),
                 panel.background = element_rect(colour = "black", size = 0.5))

p0 <- p0 + scale_fill_brewer(palette="Paired")
p0 <- p0 + ylab("Shannon")
p0 <- p0 + xlab("Group")
p0 <- p0 + theme(legend.position="none")
p0 <- p0 + scale_y_continuous(labels=scaleFUNs)
#p0 <- p0 + stat_compare_means(comparisons = my_comparisons,label = "p.signif",size=2.5)

#Fig2a
p1 <- ggplot(data=d1, aes(x = Status,y=Simpson,fill=Status))
p1 <- p1 + geom_violin(trim=FALSE)
p1 <- p1 +stat_boxplot(geom ='errorbar', width = 0.15)
p1 <- p1 + geom_boxplot(size=0.3,width=0.23,outlier.shape= NA,fill="white")
#p1 <- p1 + geom_line(aes(group = Subjects), alpha = 0.4, colour = "grey88", data = dd) 
#p0 <- p0 + geom_point(aes(fill = Time), size = 0.1, shape = 21, position = position_jitterdodge())
#p <- p + geom_jitter(shape=16, position=position_jitter(0.2),dotsize=0.00001)
#p <- p + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5,binwidth = 0.13)#shannon0.13
p1 <- p1 + theme(plot.background=element_blank(),panel.background=element_blank())
p1 <- p1 + theme_bw()
p1 <- p1 + theme(axis.text.x=element_text(colour="black",family="Times",size=8), #设置x轴刻度标签的字体显示倾斜角度为15度，并向下调整1(hjust = 1)，字体簇为Times大小为20
                 axis.text.y=element_text(colour="black",family="Times",size=8,face="plain"), #设置y轴刻度标签的字体簇，字体大小，字体样式为plain
                 axis.title.x=element_text(family="Times",size = 10,face="plain"), #设置y轴标题的字体属性
                 axis.title.y=element_text(family="Times",size = 10,face="plain"), #设置y轴标题的字体属性
                 panel.border = element_blank(),axis.line = element_line(colour = "black",size=0.3), #去除默认填充的灰色，并将x=0轴和y=0轴加粗显示(size=1)
                 legend.text=element_text(face="italic", family="Times", colour="black",  #设置图例的子标题的字体属性
                                          size=6),
                 legend.title=element_text(face="italic", family="Times", colour="black", #设置图例的总标题的字体属性
                                           size=6),
                 panel.grid.major = element_blank(),   #不显示网格线
                 panel.grid.minor = element_blank(),
                 panel.background = element_rect(colour = "black", size = 0.5))

p1 <- p1 + scale_fill_brewer(palette="Paired")
p1 <- p1 + ylab("Simpson")
p1 <- p1 + xlab("Group")
p1 <- p1 + theme(legend.position="none")
p1 <- p1 + scale_y_continuous(labels=scaleFUNss)
#p1 <- p1 + stat_compare_means(comparisons = my_comparisons,label = "p.signif",size=2.5)

p2 <- ggplot(data=d2, aes(x = Status,y=Shannon,fill=Status))
p2 <- p2 + geom_violin(trim=FALSE)
p2 <- p2 + stat_boxplot(geom ='errorbar', width = 0.15)
p2 <- p2 + geom_boxplot(size=0.3,width=0.23,outlier.shape= NA,fill="white")
#p0 <- p0 + geom_line(aes(group = Subjects), alpha = 0.4, colour = "grey88", data = dd) 
#p0 <- p0 + geom_point(aes(fill = Time), size = 0.1, shape = 21, position = position_jitterdodge())
#p <- p + geom_jitter(shape=16, position=position_jitter(0.2),dotsize=0.00001)
#p <- p + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5,binwidth = 0.13)#shannon0.13
p2 <- p2 + theme(plot.background=element_blank(),panel.background=element_blank())
p2 <- p2 + theme_bw()
p2 <- p2 + theme(axis.text.x=element_text(colour="black",family="Times",size=8), #设置x轴刻度标签的字体显示倾斜角度为15度，并向下调整1(hjust = 1)，字体簇为Times大小为20
                 axis.text.y=element_text(colour="black",family="Times",size=8,face="plain"), #设置y轴刻度标签的字体簇，字体大小，字体样式为plain
                 axis.title.x=element_text(family="Times",size = 10,face="plain"), #设置y轴标题的字体属性
                 axis.title.y=element_text(family="Times",size = 10,face="plain"), #设置y轴标题的字体属性
                 panel.border = element_blank(),axis.line = element_line(colour = "black",size=0.3), #去除默认填充的灰色，并将x=0轴和y=0轴加粗显示(size=1)
                 legend.text=element_text(face="italic", family="Times", colour="black",  #设置图例的子标题的字体属性
                                          size=6),
                 legend.title=element_text(face="italic", family="Times", colour="black", #设置图例的总标题的字体属性
                                           size=6),
                 panel.grid.major = element_blank(),   #不显示网格线
                 panel.grid.minor = element_blank(),
                 panel.background = element_rect(colour = "black", size = 0.5))

p2 <- p2 + scale_fill_brewer(palette="Paired")
p2 <- p2 + ylab("Shannon")
p2 <- p2 + xlab("Group")
p2 <- p2 + theme(legend.position="none")

#p2 <- p2 + stat_compare_means(comparisons = my_comparisons,label = "p.signif",size=2.5)

#Fig2a
p3 <- ggplot(data=d2, aes(x = Status,y=Simpson,fill=Status))
p3 <- p3 + geom_violin(trim=FALSE)
p3 <- p3 +stat_boxplot(geom ='errorbar', width = 0.15)
p3 <- p3 + geom_boxplot(size=0.3,width=0.23,outlier.shape= NA,fill="white")
#p1 <- p1 + geom_line(aes(group = Subjects), alpha = 0.4, colour = "grey88", data = dd) 
#p0 <- p0 + geom_point(aes(fill = Time), size = 0.1, shape = 21, position = position_jitterdodge())
#p <- p + geom_jitter(shape=16, position=position_jitter(0.2),dotsize=0.00001)
#p <- p + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5,binwidth = 0.13)#shannon0.13
p3 <- p3 + theme(plot.background=element_blank(),panel.background=element_blank())
p3 <- p3 + theme_bw()
p3 <- p3 + theme(axis.text.x=element_text(colour="black",family="Times",size=8), #设置x轴刻度标签的字体显示倾斜角度为15度，并向下调整1(hjust = 1)，字体簇为Times大小为20
                 axis.text.y=element_text(colour="black",family="Times",size=8,face="plain"), #设置y轴刻度标签的字体簇，字体大小，字体样式为plain
                 axis.title.x=element_text(family="Times",size = 10,face="plain"), #设置y轴标题的字体属性
                 axis.title.y=element_text(family="Times",size = 10,face="plain"), #设置y轴标题的字体属性
                 panel.border = element_blank(),axis.line = element_line(colour = "black",size=0.3), #去除默认填充的灰色，并将x=0轴和y=0轴加粗显示(size=1)
                 legend.text=element_text(face="italic", family="Times", colour="black",  #设置图例的子标题的字体属性
                                          size=6),
                 legend.title=element_text(face="italic", family="Times", colour="black", #设置图例的总标题的字体属性
                                           size=6),
                 panel.grid.major = element_blank(),   #不显示网格线
                 panel.grid.minor = element_blank(),
                 panel.background = element_rect(colour = "black", size = 0.5))

p3 <- p3 + scale_fill_brewer(palette="Paired")
p3 <- p3 + ylab("Simpson")
p3 <- p3 + xlab("Group")
p3 <- p3 + theme(legend.position="none")
p3 <- p3 + scale_y_continuous(breaks =seq(0.975, 1, .01), limit = c(0.975, 1))
#p3 <- p3 + stat_compare_means(comparisons = my_comparisons,label = "p.signif",size=2.5)

##PCOA
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
setwd("/Users/shanlin/HMS_shanlin/Project/PTSD/02_diversity/Diversity")
d3<-read.table("PTSD_diversity_species.txt",header=T)
d4<-read.table("PTSD_diversity_path.txt",header=T)
df1<-d3[,-c(1:3)]
df2<-d4[,-c(1:3)]

a.bray1 <- vegdist(df1)#bray-Curtis
a.bray2 <- vegdist(df2)#bray-Curtis
PCoA1 <- pcoa(a.bray1,correction = "none",rn=NULL)#
PCoA2 <- pcoa(a.bray2,correction = "none",rn=NULL)#

pc1 = as.data.frame(PCoA1$vectors)
pc2 = as.data.frame(PCoA2$vectors)
PCoA.result1 <- cbind(d3[,c(1,3)],pc1[,c(1,2)])
PCoA.result2 <- cbind(d4[,c(1,3)],pc2[,c(1,2)])
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
PCoA.result1$Time <- factor(PCoA.result1$Time, levels=c("No_trauma", "Trauma_no_PTSD", "PTSD"))
PCoA.result2$Time <- factor(PCoA.result2$Time, levels=c("No_trauma", "Trauma_no_PTSD", "PTSD"))

p4<- ggplot(PCoA.result1, aes(PCoA1, PCoA2, group = Time, color = Time))
p4 <- p4 + geom_point(size=1)
#p1 <- p1 + geom_text(aes(label=ID),hjust=0, vjust=0,size=0.4,colour="black")
p4 <- p4 + theme_bw()
#geom_text(aes(label=ID),hjust=0, vjust=0,size=1.6)+
#p1 <- p1 + stat_ellipse(level = 0.95, show.legend = F,linetype = 2)
p4 <- p4 + xlab(xlab2)+ylab(ylab2)+theme(panel.grid=element_blank())
#scale_color_viridis(discrete = TRUE, option = "D")+
#scale_fill_viridis(discrete = TRUE) +
p4 <- p4 + scale_color_brewer(palette="Paired")
p4 <- p4 + theme(legend.key.size = unit(0.3, "cm"))
p4 <- p4 + theme(axis.text.x=element_text(colour="black",family="Times",size=8), #设置x轴刻度标签的字体显示倾斜角度为15度，并向下调整1(hjust = 1)，字体簇为Times大小为20
                 axis.text.y=element_text(colour="black",family="Times",size=8,face="plain"), #设置y轴刻度标签的字体簇，字体大小，字体样式为plain
                 axis.title.x=element_text(family="Times",size = 10,face="plain"),
                 axis.title.y=element_text(family="Times",size = 10,face="plain"),
                 legend.position="none",
                 panel.border = element_blank(),axis.line = element_line(colour = "black",size=0.3), #去除默认填充的灰色，并将x=0轴和y=0轴加粗显示(size=1)
                 legend.text=element_text(face="italic", family="Times", colour="black",  #设置图例的子标题的字体属性
                                          size=6),
                 legend.title=element_text(face="italic", family="Times", colour="black", #设置图例的总标题的字体属性
                                           size=6),
                 panel.grid.major = element_blank(),   #不显示网格线
                 panel.grid.minor = element_blank(),
                 panel.background = element_rect(colour = "black", size = 0.5))
p4 <- p4 + theme(legend.position = c())
p4 <- p4 + theme(legend.position="none")


#pdf("PCOA_mice_OTU.pdf",width = 6,height = 4.7)
p5<- ggplot(PCoA.result2, aes(PCoA1, PCoA2, group = Time, color = Time))
p5 <- p5 + geom_point(size=1)
#p1 <- p1 + geom_text(aes(label=ID),hjust=0, vjust=0,size=0.4,colour="black")
p5 <- p5 + theme_bw()
#geom_text(aes(label=ID),hjust=0, vjust=0,size=1.6)+
#p1 <- p1 + stat_ellipse(level = 0.95, show.legend = F,linetype = 2)
p5 <- p5 + xlab(xlab1)+ylab(ylab1)+theme(panel.grid=element_blank())
#scale_color_viridis(discrete = TRUE, option = "D")+
#scale_fill_viridis(discrete = TRUE) +
p5 <- p5 + scale_color_brewer(palette="Paired")
p5 <- p5 + theme(legend.key.size = unit(0.3, "cm"))
p5 <- p5 + theme(axis.text.x=element_text(colour="black",family="Times",size=8), #设置x轴刻度标签的字体显示倾斜角度为15度，并向下调整1(hjust = 1)，字体簇为Times大小为20
                 axis.text.y=element_text(colour="black",family="Times",size=8,face="plain"), #设置y轴刻度标签的字体簇，字体大小，字体样式为plain
                 axis.title.x=element_text(family="Times",size = 10,face="plain"),
                 axis.title.y=element_text(family="Times",size = 10,face="plain"),
                 legend.position="none",
                 panel.border = element_blank(),axis.line = element_line(colour = "black",size=0.3), #去除默认填充的灰色，并将x=0轴和y=0轴加粗显示(size=1)
                 legend.text=element_text(face="italic", family="Times", colour="black",  #设置图例的子标题的字体属性
                                          size=6),
                 legend.title=element_text(face="italic", family="Times", colour="black", #设置图例的总标题的字体属性
                                           size=6),
                 panel.grid.major = element_blank(),   #不显示网格线
                 panel.grid.minor = element_blank(),
                 panel.background = element_rect(colour = "black", size = 0.5))
p5 <- p5 + theme(legend.position = c())
p5 <- p5 + theme(legend.position="none")

###BC dis 
library(ggplot2) 
setwd("/Users/shanlin/HMS_shanlin/Project/PTSD/02_diversity/Diversity")
d5<-read.table("PTSD_BC.txt",header=T)
d5$Index <- factor(d5$Index, levels=c("No_trauma", "Trauma_no_PTSD", "PTSD"))

p6 <- ggplot(data=d5, aes(x = Index,y=species,fill=Index))
p6 <- p6 + geom_violin(trim=FALSE)
p6 <- p6 +stat_boxplot(geom ='errorbar', width = 0.15)
p6 <- p6 + geom_boxplot(size=0.3,width=0.22,outlier.shape= NA,fill="white")
#p3 <- p3 + geom_line(aes(group = Subjects), alpha = 0.4, colour = "grey88", data = df) 
#p0 <- p0 + geom_point(aes(fill = Time), size = 0.1, shape = 21, position = position_jitterdodge())
#p <- p + geom_jitter(shape=16, position=position_jitter(0.2),dotsize=0.00001)
#p <- p + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5,binwidth = 0.13)#shannon0.13
p6 <- p6 + theme(plot.background=element_blank(),panel.background=element_blank())
p6 <- p6 + theme_bw()
p6 <- p6 + theme(axis.text.x=element_text(colour="black",family="Times",size=8), #设置x轴刻度标签的字体显示倾斜角度为15度，并向下调整1(hjust = 1)，字体簇为Times大小为20
                 axis.text.y=element_text(colour="black",family="Times",size=8,face="plain"), #设置y轴刻度标签的字体簇，字体大小，字体样式为plain
                 axis.title.x=element_text(family="Times",size = 10,face="plain"), #设置y轴标题的字体属性
                 axis.title.y=element_text(family="Times",size = 10,face="plain"), #设置y轴标题的字体属性
                 panel.border = element_blank(),axis.line = element_line(colour = "black",size=0.3), #去除默认填充的灰色，并将x=0轴和y=0轴加粗显示(size=1)
                 legend.text=element_text(face="italic", family="Times", colour="black",  #设置图例的子标题的字体属性
                                          size=6),
                 legend.title=element_text(face="italic", family="Times", colour="black", #设置图例的总标题的字体属性
                                           size=6),
                 panel.grid.major = element_blank(),   #不显示网格线
                 panel.grid.minor = element_blank(),
                 panel.background = element_rect(colour = "black", size = 0.5))

p6 <- p6 + scale_fill_brewer(palette="Paired")
p6 <- p6 + ylab("Bray-Curtis")
p6 <- p6 + xlab("Group")
p6 <- p6 + theme(legend.position="none")
#p6 <- p6 + scale_y_continuous(expand = c(0,0), limits = c(0, 1.01),breaks=seq(0,1.01,0.2),labels=scaleFUN)
p6 <- p6 + stat_compare_means(comparisons = my_comparisons,label = "p.signif",size=2.5)

p7 <- ggplot(data=d5, aes(x = Index,y=pathway,fill=Index))
p7 <- p7 + geom_violin(trim=FALSE)
p7 <- p7 +stat_boxplot(geom ='errorbar', width = 0.15)
p7 <- p7 + geom_boxplot(size=0.3,width=0.22,outlier.shape= NA,fill="white")
#p3 <- p3 + geom_line(aes(group = Subjects), alpha = 0.4, colour = "grey88", data = df) 
#p0 <- p0 + geom_point(aes(fill = Time), size = 0.1, shape = 21, position = position_jitterdodge())
#p <- p + geom_jitter(shape=16, position=position_jitter(0.2),dotsize=0.00001)
#p <- p + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5,binwidth = 0.13)#shannon0.13
p7 <- p7 + theme(plot.background=element_blank(),panel.background=element_blank())
p7 <- p7 + theme_bw()
p7 <- p7 + theme(axis.text.x=element_text(colour="black",family="Times",size=8), #设置x轴刻度标签的字体显示倾斜角度为15度，并向下调整1(hjust = 1)，字体簇为Times大小为20
                 axis.text.y=element_text(colour="black",family="Times",size=8,face="plain"), #设置y轴刻度标签的字体簇，字体大小，字体样式为plain
                 axis.title.x=element_text(family="Times",size = 10,face="plain"), #设置y轴标题的字体属性
                 axis.title.y=element_text(family="Times",size = 10,face="plain"), #设置y轴标题的字体属性
                 panel.border = element_blank(),axis.line = element_line(colour = "black",size=0.3), #去除默认填充的灰色，并将x=0轴和y=0轴加粗显示(size=1)
                 legend.text=element_text(face="italic", family="Times", colour="black",  #设置图例的子标题的字体属性
                                          size=6),
                 legend.title=element_text(face="italic", family="Times", colour="black", #设置图例的总标题的字体属性
                                           size=6),
                 panel.grid.major = element_blank(),   #不显示网格线
                 panel.grid.minor = element_blank(),
                 panel.background = element_rect(colour = "black", size = 0.5))

p7 <- p7 + scale_fill_brewer(palette="Paired")
p7 <- p7 + ylab("Bray-Curtis")
p7 <- p7 + xlab("Group")
p7 <- p7 + theme(legend.position="none")
p7 <- p7 + scale_y_continuous(labels=scaleFUN)
p7 <- p7 + stat_compare_means(comparisons = my_comparisons,label = "p.signif",size=2.5)


p<-plot_grid(p0,p1,p4,p6,p2,p3,p5,p7, labels="AUTO",label_size = 10,ncol=4,rel_widths = c(0.96, 1, 1, 1, 0.96, 1, 1, 1))

save_plot("PTSD_Fig4_v4.pdf", p, base_width =12,base_height =6)
