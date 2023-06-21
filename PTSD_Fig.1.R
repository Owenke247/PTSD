rm(list=ls())
library(ggplot2)
library(dplyr)
library(cowplot)
setwd("/Users/shanlin/HMS_shanlin/Project/PTSD/Diet")
Food_res<-read.table("Fig1_demo.txt",header=T)
Food_res$Species <- factor(Food_res$Species, levels=unique(Food_res$Species))
Food_res$Food <- factor(Food_res$Food, levels=unique(Food_res$Food))
Food_res$Group <- factor(Food_res$Group, levels=unique(Food_res$Group))

#add stars
Food_res <- Food_res %>%
  mutate(text = case_when(  # 一定要 get 到 case_when() 函数奥秘
    P > 0.05 ~ paste(" "), # round() 只保留两位小数
    P <= 0.05 ~ paste("*")))
##plots

p0 <- ggplot(data = Food_res, aes(x=Species, y=Name))
p0 <- p0 + geom_tile(aes(fill=Cor),size=0.01)
#p0 <- p0 + facet_grid(Group ~ ., space="free_y", scales="free_y",switch="y")
p0 <- p0 + geom_text(aes(label=text),col ="white",size = 6,hjust = 0.5, vjust = 0.8)
p0 <- p0 + scale_fill_gradient2(low = "navyblue", high = "red", mid = "gray", midpoint = 0, limit = c(-0.24,0.24), space = "Lab")
p0 <- p0 + theme_minimal()
p0 <- p0 + scale_x_discrete(expand = c(0, 0))
p0 <- p0 + scale_y_discrete(expand = c(0, 0))
p0 <- p0 + theme(legend.position = "none")
p0 <- p0 + theme(axis.text.x = element_blank(),axis.text.y=element_blank())
#p0 <- p0 + theme(strip.text.y.left = element_text(angle = 0,size=8))
p0 <- p0 + theme(panel.spacing.y=unit(0.05, "lines"))
#p0 <- p0 + coord_fixed(ratio = 6/5)
p0 <- p0 + ylab(NULL)
p0 <- p0 + xlab(NULL)
ggsave("Fig1_demo.pdf", width = 6, height = 5)
