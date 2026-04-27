library(tidyverse)
library(ggplot2)
library(ggpmisc)
library(patchwork)
library(ggridges)
library(dplyr)
library(broom)
library(ggrepel)
library(ggsci)
library(lmerTest)
library(rsq)
library(ggeffects)
library(scales)
source("Fn_myplot_lme.R")

ryj_theme <- theme(panel.grid = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.position = "none",
      legend.title = element_blank(),
      text = element_text(face = "bold",size = 25),
      plot.tag.position = c(0.2, 0.93))

####Duolun_data####
Res1 <- read.csv("Datasets/Duolun/Duolun_functional_group_stability.csv") %>%
  filter(Group == "Overall")
Res1[,7:11]<- log10(Res1[,7:11])
Res2 <- read.csv("Datasets/Duolun/Duolun_functional_group_stability.csv") %>%
  filter(Group == "Grass")
Res2[,7:11]<- log10(Res2[,7:11])
Res3 <- read.csv("Datasets/Duolun/Duolun_functional_group_stability.csv") %>%
  filter(Group == "Forb")
Res3[,7:11]<- log10(Res3[,7:11])

####e001_data####
Res4 <- read.csv("Datasets/Cedar_e001/CDR_functional_group_stability.csv") %>%
  filter(Group == "Overall")
Res4[,7:11]<- log10(Res4[,7:11])
Res5 <- read.csv("Datasets/Cedar_e001/CDR_functional_group_stability.csv") %>%
  filter(Group == "Grass")
Res5[,7:11]<- log10(Res5[,7:11])
Res6 <- read.csv("Datasets/Cedar_e001/CDR_functional_group_stability.csv") %>%
  filter(Group == "Forb")
Res6[,7:11]<- log10(Res6[,7:11])

####Fig 2####
g4 <- myplot_lm(Res1, "Nitrogen_log", "Scom", "Nitrogen", "Community stability (Log10)")+
  labs(tag = "A")+
  ryj_theme
g5 <- myplot_lm(Res1, "Nitrogen_log", "Spop", "Nitrogen", "Population stability (Log10)")+
  labs(tag = "B")+
  ryj_theme
g6 <- myplot_lm(Res1, "Nitrogen_log", "Asyn", "Nitrogen", "Asynchrony (Log10)")+
  labs(tag = "C")+
  ryj_theme

g7 <- myplot_lm(Res2, "Nitrogen_log", "Scom", "Nitrogen", "Community stability (Log10)")+
  labs(tag = "D")+
  ylim(-0.2,0.55)+
  ryj_theme
g8 <- myplot_lm(Res2, "Nitrogen_log", "Spop", "Nitrogen", "Population stability (Log10)")+
  labs(tag = "E")+
  ylim(-0.86,0.1)+
  ryj_theme
g9 <- myplot_lm(Res2, "Nitrogen_log", "Asyn", "Nitrogen", "Asynchrony (Log10)")+
  labs(tag = "F")+
  ylim(-0.25,0.52)+
  ryj_theme

g10 <- myplot_lm(Res3, "Nitrogen_log", "Scom", "Nitrogen", "Community stability (Log10)")+
  labs(tag = "G")+
  ylim(-0.2,0.55)+
  ryj_theme
g11 <- myplot_lm(Res3, "Nitrogen_log", "Spop", "Nitrogen", "Population stability (Log10)")+
  labs(tag = "H")+
  ylim(-0.86,0.1)+
  ryj_theme
g12 <- myplot_lm(Res3, "Nitrogen_log", "Asyn", "Nitrogen", "Asynchrony (Log10)")+
  labs(tag = "I")+
  ylim(-0.25,0.52)+
  ryj_theme

tiff("Fig 2.tiff", width = 12, height = 11, units = "in", res = 300)
wrap_plots(list(g4, g5, g6,  
                g7, g8, g9,  
                g10, g11, g12), ncol = 3, byrow = FALSE)
dev.off()

####Fig 3####
g3 <- myplot_lm(Res1, "Nitrogen_log", "CPE", "Nitrogen", "Compensatory effect (Log10)")+
  labs(tag = "A")+
  ryj_theme
g4 <- myplot_lm(Res1, "Nitrogen_log", "SAE", "Nitrogen", "Statistical averaging effect (Log10)")+
  labs(tag = "B")+
  ryj_theme

g5 <- myplot_lm(Res2, "Nitrogen_log", "CPE", "Nitrogen", "Compensatory effect (Log10)")+
  labs(tag = "C")+
  ylim(-1,0.55)+
  ryj_theme
g6 <- myplot_lm(Res2, "Nitrogen_log", "SAE", "Nitrogen", "Statistical averaging effect (Log10)")+
  labs(tag = "D")+
  ylim(-0.35,0.6)+
  ryj_theme

g7 <- myplot_lm(Res3, "Nitrogen_log", "CPE", "Nitrogen", "Compensatory effect (Log10)")+
  labs(tag = "E")+
  ylim(-1,0.55)+
  ryj_theme
g8 <- myplot_lm(Res3, "Nitrogen_log", "SAE", "Nitrogen", "Statistical averaging effect (Log10)")+
  labs(tag = "F")+
  ylim(-0.35,0.6)+
  ryj_theme

tiff("Fig 3.tiff", width = 12, height = 7.5, units = "in", res = 300)
wrap_plots(list(g3, g4,   
                g5, g6,  
                g7, g8), ncol = 3, byrow = FALSE)
dev.off()

####Fig 4####
A <- read.csv("Datasets/Duolun/Duolun_CPE_grass_comparison.csv")

g1 <- ggplot(A, aes(x=x2, y=x1)) +
  geom_point(color = "grey70", size = 5, alpha = 0.5) + 
  geom_smooth(linetype = "solid", color = "black", size = 2,
              formula = y ~ x, method = "lm") + 
  stat_poly_eq(aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~~")),  
               formula = y ~ x,  parse = TRUE, 
               size = 7, label.x = 0.95, label.y = 0.97) +  
  labs(tag = "A") +
  labs(x = "CPE of 2 dominant grasses", y = "CPE of grass group") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(b = 0),
        text = element_text(face = "bold",size = 25),
        axis.text.x = element_text(size = 20, color = "black", face = "bold"),   
        axis.text.y = element_text(size = 20, color = "black", face = "bold"),
        plot.tag.position = c(0.18, 0.95))

g2 <- ggplot(A, aes(x=x3, y=x1)) +
  geom_point(color = "grey70", size = 5, alpha = 0.5) + 
  geom_smooth(linetype = "dashed", color = "black", size = 2,
              formula = y ~ x, method = "lm") +
  stat_poly_eq(aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~~")),  
               formula = y ~ x,  parse = TRUE, 
               size = 7, label.x = 0.95, label.y = 0.97) + 
  labs(tag = "B") +
  xlim(0.2,1.3)+
  labs(x = "CPE of other grasses", y = " ") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(t = 0),
        text = element_text(face = "bold",size = 25),
        axis.text.x = element_text(size = 20, color = "black", face = "bold"),   
        axis.text.y = element_text(size = 20, color = "black", face = "bold"),
        plot.tag.position = c(0.18, 0.95))

d1 <- read.csv("Datasets/Duolun/Duolun_two_dominant_grass_species.csv")

g3 <- ggplot(d1[d1$Trt=="F1",], aes(x=Year, y=MeanRelBiomass, col=Species)) + 
  geom_line(linewidth=1.5) +
  labs(title = " ",y="Relative biomass",tag = "C") +
  scale_x_continuous(breaks = seq(2014, 2023, by = 4), limits = c(2014, 2030))+
  scale_color_manual(values = c("#1f77b4", "#ff7f0e")) +
  theme_bw()+
  theme(plot.margin = margin(t = 0),
        plot.tag.position = c(0.18, 0.9),
        strip.background = element_blank(),
        strip.text = element_text(size = 20,face = "bold"),
        legend.position = "none", panel.grid = element_blank(),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 25, color = "black", face = "bold"),
        axis.text.x = element_text(size = 20, color = "black", face = "bold"),   
        axis.text.y = element_text(size = 20, color = "black", face = "bold"),
        plot.tag = element_text(size = 30, face = "bold"))
g4 <- ggplot(d1[d1$Trt=="F5",], aes(x=Year, y=MeanRelBiomass, col=Species)) + 
  geom_line(linewidth=1.5) +
  labs(title = " ", y = " ",tag = "D") +
  scale_x_continuous(breaks = seq(2014, 2023, by = 4), limits = c(2014, 2030))+
  scale_y_continuous(limits = c(0,1), labels = scales::number_format(accuracy = 0.1)) + 
  scale_color_manual(values = c("#1f77b4", "#ff7f0e"))+
  theme_bw()+
  theme(plot.margin = margin(b = 0),
        plot.tag.position = c(0.15, 0.9),
        strip.background = element_blank(),
        strip.text = element_text(size = 25,face = "bold"),
        legend.position = "none", panel.grid = element_blank(),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 20, color = "black", face = "bold"),   
        axis.text.y = element_text(size = 20, color = "black", face = "bold"),
        plot.tag = element_text(size = 30, face = "bold"))

tiff("Fig 4.tiff", width=12, height=10, 
     units="in", res=300, compression = "lzw")
(g1 + g2) / (g3 + g4) 
dev.off()

####Fig 5####
g1 <- ggplot(data = Res1, aes(x = Nitrogen_log, y = Richness)) +
  geom_point(alpha = 0.5, size = 4, color = "#5B3660") + 
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, 
              alpha = 0.3, size = 2, color = "#5B3660", fill = "#5B3660") +  
  stat_poly_eq(aes(label = paste(..rr.label.., sep = "~~~")), 
               formula = y ~ x, label.x.npc = "right", label.y.npc = 0.99, size = 7) +
  stat_poly_eq(aes(label = paste(..p.value.label.., sep = "~~~")), 
               formula = y ~ x, label.x.npc = "right", label.y.npc = 0.88, size = 7) +
  labs(x=" ",y="Richness",tag = "A")+
  ylim(2.5,18)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        text = element_text(face = "bold",size = 25),
        plot.tag.position = c(0.25, 0.93))

g2 <- ggplot(data = Res3, aes(x = Nitrogen_log, y = Richness)) +
  geom_point(alpha = 0.5, size = 4, color = "#c55645") + 
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, 
              alpha = 0.3, size = 2, color = "#c55645", fill = "#c55645") +  
  stat_poly_eq(aes(label = paste(..rr.label.., sep = "~~~")), 
               formula = y ~ x, label.x.npc = "right", label.y.npc = 0.99, size = 7) +
  stat_poly_eq(aes(label = paste(..p.value.label.., sep = "~~~")), 
               formula = y ~ x, label.x.npc = "right", label.y.npc = 0.88, size = 7) +
  labs(x="Nitrogen addition",y=" ",tag = "B") +
  ylim(2.5,18)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        axis.title.y = element_blank(),
        text = element_text(face = "bold",size = 25),
        plot.tag.position = c(0.17, 0.93))

g3 <- ggplot(data = Res2, aes(x = Nitrogen_log, y = Richness)) +
  geom_point(alpha = 0.5, size = 5, color = "#60966D") + 
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, 
              alpha = 0.3, size = 2, color = "#60966D", fill = "#60966D") +  
  stat_poly_eq(aes(label = paste(..rr.label.., sep = "~~~")), 
               formula = y ~ x, label.x.npc = "right", label.y.npc = 0.99, size = 7) +
  stat_poly_eq(aes(label = paste(..p.value.label.., sep = "~~~")), 
               formula = y ~ x, label.x.npc = "right", label.y.npc = 0.88, size = 7) +
  labs(x=" ",y=" ",tag = "C") +
  ylim(2.5,18)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        axis.title.y = element_blank(),
        text = element_text(face = "bold",size = 25),
        plot.tag.position = c(0.17, 0.93))

g4 <- ggplot(data = Res1, aes(x = Richness, y = SAE)) +
  geom_point(alpha = 0.5, size = 4, color = "#5B3660") + 
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, 
              alpha = 0.3, size = 2, color = "#5B3660", fill = "#5B3660") +  
  stat_poly_eq(aes(label = paste(..rr.label.., sep = "~~~")), 
               formula = y ~ x, label.x.npc = "right", label.y.npc = 0.99, size = 7) +
  stat_poly_eq(aes(label = paste(..p.value.label.., sep = "~~~")), 
               formula = y ~ x, label.x.npc = "right", label.y.npc = 0.88, size = 7) +
  labs(x=" ",y=expression(bolditalic(SAE)),tag = "D")+
  ylim(0.3,0.65)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        text = element_text(face = "bold",size = 25),
        plot.tag.position = c(0.25, 0.93))

g5 <- ggplot(data = Res3, aes(x = Richness, y = SAE)) +
  geom_point(alpha = 0.5, size = 4, color = "#c55645") + 
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, 
              alpha = 0.3, size = 2, color = "#c55645", fill = "#c55645") +  
  stat_poly_eq(aes(label = paste(..rr.label.., sep = "~~~")), 
               formula = y ~ x, label.x.npc = "right", label.y.npc = 0.99, size = 7) +
  stat_poly_eq(aes(label = paste(..p.value.label.., sep = "~~~")), 
               formula = y ~ x, label.x.npc = "right", label.y.npc = 0.88, size = 7) +
  labs(x="Richness",y=" ",tag = "E") +
  scale_y_continuous(limits = c(-0.9,0.65),
                     labels = number_format(accuracy = 0.1))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        axis.title.y = element_blank(),
        text = element_text(face = "bold",size = 25),
        plot.tag.position = c(0.2, 0.93))

g6 <- ggplot(data = Res2, aes(x = Richness, y = SAE)) +
  geom_point(alpha = 0.5, size = 5, color = "#60966D") + 
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, 
              alpha = 0.3, size = 2, color = "#60966D", fill = "#60966D",linetype = "dashed") +  
  stat_poly_eq(aes(label = paste(..rr.label.., sep = "~~~")), 
               formula = y ~ x, label.x.npc = "right", label.y.npc = 0.99, size = 7) +
  stat_poly_eq(aes(label = paste(..p.value.label.., sep = "~~~")), 
               formula = y ~ x, label.x.npc = "right", label.y.npc = 0.88, size = 7) +
  labs(x=" ",y=" ",tag = "F") +
  scale_y_continuous(limits = c(-0.25,0.65),
                     labels = number_format(accuracy = 0.1))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        axis.title.y = element_blank(),
        text = element_text(face = "bold",size = 25),
        plot.tag.position = c(0.2, 0.93))

tiff("Fig 5.tiff", width = 12, height = 8, units = "in", res = 300)
((g1+g2+g3)/(g4+g5+g6)) 
dev.off()

####Fig S1####
g1 <- myplot_lm(Res1, "Richness", "Scom", "Richness", "Community stability (Log10)")+
  labs(tag = "A")+
  ryj_theme
g2 <- myplot_lm(Res1, "Richness", "Spop", "Richness", "Population stability (Log10)")+
  labs(tag = "B")+
  ryj_theme
g3 <- myplot_lm(Res1, "Richness", "Asyn", "Richness", "Asynchrony (Log10)")+
  labs(tag = "C")+
  ryj_theme

tiff("Fig S1.tiff", width = 3.7, height = 11, units = "in", res = 300)
g1/g2/g3
dev.off()
####Fig S2####
Res7 <- rbind(Res1,Res4)
tiff("Fig S2.tiff", width=5, height=5, units="in", res=300, compression = "lzw")
myplot.lme(D=Res7[,c("Richness","Nitrogen_log","Field")],
           fig.xlab="Nitrogen addition", fig.ylab="Richness") +
  theme(panel.grid = element_blank(),
        legend.position = c(0.1, 0.1),
        legend.text = element_text(size = 8),
        legend.background = element_rect(fill = "transparent", colour = NA), 
        legend.title = element_blank(),
        text = element_text(face = "bold"))+
  guides(color = guide_legend(direction = "vertical"),  
         fill = guide_legend(direction = "vertical"))
dev.off()

####Fig S3####
g1 <- myplot.lme(D=Res4[,c("Scom","Richness","Field")],
                 fig.xlab="Richness", fig.ylab="Community stability (Log10)")+
  labs(tag = "A") +
  ylim(-0.2,0.85)+
  ryj_theme+
  theme(legend.position = c(0.33, 0.07),
        legend.text = element_text(size = 12),
        legend.background = element_rect(fill = "transparent", colour = NA), 
        legend.title = element_blank())

g2 <- myplot.lme(D=Res4[,c("Spop","Richness","Field")],
                 fig.xlab="Richness", fig.ylab="Population stability (Log10)")+
  labs(tag = "B")+
  ylim(-0.39, 0.3)+
  ryj_theme

g3 <- myplot.lme(D=Res4[,c("Asyn","Richness","Field")],
                 fig.xlab="Richness", fig.ylab="Asynchrony (Log10)")+
  labs(tag = "C")+
  ylim(0.08,0.85)+
  ryj_theme

tiff("Fig S3.tiff", width = 3.7, height = 11, units = "in", res = 300)
g1/g2/g3
dev.off()

####Fig S4####
g4 <- myplot.lme(D=Res4[,c("Scom","Nitrogen_log","Field")],
                 fig.xlab="Nitrogen addition (Log10+1)", fig.ylab="Community stability (Log10)")+
  labs(tag = "A")+
  ylim(-0.2, 0.85)+
  ryj_theme
g5 <- myplot.lme(D=Res4[,c("Spop","Nitrogen_log","Field")],
                 fig.xlab="Nitrogen addition (Log10+1)", fig.ylab="Population stability (Log10)")+
  labs(tag = "B")+
  ylim(-0.4,0.35)+
  ryj_theme
g6 <- myplot.lme(D=Res4[,c("Asyn","Nitrogen_log","Field")],
                 fig.xlab="Nitrogen addition (Log10+1)", fig.ylab="Asynchrony (Log10)")+
  labs(tag = "C")+
  ryj_theme

g7 <- myplot.lme(D=Res5[,c("Scom","Nitrogen_log","Field")],
                 fig.xlab="Nitrogen addition (Log10+1)", fig.ylab="Community stability (Log10)")+
  labs(tag = "D")+
  ylim(-2,1.2)+
  ryj_theme
g8 <- myplot.lme(D=Res5[,c("Spop","Nitrogen_log","Field")],
                 fig.xlab="Nitrogen addition (Log10+1)", fig.ylab="Population stability (Log10)")+
  labs(tag = "E")+
  scale_y_continuous(limits = c(-2.4,0.7),
                     labels = number_format(accuracy = 0.1))+
  ryj_theme
g9 <- myplot.lme(D=Res5[,c("Asyn","Nitrogen_log","Field")],
                 fig.xlab="Nitrogen addition (Log10+1)", fig.ylab="Asynchrony (Log10)")+
  labs(tag = "F")+
  ylim(-1.46,1)+
  ryj_theme

g10 <- myplot.lme(D=Res6[,c("Scom","Nitrogen_log","Field")],
                  fig.xlab="Nitrogen addition (Log10+1)", fig.ylab="Community stability (Log10)")+
  labs(tag = "G")+
  scale_y_continuous(limits = c(-2,1.2),
                     labels = number_format(accuracy = 0.1))+
  ryj_theme
g11 <- myplot.lme(D=Res6[,c("Spop","Nitrogen_log","Field")],
                  fig.xlab="Nitrogen addition (Log10+1)", fig.ylab="Population stability (Log10)")+
  labs(tag = "H")+
  scale_y_continuous(limits = c(-2.4,0.7),
                     labels = number_format(accuracy = 0.1))+
  ryj_theme
g12 <- myplot.lme(D=Res6[,c("Asyn","Nitrogen_log","Field")],
                  fig.xlab="Nitrogen addition (Log10+1)", fig.ylab="Asynchrony (Log10)")+
  labs(tag = "I")+
  ylim(-1.46,1)+
  ryj_theme

tiff("Fig S4.tiff", width = 12, height = 11, units = "in", res = 300)
wrap_plots(list(g4, g5, g6,  
                g7, g8, g9,  
                g10, g11, g12), ncol = 3, byrow = FALSE)
dev.off()

####Fig S5####
g1 <- myplot_lm(Res1, "Richness", "CPE", "Richness", "Compensatory effect (Log10)")+
  labs(tag = "A")+
  ryj_theme
g2 <- myplot_lm(Res1, "Richness", "SAE", "Richness", "Statistical averaging effect (Log10)")+
  labs(tag = "B")+
  ryj_theme
tiff("Fig S5.tiff", width = 3.7, height = 7.5, units = "in", res = 300)
g1/g2
dev.off()
####Fig S6####
g1 <- myplot.lme(D=Res4[,c("CPE","Richness","Field")],
                 fig.xlab="Richness", fig.ylab="Compensatory effect (Log10)")+
  labs(tag = "A")+
  ylim(-0.09,0.37)+
  ryj_theme+
  theme(legend.position = c(0.35, 0.07),
        legend.text = element_text(size = 12),
        legend.background = element_rect(fill = "transparent", colour = NA), 
        legend.title = element_blank())
g2 <- myplot.lme(D=Res4[,c("SAE","Richness","Field")],
                 fig.xlab="Richness", fig.ylab=" ")+
  labs(tag = "B")+
  ylim(0.06,0.65)+
  ryj_theme
tiff("Fig S6.tiff", width = 3.7, height = 7.5, units = "in", res = 300)
g1/g2
dev.off()
####Fig S7####
g3 <- myplot.lme(D=Res4[,c("CPE","Nitrogen_log","Field")],
                 fig.xlab="Nitrogen addition (Log10+1)", fig.ylab="Compensatory effect (Log10)")+
  labs(tag = "A")+
  ryj_theme
g4 <- myplot.lme(D=Res4[,c("SAE","Nitrogen_log","Field")],
                 fig.xlab="Nitrogen addition (Log10+1)", fig.ylab="Statistical-averaging effect (Log10)")+
  labs(tag = "B")+
  ylim(0.06,0.67)+
  ryj_theme

g5 <- myplot.lme(D=Res5[,c("CPE","Nitrogen_log","Field")],
                 fig.xlab="Nitrogen addition (Log10+1)", fig.ylab=" ")+
  labs(tag = "C")+
  ylim(-3.2,2.5)+
  ryj_theme
g6 <- myplot.lme(D=Res5[,c("SAE","Nitrogen_log","Field")],
                 fig.xlab="Nitrogen addition (Log10+1)", fig.ylab=" ")+
  labs(tag = "D")+
  ylim(-1.14,1.2)+
  ryj_theme

g7 <- myplot.lme(D=Res6[,c("CPE","Nitrogen_log","Field")],
                 fig.xlab="Nitrogen addition (Log10+1)", fig.ylab=" ")+
  labs(tag = "E")+
  ylim(-3.2,2.5)+
  ryj_theme
g8 <- myplot.lme(D=Res6[,c("SAE","Nitrogen_log","Field")],
                 fig.xlab="Nitrogen addition (Log10+1)", fig.ylab=" ")+
  labs(tag = "F")+
  ylim(-3,1.2)+
  ryj_theme

tiff("Fig S7.tiff", width = 12, height = 7.5, units = "in", res = 300)
wrap_plots(list(g3, g4,   
                g5, g6,  
                g7, g8), ncol = 3, byrow = FALSE)
dev.off()
####Fig S8####
data1 <- read.csv("Datasets/Duolun/Duolun_model_coefficients.csv")

my_order <- c(
  "Community CPE ~ Grass-group CPE",
  "Community SAE ~ Grass-group SAE",
  "Community CPE ~ Grass-group Richness",
  "Community CPE ~ Forb-group CPE",
  "Community SAE ~ Forb-group SAE",
  "Community SAE ~ Forb-group Richness")

data1$model <- factor(data1$model, levels = my_order)

y_labs <- c(
  "Community CPE ~ Grass-group CPE"      = "bolditalic(CPE) ~'~'~ bolditalic(CPE)[bolditalic(Grass)]",
  "Community SAE ~ Grass-group SAE"      = "bolditalic(SAE) ~'~'~ bolditalic(SAE)[bolditalic(Grass)]",
  "Community CPE ~ Grass-group Richness" = "bolditalic(CPE) ~'~'~ bolditalic(Richness)[bolditalic(Grass)]",
  "Community CPE ~ Forb-group CPE"       = "bolditalic(CPE) ~'~'~ bolditalic(CPE)[bolditalic(Forb)]",
  "Community SAE ~ Forb-group SAE"       = "bolditalic(SAE) ~'~'~ bolditalic(SAE)[bolditalic(Forb)]",
  "Community SAE ~ Forb-group Richness"  = "bolditalic(SAE) ~'~'~ bolditalic(Richness)[bolditalic(Forb)]")

tiff("Fig 8.tiff", width=7, height=7, 
     units="in", res=300, compression = "lzw")
ggplot(data1, aes(x = estimate, y = model, color = pathway)) +
  geom_vline(xintercept = 0, linetype = 2, color = "grey50", linewidth = 0.6) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.14, linewidth = 1.0) +
  geom_point(size = 3.6) +
  geom_text(aes(label = stars), color = "black",
            nudge_y = 0.3, size = 5) +
  scale_color_manual(values = c("Grass-group" = "#60966D", 
                                "Forb-group" = "#c55645")) +
  scale_y_discrete(limits = rev(my_order),
                   labels = function(x) parse(text = y_labs[x])) +
  labs(x = "Standardized regression coefficient", y = NULL, color = NULL) +
  theme_bw(base_size = 13) +
  theme(legend.position = "top",
        legend.justification = "left",
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 11,face = "bold"), 
        axis.title.x = element_text(size = 12,face = "bold"))
dev.off()


####Fig S9####
data1 <- read.csv("Datasets/Cedar_e001/CDR_model_coefficients.csv")

my_order <- c(
  "Community CPE ~ Grass-group CPE",
  "Community SAE ~ Grass-group SAE",
  "Community CPE ~ Grass-group Richness",
  "Community CPE ~ Forb-group CPE",
  "Community SAE ~ Forb-group SAE",
  "Community SAE ~ Forb-group Richness")

data1$model <- factor(data1$model, levels = my_order)

y_labs <- c(
  "Community CPE ~ Grass-group CPE"      = "bolditalic(CPE) ~'~'~ bolditalic(CPE)[bolditalic(Grass)]",
  "Community SAE ~ Grass-group SAE"      = "bolditalic(SAE) ~'~'~ bolditalic(SAE)[bolditalic(Grass)]",
  "Community CPE ~ Grass-group Richness" = "bolditalic(CPE) ~'~'~ bolditalic(Richness)[bolditalic(Grass)]",
  "Community CPE ~ Forb-group CPE"       = "bolditalic(CPE) ~'~'~ bolditalic(CPE)[bolditalic(Forb)]",
  "Community SAE ~ Forb-group SAE"       = "bolditalic(SAE) ~'~'~ bolditalic(SAE)[bolditalic(Forb)]",
  "Community SAE ~ Forb-group Richness"  = "bolditalic(SAE) ~'~'~ bolditalic(Richness)[bolditalic(Forb)]")

tiff("Fig S9.tiff", width=7, height=7, 
     units="in", res=300, compression = "lzw")
ggplot(data1, aes(x = estimate, y = model, color = pathway)) +
  geom_vline(xintercept = 0, linetype = 2, color = "grey50", linewidth = 0.6) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.14, linewidth = 1.0) +
  geom_point(size = 3.6) +
  geom_text(aes(label = stars), color = "black",
            nudge_y = 0.3, size = 5) +
  scale_color_manual(values = c("Grass-group" = "#60966D", 
                                "Forb-group" = "#c55645")) +
  scale_y_discrete(limits = rev(my_order),
                   labels = function(x) parse(text = y_labs[x])) +
  labs(x = "Standardized regression coefficient", y = NULL, color = NULL) +
  theme_bw(base_size = 13) +
  theme(legend.position = "top",
        legend.justification = "left",
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 11,face = "bold"), 
        axis.title.x = element_text(size = 12,face = "bold"))
dev.off()
####CDR B####
####Fig S10####
res1 <- read.csv("Datasets/Cedar_e001/CDR_B_grass.csv")
res1$Group <- rep("CPE of Grass group")
res1 <- res1[,c(1:9,15:16)]
res2 <- read.csv("Datasets/Cedar_e001/Cedar_weigthed_stability_Dominant_grass.csv")
res2 <- res2[res2$Field == "CDR B",c(1:8,15:17)]
res3 <- read.csv("Datasets/Cedar_e001/Cedar_weigthed_stability_Other_grasses.csv")
res3 <- res3[res3$Field == "CDR B",c(1:8,15:17)]
colnames(res2) <- colnames(res1)
colnames(res3) <- colnames(res1)
Res <- rbind(res1,res2,res3)

A <- data.frame(Trt = Res[1:48, 2], Plot = Res[1:48, 3],
                x1 = Res[1:48, 6], x2 = Res[49:96, 6], x3 =Res[97:144,6])

g1 <- ggplot(A, aes(x=x2, y=x1)) +
  geom_point(color = "grey70", size = 5, alpha = 0.5) + 
  geom_smooth(linetype = "solid", color = "black", size = 2,
              formula = y ~ x, method = "lm") + 
  stat_poly_eq(aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~~")),  
               formula = y ~ x,  parse = TRUE, 
               size = 7, label.x = 0.95, label.y = 0.97) +  
  labs(tag = "A") +
  labs(x = "CPE of 2 dominant grasses", y = "CPE of grass group") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(b = 0),
        text = element_text(face = "bold",size = 25),
        axis.text.x = element_text(size = 20, color = "black", face = "bold"),   
        axis.text.y = element_text(size = 20, color = "black", face = "bold"),
        plot.tag.position = c(0.18, 0.95))

g2 <- ggplot(A, aes(x=x3, y=x1)) +
  geom_point(color = "grey70", size = 5, alpha = 0.5) + 
  geom_smooth(linetype = "dashed", color = "black", size = 2,
              formula = y ~ x, method = "lm") +
  stat_poly_eq(aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~~")),  
               formula = y ~ x,  parse = TRUE, 
               size = 7, label.x = 0.95, label.y = 0.97) + 
  labs(tag = "B") +
  labs(x = "CPE of other grasses", y = " ") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(t = 0),
        text = element_text(face = "bold",size = 25),
        axis.text.x = element_text(size = 20, color = "black", face = "bold"),   
        axis.text.y = element_text(size = 20, color = "black", face = "bold"),
        plot.tag.position = c(0.18, 0.95))

d2 <- read.csv("Datasets/Cedar_e001/CDR_two_dominant_grass_species.csv")

g3 <- ggplot(d2[d2$NTrt=="1" & d2$Field=="B",], aes(x=Year, y=MeanRelBiomass, col=Species)) + 
  geom_line(linewidth=1.5) +
  labs(title = " ",y="Relative biomass",tag = "C") +
  scale_x_continuous(breaks = seq(1982, 2004, by = 7), limits = c(1982, 2025))+
  scale_y_continuous(limits = c(0,1), labels = scales::number_format(accuracy = 0.1)) + 
  scale_color_manual(values = c("#1f77b4", "#ff7f0e")) +
  theme_bw()+
  theme(plot.margin = margin(t = 0),
        plot.tag.position = c(0.18, 0.9),
        strip.background = element_blank(),
        strip.text = element_text(size = 20,face = "bold"),
        legend.position = "none", panel.grid = element_blank(),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 25, color = "black", face = "bold"),
        axis.text.x = element_text(size = 20, color = "black", face = "bold"),   
        axis.text.y = element_text(size = 20, color = "black", face = "bold"),
        plot.tag = element_text(size = 30, face = "bold"))

g4 <- ggplot(d2[d2$NTrt=="8" & d2$Field=="B",], aes(x=Year, y=MeanRelBiomass, col=Species)) + 
  geom_line(linewidth=1.5) +
  scale_x_continuous(breaks = seq(1982, 2004, by = 7), limits = c(1982, 2025))+
  scale_color_manual(values = c("#1f77b4", "#ff7f0e"))+
  labs(title = " ", y = " ",tag = "D")+
  theme_bw()+
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  theme(plot.margin = margin(t = 0),
        plot.tag.position = c(0.15, 0.9),
        strip.background = element_blank(),
        strip.text = element_text(size = 20,face = "bold"),
        legend.position = "none", panel.grid = element_blank(),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 20, color = "black", face = "bold"),   
        axis.text.y = element_text(size = 20, color = "black", face = "bold"),
        plot.tag = element_text(size = 30, face = "bold"))

tiff("Fig S10.tiff", width=12, height=10, 
     units="in", res=300, compression = "lzw")
(g1 + g2)/(g3 + g4)
dev.off()
####Fig S11####
res1 <- read.csv("Datasets/Cedar_e001/CDR_C_grass.csv")
res1$Group <- rep("CPE of Grass group")
res1 <- res1[,c(1:9,15:16)]
res2 <- read.csv("Datasets/Cedar_e001/Cedar_weigthed_stability_Dominant_grass.csv")
res2 <- res2[res2$Field == "CDR C",c(1:8,15:17)]
res3 <- read.csv("Datasets/Cedar_e001/Cedar_weigthed_stability_Other_grasses.csv")
res3 <- res3[res3$Field == "CDR C",c(1:8,15:17)]
colnames(res2) <- colnames(res1)
colnames(res3) <- colnames(res1)
Res <- rbind(res1,res2,res3)

A <- data.frame(Trt = Res[1:48, 2], Plot = Res[1:48, 3],
                x1 = Res[1:48, 6], x2 = Res[49:96, 6], x3 =Res[97:144,6])

g1 <- ggplot(A, aes(x=x2, y=x1)) +
  geom_point(color = "grey70", size = 5, alpha = 0.5) + 
  geom_smooth(linetype = "solid", color = "black", size = 2,
              formula = y ~ x, method = "lm") + 
  stat_poly_eq(aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~~")),  
               formula = y ~ x,  parse = TRUE, 
               size = 7, label.x = 0.95, label.y = 0.97) +  
  labs(tag = "A") +
  labs(x = "CPE of 2 dominant grasses", y = "CPE of grass group") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(b = 0),
        text = element_text(face = "bold",size = 25),
        axis.text.x = element_text(size = 20, color = "black", face = "bold"),   
        axis.text.y = element_text(size = 20, color = "black", face = "bold"),
        plot.tag.position = c(0.18, 0.95))

g2 <- ggplot(A, aes(x=x3, y=x1)) +
  geom_point(color = "grey70", size = 5, alpha = 0.5) + 
  geom_smooth(linetype = "dashed", color = "black", size = 2,
              formula = y ~ x, method = "lm") +
  stat_poly_eq(aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~~")),  
               formula = y ~ x,  parse = TRUE, 
               size = 7, label.x = 0.95, label.y = 0.97) + 
  labs(tag = "B") +
  labs(x = "CPE of other grasses", y = " ") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(t = 0),
        text = element_text(face = "bold",size = 25),
        axis.text.x = element_text(size = 20, color = "black", face = "bold"),   
        axis.text.y = element_text(size = 20, color = "black", face = "bold"),
        plot.tag.position = c(0.18, 0.95))

d2 <- read.csv("Datasets/Cedar_e001/CDR_two_dominant_grass_species.csv")

g3 <- ggplot(d2[d2$NTrt=="1" & d2$Field=="C",], aes(x=Year, y=MeanRelBiomass, col=Species)) + 
  geom_line(linewidth=1.5) +
  labs(title = " ",y="Relative biomass",tag = "C") +
  scale_x_continuous(breaks = seq(1982, 2004, by = 7), limits = c(1982, 2025))+
  scale_y_continuous(limits = c(0,1), labels = scales::number_format(accuracy = 0.1)) + 
  scale_color_manual(values = c("#1f77b4", "#ff7f0e")) +
  theme_bw()+
  theme(plot.margin = margin(t = 0),
        plot.tag.position = c(0.18, 0.9),
        strip.background = element_blank(),
        strip.text = element_text(size = 20,face = "bold"),
        legend.position = "none", panel.grid = element_blank(),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 25, color = "black", face = "bold"),
        axis.text.x = element_text(size = 20, color = "black", face = "bold"),   
        axis.text.y = element_text(size = 20, color = "black", face = "bold"),
        plot.tag = element_text(size = 30, face = "bold"))

g4 <- ggplot(d2[d2$NTrt=="8" & d2$Field=="C",], aes(x=Year, y=MeanRelBiomass, col=Species)) + 
  geom_line(linewidth=1.5) +
  scale_x_continuous(breaks = seq(1982, 2004, by = 7), limits = c(1982, 2025))+
  scale_color_manual(values = c("#1f77b4", "#ff7f0e"))+
  labs(title = " ", y = " ",tag = "D")+
  theme_bw()+
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  theme(plot.margin = margin(t = 0),
        plot.tag.position = c(0.15, 0.9),
        strip.background = element_blank(),
        strip.text = element_text(size = 20,face = "bold"),
        legend.position = "none", panel.grid = element_blank(),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 20, color = "black", face = "bold"),   
        axis.text.y = element_text(size = 20, color = "black", face = "bold"),
        plot.tag = element_text(size = 30, face = "bold"))

tiff("Fig S11.tiff", width=12, height=10, 
     units="in", res=300, compression = "lzw")
(g1 + g2)/(g3 + g4)
dev.off()

####Fig S12####
g1 <- ggplot(data = Res4[Res4$Field=="CDR B",], aes(x = Nitrogen_log, y = Richness)) +
  geom_point(alpha = 0.5, size = 4, color = "#5B3660") + 
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, 
              alpha = 0.3, size = 2, color = "#5B3660", fill = "#5B3660") +  
  stat_poly_eq(aes(label = paste(..rr.label.., sep = "~~~")), 
               formula = y ~ x, label.x.npc = "right", label.y.npc = 0.99, size = 7) +
  stat_poly_eq(aes(label = paste(..p.value.label.., sep = "~~~")), 
               formula = y ~ x, label.x.npc = "right", label.y.npc = 0.88, size = 7) +
  labs(x=" ",y="Richness",tag = "A")+
  ylim(0,11)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        text = element_text(face = "bold",size = 25),
        plot.tag.position = c(0.25, 0.93))

g2 <- ggplot(data = Res6[Res6$Field=="CDR B",], aes(x = Nitrogen_log, y = Richness)) +
  geom_point(alpha = 0.5, size = 4, color = "#c55645") + 
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, 
              alpha = 0.3, size = 2, color = "#c55645", fill = "#c55645") +  
  stat_poly_eq(aes(label = paste(..rr.label.., sep = "~~~")), 
               formula = y ~ x, label.x.npc = "right", label.y.npc = 0.99, size = 7) +
  stat_poly_eq(aes(label = paste(..p.value.label.., sep = "~~~")), 
               formula = y ~ x, label.x.npc = "right", label.y.npc = 0.88, size = 7) +
  labs(x="Nitrogen addition",y=" ",tag = "B") +
  ylim(0,11)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        axis.title.y = element_blank(),
        text = element_text(face = "bold",size = 25),
        plot.tag.position = c(0.17, 0.93))

g3 <- ggplot(data = Res5[Res5$Field=="CDR B",], aes(x = Nitrogen_log, y = Richness)) +
  geom_point(alpha = 0.5, size = 5, color = "#60966D") + 
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, 
              alpha = 0.3, size = 2, color = "#60966D", fill = "#60966D") +  
  stat_poly_eq(aes(label = paste(..rr.label.., sep = "~~~")), 
               formula = y ~ x, label.x.npc = "right", label.y.npc = 0.99, size = 7) +
  stat_poly_eq(aes(label = paste(..p.value.label.., sep = "~~~")), 
               formula = y ~ x, label.x.npc = "right", label.y.npc = 0.88, size = 7) +
  labs(x=" ",y=" ",tag = "C") +
  ylim(0,11)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        axis.title.y = element_blank(),
        text = element_text(face = "bold",size = 25),
        plot.tag.position = c(0.17, 0.93))

g4 <- ggplot(data = Res4[Res4$Field=="CDR B",], aes(x = Richness, y = SAE)) +
  geom_point(alpha = 0.5, size = 4, color = "#5B3660") + 
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, 
              alpha = 0.3, size = 2, color = "#5B3660", fill = "#5B3660") +  
  stat_poly_eq(aes(label = paste(..rr.label.., sep = "~~~")), 
               formula = y ~ x, label.x.npc = "right", label.y.npc = 0.99, size = 7) +
  stat_poly_eq(aes(label = paste(..p.value.label.., sep = "~~~")), 
               formula = y ~ x, label.x.npc = "right", label.y.npc = 0.88, size = 7) +
  labs(x=" ",y=expression(bolditalic(SAE)),tag = "D")+
  ylim(0.2,0.5)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        text = element_text(face = "bold",size = 25),
        plot.tag.position = c(0.25, 0.93))

g5 <- ggplot(data = Res6[Res6$Field=="CDR B",], aes(x = Richness, y = SAE)) +
  geom_point(alpha = 0.5, size = 4, color = "#c55645") + 
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, 
              alpha = 0.3, size = 2, color = "#c55645", fill = "#c55645") +  
  stat_poly_eq(aes(label = paste(..rr.label.., sep = "~~~")), 
               formula = y ~ x, label.x.npc = "right", label.y.npc = 0.99, size = 7) +
  stat_poly_eq(aes(label = paste(..p.value.label.., sep = "~~~")), 
               formula = y ~ x, label.x.npc = "right", label.y.npc = 0.88, size = 7) +
  labs(x="Richness",y=" ",tag = "E") +
  scale_y_continuous(limits = c(-3,0.5),
                     labels = number_format(accuracy = 0.1))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        axis.title.y = element_blank(),
        text = element_text(face = "bold",size = 25),
        plot.tag.position = c(0.2, 0.93))

g6 <- ggplot(data = Res5[Res5$Field=="CDR B",], aes(x = Richness, y = SAE)) +
  geom_point(alpha = 0.5, size = 5, color = "#60966D") + 
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, 
              alpha = 0.3, size = 2, color = "#60966D", fill = "#60966D",linetype = "dashed") +  
  stat_poly_eq(aes(label = paste(..rr.label.., sep = "~~~")), 
               formula = y ~ x, label.x.npc = "right", label.y.npc = 0.99, size = 7) +
  stat_poly_eq(aes(label = paste(..p.value.label.., sep = "~~~")), 
               formula = y ~ x, label.x.npc = "right", label.y.npc = 0.88, size = 7) +
  labs(x=" ",y=" ",tag = "F") +
  scale_y_continuous(limits = c(0,0.65),
                     labels = number_format(accuracy = 0.1))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        axis.title.y = element_blank(),
        text = element_text(face = "bold",size = 25),
        plot.tag.position = c(0.2, 0.93))

tiff("Fig S12.tiff", width = 12, height = 8, units = "in", res = 300)
((g1+g2+g3)/(g4+g5+g6)) 
dev.off()

####CDR C####
####Fig S13####
g1 <- ggplot(data = Res4[Res4$Field=="CDR C",], aes(x = Nitrogen_log, y = Richness)) +
  geom_point(alpha = 0.5, size = 4, color = "#5B3660") + 
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, 
              alpha = 0.3, size = 2, color = "#5B3660", fill = "#5B3660") +  
  stat_poly_eq(aes(label = paste(..rr.label.., sep = "~~~")), 
               formula = y ~ x, label.x.npc = "right", label.y.npc = 0.99, size = 7) +
  stat_poly_eq(aes(label = paste(..p.value.label.., sep = "~~~")), 
               formula = y ~ x, label.x.npc = "right", label.y.npc = 0.88, size = 7) +
  labs(x=" ",y="Richness",tag = "A")+
  ylim(1,15)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        text = element_text(face = "bold",size = 25),
        plot.tag.position = c(0.25, 0.93))

g2 <- ggplot(data = Res6[Res6$Field=="CDR C",], aes(x = Nitrogen_log, y = Richness)) +
  geom_point(alpha = 0.5, size = 4, color = "#c55645") + 
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, 
              alpha = 0.3, size = 2, color = "#c55645", fill = "#c55645") +  
  stat_poly_eq(aes(label = paste(..rr.label.., sep = "~~~")), 
               formula = y ~ x, label.x.npc = "right", label.y.npc = 0.99, size = 7) +
  stat_poly_eq(aes(label = paste(..p.value.label.., sep = "~~~")), 
               formula = y ~ x, label.x.npc = "right", label.y.npc = 0.88, size = 7) +
  labs(x="Nitrogen addition",y=" ",tag = "B") +
  ylim(1,15)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        axis.title.y = element_blank(),
        text = element_text(face = "bold",size = 25),
        plot.tag.position = c(0.17, 0.93))

g3 <- ggplot(data = Res5[Res5$Field=="CDR C",], aes(x = Nitrogen_log, y = Richness)) +
  geom_point(alpha = 0.5, size = 5, color = "#60966D") + 
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, 
              alpha = 0.3, size = 2, color = "#60966D", fill = "#60966D") +  
  stat_poly_eq(aes(label = paste(..rr.label.., sep = "~~~")), 
               formula = y ~ x, label.x.npc = "right", label.y.npc = 0.99, size = 7) +
  stat_poly_eq(aes(label = paste(..p.value.label.., sep = "~~~")), 
               formula = y ~ x, label.x.npc = "right", label.y.npc = 0.88, size = 7) +
  labs(x=" ",y=" ",tag = "C") +
  ylim(1,15)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        axis.title.y = element_blank(),
        text = element_text(face = "bold",size = 25),
        plot.tag.position = c(0.17, 0.93))

g4 <- ggplot(data = Res4[Res4$Field=="CDR C",], aes(x = Richness, y = SAE)) +
  geom_point(alpha = 0.5, size = 4, color = "#5B3660") + 
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, 
              alpha = 0.3, size = 2, color = "#5B3660", fill = "#5B3660") +  
  stat_poly_eq(aes(label = paste(..rr.label.., sep = "~~~")), 
               formula = y ~ x, label.x.npc = "right", label.y.npc = 0.99, size = 7) +
  stat_poly_eq(aes(label = paste(..p.value.label.., sep = "~~~")), 
               formula = y ~ x, label.x.npc = "right", label.y.npc = 0.88, size = 7) +
  labs(x=" ",y=expression(bolditalic(SAE)),tag = "D")+
  ylim(0.2,0.65)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        text = element_text(face = "bold",size = 25),
        plot.tag.position = c(0.25, 0.93))

g5 <- ggplot(data = Res6[Res6$Field=="CDR C",], aes(x = Richness, y = SAE)) +
  geom_point(alpha = 0.5, size = 4, color = "#c55645") + 
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, 
              alpha = 0.3, size = 2, color = "#c55645", fill = "#c55645") +  
  stat_poly_eq(aes(label = paste(..rr.label.., sep = "~~~")), 
               formula = y ~ x, label.x.npc = "right", label.y.npc = 0.99, size = 7) +
  stat_poly_eq(aes(label = paste(..p.value.label.., sep = "~~~")), 
               formula = y ~ x, label.x.npc = "right", label.y.npc = 0.88, size = 7) +
  labs(x="Richness",y=" ",tag = "E") +
  scale_y_continuous(limits = c(-1.5,0.7),
                     labels = number_format(accuracy = 0.1))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        axis.title.y = element_blank(),
        text = element_text(face = "bold",size = 25),
        plot.tag.position = c(0.2, 0.93))

g6 <- ggplot(data = Res5[Res5$Field=="CDR C",], aes(x = Richness, y = SAE)) +
  geom_point(alpha = 0.5, size = 5, color = "#60966D") + 
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, 
              alpha = 0.3, size = 2, color = "#60966D", fill = "#60966D",linetype = "dashed") +  
  stat_poly_eq(aes(label = paste(..rr.label.., sep = "~~~")), 
               formula = y ~ x, label.x.npc = "right", label.y.npc = 0.99, size = 7) +
  stat_poly_eq(aes(label = paste(..p.value.label.., sep = "~~~")), 
               formula = y ~ x, label.x.npc = "right", label.y.npc = 0.88, size = 7) +
  labs(x=" ",y=" ",tag = "F") +
  scale_y_continuous(limits = c(-1.5,0.7),
                     labels = number_format(accuracy = 0.1))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        axis.title.y = element_blank(),
        text = element_text(face = "bold",size = 25),
        plot.tag.position = c(0.2, 0.93))

tiff("Fig S13.tiff", width = 12, height = 8, units = "in", res = 300)
((g1+g2+g3)/(g4+g5+g6)) 
dev.off()

####Fig S14####
Res1 <- read.csv("Datasets/Duolun/Duolun_functional_group_stability.csv") %>%
  filter(Group == "Between")

Res2 <- read.csv("Datasets/Cedar_e001/CDR_functional_group_stability.csv") %>%
  filter(Group == "Between")

Res <- rbind(Res1, Res2)

g1 <- myplot.lme(D=Res[,c("Spop","Nitrogen_log","Field")],
                 fig.xlab="Nitrogen addition", 
                 fig.ylab=expression(bolditalic(Spop[Group]))) +
  ylim(-0.25,0.8) +
  theme(panel.grid = element_blank(),
        legend.position = c(0.83, 0.11),
        legend.text = element_text(size = 12),
        legend.background = element_rect(fill = "transparent", colour = NA), 
        legend.title = element_blank(),
        text = element_text(face = "bold")) + 
  guides(color = guide_legend(direction = "vertical"),  
         fill = guide_legend(direction = "vertical"))

g2 <- myplot.lme(D=Res[,c("Asyn","Nitrogen_log","Field")],
                 fig.xlab="Nitrogen addition", 
                 fig.ylab=expression(bolditalic(Asyn[Group]))) + 
  ylim(0,0.4)+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        legend.text = element_text(size = 8),
        legend.background = element_rect(fill = "transparent", colour = NA), 
        legend.title = element_blank(),
        text = element_text(face = "bold"))
g3 <- myplot.lme(D=Res[,c("CPE","Nitrogen_log","Field")],
                 fig.xlab="Nitrogen addition", 
                 fig.ylab=expression(bolditalic(CPE[Group]))) +
  ylim(-0.1,0.3)+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        legend.text = element_text(size = 8),
        legend.background = element_rect(fill = "transparent", colour = NA), 
        legend.title = element_blank(),
        text = element_text(face = "bold"))
g4 <- myplot.lme(D=Res[,c("SAE","Nitrogen_log","Field")],
                 fig.xlab="Nitrogen addition", 
                 fig.ylab=expression(bolditalic(SAE[Group]))) +
  ylim(0,0.3)+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        legend.text = element_text(size = 8),
        legend.background = element_rect(fill = "transparent", colour = NA), 
        legend.title = element_blank(),
        text = element_text(face = "bold")) 

tiff("Fig S14.tiff", width=10, height=10, 
     units="in", res=300, compression = "lzw")
g1+g2+g3+g4+
  plot_annotation(tag_levels = 'A')+        
  plot_layout()
dev.off()

####Fig S15####
g1 <- myplot.lme(D=Res7[,c("Scom","Spop","Field")],
                 fig.xlab=expression(bolditalic(Spop)), 
                 fig.ylab=expression(bolditalic(Scom))) +
  ylim(-0.2,0.8)+
  theme(panel.grid = element_blank(),
        legend.position = c(0.8, 0.11),
        legend.text = element_text(size = 12),
        legend.background = element_rect(fill = "transparent", colour = NA), 
        legend.title = element_blank(),
        text = element_text(face = "bold"))+
  guides(color = guide_legend(direction = "vertical"),  
         fill = guide_legend(direction = "vertical"))

g2 <- myplot.lme(D=Res7[,c("Scom","Asyn","Field")],
                 fig.xlab=expression(bolditalic(Asyn)), 
                 fig.ylab=expression(bolditalic(Scom))) +
  ylim(-0.2,0.8)+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        legend.background = element_rect(fill = "transparent", colour = NA), 
        legend.title = element_blank(),
        text = element_text(face = "bold"))

g3 <- myplot.lme(D=Res7[,c("Scom","CPE","Field")],
                 fig.xlab=expression(bolditalic(CPE)), 
                 fig.ylab=expression(bolditalic(Scom))) +
  ylim(-0.2,0.9)+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        legend.background = element_rect(fill = "transparent", colour = NA), 
        legend.title = element_blank(),
        text = element_text(face = "bold"))

g4 <- myplot.lme(D=Res7[,c("Scom","SAE","Field")],
                 fig.xlab=expression(bolditalic(SAE)), 
                 fig.ylab=expression(bolditalic(Scom))) +
  ylim(-0.2,0.8)+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        legend.background = element_rect(fill = "transparent", colour = NA), 
        legend.title = element_blank(),
        text = element_text(face = "bold"))

tiff("Fig S15.tiff", width=10, height=10, units="in", res=300, compression = "lzw")
g1+g2+g3+g4+
  plot_annotation(tag_levels = 'A')+        
  plot_layout()
dev.off()
