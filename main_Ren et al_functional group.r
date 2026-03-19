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
Res1 <- read.csv("Datasets/Duolun/Duolun_Overall.csv")
Res2 <- read.csv("Datasets/Duolun/Duolun_Grass.csv")
Res3 <- read.csv("Datasets/Duolun/Duolun_Forb.csv")

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

g7 <- myplot_lm(Res2, "Nitrogen_log", "WScom", "Nitrogen", "Community stability (Log10)")+
  labs(tag = "D")+
  ylim(-0.2,0.55)+
  ryj_theme
g8 <- myplot_lm(Res2, "Nitrogen_log", "WSpop", "Nitrogen", "Population stability (Log10)")+
  labs(tag = "E")+
  ylim(-0.86,0.1)+
  ryj_theme
g9 <- myplot_lm(Res2, "Nitrogen_log", "WAsyn", "Nitrogen", "Asynchrony (Log10)")+
  labs(tag = "F")+
  ylim(-0.25,0.52)+
  ryj_theme

g10 <- myplot_lm(Res3, "Nitrogen_log", "WScom", "Nitrogen", "Community stability (Log10)")+
  labs(tag = "G")+
  ylim(-0.2,0.55)+
  ryj_theme
g11 <- myplot_lm(Res3, "Nitrogen_log", "WSpop", "Nitrogen", "Population stability (Log10)")+
  labs(tag = "H")+
  ylim(-0.86,0.1)+
  ryj_theme
g12 <- myplot_lm(Res3, "Nitrogen_log", "WAsyn", "Nitrogen", "Asynchrony (Log10)")+
  labs(tag = "I")+
  ylim(-0.25,0.52)+
  ryj_theme

tiff("Fig 2.tiff", width = 12, height = 11, units = "in", res = 300)
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

####Fig 3####
g3 <- myplot_lm(Res1, "Nitrogen_log", "CPE", "Nitrogen", "Compensatory effect (Log10)")+
  labs(tag = "A")+
  ryj_theme
g4 <- myplot_lm(Res1, "Nitrogen_log", "SAE", "Nitrogen", "Statistical averaging effect (Log10)")+
  labs(tag = "B")+
  ryj_theme

g5 <- myplot_lm(Res2, "Nitrogen_log", "WCPE", "Nitrogen", "Compensatory effect (Log10)")+
  labs(tag = "C")+
  ylim(-1,0.55)+
  ryj_theme
g6 <- myplot_lm(Res2, "Nitrogen_log", "WSAE", "Nitrogen", "Statistical averaging effect (Log10)")+
  labs(tag = "D")+
  ylim(-0.35,0.6)+
  ryj_theme

g7 <- myplot_lm(Res3, "Nitrogen_log", "WCPE", "Nitrogen", "Compensatory effect (Log10)")+
  labs(tag = "E")+
  ylim(-1,0.55)+
  ryj_theme
g8 <- myplot_lm(Res3, "Nitrogen_log", "WSAE", "Nitrogen", "Statistical averaging effect (Log10)")+
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
        plot.tag.position = c(0.19, 0.95))

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
        plot.tag.position = c(0.19, 0.95))


g3 <- ggplot(data = Res1, aes(x = Nitrogen_log, y = Richness)) +
  geom_point(alpha = 0.5, size = 4, color = "#5B3660") + 
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, 
              alpha = 0.3, size = 2, color = "#5B3660", fill = "#5B3660") +  
  stat_poly_eq(aes(label = paste(..rr.label.., sep = "~~~")), 
               formula = y ~ x, label.x.npc = "right", label.y.npc = 0.97, size = 7) +
  stat_poly_eq(aes(label = paste(..p.value.label.., sep = "~~~")), 
               formula = y ~ x, label.x.npc = "right", label.y.npc = 0.9, size = 7) +
  labs(x=" ",y="Richness",tag = "C")+
  ylim(2.5,17)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        text = element_text(face = "bold",size = 25),
        plot.tag.position = c(0.25, 0.95))

g4 <- ggplot(data = Res3, aes(x = Nitrogen_log, y = Richness)) +
  geom_point(alpha = 0.5, size = 4, color = "#c55645") + 
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, 
              alpha = 0.3, size = 2, color = "#c55645", fill = "#c55645") +  
  stat_poly_eq(aes(label = paste(..rr.label.., sep = "~~~")), 
               formula = y ~ x, label.x.npc = "right", label.y.npc = 0.97, size = 7) +
  stat_poly_eq(aes(label = paste(..p.value.label.., sep = "~~~")), 
               formula = y ~ x, label.x.npc = "right", label.y.npc = 0.9, size = 7) +
  labs(x="Nitrogen addition",y=" ",tag = "D") +
  ylim(2.5,17)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        axis.title.y = element_blank(),
        text = element_text(face = "bold",size = 25),
        plot.tag.position = c(0.17, 0.95))

g5 <- ggplot(data = Res2, aes(x = Nitrogen_log, y = Richness)) +
  geom_point(alpha = 0.5, size = 5, color = "#60966D") + 
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, 
              alpha = 0.3, size = 2, color = "#60966D", fill = "#60966D") +  
  stat_poly_eq(aes(label = paste(..rr.label.., sep = "~~~")), 
               formula = y ~ x, label.x.npc = "right", label.y.npc = 0.97, size = 7) +
  stat_poly_eq(aes(label = paste(..p.value.label.., sep = "~~~")), 
               formula = y ~ x, label.x.npc = "right", label.y.npc = 0.9, size = 7) +
  labs(x=" ",y=" ",tag = "E") +
  ylim(2.5,17)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        axis.title.y = element_blank(),
        text = element_text(face = "bold",size = 25),
        plot.tag.position = c(0.17, 0.95))

tiff("Fig 4.tiff", width = 12, height = 12, units = "in", res = 300)
((g1+g2)/(g3+g4+g5)) +
    plot_layout(heights = c(1,1.2))
dev.off()

####Fig 5#####
d1 <- read.csv("Datasets/Duolun/Duolun_0N.csv")
d2 <- read.csv("Datasets/Duolun/Duolun_maxN.csv")
d3 <- read.csv("Datasets/CDR/CDR_B_0N.csv")
d4 <- read.csv("Datasets/CDR/CDR_B_maxN.csv")
d5 <- read.csv("Datasets/CDR/CDR_C_0N.csv")
d6 <- read.csv("Datasets/CDR/CDR_C_maxN.csv")

g1 <- ggplot(d1, aes(x=Year, y=value, col=group)) + 
  geom_line(linewidth=1.5) +
  labs(y="Biomass",tag = "A") +
  scale_x_continuous(breaks = seq(2014, 2023, by = 4), limits = c(2014, 2030))+
  scale_color_manual(values = c("#1f77b4", "#ff7f0e")) +
  facet_wrap(~Field)+
  theme_bw()+
  theme(plot.margin = margin(t = 0),
        plot.tag.position = c(0.18, 0.88),
        strip.background = element_blank(),
        strip.text = element_text(size = 20,face = "bold"),
        legend.position = "none", panel.grid = element_blank(),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 25, color = "black", face = "bold"),
        axis.text.x = element_text(size = 15, color = "black", face = "plain"),   
        axis.text.y = element_text(size = 15, color = "black", face = "plain"),
        plot.tag = element_text(size = 25, face = "bold"))
g2 <- ggplot(d2, aes(x=Year, y=value, col=group)) + 
  geom_line(linewidth=1.5) +
  labs(tag = "B") +
  scale_x_continuous(breaks = seq(2014, 2023, by = 4), limits = c(2014, 2030))+
  scale_color_manual(values = c("#1f77b4", "#ff7f0e"))+
  facet_wrap(~Field)+
  theme_bw()+
  theme(plot.margin = margin(b = 0),
        plot.tag.position = c(0.12, 0.88),
        strip.background = element_blank(),
        strip.text = element_text(size = 20,face = "bold"),
        legend.position = "none", panel.grid = element_blank(),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 15, color = "black", face = "plain"),   
        axis.text.y = element_text(size = 15, color = "black", face = "plain"),
        plot.tag = element_text(size = 25, face = "bold"))

g3 <- ggplot(d3, aes(x=Year, y=value, col=group)) + 
  geom_line(linewidth=1.5) +
  labs(y="Biomass",tag = "C") +
  scale_x_continuous(breaks = seq(1982, 2004, by = 7), limits = c(1982, 2025))+
  scale_color_manual(values = c("#1f77b4", "#ff7f0e")) +
  facet_wrap(~Field)+
  theme_bw()+
  theme(plot.margin = margin(t = 0),
        plot.tag.position = c(0.18, 0.88),
        strip.background = element_blank(),
        strip.text = element_text(size = 20,face = "bold"),
        legend.position = "none", panel.grid = element_blank(),
        legend.title = element_blank(),
        axis.title.y = element_text(size = 25, color = "black", face = "bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 15, color = "black", face = "plain"),   
        axis.text.y = element_text(size = 15, color = "black", face = "plain"),
        plot.tag = element_text(size = 25, face = "bold"))

g4 <- ggplot(d4, aes(x=Year, y=value, col=group)) + 
  geom_line(linewidth=1.5) +
  scale_x_continuous(breaks = seq(1982, 2004, by = 7), limits = c(1982, 2025))+
  scale_color_manual(values = c("#1f77b4", "#ff7f0e"))+
  facet_wrap(~Field)+
  labs(tag = "D")+
  theme_bw()+
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  theme(plot.margin = margin(t = 0),
        plot.tag.position = c(0.12, 0.88),
        strip.background = element_blank(),
        strip.text = element_text(size = 20,face = "bold"),
        legend.position = "none", panel.grid = element_blank(),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 15, color = "black", face = "plain"),   
        axis.text.y = element_text(size = 15, color = "black", face = "plain"),
        plot.tag = element_text(size = 25, face = "bold"))

g5 <- ggplot(d5, aes(x=Year, y=value, col=group)) + 
  geom_line(linewidth=1.5) +
  scale_x_continuous(breaks = seq(1982, 2004, by = 7), limits = c(1982, 2025))+
  scale_color_manual(values = c("#1f77b4", "#ff7f0e"))+
  facet_wrap(~Field)+
  labs(y="Biomass",tag = "E")+
  theme_bw()+
  theme(plot.margin = margin(t = 0),
        plot.tag.position = c(0.18, 0.88),
        strip.background = element_blank(),
        strip.text = element_text(size = 20,face = "bold"),
        legend.position = "none", panel.grid = element_blank(),
        legend.title = element_blank(),
        axis.title.y = element_text(size = 25, color = "black", face = "bold"),
        axis.text.x = element_text(size = 15, color = "black", face = "plain"),   
        axis.text.y = element_text(size = 15, color = "black", face = "plain"),
        plot.tag = element_text(size = 25, face = "bold"))

g6 <- ggplot(d6, aes(x=Year, y=value, col=group)) + 
  geom_line(linewidth=1.5) +
  scale_x_continuous(breaks = seq(1982, 2004, by = 7), limits = c(1982, 2025))+
  scale_color_manual(values = c("#1f77b4", "#ff7f0e"))+
  facet_wrap(~Field)+
  labs(tag = "F")+
  theme_bw()+
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  theme(plot.margin = margin(t = 0),
        plot.tag.position = c(0.12, 0.88),
        strip.background = element_blank(),
        strip.text = element_text(size = 20,face = "bold"),
        legend.position = "none", panel.grid = element_blank(),
        legend.title = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 15, color = "black", face = "plain"),   
        axis.text.y = element_text(size = 15, color = "black", face = "plain"),
        plot.tag = element_text(size = 25, face = "bold"))

tiff("Fig 5.tif", width=10, height=15, 
     units="in", res=300, compression = "lzw")
((g1 + g2) / (g3 + g4) / (g5 + g6)) 
dev.off()

####e001_data####
res4 <- c("Datasets/CDR/CDR_B_overall.csv",
          "Datasets/CDR/CDR_C_overall.csv")
Res4 <- do.call(rbind, lapply(res4, read.csv))
Res4$Group <- rep("Overall")
Res5 <- read.csv("Datasets/CDR/Cedar_weigthed_stability_Grass.csv")
Res5[,4:8] <- log10(Res5[,4:8])
Res6 <- read.csv("Datasets/CDR/Cedar_weigthed_stability_Forb.csv")
Res6[,4:8] <- log10(Res6[,4:8])

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

g7 <- myplot.lme(D=Res5[,c("WScom","Nitrogen_log","Field")],
           fig.xlab="Nitrogen addition (Log10+1)", fig.ylab="Community stability (Log10)")+
  labs(tag = "D")+
  ylim(-2,1.2)+
  ryj_theme
g8 <- myplot.lme(D=Res5[,c("WSpop","Nitrogen_log","Field")],
           fig.xlab="Nitrogen addition (Log10+1)", fig.ylab="Population stability (Log10)")+
  labs(tag = "E")+
  scale_y_continuous(limits = c(-2.4,0.7),
                     labels = number_format(accuracy = 0.1))+
  ryj_theme
g9 <- myplot.lme(D=Res5[,c("WAsyn","Nitrogen_log","Field")],
           fig.xlab="Nitrogen addition (Log10+1)", fig.ylab="Asynchrony (Log10)")+
  labs(tag = "F")+
  ylim(-1.46,1)+
  ryj_theme

g10 <- myplot.lme(D=Res6[,c("WScom","Nitrogen_log","Field")],
           fig.xlab="Nitrogen addition (Log10+1)", fig.ylab="Community stability (Log10)")+
  labs(tag = "G")+
  scale_y_continuous(limits = c(-2,1.2),
                     labels = number_format(accuracy = 0.1))+
  ryj_theme
g11 <- myplot.lme(D=Res6[,c("WSpop","Nitrogen_log","Field")],
           fig.xlab="Nitrogen addition (Log10+1)", fig.ylab="Population stability (Log10)")+
  labs(tag = "H")+
  scale_y_continuous(limits = c(-2.4,0.7),
                     labels = number_format(accuracy = 0.1))+
  ryj_theme
g12 <- myplot.lme(D=Res6[,c("WAsyn","Nitrogen_log","Field")],
           fig.xlab="Nitrogen addition (Log10+1)", fig.ylab="Asynchrony (Log10)")+
  labs(tag = "I")+
  ylim(-1.46,1)+
  ryj_theme

tiff("Fig S4.tiff", width = 12, height = 11, units = "in", res = 300)
wrap_plots(list(g4, g5, g6,  
                g7, g8, g9,  
                g10, g11, g12), ncol = 3, byrow = FALSE)
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

g5 <- myplot.lme(D=Res5[,c("WCPE","Nitrogen_log","Field")],
           fig.xlab="Nitrogen addition (Log10+1)", fig.ylab=" ")+
  labs(tag = "C")+
  ylim(-3.2,2.5)+
  ryj_theme
g6 <- myplot.lme(D=Res5[,c("WSAE","Nitrogen_log","Field")],
           fig.xlab="Nitrogen addition (Log10+1)", fig.ylab=" ")+
  labs(tag = "D")+
  ylim(-1.14,1.2)+
  ryj_theme

g7 <- myplot.lme(D=Res6[,c("WCPE","Nitrogen_log","Field")],
           fig.xlab="Nitrogen addition (Log10+1)", fig.ylab=" ")+
  labs(tag = "E")+
  ylim(-3.2,2.5)+
  ryj_theme
g8 <- myplot.lme(D=Res6[,c("WSAE","Nitrogen_log","Field")],
           fig.xlab="Nitrogen addition (Log10+1)", fig.ylab=" ")+
  labs(tag = "F")+
  ylim(-1.14,1.2)+
  ryj_theme

tiff("Fig S7.tiff", width = 12, height = 7.5, units = "in", res = 300)
wrap_plots(list(g3, g4,   
                g5, g6,  
                g7, g8), ncol = 3, byrow = FALSE)
dev.off()

####Fig S2,S11####
Res7 <- rbind(Res1,Res4)
tiff("Fig S2.tiff", width=5, height=5, units="in", res=300, compression = "lzw")
myplot.lme(D=Res7[,c("Nitrogen_log","Richness","Field")],
           fig.xlab="Richness", fig.ylab="Nitrogen addition") +
  ylim(-0.6, 3) +
  theme(panel.grid = element_blank(),
        legend.position = c(0.1, 0.1),
        legend.text = element_text(size = 8),
        legend.background = element_rect(fill = "transparent", colour = NA), 
        legend.title = element_blank(),
        text = element_text(face = "bold"))+
  guides(color = guide_legend(direction = "vertical"),  
         fill = guide_legend(direction = "vertical"))
dev.off()

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

tiff("Fig S11.tiff", width=10, height=10, units="in", res=300, compression = "lzw")
g1+g2+g3+g4+
  plot_annotation(tag_levels = 'A')+        
  plot_layout()
dev.off()

####CDR B####
####Fig S8####
res1 <- read.csv("Datasets/CDR/CDR_B_grass.csv")
res1$Group <- rep("Datasets/CDR/CPE of Grass group")
res1 <- res1[,c(1:9,15:16)]
res2 <- read.csv("Datasets/CDR/Cedar_weigthed_stability_Dominant_grass.csv")
res2 <- res2[res2$Field == "CDR B",c(1:8,15:17)]
res3 <- read.csv("Datasets/CDR/Cedar_weigthed_stability_Other_grasses.csv")
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
        plot.tag.position = c(0.19, 0.95))

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
        plot.tag.position = c(0.19, 0.95))

data1 <- Res4[Res4$Field=="CDR B",]
data2 <- Res6[Res6$Field=="CDR B",]
data3 <- Res5[Res5$Field=="CDR B",]

g3 <- ggplot(data = data1, aes(x = Nitrogen_log, y = Richness)) +
  geom_point(alpha = 0.5, size = 4, color = "#5B3660") + 
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, 
              alpha = 0.3, size = 2, color = "#5B3660", fill = "#5B3660") +  
  stat_poly_eq(aes(label = paste(..rr.label.., sep = "~~~")), 
               formula = y ~ x, label.x.npc = "right", label.y.npc = 0.97, size = 7) +
  stat_poly_eq(aes(label = paste(..p.value.label.., sep = "~~~")), 
               formula = y ~ x, label.x.npc = "right", label.y.npc = 0.9, size = 7) +
  labs(x=" ",y="Richness",tag = "C")+
  ylim(0,11)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        text = element_text(face = "bold",size = 25),
        plot.tag.position = c(0.26, 0.95))

g4 <- ggplot(data = data2, aes(x = Nitrogen_log, y = Richness)) +
  geom_point(alpha = 0.5, size = 4, color = "#c55645") + 
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, 
              alpha = 0.3, size = 2, color = "#c55645", fill = "#c55645") +  
  stat_poly_eq(aes(label = paste(..rr.label.., sep = "~~~")), 
               formula = y ~ x, label.x.npc = "right", label.y.npc = 0.97, size = 7) +
  stat_poly_eq(aes(label = paste(..p.value.label.., sep = "~~~")), 
               formula = y ~ x, label.x.npc = "right", label.y.npc = 0.9, size = 7) +
  labs(x="Nitrogen addition",y=" ",tag = "D") +
  ylim(0,11)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        axis.title.y = element_blank(),
        text = element_text(face = "bold",size = 25),
        plot.tag.position = c(0.13, 0.95))

g5 <- ggplot(data = data3, aes(x = Nitrogen_log, y = Richness)) +
  geom_point(alpha = 0.5, size = 5, color = "#60966D") + 
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, 
              alpha = 0.3, size = 2, color = "#60966D", fill = "#60966D") +  
  stat_poly_eq(aes(label = paste(..rr.label.., sep = "~~~")), 
               formula = y ~ x, label.x.npc = "right", label.y.npc = 0.97, size = 7) +
  stat_poly_eq(aes(label = paste(..p.value.label.., sep = "~~~")), 
               formula = y ~ x, label.x.npc = "right", label.y.npc = 0.9, size = 7) +
  labs(x=" ",y=" ",tag = "E") +
  ylim(0,11)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        axis.title.y = element_blank(),
        text = element_text(face = "bold",size = 25),
        plot.tag.position = c(0.13, 0.95))

tiff("Fig S8.tiff", width = 12, height = 12, units = "in", res = 300)
((g1+g2)/(g3+g4+g5)) +
  plot_layout(heights = c(1,1.2))
dev.off()
####CDR C####
####Fig S9####
res1 <- read.csv("Datasets/CDR/CDR_C_grass.csv")
res1$Group <- rep("Datasets/CDR/CPE of Grass group")
res1 <- res1[,c(1:9,15:16)]
res2 <- read.csv("Datasets/CDR/Cedar_weigthed_stability_Dominant_grass.csv")
res2 <- res2[res2$Field == "CDR C",c(1:8,15:17)]
res3 <- read.csv("Datasets/CDR/Cedar_weigthed_stability_Other_grasses.csv")
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
        plot.tag.position = c(0.19, 0.95))

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
        plot.tag.position = c(0.19, 0.95))

data1 <- Res4[Res4$Field == "CDR C", ]
data2 <- Res6[Res6$Field == "CDR C", ]
data3 <- Res5[Res5$Field == "CDR C", ]

g3 <- ggplot(data = data1, aes(x = Nitrogen_log, y = Richness)) +
  geom_point(alpha = 0.5, size = 4, color = "#5B3660") + 
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, 
              alpha = 0.3, size = 2, color = "#5B3660", fill = "#5B3660") +  
  stat_poly_eq(aes(label = paste(..rr.label.., sep = "~~~")), 
               formula = y ~ x, label.x.npc = "right", label.y.npc = 0.97, size = 7) +
  stat_poly_eq(aes(label = paste(..p.value.label.., sep = "~~~")), 
               formula = y ~ x, label.x.npc = "right", label.y.npc = 0.9, size = 7) +
  labs(x=" ",y="Richness",tag = "C")+
  ylim(1,15)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        text = element_text(face = "bold",size = 25),
        plot.tag.position = c(0.25, 0.95))

g4 <- ggplot(data = data2, aes(x = Nitrogen_log, y = Richness)) +
  geom_point(alpha = 0.5, size = 4, color = "#c55645") + 
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, 
              alpha = 0.3, size = 2, color = "#c55645", fill = "#c55645") +  
  stat_poly_eq(aes(label = paste(..rr.label.., sep = "~~~")), 
               formula = y ~ x, label.x.npc = "right", label.y.npc = 0.97, size = 7) +
  stat_poly_eq(aes(label = paste(..p.value.label.., sep = "~~~")), 
               formula = y ~ x, label.x.npc = "right", label.y.npc = 0.9, size = 7) +
  labs(x="Nitrogen addition",y=" ",tag = "D") +
  ylim(1,15)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        axis.title.y = element_blank(),
        text = element_text(face = "bold",size = 25),
        plot.tag.position = c(0.17, 0.95))

g5 <- ggplot(data = data3, aes(x = Nitrogen_log, y = Richness)) +
  geom_point(alpha = 0.5, size = 5, color = "#60966D") + 
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, 
              alpha = 0.3, size = 2, color = "#60966D", fill = "#60966D") +  
  stat_poly_eq(aes(label = paste(..rr.label.., sep = "~~~")), 
               formula = y ~ x, label.x.npc = "right", label.y.npc = 0.97, size = 7) +
  stat_poly_eq(aes(label = paste(..p.value.label.., sep = "~~~")), 
               formula = y ~ x, label.x.npc = "right", label.y.npc = 0.9, size = 7) +
  labs(x=" ",y=" ",tag = "E") +
  ylim(1,15)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        axis.title.y = element_blank(),
        text = element_text(face = "bold",size = 25),
        plot.tag.position = c(0.17, 0.95))

tiff("Fig S9.tiff", width = 12, height = 12, units = "in", res = 300)
((g1+g2)/(g3+g4+g5)) +
  plot_layout(heights = c(1,1.2))
dev.off()

####Fig S10####
Res <- read.csv("Datasets/Duolun_CDR_Combined_between.csv")

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
  ylim(0,0.7)+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        legend.text = element_text(size = 8),
        legend.background = element_rect(fill = "transparent", colour = NA), 
        legend.title = element_blank(),
        text = element_text(face = "bold"))
g3 <- myplot.lme(D=Res[,c("CPE","Nitrogen_log","Field")],
                 fig.xlab="Nitrogen addition", 
                 fig.ylab=expression(bolditalic(CPE[Group]))) +
  ylim(-0.1,0.5)+
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

tiff("Fig S10.tiff", width=10, height=10, 
     units="in", res=300, compression = "lzw")
g1+g2+g3+g4+
  plot_annotation(tag_levels = 'A')+        
  plot_layout()
dev.off()

