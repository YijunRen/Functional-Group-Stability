
fn_getstar <- function(p){
  if(p<0.001){
    star <- " ***"
  }else if(p<0.01){
    star <- " **"
  }else if(p<0.05){
    star <- " *"
  }else{star <- " "}
  return(star)
}


###### plot lm results
myplot_lm <- function(data, xvar, yvar, xlab, ylab) {
  formula <- as.formula(paste(yvar, "~", xvar))
  model <- lm(formula, data = data)
  model_summary <- tidy(model)
  
  slope <- model_summary[2, "estimate"]  
  p_value <- model_summary[2, "p.value"]  
  r_squared <- summary(model)$r.squared  
  
  
  if(p_value>0.05){my.lty="dashed"}else{my.lty="solid"}
  slope_label <- paste0("Slope = ", round(slope, 3), fn_getstar(p_value))
  r2_label <- paste0("~italic(R)^2== ", round(r_squared, 3))
  
  if(data[1,16] == "Overall"){ 
  plot <- ggplot(data, aes_string(x = xvar, y = yvar)) +
    geom_point(size=3, alpha=0.3,color = "#5B3660") +
    geom_smooth(method="lm", se=T, size=2.5,alpha = 0.3, color = "#5B3660",fill = "#5B3660", linetype=my.lty)+
    annotate("text", x = Inf, y = Inf, label = slope_label, hjust = 1.05, vjust = 1.5, size=7) +
    annotate("text", x = Inf, y = Inf, label = r2_label, hjust = 1.05, vjust = 2.5, parse=T,size=7) + 
    labs(x=xlab, y=ylab) +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
    theme_bw()
  }else if(data[1,17] == "Grass"){
    plot <- ggplot(data, aes_string(x = xvar, y = yvar)) +
      geom_point(size=3, alpha=0.3,color = "#60966D") +
      geom_smooth(method="lm", se=T, size=2.5,alpha = 0.3, color = "#60966D",fill = "#60966D", linetype=my.lty)+
      annotate("text", x = Inf, y = Inf, label = slope_label, hjust = 1.05, vjust = 1.5, size=7) +
      annotate("text", x = Inf, y = Inf, label = r2_label, hjust = 1.05, vjust = 2.5, parse=T,size=7) + 
      labs(x=xlab, y=ylab) +
      scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
      theme_bw()
  }else{
    plot <- ggplot(data, aes_string(x = xvar, y = yvar)) +
      geom_point(size=3, alpha=0.3,color = "#c55645") +
      geom_smooth(method="lm", se=T, size=2.5,alpha = 0.3, color = "#c55645",fill = "#c55645", linetype=my.lty)+
      annotate("text", x = Inf, y = Inf, label = slope_label, hjust = 1.05, vjust = 1.5, size=7) +
      annotate("text", x = Inf, y = Inf, label = r2_label, hjust = 1.05, vjust = 2.5, parse=T,size=7) + 
      labs(x=xlab, y=ylab) +
      scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
      theme_bw()
  }
  
  return(plot)
}



###### plot lme results

#D should be a data frame with y, x, and group
#type should be "intercept" or "both" (both slope and intercept)
#"lmer", "ggeffects", "ggplot2", "patchwork", "ggsci"

myplot.lme <- function(D, type="intercept", 
                       fig.xlab="your.x", fig.ylab="your.y", 
                       mycex=1,
                       point.hide=F){
  colnames(D) <- c("y", "x", "Site")
  D$Site <- as.factor(D$Site)
  
  if(type=="intercept"){
    lme.fit <- lmer(y ~ x + (1|Site), data=D)  #random intercept
  }else{lme.fit <- lmer(y ~ x + (x|Site), data=D)}   #random slope and intercept
  
  #plot_model(lme.comm.v.comp, type="re", show.values = T)  #packgage "sjPlot" and "glmmTMB"
  R2 <- rsq.lmm(lme.fit)
  
  pred <- ggpredict(lme.fit, terms = c("x [n=20]"), type="fixed")
  coefs <- coef(lme.fit)$Site  #coefficient for each group
  xmin <- tapply(D$x, D$Site, min)
  xmax <- tapply(D$x, D$Site, max)
  ymin <- xmin * coefs[,2] + coefs[,1]
  ymax <- xmax * coefs[,2] + coefs[,1]
  pred.random <- data.frame(x=c(xmin,xmax), y=c(ymin, ymax), Site=as.factor(rep(names(xmin), 2)))
  
  
  z <- summary(lme.fit)$coefficients
  if(z[2,5]>0.05){my.lty="dashed"}else{my.lty="solid"}
  mytext <- paste0(" Slope = ",round(z[2,1], 3), fn_getstar(z[2,5]))
  mytext2 <- paste0("~italic(R[m])^2==",round(R2$fixed,2))
  mytext3 <- paste0("~italic(R[c])^2==",round(R2$model,2))
  
  if(point.hide==F){
    g1 <- ggplot(pred) + 
      geom_point(data = D, aes(x = x, y = y, colour = Site), alpha=0.3,size=3) + 
      scale_color_lancet()+
      geom_line(aes(x = x, y = predicted), linewidth=1.5, linetype=my.lty) +          # slope
      #geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), fill = "white", alpha = 0.5) + # error band
      geom_line(data=pred.random, aes(x=x, y=y, color=Site), alpha=2, 
                linewidth=1.2, linetype=my.lty)+
      labs(x=fig.xlab,y=fig.ylab)+
      theme(panel.grid=element_blank(), panel.background=element_rect(fill='transparent', color='black'),
            legend.position="bottom", text = element_text(size = 25*mycex))+
      guides(color=guide_legend(nrow=1))+
      annotate("text", x = Inf, y = Inf, hjust = 1.05, vjust = 1.5, label = mytext, size=7*mycex)+
      annotate("text", x = Inf, y = Inf, hjust= 1.05, vjust=2.5, label = mytext2, parse=T, size=7*mycex)
  }
   
  return(g1)
}




