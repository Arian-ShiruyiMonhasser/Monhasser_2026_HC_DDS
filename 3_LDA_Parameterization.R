

library(MASS)   
library(ggplot2) 
library(kernlab)
library(scico)


Output=read.csv("~/Output.csv")
Parameters = data.frame()
Dimens= data.frame()


Parameters = Output %>%
  transmute(
    name,
    case,
    dtw_3,
    dtw_5,
    dtw_2,
    sd,
    sw,
    Plasticity= (sd/(sw-sd)),
    D=D_mean,
    # Plasticity= 1/(sd*(1+1/sw)),
    r,
    r_inv=1/r,
    IAT_min=IAT_minline,
    IAT_max=IAT_maxline,
    IAT_mid=IAT_midline,
    IAT_ratio=IAT_minline/IAT_maxline ,
    P_I=P_midline,
    AI=ai,
    Buydko=1/ai,
    P_d_grad=P_midline*D_mean*24/IAT_minline,
    P_d_spike= P_maxline*D_mean*24/IAT_maxline,
    EP_d= EP_mean,
    # ds=1/(sw-sd),
    P_max= P_maxline,
    Ep_midline=Ep_midline,
    Ep_maxline=Ep_maxline,
    
    
    N1=(P_midline*D_mean*24/IAT_minline)/(r*(sw-sd)*0.42*25),
    # N2= P_midline*D_mean*24/IAT_minline*0.8/((sw-sd)*0.42*25)*((P_midline*D_mean*24/IAT_minline)/(r*(sw-sd)*0.42*25))^(0.65),
    N2=P_midline*D_mean*24/IAT_minline*0.8/((sw-sd)*0.42*25),
    Rec_duty= D_mean/(D_mean+IAT_minline),
    eff_duty= (D_mean/(D_mean+IAT_maxline)),
    ds= (sw-sd),
    IAT_ratio=( IAT_minline/IAT_maxline),
    AI_d= EP_mean-(P_midline*D_mean*24/IAT_minline)

  )


sum(Output$dtw_3 == 3)

Parameters$x1=log10(Parameters$N1)
Parameters$x2= log10(Parameters$N2)
Parameters$x3= log10(Parameters$Plasticity)
Parameters$x4= log10(Parameters$eff_duty)
Parameters$x5= log10(Parameters$IAT_ratio)
Parameters$x6= log10(1/Parameters$AI)
Parameters$x7= log10((Parameters$P_I)/Parameters$P_max)
Parameters$x8= log10((Parameters$Ep_midline)/Parameters$Ep_maxline)


write.csv(Parameters,file = "Parameters_clusters2,3,5.csv",row.names = TRUE)
# X = Parameters[, c("sd","r","IAT_min","IAT_max","P_d")]
# X = Parameters[, c("N2","eff_duty","Plasticity","IAT_ratio","AI")] 
X = Parameters[, c("x1","x2","x3","x4","x5","x6","x7","x8")] 
y = as.factor(Output$dtw_3)     



# LDA
lda_model = lda(y ~ ., data = data.frame(X, y))
lda_proj = predict(lda_model, X)

lda_model$prior


lda_model$svd


props <- lda_model$svd^2 / sum(lda_model$svd^2)

sv <- lda_model$svd
prop_trace <- sv^2 / sum(sv^2)



# Combine with labels for plotting
plot_data = data.frame(name=Parameters$name,
  lda_proj$x,IAT_mid=Parameters$IAT_mid, dtw_3 = y, AI=Parameters$AI, dtw_2=Parameters$dtw_2,
  case=Output$case,IAT_ratio=Parameters$IAT_ratio, r=Output$r,N2=Parameters$N2 ,Plasticity= Parameters$Plasticity, 
  eff_duty=Parameters$eff_duty,x1=Parameters$x1,x2=Parameters$x2,x3=Parameters$x3,x4=Parameters$x4,x5=Parameters$x5,
  x6=Parameters$x6,x7=Parameters$x7,x8=Parameters$x8  ,  p4= Parameters$eff_duty,dtw_5=Parameters$dtw_5
  )



plot_data$AI_ = paste0("AI = ", round(plot_data$AI,2))
plot_data$x_4 = paste0("x4 = ", round(plot_data$x4,2))
plot_data$PI_4 = paste0("П4 = ", round(plot_data$p4,3))




write.csv(plot_data,file = "LDA_final.csv",row.names = TRUE)

lda_scores = data.frame(lda_proj$x)

plot_data= read.csv("LDA_final.csv")


# round(lda_model$scaling,2)


# var(lda_model$scaling[,1])

# ggplot(plot_data, aes(x = LD1^3, y = LD2, color = dtw_3)) +
# ggplot(plot_data, aes(x = LD1, y = LD2, color = as.factor(dtw_5),shape=as.factor(dtw_5))) +

# ggplot(plot_data, aes(x = LD1^3, y = eff_duty*IAT_ratio/Plasticity/r/IAT_mid,shape=as.factor(dtw_3),color = as.factor(dtw_3))) +
  ggplot(plot_data, aes(x = LD1^3, y = eff_duty*IAT_ratio/Plasticity/r/IAT_mid,shape=as.factor(dtw_3),color = as.factor(dtw_3))) +
  # ggplot(plot_data, aes(x = eff_duty/AI*x7, y = eff_duty*IAT_ratio/Plasticity/r/IAT_mid,shape=as.factor(dtw_3),color = as.factor(dtw_3))) +
    
  geom_point(size = 3, alpha = 0.65) +
  # geom_vline(
    # xintercept = max(plot_data$LD1[plot_data$dtw_3 == 1], na.rm = TRUE)^3,
    # linetype = "dashed", color = "purple4"
  # ) +
  # geom_vline(
    # xintercept = 0,
    # linetype = "dashed", color = "red"
  # ) +
  # geom_vline(
    # xintercept = 2,
    # linetype = "dashed", color = "green2"
  # ) +
  # facet_wrap(~case+AI_, ncol = 4)+
  # facet_wrap(~x_4+AI_+case,ncol = 4)+
  facet_wrap(~dtw_3)+
  # facet_wrap(~PI_4+AI_+case,ncol = 4)+
  # facet_wrap(~ PI_4 + case, ncol = 4, labeller = label_parsed)+
  scale_color_scico_d(name = "DTW Group", palette = "berlin")+
  scale_shape_discrete(name = "DTW Group") +
  # labs(y = bquote((pi[4] * pi[5]) / (r * lambda[mid] * pi[3])),
  #      x= bquote(LD[1]^3))+
    labs(
      y = expression(Pi[R]*"- Recovery Potential"),
      x = expression( Pi[X]*"- Degradation Potential")
    )+
  # labs(y =  bquote(LD[2]),
       # x= bquote(LD[1]))+
  theme_bw(base_size = 20)+
  theme(legend.title = element_text(size=16))






## Point Selection for Monte Carlo ##

# availabe scio palletes:
scico_palette_show()

simu <- c(601,1304,1703,6403,4706,6703,6805,3908,6304,2608)
seed1_simu <- paste0(simu, ".Rdata")






# 

ggplot(plot_data, aes(x = (LD1), y =  eff_duty*IAT_ratio/Plasticity/r/IAT_mid, color = as.factor(dtw_3))) +
  # ggplot(plot_data, aes(x = LD1, y = eff_duty*Plasticity*IAT_mid/r/IAT_ratio, color = dtw_2)) +
  geom_point(size = 2, alpha = 0.75) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed", color = "red"
  ) +
  # scale_y_log10()+
  # scale_x_log10()+
  facet_wrap(~dtw_3)+
  # facet_wrap(~eff_duty)+
  scale_color_viridis_d()+
  theme_bw()
# subset(plot_data,case %in% c("e","b","d"))
ggplot(plot_data, aes(x = LD1, y = LD2, color = dtw_3)) +
  # ggplot(plot_data, aes(x = LD1^(3), y = LD2^(3), color = dtw_3)) +
  # ggplot(subset(plot_data,case %in% c("e","b","d")), aes(x = LD1, y = LD2, color = dtw_3)) +
  geom_point(size = 2, alpha = 0.55) +
  # geom_vline(
    # xintercept = max(plot_data$LD1[plot_data$dtw_3 == 1], na.rm = TRUE),
    # linetype = "dashed", color = "green4"
  # ) +
  geom_hline(
    yintercept = max(plot_data$LD2[plot_data$dtw_3 == 1], na.rm = TRUE),
    linetype = "dashed", color = "red"
  ) +
  geom_vline(
    xintercept = min(plot_data$LD1[plot_data$dtw_3 == 3], na.rm = TRUE),
    linetype = "dashed", color = "green4"
  ) +
  geom_hline(
    yintercept = min(plot_data$LD2[plot_data$dtw_3 == 3], na.rm = TRUE),
    linetype = "dashed", color = "steelblue2"
  ) +
  # facet_wrap(~dtw_3)+
  facet_wrap(~N2)+
  # scale_x_log10()+
  # scale_y_log10()+
  theme_bw()











N=-31
E=119

12424 - 122.4*E + 0.1494*E^2 + 680.3*N - 5.754*E*N + 5.491*N^2 - 3.947e-4*E^2*N^2
-629727 + 9423.8*E - 33.41*E^2 - 9584*N + 86.14*E*N - 74.39*N^2 + 5.612e-3*E^2*N^2


Dimens = Parameters %>% transmute(
  sd,
  r=1/r,
  N1=N1,
  N2,
  Rec_duty,
  eff_duty,
  Plasticity,
  IAT_ratio,
  N_rec= (N1*Rec_duty),
  AI,
  Budyko=1/AI,
  N_deg= (N2*eff_duty)
)




# LDA
lda_model = lda(y ~ ., data = data.frame(X, y))
lda_proj = predict(lda_model, X)

# Combine with labels for plotting
plot_data = data.frame(lda_proj$x, dtw_3 = y)


# plot LD1 vs LD2
# as.factor(Output$ai)


ggplot(plot_data, aes(x = LD1, y = LD2, 
                      color =dtw_3
                      # color=as.factor(Output$ai)
                      
                      )) +
  geom_point(size = 2, alpha = 0.5) +
  theme_minimal()

lda_model$scaling


plot_ly(Parameters,
        y = ~exp(-eff_duty*IAT_ratio/(r*IAT_mid)), 
        x = ~1/AI,
        z = ~eff_duty*IAT_ratio,
        color = ~as.factor(Output$dtw_3),
        colors = colors()[c(40, 50, 130, 300, 552)[1:length(unique(Output$dtw_3))]],
        type = "scatter3d",
        mode = "markers",
        marker = list(size = 7, opacity = 1),
        text = ~Output$name,
        hoverinfo = "text"
        # )
)%>%
  layout(
    scene = list(
      xaxis = list(type = "linear"),
      yaxis = list(type = "linear"),
      zaxis = list(type = "linear")
    )
  )



lda_values = lda_proj$x[,1]  # first LDA component
classes = y                   # your class labels

# Between-class variance
class_means = tapply(lda_values, classes, mean)
overall_mean = mean(lda_values)
n_classes = table(classes)
S_B = sum(n_classes * (class_means - overall_mean)^2)

# Within-class variance
S_W <- sum(tapply(lda_values, classes, function(x) sum((x - mean(x))^2)))

# Scatter ratio (Fisher criterion)
J <- S_B / S_W
J

