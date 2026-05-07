


EP_seasonality=ggplot() +
  geom_line(aes(x = 0:365, y = Ep_annual_day_ave)) +
  geom_line(aes(DOY,Ep),col = "red3",linewidth=1.5)+
  labs(x = "Day of Year", y = bquote(E[p]
                                     ~" (mm/d)")) +
  theme_bw(base_size = 18)

P_seasonality=ggplot() +
  geom_line(aes(x = 0:365, y = P_annual_day_ave)) +
  geom_line(aes(DOY,D_avg*P_avg*24/IAT_avg),col = "steelblue3",linewidth=1.5)+
  labs(x = "Day of Year", y = bquote(P[mu]
                                     ~" (mm/d)")) +
  theme_bw(base_size = 18)


climatic_daily=ggplot() +
  geom_line(aes(x = 0:365, y = climatic_daily_wb_ave)) +
  geom_line(aes(DOY,D_avg*24*P_avg/IAT_avg-Ep),col="yellow",linewidth=1.5)+
  geom_hline(aes(yintercept=0),linetype="dashed",col="maroon",linewidth=0.5)+
  labs(x = "Day of Year", y = bquote("Climatic Water Balance"~ P[mu]-E[p]
                                     ~" (mm/d)")) +
  theme_bw(base_size = 18)


# Moving_averages_plots
rainfall_seasonality = ggplot() +
  geom_line(data = NULL, aes(DOY, Ep), col = "red3") +
  geom_line(data = NULL, aes(DOY, P_avg), col = "steelblue3") +
  geom_line(data = NULL, aes(DOY, IAT_norm* Ep_amp + Ep_mid)) +
  scale_y_continuous( limits = c(0,max(Ep+2)),
                      name = bquote(rho[E]~ " (mm/h) ," ~
                                      E[P]~ " (mm/d)"
                      ),
                      sec.axis = sec_axis( breaks= c(IAT_mid-IAT_amp,IAT_mid,IAT_mid+IAT_amp) ,
                                           trans = ~ ((. - Ep_mid) / Ep_amp) * 2 * IAT_amp + (IAT_mid - IAT_amp),
                                           name = bquote(lambda[E]~ " (d)")
                      )) +
  labs(x = "Day of Year")+
  theme_bw(base_size = 18)




seasonality= ((rainfall_seasonality / EP_seasonality) | (P_seasonality / climatic_daily)) +
  plot_layout(axes = "collect")

seasonality
  # plot_annotation(title = bquote(
  #   P[A*","*mu] == .(round(P_annual_avg, 2)) ~ " (mm), " ~
  #     Ep[A*","*mu] == .(round(Ep_annual_avg, 2)) ~ " (mm), " ~
  #     AI[mu] == .(round(AI, 2)) ~ ", " ~
  #     delta[mu] == .(round(Del_avg, 2) * 24) ~ " (h),"~
  #     delta[sigma^2]== .(round(Del_var,2))~ ~ "(d)"^2 ~", "~
  #     lambda[mu]  == .(round(mean_IAT,2))~ "(d),"~
  #     lambda[sigma^2] == .(round(var_IAT,2))~ "(d)"^2
  # ))


seasonality


IAT_hist= 
  ggplot() +
  geom_histogram(data=NULL, aes(x=IAT)
                 ,fill="lightblue", colour="navy",boundary = 0
  )+
  scale_y_continuous(name="Number of Events",
                     sec.axis = sec_axis(~ . / length(IAT), name="Relative Frequency"))+
  labs(x = bquote("Rainfall Interarrival time" ~ lambda ~" (d)"), y = "Number of Events")+
  plot_annotation()+
  theme_bw(base_size = 18)

P_i_hist=
  ggplot()+
  geom_histogram(data=NULL, aes(x=P_i)
                 ,fill="steelblue3", colour="navy",boundary = 0
  )+
  scale_y_continuous(name="Number of Events",
                     sec.axis = sec_axis(~ . / length(P_i), name="Relative Frequency"))+
  labs(x = bquote("Rainfall Intensity" ~ rho ~" (mm/hr)"), y = "Number of Events")+
  plot_annotation()+
  theme_bw(base_size = 18)

D_hist=
  ggplot()+
  geom_histogram(data=NULL, aes(x=Del*24),binwidth = 2.5,
                 ,fill="grey", colour="navy",boundary = 0
  )+
  scale_y_continuous(name="Number of Events",
                     sec.axis = sec_axis(~ . / length(P_i), name="Relative Frequency"))+
  labs(x = bquote("Duration"~delta~"(hr)"), y = "Number of Events")+
  plot_annotation()+
  theme_bw(base_size = 18)

Annual_P= ggplot()+
  geom_bar(data=NULL,aes(x=factor(annums/365),y=P_annual),stat = "identity",
           fill="blue4",col="black",alpha=0.4)+
  geom_hline(aes(yintercept=P_annual_avg),linetype="dashed")+
  scale_x_discrete(breaks = seq(1, max(annums), by =4 )) +
  labs(x = "Year", y = "Total Annual Rainfall (mm)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_bw(base_size = 18)


Rainfall_stats= ((P_i_hist/D_hist)|(IAT_hist/Annual_P))+
  plot_annotation(bquote(
    rho[mu] == .(round(P_i_avg, 2)) ~ " (mm/hr), " ~
      lambda[mu] == .(round(mean_IAT, 2)) ~ " (d), " ~
      delta[E] == .(round(D_avg, 2)*24) ~ " (hr), " ~
      delta[mu] == .(round(Del_avg, 2)*24) ~ " (hr), " ~
      P["A,"*mu] == .(round(P_annual_avg, 2)) ~ " (mm)"
    # EP[mu] == .(round(Ep_annual_avg, 2)) ~ " (mm), " ~
    # mu[AI] == .(round(AI, 2)) ~ ", " ~
    # delta[E]== .(round(D_avg, 2) * 24) ~ " (h), " ~  
    # delta[mu] == .(round(Del_avg, 2) * 24) ~ " (h)" ~ 
    # P[A*mu] == .(round(P_annual_avg, 2)) ~ " (mm), "
    
  ))

Rainfall_stats








Del_avg=mean(Del)
Del_var= var(Del)

P_i_avg= mean(P_i)
P_i_var=var(P_i)

mean_IAT= mean(IAT)
var_IAT= var(IAT)

mean_IET= mean(t_x$IET)
var_IET= var(t_x$IET)

x_count=length(tc_wd)
P_count=length(i_Ps) 

total_P= sum(P)
total_E= sum(EVP)
total_I= sum(INFL)
total_R= sum(RNF)
total_D= sum(DRN)



daily_breaks= c(0,rep(1:365,tf/365))



Ep_daily = tapply(EVP_pot, floor(t), sum)
Ep_annual_day_ave = tapply(Ep_daily, daily_breaks, mean)


P_daily = tapply(p, floor(t), sum)
P_annual_day_ave = tapply(P_daily, daily_breaks, mean)

climatic_daily_wb= P_daily-Ep_daily
climatic_daily_wb_ave = tapply(climatic_daily_wb, daily_breaks, mean)



IAT_norm = (IAT_avg - IAT_mid + IAT_amp) / (2 * IAT_amp)



common_x = scale_x_continuous(
  limits = c(0, 365)
)


acc_fluxes = ggplot(data = cumFluxes_df) +
  geom_area(aes(t, P, color = "Precipitation", fill = "Precipitation"), alpha = 0.1, linewidth = 1) +
  geom_line(aes(t, INFL, color = "Infiltration"), size = 1.2, alpha = 1) +
  geom_line(aes(t, EVP, color = "Evaporation", linetype = "Evaporation"), linewidth = 1.2, alpha = 1) +
  geom_line(aes(t, DRN, color = "Drainage", linetype = "Drainage"), linewidth = 1.2, alpha = 1) +
  geom_line(aes(t, RNF, color = "Runoff"), linewidth = 1.2, alpha = 1) +
  geom_line(aes(t, EVP_pot/10, color = "Pot.Evaporation"), linewidth = 1.2, alpha = 1) +
  scale_color_manual(
    name = "Flux Type",
    values = c(
      "Precipitation" = "steelblue3",
      "Infiltration" = "cyan",
      "Evaporation" = "orange",
      "Drainage" = "navy",
      "Runoff" = "purple3",
      "Pot.Evaporation" = "red4"
    )
  ) +
  scale_fill_manual(
    name = "Flux Type",
    values = c("Precipitation" = "steelblue2"),
    guide = "none"
  ) +
  scale_linetype_manual(
    name = "Flux Type",
    values = c(
      "Evaporation" = "dashed",
      "Drainage" = "dashed"
      #
    )
    ,guide="none"
  ) +
  labs(x = "t (d)",
       y = "Cumulative Sum (mm)") +
  scale_y_continuous(
    sec.axis = sec_axis(breaks = c(0,sum(EVP_pot[1:100000])),~ .*10, name="Cumulative Pot.Evaporation (mm)"))+
  theme_bw(base_size = 18)






#
# s_tplot
# alpha_t_plot
# acc_fluxes
# # 
zoomed_s_tplots <- s_tplot +
  # coord_cartesian(xlim = c(t_deg[1]-200, t_deg[1] + 15000))
coord_cartesian(xlim = c(0,5000))
zoomed_s_tplots
# # # zoomed_s_tplots <- s_tplot +
# #   # coord_cartesian(xlim = c(max(tc_dd[tc_dd < t_deg[1]]) - 25, t_deg[1] + 50))
# # zoomed_s_tplots
# # zoomed_alpha_t_plots <- alpha_t_plot +
# #   coord_cartesian(xlim = c(0, t_deg[1] + 5500))
# # zoomed_alpha_t_plots
# 
# 
# zoomed_acc_fluxes= acc_fluxes +
#   coord_cartesian(xlim = c(0, t_deg[1] + 5500),
#                   ylim= c(0,cumFluxes_df[5800,3]
#                   ))
#   
# 
# zoomed_s_tplots/zoomed_alpha_t_plots/zoomed_acc_fluxes
# (zoomed_s_tplots / zoomed_alpha_t_plots / zoomed_acc_fluxes) + plot_layout(,axes = "collect")
# 
# cumFluxes_df[8240,4]
# cumFluxes_df[40000,3]

Y= case_name
Ph= p/(h*24)
scaling=max(Ph)*5
shifting=0.85+max(Ph)/scaling



s_tplot=ggplot()+
  geom_step(aes(y=-Ph/(scaling)+shifting,x=t),col="steelblue")+
  geom_line(aes(t,s))+
  scale_y_continuous(limits = c(0, shifting),name="s (-)",breaks=seq(0,1,0.2),
                     sec.axis = sec_axis(name = "P (mm)",
                                         trans = ~(.-shifting)* -(scaling),
                                         breaks = round(seq(0, max(Ph), length.out = 3))
                     ))+
  geom_hline(aes(yintercept = sd),linetype="dashed")+
  geom_vline(aes(xintercept=t_deg[1]),linetype="dashed",col="maroon")+
  geom_hline(aes(yintercept = sw),linetype="dashed")+
  labs(x = "t (d)",y = "s")+
  theme_bw(base_size = 18)

s_tplot
alpha_t_plot= ggplot()+
  geom_line(aes(t,alpha),col="black",alpha=1,linewidth=0.8)+
  geom_line(aes(t,INFL_Cap/fmax),col="cyan",linewidth=1.2,alpha=0.5)+
  scale_y_continuous(name= bquote(alpha~ " (-) " ), sec.axis = sec_axis(name = "f (mm/d)", trans = ~./(1/fmax)))+
  labs(x = "t (d)")+
  geom_hline(aes(yintercept = alpha_a),linetype="dashed",col="orange")+
  geom_hline(aes(yintercept = alpha_b),linetype="dashed",col="orange")+
  geom_hline(aes(yintercept = fc/fmax),linetype="dashed",col="navy")+
  theme_bw(base_size = 18)
alpha_t_plot
s_alpha=ggplot()+
  geom_path(data=NULL,aes(alpha,s,col=t),size=1.2)+
  scale_colour_gradientn(colours = c("cyan", "yellow","purple1"))+
  labs(y = "s (-)",x = bquote(alpha ~ " (-) " ))+
  geom_point(data=NULL,aes(alpha[1],s[1]),col="cyan",size=2.7)+
  geom_point(data=NULL,aes(alpha[length(t)],s[length(t)]),col="purple3",size=2.7)+
  theme_bw(base_size = 18)


s_Pdf=ggplot()+
  geom_density(data = NULL,aes(s),fill="khaki1",alpha=0.4)+
  geom_vline(aes(xintercept=sd),linetype="dashed")+
  geom_vline(aes(xintercept=sw),linetype="dashed")+
  geom_vline(aes(xintercept=sfc),linetype="dashed")+
  geom_text(aes(x = sd, y = 50, label = "s[d]"), parse = TRUE, hjust = -0.1)+
  geom_text(aes(x = sw, y = 50, label = "s[w]"), parse = TRUE, hjust = -0.1)+
  geom_text(aes(x = sfc, y = 50, label = "s[fc]"), parse = TRUE, hjust = -0.1)+
  ylab("P(s)")+
  scale_x_continuous(limits = c(0, NA))+
  theme_bw(base_size = 18)

alpha_Pdf= ggplot()+
  geom_density(data = NULL,aes(alpha),fill="black",alpha=0.2)+
  ylab(bquote(P*(alpha))) +
  xlab(bquote(alpha ~ " (-) " ))+
  scale_x_continuous(limits = c(0, NA))+
  theme_bw(base_size = 18)

PDF_plots= (s_Pdf/alpha_Pdf)


P1=PDF_plots|s_alpha

P1





P2= (s_tplot|s_alpha)/(alpha_t_plot|acc_fluxes)

P2





s_alpha_t_plots = ((s_tplot/alpha_t_plot) | (s_alpha/PDF_plots)) + 
  plot_layout(widths = c(2, 1))+
  plot_annotation(title = bquote(
    P[A*","*mu] == .(round(P_annual_avg, 2)) ~ " (mm), " ~
      s[w] == .(sw) ~ ", " ~
      s[d] == .(sd) ~", " ~
      s[fc] == .(sfc) ~", " ~
      alpha[b] == .(alpha_b) ~", " ~
      c == .(c) ~", " ~
      r == .(round(r,3)), 
  ))
s_alpha_t_plots


###2: Water Balance####

# ds= F*dt -> plot ds/dt= F -> a plot of ds and dt and correpsonding Fs

F_s= (INFL-EVP-DRN)*h/(n*Z)




Fluxes_barplot=ggplot(data = data.frame(
  Flux = factor(c("Precipitation","Infiltration","Evaporation", "Runoff", "Drainage"),
                levels = c("Precipitation","Infiltration" ,"Evaporation", "Runoff", "Drainage")),
  Total = c(total_P,total_I, total_E,total_R, total_D))
) +
  geom_bar(aes(x = Flux, y = Total, fill = Flux), stat = "identity") +
  scale_fill_manual(values = c(
    "Precipitation" = "steelblue3",
    "Infiltration" = "cyan",
    "Evaporation" = "orange",
    "Drainage" = "navy",
    "Runoff" = "purple3"
  )) +
  labs(x = "Flux Type",
       y = "Total (mm)") +
  theme_bw() +
  theme(legend.position = "none")

f_INFL_plot= ggplot()+
  geom_area(aes(t,INFL_Cap),col="cyan3",fill="cyan3",alpha=0.3)+
  geom_point(aes(t,INFL),col="black")+
  geom_vline(aes(xintercept = t_deg), color = "maroon", linetype = "dashed") +
  labs(x = "Day",
       y = "f (mm/d) vs Infiltration (mm/d) ") +
  theme_bw()







delta_s = c(0, diff(s))
ds_dt=ggplot()+
  geom_line(data=NULL,aes(t,delta_s),col="khaki3",linewidth=0.4)+
  geom_hline(aes(yintercept=0),col="black",linetype="dashed")+
  geom_vline(aes(xintercept=t_deg),color = "maroon",linetype="dashed")+
  scale_x_continuous()+
  labs(x = "Day", y = bquote(Delta*s/Delta*t)) +
  theme_bw()


annual_aridity_plots= ggplot(yearly_aridity, aes(x = factor(Year), 
                                                 y = Aridity_Index)) +
  geom_bar(stat = "identity", fill = "orange",alpha=0.3, color = "red4") +
  geom_hline(aes(yintercept=mean_AI),linetype="dashed")+
  xlab("Year") +
  ylab("Aridity Index (Ep/P)") +
  scale_x_discrete(breaks = levels(factor(yearly_aridity$Year))[seq(1,
                                                                    length(unique(yearly_aridity$Year)), by = 4)]) +
  theme_bw()


yearly_infiltration_coeff = data.frame(Year = annums / 365, c_annual = C_annual)
events_infiltration_coeff = data.frame(Event = eventum, c_event = C_event)



EVP_tot= cumsum(EVP)
EVAP_annual= diff(c(0, EVP_tot[match(annums, t, nomatch = length(t))]))
RNF_tot= cumsum(RNF)
RNF_annual= diff(c(0, RNF_tot[match(annums, t, nomatch = length(t))]))

E_annual=round(RNF_annual/EVAP_annual,2)



annual_C_plots= ggplot(yearly_infiltration_coeff, aes(x = factor(Year), y = c_annual)) +
  geom_bar(stat = "identity", fill = "steelblue1",
           alpha=0.75, color = "navy") +
  xlab("Year") +
  ylab("Runoff Coefficient (R/P)") +
  scale_x_discrete(breaks = levels(factor(yearly_infiltration_coeff$Year))[seq(1, 
                                                                               length(unique(yearly_aridity$Year)), by = 4)]) +
  scale_y_continuous(limits = c(0, max(yearly_infiltration_coeff$c_annual))) +
  theme_bw(base_size=20)
annual_C_plots


annual_E_plots = ggplot(data = NULL, aes(x = annums/365, y = E_annual)) +
  geom_bar(stat = "identity", fill = "violet", alpha = 0.65, color = "navy") +
  xlab("Year") +
  ylab("Modified Runoff Coefficient (R/E)") +
  scale_x_continuous(breaks = seq(1, max(annums/365), by = 4), expand = c(0,0)) +
  scale_y_continuous(limits = c(0, max(E_annual))) +
  theme_bw(base_size = 20)

annual_E_plots

eventual_C_plots = ggplot() +
  geom_line(data = events_infiltration_coeff, 
            aes(x = factor(Event), y = c_event, group = 1), 
            color = "navy", size = 1) +
  xlab("Rainfall Event") +
  ylab("Runoff Coefficient (R/P)") +
  scale_x_discrete(
    breaks = levels(factor(events_infiltration_coeff$Event))[seq(1, length(levels(factor(events_infiltration_coeff$Event))),
                                                                 by = floor(length(eventum)/10))]
  ) +
  theme_bw(base_size = 18) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# water_balance_plot=((acc_fluxes / annual_aridity_plots) |(eventual_C_plots / annual_C_plots))
water_balance_plot=((acc_fluxes ) |(eventual_C_plots ))

water_balance_plot



if (length(t_deg) >= 1) {
  zoomed_s_tplots <- s_tplot +
    coord_cartesian(xlim = c(max(tc_dd[tc_dd < t_deg[1]]) - 1, t_deg[1] + 100))
  
  if (length(t_deg) >= 2) {
    for (q in 2:length(t_deg)) {
      zoomed_s_tplot <- s_tplot +
        coord_cartesian(xlim = c(max(tc_dd[tc_dd < t_deg[q]]) - 1, t_deg[q] + 1300))
      
      zoomed_s_tplots <- zoomed_s_tplots + zoomed_s_tplot
    }
  }
  
  zoomed_s_tplots = zoomed_s_tplots & 
    theme(
      axis.title = element_text(size = 14),      # axis titles
      axis.text = element_text(size = 12),       # tick labels
      legend.title = element_text(size = 12),    # legend title
      legend.text = element_text(size = 10)      # legend items
    ) &
    plot_layout(guides = "collect")             # collects legends
}
library(patchwork)









s_tplot=ggplot()+
  geom_step(aes(y=-Ph[0:100000]/(scaling)+shifting,x=t[0:100000]),col="steelblue")+
  geom_line(aes(t[0:100000],s[0:100000]),linewidth=0.3)+
  scale_y_continuous(limits = c(0, shifting),name="s (-)",breaks=seq(0,1,0.2),
                     sec.axis = sec_axis(name = "P (mm)",
                                         trans = ~(.-shifting)* -(scaling),
                                         breaks = round(seq(0, max(Ph), length.out = 3))
                     ))+
  geom_hline(aes(yintercept = sd),linetype="dashed")+
  geom_vline(aes(xintercept=t_deg[1]),linetype="dashed",col="maroon")+
  geom_hline(aes(yintercept = sw),linetype="dashed")+
  labs(x = "t (d)",y = "s")+
  theme_bw(base_size = 18)

s_tplot

alpha_t_plot= ggplot()+
  geom_line(aes(t[0:100000],alpha[0:100000]),col="black",alpha=1,linewidth=0.8)+
  geom_line(aes(t[0:100000],INFL_Cap[0:100000]/fmax),col="cyan",linewidth=1.2,alpha=0.5)+
  scale_y_continuous(name= bquote(alpha~ " (-) " ), sec.axis = sec_axis(name = "f (mm/d)", trans = ~./(1/fmax)))+
  labs(x = "t (d)")+
  geom_hline(aes(yintercept = alpha_a),linetype="dashed",col="orange")+
  geom_hline(aes(yintercept = alpha_b),linetype="dashed",col="orange")+
  geom_hline(aes(yintercept = fc/fmax),linetype="dashed",col="navy")+
  theme_bw(base_size = 18)
alpha_t_plot

s_alpha = ggplot() +
  geom_path(
    aes(alpha[1:100000], s[1:100000], col = t[1:100000]),
    size = 0.2
  ) +
  scale_colour_gradientn(
    colours = c("cyan", "yellow", "purple1"),
    limits = range(t[1:100000])
  ) +
  labs(y = "s (-)", x = bquote(alpha ~ " (-) ")) +
  geom_point(aes(alpha[1], s[1]), col = "cyan", size = 2.7) +
  geom_point(aes(alpha[100000], s[100000]), col = "pink3", size = 2.7) +
  theme_bw(base_size = 18)
s_alpha

s_Pdf=ggplot()+
  geom_density(data = NULL,aes(s),fill="khaki1",alpha=0.4)+
  geom_vline(aes(xintercept=sd),linetype="dashed")+
  geom_vline(aes(xintercept=sw),linetype="dashed")+
  geom_vline(aes(xintercept=sfc),linetype="dashed")+
  geom_text(aes(x = sd, y = 50, label = "s[d]"), parse = TRUE, hjust = -0.1)+
  geom_text(aes(x = sw, y = 50, label = "s[w]"), parse = TRUE, hjust = -0.1)+
  geom_text(aes(x = sfc, y = 50, label = "s[fc]"), parse = TRUE, hjust = -0.1)+
  ylab("P(s)")+
  scale_x_continuous(limits = c(0, NA))+
  theme_bw(base_size = 18)

alpha_Pdf= ggplot()+
  geom_density(data = NULL,aes(alpha),fill="black",alpha=0.2)+
  ylab(bquote(P*(alpha))) +
  xlab(bquote(alpha ~ " (-) " ))+
  scale_x_continuous(limits = c(0, NA))+
  theme_bw(base_size = 18)

cumFluxes_df=data.frame(cumFluxes_df)

# 
# cumFluxes_df_cut = cumFluxes_df[1:min(7500, nrow(cumFluxes_df)), ]
# cumFluxes_df_cut$t = 1:nrow(cumFluxes_df_cut)
# acc_fluxes = ggplot(data = cumFluxes_df) +
#   geom_area(aes(t, P, color = "Precipitation", fill = "Precipitation"), alpha = 0.1, linewidth = 1) +
#   geom_line(aes(t, INFL, color = "Infiltration"), size = 1.2, alpha = 1) +
#   geom_line(aes(t, EVP, color = "Evaporation", linetype = "Evaporation"), linewidth = 1.2, alpha = 1) +
#   geom_line(aes(t, DRN, color = "Drainage", linetype = "Drainage"), linewidth = 1.2, alpha = 1) +
#   geom_line(aes(t, RNF, color = "Runoff"), linewidth = 1.2, alpha = 1) +
#   geom_line(aes(t, EVP_pot/10, color = "Pot.Evaporation"), linewidth = 1.2, alpha = 1) +
#   scale_color_manual(
#     name = "Flux Type",
#     values = c(
#       "Precipitation" = "steelblue3",
#       "Infiltration" = "cyan",
#       "Evaporation" = "orange",
#       "Drainage" = "navy",
#       "Runoff" = "purple3",
#       "Pot.Evaporation" = "red4"
#     )
#   ) +
#   scale_fill_manual(
#     name = "Flux Type",
#     values = c("Precipitation" = "steelblue2"),
#     guide = "none"
#   ) +
#   scale_linetype_manual(
#     name = "Flux Type",
#     values = c(
#       "Evaporation" = "dashed",
#       "Drainage" = "dashed"
#       #
#     )
#     ,guide="none"
#   ) +
#   labs(x = "t (d)",
#        y = "Cumulative Sum (mm)") +
#   scale_y_continuous(
#     sec.axis = sec_axis(breaks = c(0,sum(EVP_pot[1:7500])),~ .*10, name="Cumulative Pot.Evaporation (mm)"))+
#   theme_bw(base_size = 18)

acc_fluxes

fluxes_cut=acc_fluxes + coord_cartesian(xlim = c(0,10000))


# PDF_plots= (s_Pdf/alpha_Pdf)


# P1=PDF_plots|s_alpha

# P1





P2= (s_tplot|s_alpha)/(alpha_t_plot|fluxes_cut)

P2










