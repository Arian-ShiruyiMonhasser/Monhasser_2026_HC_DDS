library(tidyverse)

# # Model parameter ranges
alpha_b = c(0.01)
sw      = c(0.2) #0.35 0.2 0.38
sd      = c(0.02,4) #0.15 0.04
sfc     = c(0.3)
c_vals  = c(1.8) #1.8
#   # c(5)
# # c(seq(1,2.2,0.2),5)
# # seq(1,2.2,0.2)
# # Create parameter grid
# 
# 
param_space = expand.grid(
  alpha_b = alpha_b,
  sd = sd,
  sw = sw,
  sfc = sfc,
  c = c_vals
)

# change the from=0 to any prefix for own codes
param_space$sim_no= seq(0,length(param_space$c)-1,1)
# 

# param_space=data.frame(inner_space)

# inner_space=inner_space[-dim(inner_space)[1],] quick fix Justin Case

param_space$r = param_space$c / 240

# file_nums = as.integer(gsub("\\.Rdata$", "", file_names))


# param_space$sim_no = file_nums 


ls(pattern="^(param_space)")

for (j in 1:nrow(param_space)) {
  
  j=1
  # Optional clean environment (preserve param_space and i)
  # rm(list = setdiff(ls(), c("param_space", "i")))
  rm(list = ls() %>% str_subset("^(?!(param_space|j))"))
  case_name = param_space$sim_no[j]
  
  set.seed(1)
  
  
  # Assign parameters
  alpha_b = param_space$alpha_b[j]
  sw      = param_space$sw[j]
  sd      = param_space$sd[j]
  sfc     = param_space$sfc[j]
  c       = param_space$c[j]
  r       = param_space$r[j]
  


  
  # Rainfall intensity (mm/d) affected by seasonality (mid > flux)
  P_mid=1.4
  P_amp=0.2
  thetha_3=30
  # interravial time (d) (mid > flux)
  IAT_mid=8.5
  IAT_amp=5.5
  thetha_2=0
  ## daily evaporative demand (mm/d)
  Ep_mid=7
  Ep_amp=5
  thetha_1=0
  
  
  
  D_avg =6/24 #per day
  D_avg= round(D_avg,1)
  
  
  # time step for numerical model
  h=0.1
  # time laps and initial time 
  tf= 365*50
  t0=0
  
  s0=0
  alpha_0=1
  
  # define a vector of time-scale
  t= seq(t0,tf,h)
  
  # shifting the rainfall
  h_rain= 24*h
  
  
  DOY= floor(t%%365)  # for an annual cycle (also useul for input stats)
  DOY[DOY == 0] = 365
  Year= floor(t/365)
  
  
  IAT_avg= (IAT_amp)*cos((DOY)*pi*2/365)+IAT_mid
  Ep= (Ep_amp)*cos((DOY)*pi*2/365)+Ep_mid 
  # P_avg= (P_amp)*cos((DOY)*pi*2/365)+P_mid
  P_avg= (P_amp)*cos((DOY)*pi*2/365+thetha_3)+P_mid
  
  
  # rainfall intensity per time-step
  P_Num= P_avg*h_rain 
  
  #Arian: how are these probabilities defined?
  qq = h/D_avg; # probability that it rain starts when not raining 
  pp = h/(IAT_avg - D_avg)  # probability that rain stops when raining
  t_P= rep(0,length(t))
  for (i in seq(2,length(t))){
    rr = runif(1)
    if (0 == t_P[i-1])
    {
      t_P[i] = (rr < pp[i])
    } else {
      t_P[i] = (rr > qq)
    }
  }
  
  t_P[length(t)] = 0
  
  i_Ps = which( (t_P[2:length(t)]!=0) & (t_P[1:length(t)-1]==0))
  i_Pend = which( (t_P[2:length(t)]==0) & (t_P[1:length(t)-1]!=0))
  
  IAT = h*(i_Ps[2:length(i_Ps)]- i_Ps[1:length(i_Ps)-1])
  Del = h*(i_Pend-i_Ps)
  
  
  
  t_E=ifelse(t_P==0,1,0)
  
  # rainfall per time step 
  p = ifelse(t_P == 1, rexp(length(t_P), 1 / P_Num), 0)
  
  
  # sandy clay (mm/d) -> (mm/step): ? step: mm/day * 1/24 mm/h * 2.4 mm/step 2.4/24
  beta=12.5
  gamma= 12.5
  Ksat_s=30
  sfc=0.3
  
  fmax= 60
  fc=Ksat_s
  
  
  n=0.43 # (-)
  Z=25 #mm
  
  
  # upper and lower limits of alpha
  alpha_a=1
  alpha_b = alpha_b
  
  
  sw= sw
  sd= sd
  
  
  ### sloppy way of initialization, sorry.
  s= rep(s0,length(t))
  alpha= rep(alpha_0,length(t))
  
  
  
  # recovery rate
  r=r
  
  # delay of recovery
  c=c
  
  # maximum slope of gompertz, a quarter of which is the slaking threshold
  te= c/r 
  
  dt_deg= 0.8
  
  print(c/r)
  # dt_cross= 200
  
  
  # these empty vectors helps us mark the occurence of some events. (defined in loop)
  
  # event of hitting the critically dry moisture
  tc_d=c() 
  
  # event of hitting the critically wet moisture
  tc_w=c()
  
  # event of degradation
  t_deg=c()
  
  
  # event of hitting the critically wet moisture from above
  tc_ww=c() 
  
  # event of hitting the critically dry moisture from below
  tc_dd=c() 
  
  # event of hitting the critically wet moisture from below; including degradation. 
  tc_wd=c() 
  
  # event of hitting the critically dry moisture from above
  tc_dw=c() 
  
  t_cross=c()
  
  del_tc=c()
  
  recent_del_tc=c()
  
  
  
  print(te/4)
  
  
  # x- crossing: t_x: time of threshold crossing
  t_x = data.frame(
    t_c = numeric(),
    event = character(),
    stringsAsFactors = FALSE
  )
  
  
  
  
  
  # Rainfall signal, *NEW equation* DO WE NEED it?
  # precip=function(tp){
  #   
  #   
  #   p= P_t*tp
  #   
  #   
  #   return(p)}
  
  evap_pot= Ep*t_E*h
  
  
  
  # Infiltration capacity, equation 18
  infil_cap= function(y,x)
  {
    
    if(y>1)
    {f=0}
    
    else if (y < sfc)
    {f=fc+(fmax-fc)*exp(-gamma*(y)/(sfc-y))}
    
    
    else{f= fc}
    
    f=f*x
    
    return(f)
    
    infil_cap(sd,1)
    
  }
  infl= function(y,x,t,tp,p) #infl= function(y,x,t,p)
  {
    
    
    f= infil_cap(y,x)
    p=p
    
    inp=min(p,f)
    
    return(inp)
    
  }
  
  
  
  # Evaporation, equation 14; absent during rainfall
  evap=function(s,te){ 
    
    if ( s > sfc) {
      e=Ep}
    else if (s>=0) {
      e=Ep*(s/sfc)}
    else(e=0)
    
    e= e*te*h
    
    return(e)}
  
  
  
  # Drainage, equation 15
  drng= function(s,x){
    
    if (s > sfc )
    {L=(exp(beta*(s-sfc))-1)/(exp(beta*(1-sfc))-1)}
    else 
    {L=0}
    
    Dr= (L*Ksat_s*x*0.1)
    
    return(Dr)
  }
  
  
  
  
  
  
  
  # water balance to be solved, y=s,x=alpha, t=t
  # the reason behind defining auxillary variables are comparisons in if statement.
  ds=function(y,x,t,tp,te,p){
    (infl(y,x,t,tp,p)-drng(y,x)-evap(y,te))/(n*Z)}
  
  # Recovery function for Infiltration Coefficient
  # lag variables serve as memory of the system
  dfI= function(alpha,I_lag,t,t_star) {
    (alpha_a-I_lag)*r*exp(-exp(c-r*(t-t_star))+c-r*(t-t_star)) }
  
  # initialization of functions appearing in the solver
  INFL_Cap=c(infil_cap(s0,alpha_0))
  INFL=c(infl(s0,alpha_0,t0,t_P[1],p[1]))
  DRN=c(drng(s0,alpha_0))
  EVP=rep(evap(s0,t_E[1]))
  P= c(p[1])
  cummP=c(p[1])
  EVP_pot=c(evap_pot[1])
  
  cp_d=0
  cp_w=0
  
  # R starts to count from 1, which is already defined in initialization.
  for (i in 2:length(t)){
    
    
    # explicit euler method to solve s, water balance
    s[i]= s[i-1]+h*ds(s[i-1],alpha[i-1],t[i-1],t_P[i-1],t_E[i-1],p[i-1]) 
    
    # upper and lower tails
    s[i]= ifelse(s[i]>=1,1,s[i])
    s[i]= ifelse(s[i]<=0,0,s[i])
    
    
    # sw and sd are critically dry and wet moisture
    
    # below or above these thresholds no changes occur 
    if (s[i]>sw |s[i]<sd)  
    {alpha[i]=alpha[i-1]}
    
    
    # crossing of the the critically dry water content from below
    if (s[i]>= sd && s[i-1]< sd ) 
    {
      
      # save the time-step every time it happens
      tc_d=c(tc_d,t[i])
      
      # collect every critical time needed to recover // hypothetical variable
      t_cross= c(t_cross,t[i])
      t_x=rbind(t_x,data.frame(t_c=t[i],event="td1"))
      
      # it prints out this sentence: critically wet at "this time-step"
      print(c("critically dry due to wetting at",t[i]))
      
      # event of hitting the critically dry moisture from below
      tc_dd=c(tc_dd,t[i]) 
      
      cp_d= 1 # first requirement of slaking is unlocked
    }
    
    # crossing of the the critically dry water content from above.
    if(s[i]<=sd && s[i-1]>sd)
      
    { tc_d=c(tc_d,t[i])
    t_cross= c(t_cross,t[i])
    print(c("critically dry due to drying at",t[i]))
    
    t_x=rbind(t_x,data.frame(t_c=t[i],event="td2"))
    # event of hitting the critically dry moisture from above
    tc_dw=c(tc_dw,t[i]) 
    cp_d=0
    
    }
    
    # crossing of the critically wet water content from below; slaking possible.
    if (s[i]>=sw && s[i-1]< sw )
    {  
      
      # save the timestep when the crossing happened
      tc_w=c(tc_w,t[i])
      t_cross= c(t_cross,t[i])
      print(c("critically wet at",t[i]))
      
      t_x=rbind(t_x,data.frame(t_c=t[i],event="tw1"))
      # event of hitting the critically wet moisture from below; including degradation. 
      tc_wd=c(tc_wd,t[i]) 
      cp_w=1 # wet control of slaking
      
      
      # potential slaking check - part 1
      
      # check if both thresholds are crossed, just by the size of the steps 
      if (length(tc_w) > 0 && length(tc_d) > 0) { 
        
        # del_tc is the time-lapse between upward critically dry and wet crossings
        # this time-lapse is updated each time the critically wet moisture is crossed  
        
        recent_del_tc= tc_w[length(tc_w)] - tc_d[length(tc_d)]
        
        # all tcs are saved
        del_tc= c(del_tc,recent_del_tc)
        
        # potential slaking check :
        # slaking is only possible either from 1) tc_dd to tc_wd
        # or from 2) tc_dw to tc_
        
        # if the recent time-lapse between threshold crossing is less than 
        # the natural delay of gompertz, degradation occurs.
        # events must belong to a unique dry threshold exceedence.
        # 
        
        if (recent_del_tc < dt_deg && cp_w==1 && cp_d==1 ) {
          
          
          print("slaking occured")
          alpha[i]=alpha_b
          t_deg=c(t_deg,t[i])
          cp_w=0
          cp_d=0
          
          ############ remove this if only one slaking occurs!
          if (length(t_deg) >= 2){
            alpha[i]=alpha[i-1]
          }
        }
        
      }
      
    }
    
    # crossing of the critically wet water content from above
    if (s[i]<=sw && s[i-1]>= sw )
    {
      
      tc_w=c(tc_w,t[i])
      t_cross= c(t_cross,t[i])
      print(c("critically wet due to water loss at",t[i]))
      
      t_x=rbind(t_x,data.frame(t_c=t[i],event="tw2"))
      # event of hitting the critically wet moisture from above
      tc_ww=c(tc_ww,t[i]) 
      cp_w=0
      cp_d=0
    }
    
    
    # Recovery if the water content is maintained positioned in the critical zone
    if (s[i] < sw && s[i]> sd)
    {
      
      # in case we start from moderately wet initial water contents
      if(s[1] > sd && s[1]< sw)
      {
        # the degradation control has to be defined (see below to know why)
        # tc_w[1]=0
        t_star=0
        alpha[i]= alpha[i-1]+ dfI(alpha[i-1],alpha[t==t_star],t[i-1],t_star)*h 
      }
      
      else{
        # a lag time serving as a memory is introduced as the first instance of entering
        # the recovery zone
        t_star=max(tc_d[length(tc_d)],tc_w[length(tc_w)])
        
        # recovery is then instigated, based on the memory of the system
        # alpha[t==lag] also picks off from the alpha as it enters the region...
        # ...useful in cases of abrupt recovery cycles.
        alpha[i]= alpha[i-1]+ dfI(alpha[i-1],alpha[t==t_star],t[i-1],t_star)*h 
        
      }}
    
    
    
    
    DRN[i]= drng(s[i],alpha[i])
    EVP[i]= evap(s[i],t_E[i])
    INFL[i]=infl(s[i],alpha[i],t[i],t_P[i],p[i])
    INFL_Cap[i]= infil_cap(s[i],alpha[i])
  }
  
  P= p
  RNF=P-INFL
  
  
  # rainfall intensity - per time step
  P_I=p[p!=0]
  # rainfall intensity - per time step
  P_i= P_I/(h_rain)
  P_i_avg=mean(P_i)
  P_i_var=var(P_i)
  
  
  EVP_pot= evap_pot
  annums= seq(t0+365,tf,365) #########$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  
  
  #runoff coefficient C= 1-I/P on annual and rainfall event scale
  # breaks on annual scale
  annums= seq(t0+365,tf,365)
  #breaks on event-based scale
  eventum = seq_along(t[i_Pend])
  
  
  P_tot=cumsum(P)
  Ep_tot= cumsum(EVP_pot)
  I_tot= cumsum(INFL)
  
  
  #annual based diffrences
  P_annual = diff(c(0, P_tot[match(annums, t, nomatch = length(t))]))  
  Ep_annual = diff(c(0, Ep_tot[match(annums, t, nomatch = length(t))]))  
  I_annual = diff(c(0, I_tot[match(annums, t, nomatch = length(t))]))  
 
  
  aridity_index= Ep_annual/(P_annual)
  C_annual= 1 - I_annual/P_annual
  AI= mean(aridity_index)
  
  P_annual_avg= mean(P_annual)
  Ep_annual_avg=mean(Ep_annual)
  mean_AI= round(mean(aridity_index),3)
  mean_C=round(mean(C_annual),3)
  
  yearly_aridity = data.frame(Year = annums / 365, Aridity_Index = aridity_index)
  yearly_infiltration_coeff = data.frame(Year = annums / 365, c_annual = C_annual)
  
  #event based scale (in the case of constant rainfall it should be equal)
  
  P_event = diff(P_tot[i_Pend])
  I_event = diff(I_tot[i_Pend])
  
  C_event= c(0,abs(1- I_event/P_event))
  
  Fluxes_df=cbind(DRN,EVP,EVP_pot,P,INFL,INFL_Cap,RNF)
  
  cumFluxes_df=cbind(cumsum(DRN),cumsum(EVP),cumsum(EVP_pot),
                     cumsum(P),cumsum(INFL),cumsum(INFL_Cap),cumsum(RNF))
  colnames(cumFluxes_df)=c("DRN","EVP","EVP_pot","P","INFL","INFL_Cap","RNF")
  
  t_x = rbind(t_x, data.frame(t_c = tf, event = "end"))
  t_x_test= t_x
  t_x= t_x_test
  
  ## Event classification to check solver's integrity
  
  if (length(t_x$t_c)>1){ 
    t_x=cbind(t_x,IET=c(t_x[1,1],diff(t_x$t_c)))
    
    
    t_x= cbind(t_x,state=as.character(NA))
    t_x= cbind(t_x,state_details=as.character(NA))
    t_x$state[1]="fpt" 
    t_x$state_details[1]="fpt"
    if(s0 < sd ){
      t_x$state[1]="stable"  
      t_x$state_details[1]="dry stability"
    }
    else if(s0 > sw ){
      t_x$state[1]="stable"   
      t_x$state_details[1]="wet stability"
    }
    else{
      t_x$state[1]="recovery"   
      t_x$state_details[1]="drying recovery"  
    }
    
    
    for (i in 2:length(t_x$t_c)) {
      
      
      if (t_x$event[i-1]=="td1" & t_x$event[i]=="tw1")     #2,3
      {
        t_x$state[i]= ifelse(t_x$IET[i]<=dt_deg,"degradation","recovery")
        t_x$state_details[i]= ifelse(t_x$state[i] == "degradation","degradation","wetting recovery")
      }
      
      if (t_x$event[i-1]=="tw2" & t_x$event[i]=="td2") #4
      {t_x$state[i]="recovery"
      t_x$state_details[i]="drying recovery"
      }
      
      if (t_x$event[i-1]=="td1" & t_x$event[i]=="td2") #5
      {t_x$state[i]="recovery"
      t_x$state_details[i]="wet-dry recovery"
      }
      
      if (t_x$event[i-1]=="td2" & t_x$event[i]=="td1") #6
      {t_x$state[i]="stable"
      t_x$state_details[i]="dry stability"
      }
      
      if (t_x$event[i-1]=="tw2" & t_x$event[i]=="tw1") #7
      {t_x$state[i]="recovery"
      t_x$state_details[i]="dry-wet recovery"
      }
      
      if (t_x$event[i-1]=="tw1" & t_x$event[i]=="tw2") #8
      {t_x$state[i]="stable"
      t_x$state_details[i]="wet stability"
      }
      
      if (t_x$event[i]=="end") #8
      {
        if (t_x$event[i-1]=="td1")     #2,3
        {
          t_x$state[i]= ifelse(t_x$IET[i]<=dt_deg,"degradation","recovery")
          t_x$state_details[i]= ifelse(t_x$state[i] == "degradation","degradation","wetting recovery")
        }
        
        if (t_x$event[i-1]=="tw2") #4
        {t_x$state[i]="recovery"
        t_x$state_details[i]="drying recovery"
        }
        
        if (t_x$event[i-1]=="td1") #5
        {t_x$state[i]="recovery"
        t_x$state_details[i]="wet-dry recovery"
        }
        
        if (t_x$event[i-1]=="td2") #6
        {t_x$state[i]="stable"
        t_x$state_details[i]="dry stability"
        }
        
        if (t_x$event[i-1]=="tw2") #7
        {t_x$state[i]="recovery"
        t_x$state_details[i]="dry-wet recovery"
        }
        
        if (t_x$event[i-1]=="tw1") #8
        {t_x$state[i]="stable"
        t_x$state_details[i]="wet stability"
        }   
        
      }
      
    }
    
  }else{
    t_x=data.frame(t_c = NA, event = "none")
    t_x= cbind(t_x,state="none",state_details="none",IET=tf-t0)
    
  }
  
  t_x$State = ifelse(t_x$state == "recovery", "Recovery",
                      ifelse(t_x$state_details == "dry stability", "Dry",
                             ifelse(t_x$state_details == "wet stability" | t_x$state_details == "degradation", "Wet", NA)))
  
  
  t_x$State = factor(t_x$State, levels = c("Recovery", "Dry", "Wet"))
  
  
  start_times <- t_x$t_c
  end_times <- c(t_x$t_c[-1], max(t_x$t_c) + 1)  # Add dummy 1 unit to extend final state
  
  state_periods <- data.frame(
    State = t_x$State,
    start = start_times,
    end = end_times
  )
  
  
  
  # Save results
  file_name = paste0(case_name,".Rdata")
  save_path = "~/DDSA/sims"
  file_path = file.path(save_path, file_name)
  save.image(file_path)
  
  Sys.sleep(3)  # Avoid duplicate timestamps
}

# By Arian Monhasser
