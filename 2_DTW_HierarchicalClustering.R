library(readxl)
library(dtwclust)
library(ggplot2)


# Store the names of all your *.Rdata simulations in that_excel
that_excel_file=read_excel("~/DDSA/sims/that_excel.xlsx",col_names = FALSE)
colnames(that_excel_file)="name"

E_coeffs=c()
meta_info=c()

for (i in 1:length(that_excel_file$name)){
  
  
  A=that_excel_file$name[i]
  
  
  
  path = "~/DDSA/sims"
  full_path = file.path(path, A)
  
  load(full_path)
  
  Y= case_name
  deg_count= length(t_deg)
  
  
  
  EVP_tot= cumsum(EVP)
  EVAP_annual= diff(c(0, EVP_tot[match(annums, t, nomatch = length(t))]))
  RNF_tot= cumsum(RNF)
  RNF_annual= diff(c(0, RNF_tot[match(annums, t, nomatch = length(t))]))

  E_coeffs=rbind(E_coeffs,round(RNF_annual/EVAP_annual,2))
  
  
  
  meta_info = rbind(meta_info, data.frame(
    name = paste0(file_name),
    r = r,
    c = c,
    sd = sd,
    sw = sw,
    EP_mean= mean(Ep),
    D_mean= mean(Del),
    ai=AI,
    IAT_meanline= mean(IAT),
    IAT_midline= IAT_mid,
    IAT_maxline= IAT_mid+IAT_amp,
    IAT_minline= IAT_mid-IAT_amp,
    P_midline= P_mid,
    P_maxline= P_mid+P_amp,
    Ep_midline=Ep_mid,
    Ep_maxline=Ep_mid+Ep_amp
    
  ))
  
  
}



X0= E_coeffs
# write.csv(X0, file = "X0.csv", row.names = FALSE)
# write.csv(meta_info,file = "Y448.csv",row.names = TRUE)




## Running for different amounts clusters

results = list()
cvi_scores = data.frame(k = integer(),
                         Silhouette = numeric(),
                         Davies_Bouldin = numeric(),
                         Dunn = numeric(),
                         stringsAsFactors = FALSE)

for (k in 2:5) {
  clust = tsclust(X0,
                  type = "hierarchical",   
                  k = k,
                  distance = "dtw",
                  control = hierarchical_control(method = "average"))
  
  results[[as.character(k)]] = clust
  
  idx = cvi(clust)
  cvi_scores = rbind(cvi_scores,
                     data.frame(k = k,
                                Silhouette = idx["Sil"],
                                `Davies-Bouldin` = idx["DB"],
                                Dunn = idx["D"],
                                `Calinski-Harabasz` = idx["CH"],
                                COP = idx["COP"]
                     ))
  
  meta_info[[paste0("dtw_", k)]] = clust@cluster
}


clust <- tsclust(
  X0,
  type = "hierarchical",
  k = 3,
  distance = "dtw",
  control = hierarchical_control(method = "average")
)


clust448@centroids

clust448=clust 



str(clust)


saveRDS(clust448,
        file = "seed1clust.rds")


read_rds( file = "seed1clust.rds")
# grab the dendrogram
dend <- as.dendrogram(clust)
plot(dend)

# plot it
plot(hc, main = "DTW Hierarchical Clustering Dendrogram (k = 3)")



#check meta_info columns


cvi_long = tidyr::pivot_longer(cvi_scores,
                         cols = -k,
                         names_to = "Index",
                         values_to = "Score")

ggplot(cvi_long, aes(x = k, y = Score, color = Index)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~ Index, scales = "free_y") +
  theme_bw(base_size=20) +
  labs(x = "Number of clusters (k)",
       y = "Validity Score")+
  theme(legend.position = "none")

## messy stuff
E_coeffs_df = as.data.frame(E_coeffs)
colnames(E_coeffs_df)= annums/365

E_coeffs_df = cbind(meta_info, E_coeffs_df)

long_df = tidyr::pivot_longer(
  E_coeffs_df,
  cols = matches("^[0-9]+$"),  # selects year-labeled columns
  names_to = "Year",
  values_to = "E_coeff"
)
class(long_df$Year)


Output = long_df %>%
  distinct(name, .keep_all = TRUE)
Output$case=c()


# This part is done to name my simulation according to the works of McGrath and Hipsey
for (i in 1:nrow(Output)) {
  num = as.numeric(sub("\\.Rdata$", "", Output$name[i]))
  if (num >= 2000 & num < 3000) {
    Output$case[i] = "a"
  } else if (num >= 3000 & num < 4000) {
    Output$case[i] = "b"
  } else if (num >= 4000 & num < 5000) {
    Output$case[i] = "c"
  } else if (num >= 5000 & num < 6000) {
    Output$case[i] = "d"
  } else if (num >= 6000 & num < 7000) {
    Output$case[i] = "e"
  } else if (num >= 1000 & num < 2000) {
    Output$case[i] = "f"
  } else if (num >= 7000 & num < 8000) {
    Output$case[i] = "g"
  } else if (num >= 100 & num < 1000) {
    Output$case[i] = "h"
  } else {
    Output$case[i] = NA
  }
}


# 

write.csv(Output,file = "Output.csv",row.names = TRUE)

write.csv(E_coeffs_df,file = "E_coeffs_df.csv",row.names = TRUE)



Output=read.csv("~/Output.csv")

Y448= read.csv("~/Y448.csv")

# read.csv("Output.csv")
####


write.csv(long_df,file = "long_df.csv",row.names = TRUE)


#####################################################

long_df=read.csv("long_df.csv")


median_df = long_df %>%
  group_by(dtw_3, Year) %>%
  summarise(E_coeff = median(E_coeff, na.rm = TRUE),
            .groups = "drop")

write.csv(median_df,file = "448clustermedian.csv",row.names = TRUE)

# seed1clusts=read_rds( file = "seed1clust.rds")
protos=seed1clusts@centroids
proto_df <- lapply(seq_along(protos), function(k) {
  data.frame(
    Year = seq_along(protos[[k]]),
    E_coeff = as.numeric(protos[[k]]),
    dtw_3 = k
  )
}) |> bind_rows()

regime_clusters =
  ggplot() +
  geom_line(data = long_df,
            aes(x = Year, y = E_coeff, group = name, col = name),
            linewidth = 0.5, alpha = 0.05) +
  # geom_line(data = median_df,
            # aes(x = Year, y = E_coeff),
            # linewidth = 1.5, color = "black") +
  geom_line(data = proto_df,
            aes(x = Year, y = E_coeff),
            linewidth = 1, color = "black",linetype="dashed") +
  facet_wrap(~ dtw_3, nrow = 1) +
  theme_bw(base_size = 20) +
  labs(x = "Year", y = "Runoff/Evaporation", fill = "Cluster") +
  theme(legend.position = "side")
regime_clusters












