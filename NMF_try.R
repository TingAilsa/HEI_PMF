#### install package "Biobase" ####
# Biobase is necessary for NMF package, but not able to be installed directly
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("Biobase")

##packages in need
library(Biobase)
library(NMF)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggsci)
# library(cowplot) # could be used to combine multiple ggplot

#### Data preparation ####
# Make sure the dataset only contains non-negative values  
# remove any missing values or outliers
conc_try = read.csv("/Users/TingZhang/Documents/EPA PMF/Data/CSN_Cluster6_site3_2011-14_PMF_conc_EPAmdl.csv")
conc_try$P = conc_try$S = conc_try$PM25 = NULL
conc_try$Date = as.Date(conc_try$Date, format = "%m/%d/%y")
conc.col = ncol(conc_try)

# Normalize the data
conc_norm = conc_try
conc_norm[, 2:conc.col] <- conc_norm[, 2:conc.col]/
  apply(conc_norm[, 2:conc.col], 2, max)

# prepare the matrix used for NMF
conc_nmf = as.matrix(conc_norm[, 2:conc.col])

#### NMF analysis - lee method ####
# number of factors (sources)
k <- 9
# define the NMF method
lee.algo <- 'lee' # Lee-Seung algorithm
# nmfAlgorithm()

# Perform NMF with lee
lee_result <- nmf(conc_nmf, k, lee.algo)
fit(lee_result)
summary(lee_result)
# compute the scores
score_lee = featureScore(lee_result) 
summary(score_lee)

# Extract source profiles and source contributions
lee_Profil <- data.frame(basis(lee_result))
lee_Contri <- data.frame(coef(lee_result))

# estimate the percent contribution
Contri_mx = as.matrix(coef(lee_result))
# dividing columns by rowSums
# sweep, return an array by sweeping out a summary statistic.
lee_Contri_per = data.frame(
  sweep(
    Contri_mx, 2, 
    rowSums(Contri_mx), 
    `/`))

# Assign date and Facrtor Num to the results
lee_Profil$Date = conc_norm$Date
lee_Contri$Factor = paste0("Factor", 1:9)
lee_Contri_per$Factor = paste0("Factor", 1:9)

# change olumn names & remove Date or Factor seriel to the first column
colnames(lee_Profil)[1:9] = paste0("Factor", 1:9)
lee_Profil = lee_Profil %>% 
  relocate(Date, 
           .before = Factor1)

lee_Contri = lee_Contri %>% 
  relocate(Factor, 
           .before = Al)

lee_Contri_per = lee_Contri_per %>% 
  relocate(Factor, 
           .before = Al)

#### NMF analysis - ns method #### 
# Nonsmooth NMF method, nsNMF
# number of factors (sources)
k <- 9
# define the NMF method
ns.algo <- 'ns' # ns-Seung algorithm
# nmfAlgorithm()

# Perform NMF with ns
ns_result <- nmf(conc_nmf, k, ns.algo, theta=0.7)
fit(ns_result)
summary(ns_result)
# compute the scores
score_ns = featureScore(ns_result) 
summary(score_ns)

# Calculate the RMSE between the original data and the reconstructed data
RMSE <- sqrt(mean((conc_nmf - fitted(ns_result))^2))
cat("RMSE:", RMSE)

# Extract source profiles and source contributions
ns_Profil <- data.frame(basis(ns_result))
ns_Contri <- data.frame(coef(ns_result))

# estimate the percent contribution
Contri_mx = as.matrix(coef(ns_result))
# dividing columns by rowSums
# sweep, return an array by sweeping out a summary statistic.
ns_Contri_per = data.frame(
  sweep(
    Contri_mx, 2, 
    rowSums(Contri_mx), 
    `/`))

# Assign date and Facrtor Num to the results
ns_Profil$Date = conc_norm$Date
ns_Contri$Factor = paste0("Factor", 1:9)
ns_Contri_per$Factor = paste0("Factor", 1:9)

# change olumn names & remove Date or Factor seriel to the first column
colnames(ns_Profil)[1:9] = paste0("Factor", 1:9)
ns_Profil = ns_Profil %>% 
  relocate(Date, 
           .before = Factor1)

ns_Contri = ns_Contri %>% 
  relocate(Factor, 
           .before = Al)

ns_Contri_per = ns_Contri_per %>% 
  relocate(Factor, 
           .before = Al)

#### visualization ####
library(patchwork)

# define the theme
theme.1 = theme(axis.title.y.right = element_blank(),
                panel.spacing = unit(10, "mm"),   
                legend.background = element_blank(),
                strip.text = element_text(face="bold", size=rel(1.5)),
                legend.position = 'none',
                strip.background = element_rect(fill="lightblue", colour="grey", size=16),
                axis.title.x = element_text(color="grey25", size = 24, vjust=-2, margin=margin(0,0,0,300)), 
                axis.title.y = element_text(color="grey25", size = 24, vjust=2, margin=margin(0,2,0,0)),
                plot.title=element_text(size=rel(2)), 
                axis.text.x = element_text(color="grey25", size = 20, angle = 0, hjust = 0.5, vjust = 0.3), plot.margin = unit(c(2,1,2, 2), "lines"),
                axis.text.y = element_text(color="grey25", size = 20, angle = 0, hjust = 0.5))

#### visualization - lee ####

# prepare dataset for plotting
Profil_plot = lee_Profil %>% gather(Factor, profile, -Date)
Contri_plot = lee_Contri %>% gather(Species, contribution, -Factor)
percent_contri = lee_Contri_per %>% gather(Species, percent, -Factor)

source_profile_plot = ggplot(Profil_plot, aes(Date, profile, color = Factor)) +
  geom_line() +
  scale_color_npg() + 
  theme_bw() +
  theme.1

source_profile_zoom = ggplot(Profil_plot, aes(Date, profile, color = Factor)) +
  geom_line() +
  ylim(0, 0.1) +
  scale_color_npg() + 
  theme_bw() +
  theme.1

source_profile_plot + source_profile_zoom + plot_layout(ncol = 1, widths = c(1, 1))

ggplot() +
  geom_point(data = Contri_plot, aes(x = Species, y = contribution), color = "red", size = 2) +
  geom_bar(data = percent_contri, aes(x = Species, y = percent), 
           fill = "lightblue", stat = "identity", position = "dodge") +
  facet_wrap( ~ Factor, scales = 'free') +
  labs(x = 'contribution', y = 'percent') +
  theme_bw() +
  theme(legend.position = 'none') +
  theme.1


# Create the point plot using data1
ggplot(data = Contri_plot, aes(x = Species, y = contribution)) +
  geom_bar(fill = "lightblue", stat = 'identity', position = 'dodge') +
  labs(x = '', y = 'contribution') +
  facet_wrap( ~ Factor, scales = "free_y", ncol = 1) +
  theme_bw() 

ggplot() +
  geom_bar(data = Contri_plot, aes(x = Species, y = contribution),
           fill = "lightblue", stat = 'identity', position = 'dodge') +
  labs(x = '', y = 'conc of species') +
  facet_wrap(Factor ~ ., scales = "free_y", ncol = 1) +
  scale_y_continuous(labels = function(x) format(x, scientific = T)) +
  theme_bw() 

# Create the bar plot using data2
point_plot <- ggplot(data = percent_contri, aes(x = Species, y = percent)) +
  geom_point(color = 'red', size = 2) +
  labs(x = 'Species', y = 'percent') +
  theme_bw() +
  theme(legend.position = 'none')

