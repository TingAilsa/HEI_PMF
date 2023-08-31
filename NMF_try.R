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

#### Data preparation 1, only concentration ####
# Make sure the dataset only contains non-negative values  
# remove any missing values or outliers
conc_try = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_PMF_files_Ting/CSN_PMF_Try_data/EPA MDL/CSN_Cluster6_site3_2011-14_PMF_conc_EPAmdl.csv")
conc_try$X = conc_try$SiteCode = conc_try$P = conc_try$S = conc_try$PM25 = NULL
conc_try$Date = as.Date(conc_try$Date)
conc.col = ncol(conc_try)
head(conc_try)

# Normalize the data
conc_norm = conc_try
conc_norm[, 2:conc.col] <- conc_norm[, 2:conc.col]/
  apply(conc_norm[, 2:conc.col], 2, max)

# prepare the matrix used for NMF
conc_nmf = as.matrix(conc_norm[, 2:conc.col])

#### Data preparation 2, concentration & uncertainty ####
conc_site = read.csv("/Users/TingZhang/Documents/EPA PMF/Data/CSN_C6_S60371103_2011-20_conc.csv")
unc_site = read.csv("/Users/TingZhang/Documents/EPA PMF/Data/CSN_C6_S60371103_2011-20_unc.csv")

# check distribution of uncertainty
hist(unc_site$Al)
hist(unc_site$Mn)
hist(unc_site$OC)
hist(unc_site$EC)
hist(unc_site$SO4)
hist(unc_site$NO3)

############## 1. normal distribution
# assume the uncertainties are normally distributed and that the measurements are independent
# Compute weights as reciprocal of variance
weight_NM = 1 / (unc_site ^ 2)

# function to get the max of each column
colMax <- function(data) sapply(data, max, na.rm = TRUE)

# rescale the weights to range between 0-1
weight_NM = purrr::map2_df(weight_NM, colMax(weight_NM), `/`)

# check distribution of weight
hist(weight_NM$Al)
hist(weight_NM$Mn)
hist(weight_NM$OC)
hist(weight_NM$EC)
hist(weight_NM$SO4)
hist(weight_NM$NO3)

############## 2. skewed distribution
# Apply log transformation to reduce skewness
unc_log = log(unc_site)

# Compute weights as reciprocal of squared transformed uncertainties
weight_SK = 1 / (unc_log ^ 2)

# Scale the weights to range between 0 and 1
# weight_SK = Map(`/`, weight_SK, colMax(weight_SK))
weight_SK = purrr::map2_df(weight_SK, colMax(weight_SK), `/`)

# check distribution of weight
hist(weight_SK$Al)
hist(weight_SK$Mn)
hist(weight_SK$OC)
hist(weight_SK$EC)
hist(weight_SK$SO4)
hist(weight_SK$NO3)

conc_nmf = weight_SK * conc_site

# Normalize the data
conc_nmf <- purrr::map2_df(conc_nmf, colMax(conc_nmf), `/`)
conc_nmf = sapply(conc_nmf, as.numeric)

# prepare the matrix used for NMF
conc_nmf = as.matrix(conc_nmf)

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
#           .before = Al)

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



##### tutorial - GNMF #####
# Install and load the 'NMF' package
# install.packages("NMF")
# Install and load the 'NMF' package
library(NMF)

# Generate some random non-negative data
set.seed(123)
V <- abs(rnorm(100*20))
V <- matrix(V, nrow=100)

# Specify the rank of the factorization
rank <- 5

# Define the divergence measure for GNMF
# Here, we use the Kullback-Leibler divergence as an example
divergence <- function(x, y) sum(x * log(x / y) - x + y)

# Define the update rules for GNMF
# Here, we use the multiplicative update rules derived for the Kullback-Leibler divergence
updateW <- function(W, H, V) W * ((V / (W %*% H + .Machine$double.eps)) %*% t(H)) / (matrix(1, nrow=nrow(W), ncol=ncol(W)) %*% t(H))
updateH <- function(W, H, V) H * (t(W) %*% (V / (W %*% H + .Machine$double.eps))) / (t(W) %*% matrix(1, nrow=nrow(W), ncol=ncol(W)))

# Define the update rules for GNMF
updateW <- function(W, H, V) {
  W * ((V / (W %*% H + .Machine$double.eps)) %*% t(H)) / rep(1, each=nrow(W)) %*% t(H)
}

updateH <- function(W, H, V) {
  H * (t(W) %*% (V / (W %*% H + .Machine$double.eps))) %*% V
}

# Initialize the factor matrices
W <- abs(rnorm(nrow(V)*rank))
W <- matrix(W, nrow=nrow(V))
H <- abs(rnorm(rank*ncol(V)))
H <- matrix(H, nrow=rank)

# Perform the GNMF algorithm
for (i in 1:1000) {
  W <- updateW(W, H, V)
  H <- updateH(W, H, V)
  
  if (i %% 100 == 0) {
    # ‘%%’ indicates ‘x mod y’, the reminder
    # running a loop & print progress indicator to screen every nth iteration
    # if (i %% 10 == 0) { #do something} to do something every 10th iteration
    print(paste("Iteration", i, "- Divergence:", divergence(V, W %*% H)))
  }
}

# Print the factor matrices
print(W)
print(H)




##### tutorial - Weighted NMF #####

# Install and load the 'nnlm' package
# install.packages("nnlm")

library(devtools)
install_github('linxihui/NNLM')

library(nnlm)


# Generate some random non-negative data
set.seed(123)
V <- abs(rnorm(100 * 20))
V <- matrix(V, nrow = 100)
dim(V)
head(V)


# Define the rank of the factorization
rank <- 5

# Generate weights (e.g., based on measurement uncertainty)
W <- runif(length(V))
W <- matrix(W, nrow = nrow(V))
dim(W)
head(W)

#################### try with the nnlm from linxihui, but not work
# Perform Weighted NMF
nmf_result <- nnlm(V, k = rank, method = "scd", loss = "mkl", W = W, n.threads = 1)

# Get the factor matrices
W_matrix <- coef(nmf_result, "coef")
H_matrix <- coef(nmf_result, "basis")

# Print the factor matrices
print(W_matrix)
print(H_matrix)
#################### try with the nnlm from linxihui, but not work


# Define a custom NMF algorithm that incorporates weights
custom_algorithm <- nmfAlgorithm("lee", .options="w")

# Create a list that contains your weight matrix
# The list names should match the names of the parameters in the update rules
wlist <- list(w = W)

# Run weighted NMF
k = 2
nmf_pseudo <- nmf(V, k, custom_algorithm, .pargs = wlist)

# compute the scores
score_nmf_psd = featureScore(nmf_pseudo) 
summary(score_nmf_psd)

# Calculate the RMSE between the original data and the reconstructed data
RMSE <- sqrt(mean((conc_nmf - fitted(nmf_pseudo))^2))
cat("RMSE:", RMSE)

# Extract source profiles and source contributions
nmf_psd_Profil <- data.frame(basis(nmf_pseudo))
nmf_psd_Contri <- data.frame(coef(nmf_pseudo))

# estimate the percent contribution
Contri_mx = as.matrix(coef(nmf_pseudo))
# dividing columns by rowSums
# sweep, return an array by sweeping out a summary statistic.
nmf_psd_Contri_per = data.frame(
  sweep(
    Contri_mx, 2, 
    rowSums(Contri_mx), 
    `/`))

# Assign date and Facrtor Num to the results
nmf_psd_Profil$Date = conc_norm$Date
nmf_psd_Contri$Factor = paste0("Factor", 1:9)
nmf_psd_Contri_per$Factor = paste0("Factor", 1:9)

# change olumn names & remove Date or Factor seriel to the first column
colnames(nmf_psd_Profil)[1:9] = paste0("Factor", 1:9)
nmf_psd_Profil = nmf_psd_Profil %>% 
  relocate(Date, 
           .before = Factor1)

nmf_psd_Contri = nmf_psd_Contri %>% 
  relocate(Factor, 
           .before = Al)

nmf_psd_Contri_per = nmf_psd_Contri_per %>% 
  relocate(Factor, 
           .before = Al)

##### tutorial - Convolutive NMF #####

# Install and load the 'chNMF' package
install.packages("chNMF")
library(chNMF)

# Generate some random non-negative data
set.seed(123)
V <- abs(rnorm(100 * 20))
V <- matrix(V, nrow = 100)

# Define the rank and window size for the convolutive NMF
rank <- 5
window_size <- 3

# Perform Convolutive NMF
convnmf_result <- chNMF(V, K = rank, L = window_size)

# Get the factor matrices and convolutional weights
W_matrix <- convnmf_result$W
H_matrix <- convnmf_result$H
A_matrix <- convnmf_result$A

# Print the factor matrices and convolutional weights
print(W_matrix)
print(H_matrix)
print(A_matrix)






