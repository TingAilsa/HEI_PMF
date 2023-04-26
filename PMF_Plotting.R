##clear environment
# rm(list=ls())

##set working directory
setwd("/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results")
getwd()
data.dir <- "/Users/TingZhang/Documents/HEI HAQ PMF/PMF_Results"

##packages in need
library(tidyr) # separate{tidyr}, gather{tidyr}, spread{tidyr},  spread is VIP function, str_split_fixed{stringr} is better than separate
library(dplyr)
library(stats) # aggregate{stats}, VIP function
library(scales) # percent{}
library(ggrepel)
library(ggsci)
library(ggpattern)
library(ggplot2)

# read species-class dataset
species_class = read.csv("/Users/TingZhang/Documents/HEI HAQ PMF/IMPROVE & CNS original/CSN_Species_class.csv")
species_class$X = NULL

#### Factor - concentration contributions ####
# read dataset of concentration contributions for each factor
site_conc = read.csv("C6S3F8_base_Concentration.csv")
colnames(site_conc)[1] = "Species"

# replace species names
site_conc$Species[site_conc$Species == "K."] = "K+"
site_conc$Species[site_conc$Species == "Na."] = "Na+"
site_conc$Species[site_conc$Species == "NH4."] = "NH4+"
site_conc$Species[site_conc$Species == "NO3"] = "NO3-"
site_conc$Species[site_conc$Species == "SO4"] = "SO4(2-)"

# gather the data for plotting
site_conc = gather(site_conc, 
                   "Factor", 
                   "Concentration", 
                   -Species)

# match species class
site_conc = merge(site_conc, 
                  species_class, 
                  all.x = T)

# site_conc$data = "Concentration"

#### Factor - Percent contributions ####
# read dataset of percent contributions for each factor
site_perc = read.csv("C6S3F8_base_Percent.csv")
colnames(site_perc)[1] = "Species"

# replace species names
site_perc$Species[site_perc$Species == "K."] = "K+"
site_perc$Species[site_perc$Species == "Na."] = "Na+"
site_perc$Species[site_perc$Species == "NH4."] = "NH4+"
site_perc$Species[site_perc$Species == "NO3"] = "NO3-"
site_perc$Species[site_perc$Species == "SO4"] = "SO4(2-)"

# gather the data for plotting
site_perc = gather(site_perc, 
                   "Factor", 
                   "Percent", 
                   -Species)

# match species class
site_perc = merge(site_perc, 
                  species_class, 
                  all.x = T)

# site_perc$data = "Percent"

#### Factor - time-series ####
# read dataset of concentration contributions for each factor
site_ts = read.csv("C6S3F8_base_TimeSeries.csv")
colnames(site_ts)[1] = "Date"
site_ts$Date = as.Date(site_ts$Date, format = "%m/%d/%y")

# match with PM2.5 concentration
site_ts_PM = site_ts
site_PM = read.csv("/Users/TingZhang/Library/CloudStorage/Dropbox/HEI_PMF_files_Ting/CSN_PMF_Try_data/Top_PM_cluster&site/CSN_C6_S3_2011-14_conc.csv")
site_ts_PM$PM2.5 = site_PM$PM25
site_ts_PM$Date = NULL

# gather the data for plotting
site_ts = gather(site_ts, 
                 "Factor", 
                 "Contribution", 
                 -Date)
summary(site_ts)

# factor contribution - linear
site_ts_PM_lm = lm(PM2.5 ~ ., data = site_ts_PM)
site_ts_PM_lm_beta = summary(site_ts_PM_lm)$coefficients[, 1]
site_ts_PM_lm_beta = data.frame(site_ts_PM_lm_beta[2:length(site_ts_PM_lm_beta)])
colnames(site_ts_PM_lm_beta) = "lm.beta.site-SiteBased"
site_ts_PM_lm_beta$Factor = rownames(site_ts_PM_lm_beta)
site_ts_PM_lm_beta$Factor.contribution = (site_ts_PM_lm_beta$lm.beta.site/
                                                   sum(site_ts_PM_lm_beta$lm.beta.site))*100

#### plotting - Concentration & percent contribution ####

site_conc_perc = merge(site_conc, site_perc)
sapply(site_conc_perc, class)
head(site_conc_perc)
site_conc_perc = subset(site_conc_perc,
                        Species != "PM25")

site_conc_perc$value.fake = 0

ggplot(site_conc_perc, 
       aes(x = reorder(Species, sequence), 
           group = Factor)) +
  facet_grid(Factor ~.) +
  # geom_bar( aes(y = value.fake), stat="identity")+
  geom_point( aes(y = Percent), color = "black", shape = 15) +
  scale_y_continuous(
    position = "right",
    name = "% of Species"
    # Features of the first axis
    #name = "Concentration of Species",
    # Add a second axis and specify its features
    #sec.axis = sec_axis(~., name="% of Species") # second y-axis, dual y-axis
    ) +
  theme_bw() +
  theme(panel.grid = element_line(colour = "white"),
        plot.title = element_text(hjust = 0.05, vjust = -25, size = 16),
        strip.background = element_blank(), strip.text = element_blank(),
        axis.title.x = element_text(color="grey25", size = 12, vjust=0, margin=margin(0,0,0,300)), 
        axis.title.y = element_text(color="grey25", size = 12, vjust=1, margin=margin(0,2,0,0)),
        axis.text.x = element_text(color="grey25", size = 11, angle = 90, hjust = 0, vjust = 0.3), plot.margin = unit(c(2,1,2, 2), "lines"),
        axis.text.y = element_text(color="grey25", size = 11, angle = 0, hjust = 0.5))

ggplot(site_conc_perc, 
       aes(x = reorder(Species, sequence), 
           group = Factor)) +
  facet_grid(Factor ~.) +
  geom_point( aes(y = Percent), color = "black", shape = 15) +
  scale_y_continuous(
    position = "right",
    name = "% of Species"
  ) +
  theme_classic() +
  theme(panel.grid = element_line(colour = "white"),
        plot.title = element_text(hjust = 0.05, vjust = -25, size = 16),
        strip.background = element_blank(), strip.text = element_blank(),
        axis.line.x.bottom=element_line(color=NA),
        axis.title.x = element_text(color="grey25", size = 12, vjust=0, margin=margin(0,0,0,300)), 
        axis.title.y = element_text(color="grey25", size = 12, vjust=1, margin=margin(0,2,0,0)),
        axis.text.x = element_text(color="grey25", size = 11, angle = 90, hjust = 0, vjust = 0.3), plot.margin = unit(c(2,1,2, 2), "lines"),
        axis.text.y = element_text(color="grey25", size = 11, angle = 0, hjust = 0.5))

# for bar start from a value less than 0
# https://stackoverflow.com/questions/22295253/force-bars-to-start-from-a-lower-value-than-0-in-ggplot-geom-bar-in-r
require(scales)
mylog_trans <- function(base=exp(1), from=0) {
  trans <- function(x) log(x, base)-from
  inv <- function(x) base^(x+from)
  trans_new("mylog", trans, inv, log_breaks(base=base), 
            domain = c(base^from, Inf))
}

ggplot(site_conc_perc, 
       aes(x = reorder(Species, sequence), 
           group = Factor, fill = Factor)) +
  facet_grid(Factor ~.) +
  geom_bar( aes(y = Concentration), 
            stat="identity", width = 0.6)+
  scale_y_log10() + 
  scale_y_continuous(trans = mylog_trans(base=10, from=-5)) +
  scale_fill_nejm() +
  theme_bw() +
  theme(panel.grid = element_line(colour = "white"),
        plot.title = element_text(hjust = 0.05, vjust = -25, size = 16),
        strip.background = element_blank(), strip.text = element_blank(),
        axis.title.x = element_text(color="grey25", size = 12, vjust=0, margin=margin(0,0,0,300)), 
        axis.title.y = element_text(color="grey25", size = 12, vjust=1, margin=margin(0,2,0,0)),
        axis.text.x = element_text(color="grey25", size = 11, angle = 90, hjust = 0, vjust = 0.3), plot.margin = unit(c(2,1,2, 2), "lines"),
        axis.text.y = element_text(color="grey25", size = 11, angle = 0, hjust = 0.5))


ggplot(site_conc_perc, 
       aes(x = reorder(Species, sequence), 
           group = Factor, fill = Factor)) +
  facet_grid(Factor ~.) +
  geom_bar( aes(y = Concentration), 
            stat="identity", width = 0.6)+
  geom_point( aes(y = value.fake), color = NA) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))


#### plotting - time series ####

ggplot(site_ts, 
       aes(x = Date, y = Contribution, 
           group = Factor, color = Factor)) +
  facet_grid(Factor ~.) +
  geom_line(alpha = 0.8)+
  geom_point(size = 0.5) +
  scale_color_nejm() +
  theme_bw() +
  theme(panel.grid = element_line(colour = "white"),
        plot.title = element_text(hjust = 0.05, vjust = -25, size = 16),
        strip.background = element_blank(), strip.text = element_blank(),
        axis.title.x = element_text(color="grey25", size = 12, vjust=0, margin=margin(0,0,0,300)), 
        axis.title.y = element_text(color="grey25", size = 12, vjust=1, margin=margin(0,2,0,0)),
        plot.margin = unit(c(2,1,2, 2), "lines"),
        axis.text.x = element_text(color="grey25", size = 11, angle = 0, hjust = 0.5, vjust = 0), 
        axis.text.y = element_text(color="grey25", size = 11, angle = 0, hjust = 0.5))

#### plotting - factor percent contribution to PM2.5 ####
# stripe, https://coolbutuseless.github.io/package/ggpattern/articles/pattern-stripe.html

library(ggpattern)

ggplot(site_ts_PM_lm_beta, 
       aes(x = Factor, y = Factor.contribution)) +
 # geom_bar(stat="identity", width = 0.2) +    
  geom_col_pattern(aes(pattern_fill = Factor),  # fill = Factor
                   width = 0.2, fill = "white",
                   pattern = 'stripe', pattern_density = 0.5) +
  scale_fill_nejm() +
  scale_y_continuous(position = "right") +
  theme_bw() +
  theme(panel.grid.major.x = element_line(colour="grey60", linetype="dashed"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.x = element_text(color="grey25", size = 11, angle = 90, hjust = 0.5, vjust = 0), 
        axis.text.y = element_text(color="grey25", size = 11, angle = 90, hjust = 0.5))



ggplot(site_ts_PM_lm_beta, 
       aes(x = Factor, y = Factor.contribution, fill = Factor)) +
  geom_bar(stat="identity", width = 0.2, pattern_fill = "slashes") +    
  scale_fill_nejm() +
  scale_y_continuous(position = "right") +
  theme_bw() +
  theme(panel.grid.major.x = element_line(colour="grey60", linetype="dashed"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.x = element_text(color="grey25", size = 11, angle = 90, hjust = 0.5, vjust = 0), 
        axis.text.y = element_text(color="grey25", size = 11, angle = 90, hjust = 0.5))


