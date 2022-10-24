library("tidyverse")
library("readxl")

comp_conc = read_excel("component concentration.xlsx")
comp_conc_1 = comp_conc
comp_conc_1$Date = NULL

mdl_k = read_excel("LODProcessing.xlsx")
mdl_k_expanded = mdl_k[rep(row.names(mdl_k), each = nrow(comp_conc_1)), ]
comp_mdl = subset(mdl_k_expanded, data == "LOD ug/m3")
comp_k = subset(mdl_k_expanded, data == "K value")
comp_mdl$data = comp_k$data = NULL

# compare conc and MDL of a given component
conc_vs_mdl = data.frame(Map(">", comp_conc_1, comp_mdl))
# assign data conditionally to concentration and uncertainty values for PMF
conc_pmf = conc_vs_mdl * comp_conc_1 +
  (!conc_vs_mdl) * comp_mdl * 0.5
unc_pmf = conc_vs_mdl * (comp_mdl / 3 + comp_k * comp_conc_1) +
  (!conc_vs_mdl) * 5/6 * comp_mdl
unc_pmf$PM2.5 = 3 * comp_conc_1$PM2.5

# insert date into  concentration & uncertainty datasets
conc_pmf <- cbind(Date = rownames(conc_pmf), conc_pmf)
unc_pmf <- cbind(Date = rownames(unc_pmf), unc_pmf)
conc_pmf$Date = unc_pmf$Date = comp_conc$Date

write.csv(conc_pmf, "conc for PMF.csv")
write.csv(unc_pmf, "uncertainty for PMF.csv")
