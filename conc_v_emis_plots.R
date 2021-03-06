# conc_v_emis_plots.R
# ---------------------------------------------------------------------------------------
# Comparing the CO2 concentrations in RCP and emissions-driven runs.
# Figures S4 qnd  S5 for supplementary material of
# "How much CO2 at 1.5 deg C and 2 deg C"  Betts & McNeall 2018, Nature Climate Change
# Doug McNeall dougmcneall@gmail.com
# ---------------------------------------------------------------------------------------

emsi_v_conc_1_5 = read.csv('emis_v_conc_1_5.csv', head = TRUE)
emsi_v_conc_2 = read.csv('emis_v_conc_2.csv', head = TRUE)

# Figure S4
pdf(file = 'emis_v_conc_1_5.pdf')
par(las = 1)
plot(emsi_v_conc_1_5[,2], emsi_v_conc_1_5[,3], pch = 19, col = 'tomato2',
     xlim = c(390, 520),ylim = c(390,520),
     xlab = 'Emissions driven runs',
     ylab = 'Concentration driven runs',
     main = expression(paste('CO'[2], ' concentration (ppm) at time of first reaching 1.5',degree,'C'))
     )
abline(0,1)
text(emsi_v_conc_1_5[,2], emsi_v_conc_1_5[,3],
     labels = as.character(emsi_v_conc_1_5[,1]), cex = 0.7, pos = 4,
     col = 'black')

dev.off()

# Figure S5.
pdf(file = 'emis_v_conc_2.pdf')
par(las = 1)
plot(emsi_v_conc_2[,2], emsi_v_conc_2[,3], pch = 19, col = 'tomato2',
     xlim = c(420, 630),ylim = c(420,630),
     xlab = 'Emissions driven runs',
     ylab = 'Concentration driven runs',
     main = expression(paste('CO'[2], ' concentration (ppm) at time of first reaching 2',degree,'C'))
)
abline(0,1)
text(emsi_v_conc_2[,2], emsi_v_conc_2[,3],
     labels = as.character(emsi_v_conc_2[,1]), cex = 0.7, pos = 4,
     col = 'black')

dev.off()
