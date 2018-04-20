# uncertaintyTCR.R
# Uncertainty analysis of Transient Climate Response (TCR)
# What is the probability distribution of CO2 concentration
# at a particular temperature change, given a distribution of TCR?

col2 = 'black'
col1_5 = 'blue'

tfunc  = function(tcr, c0=280, dT=2, dF2co2=3.44){
  # Transfer function that takes in TCR and outputs
  # a co2 concentration at a given temperature change
  # Inputs
  # tcr     ...  Transient climate response
  # c0      ...  baseline co2 concentration in ppm
  # dT      ...  temperature change from baseline (degC)
  # dF2co2  ...  Forcing due to doubling of co2 (w/m^2)
  
  out = c0 * exp( (dT * dF2co2) / (5.35 * tcr))
  out
}

dx = 100
tcr_range = seq(from = 0.5, to = 3.5, by = 1/dx)

# What does the transfer function between TCR and co2 concentration look like?
c_at_2deg =  tfunc(tcr = tcr_range, c0 = 280, dT = 2, dF2co2=3.44)
c_at_1.5deg =  tfunc(tcr = tcr_range, c0 = 280, dT = 1.5, dF2co2=3.44)

pdf(width = 5, height = 4, file = 'transfer.pdf')
par(mar = c(4,5,1,1), bty = 'n', las = 1)
lwd = 2
plot(tcr_range, c_at_2deg, type = 'l', lwd = lwd, ylim = c(0, 4000),
     main = "",
     xlab = "TCR",
     ylab = expression(paste("CO"[2], " concentration (ppm)"))
     )
lines(tcr_range, c_at_1.5deg, type = 'l', lwd = lwd, col = 'blue')
text(1.2,1900, "at 2 degrees", col = 'black')
text(0.9,400, "at 1.5 degrees", col = 'blue')
dev.off()



pdf(width = 10, height = 6, file = 'probs.pdf')
par(mfrow = c(2,2))
plot(tcr_range, c_at_2deg, type = 'l', lwd = lwd, ylim = c(0, 4000),
     main = "transfer function",
     xlab = "TCR",
     ylab = "CO2 conc.")
lines(tcr_range, c_at_1.5deg, type = 'l', lwd = lwd, col = 'red')
text(1,2100, "at 2 degrees", col = 'black')
text(0.8,500, "at 1.5 degrees", col = 'red')

# Very approximately, the TCR distribution is N(1.75, 0.5)
# MC sample, and plug that into the transfer function
# Still to do: pick a closer mean/sd (perhaps non normal?)
tcrmean = 1.75
#tcrsd = 0.5
tcrsd = 0.45  # The final result is very sensitive to lower tail of tcr distribution

# A plot of what the assumed tcr distribution looks like
tcrdens = dnorm(tcr_range, mean = tcrmean, sd = tcrsd)
#plot(tcr_range, tcrdens, type = 'l', lwd = 2, main = "pr(TCR)", xlab = "TCR", ylab = "Probability density" )

# Monte Carlo sample from the tcr distribution and push it through the 
# transfer function
tcr_samp = rnorm(1000000, mean = tcrmean, sd = tcrsd)

samp_2deg = tfunc(tcr = tcr_samp, c0 = 280, dT = 2, dF2co2=3.44)
# truncate because you get some very large values at low tcr
samp_2deg.trunc = samp_2deg[samp_2deg<1500]
hist(samp_2deg.trunc, freq = FALSE, breaks = 30, col = 'lightgrey',
     main = "Pr(CO2 conc.) at 2 degrees",
     xlab = "CO2 concentration (ppm)",
     ylab = "probability density")

samp_1.5deg = tfunc(tcr = tcr_samp, c0 = 280, dT = 1.5, dF2co2=3.44)
samp_1.5deg.trunc = samp_1.5deg[samp_1.5deg<1500]
hist(samp_1.5deg.trunc, freq = FALSE, breaks = 30, col = 'lightgrey',
     main = "Pr(CO2 conc.) at 1.5 degrees",
     xlab = "CO2 concentration (ppm)",
     ylab = "probability density")
dev.off()

print('AR5 2 degrees')
print(quantile(samp_2deg, probs = c(0.05, 0.5, 0.95)))

print('AR5 1.5 degrees')
print(quantile(samp_1.5deg, probs = c(0.05, 0.5, 0.95)))

# Might do better with lower standard deviation
# tcrdens = dnorm(tcr_range, mean = 1.75, sd = 0.45)
tcrdens = dlnorm(tcr_range, meanlog = log(1.9), sdlog =0.4)
#plot(tcr_range, tcrdens, type = 'l')
sum(tcrdens)/dx
sum((tcrdens/dx)[tcr_range<1])
sum((tcrdens/dx)[tcr_range<3.3])

tcr_range[which.max(tcrdens)]

# ----------------------------------------------------------------
# Read TCR distribution (sample) from Richardson et al. (2016)
# A comparison of the AR5 and R16 histograms
# ----------------------------------------------------------------

atcrpdf = as.numeric(readLines("richardson/atcrpdf.txt"))
atcrpdf.trunc = atcrpdf[atcrpdf<10 & atcrpdf >0]

length(atcrpdf.trunc[atcrpdf.trunc<=1.04])/length(atcrpdf.trunc)
length(atcrpdf.trunc[atcrpdf.trunc<=3.3])/length(atcrpdf.trunc)

ar5col = 'grey'
R16col = rgb(col2rgb('tomato2')[1]/256,
             col2rgb('tomato2')[2]/256,
             col2rgb('tomato2')[3]/256,
             0.5)

pdf(width = 10, height = 8, file = 'R16vsAR5.pdf')
par(mfrow = c(2,2))
plot(tcr_range, c_at_2deg, type = 'l', lwd = lwd, ylim = c(0, 4000),
     main = "Transfer function",
     xlab = "TCR",
     ylab = "CO2 concentration (ppm)")
lines(tcr_range, c_at_1.5deg, type = 'l', lwd = lwd, col = 'blue')
text(1,2100, "at 2 degrees", col = 'black')
text(0.8,500, "at 1.5 degrees", col = 'blue')

tcr_samp.trunc = tcr_samp[tcr_samp<10 & tcr_samp >0]

breaks = seq(from = 0, to = 10, by = 0.25)
hist(tcr_samp.trunc,  col = ar5col, xlim = c(0,6), freq = FALSE,
     breaks = breaks,
     xlab = 'TCR',
     main = 'Pr(TCR)')
hist(atcrpdf.trunc, add = TRUE, col = R16col, freq = FALSE, breaks = breaks)
legend('topright', legend = c('AR5', 'R16'), fill = c(ar5col, R16col),
       bty = 'n')

R16_2deg = tfunc(tcr = atcrpdf.trunc, c0 = 280, dT = 2, dF2co2=3.44)
R16_2deg.trunc = R16_2deg[R16_2deg<1500]

R16_1.5deg = tfunc(tcr = atcrpdf.trunc, c0 = 280, dT = 1.5, dF2co2=3.44)
R16_1.5deg.trunc = R16_1.5deg[R16_1.5deg<1500]

hist(samp_1.5deg.trunc, freq = FALSE, breaks = 30, 
     xlim = c(280,1200),
     col = ar5col,
     main = "Pr(CO2 conc.) at 1.5 degrees",
     xlab = "CO2 concentration (ppm)",
     ylab = "probability density")

hist(R16_1.5deg.trunc, freq = FALSE,
     breaks = 30, add = TRUE, col = R16col
)
legend('topright', legend = c('AR5', 'R16'), fill = c(ar5col, R16col),
       bty = 'n')

hist(samp_2deg.trunc, freq = FALSE, breaks = 30, 
     xlim = c(280,1200),
     col = ar5col,
     main = "Pr(CO2 conc.) at 2 degrees",
     xlab = "CO2 concentration (ppm)",
     ylab = "probability density"
)
hist(R16_2deg.trunc, freq = FALSE,
     breaks = 30, add = TRUE,col = R16col)
legend('topright', legend = c('AR5', 'R16'), fill = c(ar5col, R16col), 
       bty = 'n')

dev.off()

quantile(atcrpdf, probs = c(0.05, 0.5, 0.95))

print('R16 2 degrees')
print(quantile(R16_2deg, probs = c(0.05, 0.5, 0.95)))
print('R16 1.5 degrees')
print(quantile(R16_1.5deg, probs = c(0.05, 0.5, 0.95)))


pdf(file = 'tcr_dist.pdf', width = 5, height= 3.5)
par(las = 1, mar = c(5,4,1,1))
breaks = seq(from = 0, to = 10, by = 0.25)
hist(tcr_samp.trunc,  col = ar5col, xlim = c(0,6), freq = FALSE,
     breaks = breaks,
     xlab = 'TCR',
     main = ''
     )
hist(atcrpdf.trunc, add = TRUE, col = R16col, freq = FALSE, breaks = breaks)
legend('topright', legend = c('AR5', 'R16'), fill = c(ar5col, R16col),
       bty = 'n')
dev.off()


pdf(width = 8, height = 4, file = 'co2_dist.pdf')
par(mfrow = c(1,2))
hist(samp_1.5deg.trunc, freq = FALSE, breaks = 30, 
     xlim = c(280,1200),
     col = ar5col,
     main = "1.5 degrees",
     xlab = expression(paste("CO"[2]," conc. (ppm)")),
     ylab = "density")

hist(R16_1.5deg.trunc, freq = FALSE,
     breaks = 30, add = TRUE, col = R16col
)
legend('topright', legend = c('AR5', 'R16'), fill = c(ar5col, R16col),
       bty = 'n')

hist(samp_2deg.trunc, freq = FALSE, breaks = 30, 
     xlim = c(280,1200),
     col = ar5col,
     main = "2 degrees",
     xlab = expression(paste("CO"[2]," conc. (ppm)")),
     ylab = "density"
)
hist(R16_2deg.trunc, freq = FALSE,
     breaks = 30, add = TRUE,col = R16col)
legend('topright', legend = c('AR5', 'R16'), fill = c(ar5col, R16col), 
       bty = 'n')
dev.off()


# --------------------------------------------------------------------
# Plot CO2 concentration,
# when CMIP5 models cross warming thresholds
# --------------------------------------------------------------------

modco2 = read.csv("CMIP5_CO2_warming_summary - Sheet1.csv", sep = ',', strip.white = TRUE)
modlist = c('GFDL-ESM2M', 'NorESM1-M', 'MIROC-ESM-CHEM',
            'HadGEM2-ES', 'MIROC5', 'IPSL-CM5A-LR')

RCP85_2deg = subset(modco2, RCP == "RCP85" & SWL == 2)
RCP85_1_5deg = subset(modco2, RCP == "RCP85" & SWL == 1.5)

RCP85_2deg_isimip = subset(RCP85_2deg, model%in% modlist)
RCP85_1_5deg_isimip = subset(RCP85_1_5deg, model%in% modlist)

RCP6_2deg = subset(modco2, RCP == "RCP6" & SWL == 2)
RCP6_1_5deg = subset(modco2, RCP == "RCP6" & SWL == 1.5)

RCP6_2deg_isimip = subset(RCP6_2deg, model%in% modlist)
RCP6_1_5deg_isimip = subset(RCP6_1_5deg, model%in%modlist)

RCP45_2deg = subset(modco2, RCP == "RCP45" & SWL == 2)
RCP45_1_5deg = subset(modco2, RCP == "RCP45" & SWL == 1.5)

RCP45_2deg_isimip = subset(RCP45_2deg, model%in%modlist)
RCP45_1_5deg_isimip = subset(RCP45_1_5deg, model%in%modlist)

RCP26_2deg = subset(modco2, RCP == "RCP26" & SWL == 2)
RCP26_1_5deg = subset(modco2, RCP == "RCP26" & SWL == 1.5)

RCP26_2deg_isimip = subset(RCP26_2deg, model%in%modlist)
RCP26_1_5deg_isimip = subset(RCP26_1_5deg, model%in%modlist)

pdf(width = 5, height = 5, file = 'modco2.pdf')
par(las = 1, mar = c(5,6,3,2))
plot(RCP85_2deg$CO2_ppmv, rep(4, length(RCP85_2deg$CO2_ppmv)),
     ylim = c(0,5),
     xlim = c(300, 800),
     axes = FALSE,
     xlab = expression(paste('CO'[2], ' conc. (ppm)')),
     ylab = ''
)

points(RCP6_2deg$CO2_ppmv,rep(3, length(RCP6_2deg$CO2_ppmv)))
points(RCP45_2deg$CO2_ppmv,rep(2, length(RCP45_2deg$CO2_ppmv)))
points(RCP26_2deg$CO2_ppmv,rep(1, length(RCP26_2deg$CO2_ppmv)))


points(RCP85_1_5deg$CO2_ppmv,rep(3.7, length(RCP85_1_5deg$CO2_ppmv)), col = col1_5)

points(RCP6_1_5deg$CO2_ppmv,rep(2.7, length(RCP6_1_5deg$CO2_ppmv)), col = col1_5)

points(RCP45_1_5deg$CO2_ppmv,rep(1.7, length(RCP45_1_5deg$CO2_ppmv)), col = col1_5)

points(RCP26_1_5deg$CO2_ppmv,rep(0.7, length(RCP26_1_5deg$CO2_ppmv)), col = col1_5)
axis(1)
axis(2, labels = c('RCP26', 'RCP45', 'RCP6', 'RCP85'), at = 1:4)

legend('topright', legend = c('2 degrees', '1.5 degrees'), 
       pch = 21, col = c(col2, col1_5) , bty = 'n'
       )
dev.off()


# --------------------------------------------------------------------
# Combine model estimates and observational estimates of CO2
# concentration at temperature thresholds with a rug plot
# --------------------------------------------------------------------
RCP_co2 = read.csv('RCP_CO2_concentration_HELIX_WP3.csv', sep = ',', strip.white = TRUE)
ERF_total_anthro = read.csv('WG1AR5_AII.6.8_Total_anthropogenic_ERF_from_published_RCPs_(W_m–2).csv', sep = ',', strip.white = TRUE)
ERF_co2 = read.csv('WG1AR5_TableAII.6.1_ERF_from_CO2_(W_m–2).csv', sep = ',', strip.white = TRUE)
#plot(ERF_co2$RCP26)

erf = read.csv('co2_and_total_erf.csv', sep = ',', strip.white = TRUE)

transfer_ppm = function(ppm,co2_ppm_RCP, tot_anthro_erf_RCP, co2_erf_RCP){
  # transfer between co2 and co2e (co2 equivalent forcing)
  co2_tot_erf = approx(x=co2_ppm_RCP, 
                       y=tot_anthro_erf_RCP,
                       xout = ppm)
  
  tot_anthro_co2ppm = approx(x=co2_erf_RCP,
                             y = co2_ppm_RCP,
                             xout = co2_tot_erf$y)
  co2e = tot_anthro_co2ppm$y
  co2e
}

RCP85_1_5deg_CO2e = transfer_ppm(ppm=RCP85_1_5deg$CO2_ppmv,
                                 co2_ppm_RCP=erf$co2_ppm_RCP85,
                                 tot_anthro_erf_RCP=erf$tot_anthro_erf_RCP85,
                                 co2_erf_RCP=erf$co2_erf_RCP85)
RCP85_2deg_CO2e = transfer_ppm(ppm=RCP85_2deg$CO2_ppmv,
                                 co2_ppm_RCP=erf$co2_ppm_RCP85,
                                 tot_anthro_erf_RCP=erf$tot_anthro_erf_RCP85,
                                 co2_erf_RCP=erf$co2_erf_RCP85)

# use the RCPco2 relationship for extrapolation
RCP60_1_5deg_CO2e = transfer_ppm(ppm=RCP6_1_5deg$CO2_ppmv,
                                 co2_ppm_RCP=erf$co2_ppm_RCP85,
                                 tot_anthro_erf_RCP=erf$tot_anthro_erf_RCP60,
                                 co2_erf_RCP=erf$co2_erf_RCP60
)

RCP60_2deg_CO2e = transfer_ppm(ppm=RCP6_2deg$CO2_ppmv,
                               co2_ppm_RCP=erf$co2_ppm_RCP85,
                               tot_anthro_erf_RCP=erf$tot_anthro_erf_RCP60,
                               co2_erf_RCP=erf$co2_erf_RCP60
)
# use the RCPco2 relationship for extrapolation
RCP45_1_5deg_CO2e = transfer_ppm(ppm=RCP45_1_5deg$CO2_ppmv,
                                 co2_ppm_RCP=erf$co2_ppm_RCP85,
                                 tot_anthro_erf_RCP=erf$tot_anthro_erf_RCP45,
                                 co2_erf_RCP=erf$co2_erf_RCP45
)
RCP45_2deg_CO2e = transfer_ppm(ppm=RCP45_2deg$CO2_ppmv,
                               co2_ppm_RCP=erf$co2_ppm_RCP85,
                               tot_anthro_erf_RCP=erf$tot_anthro_erf_RCP45,
                               co2_erf_RCP=erf$co2_erf_RCP45
)

RCP26_1_5deg_CO2e = transfer_ppm(ppm=RCP26_1_5deg$CO2_ppmv,
                                 co2_ppm_RCP=erf$co2_ppm_RCP26,
                                 tot_anthro_erf_RCP=erf$tot_anthro_erf_RCP26,
                                 co2_erf_RCP=erf$co2_erf_RCP26
)

RCP26_2deg_CO2e = transfer_ppm(ppm=RCP26_2deg$CO2_ppmv,
                               co2_ppm_RCP=erf$co2_ppm_RCP26,
                               tot_anthro_erf_RCP=erf$tot_anthro_erf_RCP26,
                               co2_erf_RCP=erf$co2_erf_RCP26
)



rpch = '|'
xlim = c(300, 1100)
ylim = c(0, 0.007)

ar5col = 'darkgrey'
R16col = rgb(col2rgb('tomato2')[1]/256,
             col2rgb('tomato2')[2]/256,
             col2rgb('tomato2')[3]/256,
             0.5)

isicol = 'dodgerblue'
ecol = 'darkorange'
edcol = 'pink1'

pdf(width = 7, height = 6, file = 'co2_dist_rug.pdf')

nf <- layout(matrix(c(1,2,3,4),2,2,byrow = TRUE), widths= c(4,4), heights= c(3.5,2.5), TRUE)
#layout.show(nf)
par(mar = c(0,5,1,1), las = 1, fg = 'white')
hist(samp_1.5deg.trunc, freq = FALSE, breaks = 30, 
     xlim = xlim,
     ylim = ylim,
     col = ar5col,
     main = "",
     xlab = expression(paste("CO"[2]," conc. (ppm)")),
     ylab = "Relative probability density",
     axes = FALSE)

hist(R16_1.5deg.trunc, freq = FALSE,
     breaks = 30, add = TRUE, col = R16col,
     fg = 'white'
)
mtext('1.5 degrees', side = 3, line = -2, font = 2, col = 'black')

#axis(2)
par(mar = c(0,2,1,4), las = 1)
hist(samp_2deg.trunc, freq = FALSE, breaks = 30, 
     xlim = xlim,
     ylim = ylim,
     col = ar5col,
     main = "",
     xlab = '',
     ylab = '',
     axes = FALSE
)

hist(R16_2deg.trunc, freq = FALSE,
     breaks = 30, add = TRUE,col = R16col)
legend('right', legend = c('AR5', 'R16'), fill = c(ar5col, R16col), 
       bty = 'n', text.col = 'black', border = 'white')

mtext('2 degrees', side = 3, line = -2, font = 2, col = 'black')

par(mar = c(4,5,0,1), las = 1)
plot(RCP85_1_5deg$CO2_ppmv,rep(4.2, length(RCP85_1_5deg$CO2_ppmv)), 
     ylim = c(0.5,5),
     xlim = xlim,
     axes = FALSE,
     xlab = expression(paste('CO'[2], ' concentration (ppm)')),
     ylab = '',
     col = 'black',
     pch = rpch)

points(passyears$conc_1.5, rep(4.6, length(passyears$conc_1.5)),
       col = edcol,
       pch = rpch)

points(RCP6_1_5deg$CO2_ppmv,rep(3.2, length(RCP6_1_5deg$CO2_ppmv)),
       col = 'black',
       pch = rpch)

points(RCP45_1_5deg$CO2_ppmv,rep(2.2, length(RCP45_1_5deg$CO2_ppmv)), 
       col = 'black',
       pch = rpch)

points(RCP26_1_5deg$CO2_ppmv,rep(1.2, length(RCP26_1_5deg$CO2_ppmv)),
       col = 'black',
       pch = rpch)

points(RCP85_1_5deg_isimip$CO2_ppmv,rep(4.2, length(RCP85_1_5deg_isimip$CO2_ppmv)),
       col = isicol,
       pch = rpch)

points(RCP6_1_5deg_isimip$CO2_ppmv,rep(3.2, length(RCP6_1_5deg_isimip$CO2_ppmv)),
       col = isicol,
       pch = rpch)

points(RCP45_1_5deg_isimip$CO2_ppmv,rep(2.2, length(RCP45_1_5deg_isimip$CO2_ppmv)), 
       col = isicol,
       pch = rpch)

points(RCP26_1_5deg_isimip$CO2_ppmv,rep(1.2, length(RCP26_1_5deg_isimip$CO2_ppmv)),
       col = isicol,
       pch = rpch)


# Plot CO2e
points(RCP85_1_5deg_CO2e,rep(3.8, length(RCP85_1_5deg_CO2e)),
       col = ecol,
       pch = rpch)

points(RCP60_1_5deg_CO2e,rep(2.8, length(RCP60_1_5deg_CO2e)),
       col = ecol,
       pch = rpch)

points(RCP45_1_5deg_CO2e,rep(1.8, length(RCP45_1_5deg_CO2e)), 
       col = ecol,
       pch = rpch)

points(RCP26_1_5deg_CO2e,rep(0.8, length(RCP26_1_5deg_CO2e)),
       col = ecol,
       pch = rpch)



axis(1, col = 'black')
axis(2, labels = c('RCP2.6', 'RCP4.5', 'RCP6.0', 'RCP8.5'), at = c(1,2,3, 4.2), col = 'black')

par(mar = c(4,2,0,4), las = 1)
plot(RCP85_2deg$CO2_ppmv, rep(4.2, length(RCP85_2deg$CO2_ppmv)),
     ylim = c(0.5,5),
     xlim = xlim,
     axes = FALSE,
     xlab = expression(paste('CO'[2], ' concentration (ppm)')),
     ylab = '',
     pch = rpch,
     col = 'black'
)
axis(1, col = 'black')

points(passyears$conc_2, rep(4.6, length(passyears$conc_2)),
       col = edcol,
       pch = rpch)

points(RCP6_2deg$CO2_ppmv,rep(3.2, length(RCP6_2deg$CO2_ppmv)),
       pch = rpch,col = 'black')
points(RCP45_2deg$CO2_ppmv,rep(2.2, length(RCP45_2deg$CO2_ppmv)),
       pch = rpch,col = 'black')
points(RCP26_2deg$CO2_ppmv,rep(1.2, length(RCP26_2deg$CO2_ppmv)),
       pch = rpch,col = 'black')

points(RCP85_2deg_isimip$CO2_ppmv,rep(4.2, length(RCP85_2deg_isimip$CO2_ppmv)),
       pch = rpch,col = isicol)
points(RCP6_2deg_isimip$CO2_ppmv,rep(3.2, length(RCP6_2deg_isimip$CO2_ppmv)),
       pch = rpch,col = isicol)
points(RCP45_2deg_isimip$CO2_ppmv,rep(2.2, length(RCP45_2deg_isimip$CO2_ppmv)),
       pch = rpch,col = isicol)
points(RCP26_2deg_isimip$CO2_ppmv,rep(1.2, length(RCP26_2deg_isimip$CO2_ppmv)),
       pch = rpch,col = isicol)

# Plot CO2e
points(RCP85_2deg_CO2e,rep(3.8, length(RCP85_2deg_CO2e)),
       col = ecol,
       pch = rpch)

points(RCP60_2deg_CO2e,rep(2.8, length(RCP60_2deg_CO2e)),
       col = ecol,
       pch = rpch)

points(RCP45_2deg_CO2e,rep(1.8, length(RCP45_2deg_CO2e)), 
       col = ecol,
       pch = rpch)

points(RCP26_2deg_CO2e,rep(0.8, length(RCP26_2deg_CO2e)),
       col = ecol,
       pch = rpch)

legend('right', legend = c('Emissions driven', 'CMIP5', 'CMIP5 & ISIMIP', expression(paste('CO'[2],'e'))), pch = rpch, 
       col = c(edcol,'black', isicol,ecol), text.col = 'black', cex = 0.8, bty = 'n'
       )
dev.off()


print("RCP8.5 2 degrees")
print(RCP85_2deg[order(RCP85_2deg$CO2_ppmv, decreasing = TRUE) , ])

print("RCP6.0 2 degrees")
print(RCP6_2deg[order(RCP6_2deg$CO2_ppmv, decreasing = TRUE) , ])

print("RCP4.5 2 degrees")
print(RCP45_2deg[order(RCP45_2deg$CO2_ppmv, decreasing = TRUE) , ])

print("RCP2.6 2 degrees")
print(RCP26_2deg[order(RCP26_2deg$CO2_ppmv, decreasing = TRUE) , ])


print("RCP8.5 1.5 degrees")
print(RCP85_1_5deg[order(RCP85_1_5deg$CO2_ppmv, decreasing = TRUE) , ])

print("RCP6.0 1.5 degrees")
print(RCP6_1_5deg[order(RCP6_1_5deg$CO2_ppmv, decreasing = TRUE) , ])

print("RCP4.5 1.5 degrees")
print(RCP45_1_5deg[order(RCP45_1_5deg$CO2_ppmv, decreasing = TRUE) , ])

print("RCP2.6.0 1.5 degrees")
print(RCP26_1_5deg[order(RCP26_1_5deg$CO2_ppmv, decreasing = TRUE) , ])

# -----------------------------------------------------------------
# How do CO2 and total Radiative forcing vary together?
# -----------------------------------------------------------------




pdf(width = 5, height = 6, file = 'erf.pdf')
par(las = 1, mar = c(5,5,3,1))
plot(erf$co2_ppm_RCP85, erf$tot_anthro_erf_RCP85,
     type = 'l', lwd = 2.5,
     xlim = c(350,1000), ylim = c(1,9),
     xlab = expression(paste('CO'[2], ' concentration (ppm)')),
     ylab = expression(paste('Forcing (Wm'^-2,')')),
     bty = 'n'
     )
lines(erf$co2_ppm_RCP85, erf$co2_erf_RCP85, lwd = 2.5, lty = 'dashed')

lines(erf$co2_ppm_RCP60, erf$tot_anthro_erf_RCP60, lwd = 2.5, col = 'tomato2')
lines(erf$co2_ppm_RCP60, erf$co2_erf_RCP60, lty = 'dashed',lwd = 2.5, col = 'tomato2')

lines(erf$co2_ppm_RCP45, erf$tot_anthro_erf_RCP45, lwd = 2.5, col = 'dodgerblue')
lines(erf$co2_ppm_RCP45, erf$co2_erf_RCP45, lwd = 2.5, lty = 'dashed', col = 'dodgerblue')

lines(erf$co2_ppm_RCP26, erf$tot_anthro_erf_RCP26, lwd = 2.5 , col = 'grey')
lines(erf$co2_ppm_RCP26, erf$co2_erf_RCP26, lwd = 2.5,lty = 'dashed', col = 'grey')
legend('topleft',
       c('RCP8.5', 'RCP6.0', 'RCP4.5', 'RCP2.6', 'total ERF', expression(paste('CO'[2],' ERF'))),
       lty = c('solid', 'solid','solid', 'solid','solid', 'dashed'),
       col = c('black', 'tomato2', 'dodgerblue', 'grey', 'black', 'black'),
       bty = 'n',
       lwd = 2, cex = 0.8
       )

dev.off()

# # transfer between co2 and co2equivalent forcing
# # # this code is the initial calculation
# apco2_toterf = approx(x=erf$co2_ppm_RCP85,
#                        y = erf$tot_anthro_erf_RCP85, xout = 600)
# # 
# aptot_anthro_co2ppm = approx(x=erf$co2_erf_RCP85,
#                               y = erf$co2_ppm_RCP85, xout = apco2_toterf$y)
# 
# 
# 
# transfer_ppm(ppm=RCP85_2deg$CO2_ppmv,
#              co2_ppm_RCP=erf$co2_ppm_RCP85,
#              tot_anthro_erf_RCP=erf$tot_anthro_erf_RCP85,
#              co2_erf_RCP=erf$co2_erf_RCP85
# )
# 
# transfer_ppm(ppm=600,
#              co2_ppm_RCP=erf$co2_ppm_RCP85,
#              tot_anthro_erf_RCP=erf$tot_anthro_erf_RCP60,
#              co2_erf_RCP=erf$co2_erf_RCP60
# )
# 
# transfer_ppm(ppm=440,
#              co2_ppm_RCP=erf$co2_ppm_RCP85,
#              tot_anthro_erf_RCP=erf$tot_anthro_erf_RCP45,
#              co2_erf_RCP=erf$co2_erf_RCP45
# )
# 
# transfer_ppm(ppm=400,
#              co2_ppm_RCP=erf$co2_ppm_RCP26,
#              tot_anthro_erf_RCP=erf$tot_anthro_erf_RCP26,
#              co2_erf_RCP=erf$co2_erf_RCP26
# )

perc = function(samp, thres){
  
  out = rep(NA, length(thres))
  for (i in 1:length(thres)){
    
    out[i] = sum(samp<thres[i], na.rm = TRUE)/(length(samp))
  }
  out
}

# ranges of the co2 pdfs spanned by the models
print('CMIP5 RCP85 range of AR5 2deg pdf %ile') 
print(perc(samp=samp_2deg, thres = range(RCP85_2deg$CO2_ppmv)) *100)
print('CMIP5 RCP60 range of AR5 2deg pdf %ile') 
print(perc(samp=samp_2deg, thres = range(RCP6_2deg$CO2_ppmv)) *100)
print('CMIP5 RCP45 range of AR5 2deg pdf %ile') 
print(perc(samp=samp_2deg, thres = range(RCP45_2deg$CO2_ppmv)) * 100)
print('CMIP5 RCP26 range of AR5 2deg pdf %ile') 
print(perc(samp=samp_2deg, thres = range(RCP26_2deg$CO2_ppmv)) * 100)

print('CMIP5 RCP85 range of AR5 1.5deg pdf %ile') 
print(perc(samp=samp_1.5deg, thres = range(RCP85_1_5deg$CO2_ppmv)) * 100)
print('CMIP5 RCP60 range of AR5 1.5deg pdf %ile')
print(perc(samp=samp_1.5deg, thres = range(RCP6_1_5deg$CO2_ppmv)) * 100)
print('CMIP5 RCP45 range of AR5 1.5deg pdf %ile') 
print(perc(samp=samp_1.5deg, thres = range(RCP45_1_5deg$CO2_ppmv)) * 100)
print('CMIP5 RCP26 range of AR5 1.5deg pdf %ile') 
print(perc(samp=samp_1.5deg, thres = range(RCP26_1_5deg$CO2_ppmv)) * 100)


print('CMIP5 RCP85 range of R16 2deg pdf %ile') 
print(perc(samp=R16_2deg, thres = range(RCP85_2deg$CO2_ppmv)) * 100)
print('CMIP5 RCP60 range of R16 2deg pdf %ile') 
print(perc(samp=R16_2deg, thres = range(RCP6_2deg$CO2_ppmv)) * 100)
print('CMIP5 RCP45 range of R16 2deg pdf %ile') 
print(perc(samp=R16_2deg, thres = range(RCP45_2deg$CO2_ppmv)) * 100)
print('CMIP5 RCP26 range of R16 2deg pdf %ile') 
print(perc(samp=R16_2deg, thres = range(RCP26_2deg$CO2_ppmv)) * 100)


print('CMIP5 RCP85 range of R16 1.5deg pdf %ile') 
print(perc(samp=R16_1.5deg, thres = range(RCP85_1_5deg$CO2_ppmv)) * 100)
print('CMIP5 RCP60 range of R16 1.5deg pdf %ile') 
print(perc(samp=R16_1.5deg, thres = range(RCP6_1_5deg$CO2_ppmv)) * 100)
print('CMIP5 RCP45 range of R16 1.5deg pdf %ile') 
print(perc(samp=R16_1.5deg, thres = range(RCP45_1_5deg$CO2_ppmv)) * 100)
print('CMIP5 RCP26 range of R16 1.5deg pdf %ile') 
print(perc(samp=R16_1.5deg, thres = range(RCP26_1_5deg$CO2_ppmv)) * 100)

# ranges of the co2 pdfs spanned by the models using co2e
print('CMIP5 Co2e RCP85 range of AR5 2deg pdf %ile') 
print(perc(samp=samp_2deg, thres = range(RCP85_2deg_CO2e)) * 100)
print('CMIP5 Co2e RCP85 range of AR5 2deg pdf %ile') 
print(perc(samp=samp_2deg, thres = range(RCP60_2deg_CO2e)) * 100)
print('CMIP5 Co2e RCP85 range of AR5 2deg pdf %ile')
print(perc(samp=samp_2deg, thres = range(RCP45_2deg_CO2e)) * 100)
print('CMIP5 Co2e RCP85 range of AR5 2deg pdf %ile') 
print(perc(samp=samp_2deg, thres = range(RCP26_2deg_CO2e)) * 100)


print('CMIP5 Co2e RCP85 range of AR5 1.5deg pdf %ile') 
print(perc(samp=samp_1.5deg, thres = range(RCP85_1_5deg_CO2e)) * 100)
print('CMIP5 Co2e RCP60 range of AR5 1.5deg pdf %ile') 
print(perc(samp=samp_1.5deg, thres = range(RCP60_1_5deg_CO2e)) * 100)
print('CMIP5 Co2e RCP45 range of AR5 1.5deg pdf %ile') 
print(perc(samp=samp_1.5deg, thres = range(RCP45_1_5deg_CO2e)) * 100)
print('CMIP5 Co2e RCP26 range of AR5 1.5deg pdf %ile') 
print(perc(samp=samp_1.5deg, thres = range(RCP26_1_5deg_CO2e)) * 100)

print('CMIP5 Co2e RCP85 range of R16 2deg pdf %ile') 
print(perc(samp=R16_2deg, thres = range(RCP85_2deg_CO2e)) * 100)
print('CMIP5 Co2e RCP60 range of R16 2deg pdf %ile') 
print(perc(samp=R16_2deg, thres = range(RCP60_2deg_CO2e)) * 100)
print('CMIP5 Co2e RCP45 range of R16 2deg pdf %ile') 
print(perc(samp=R16_2deg, thres = range(RCP45_2deg_CO2e)) * 100)
print('CMIP5 Co2e RCP26 range of R16 2deg pdf %ile') 
print(perc(samp=R16_2deg, thres = range(RCP26_2deg_CO2e)) * 100)


print('CMIP5 Co2e RCP85 range of R16 1.5deg pdf %ile') 
print(perc(samp=R16_1.5deg, thres = range(RCP85_1_5deg_CO2e)) * 100)
print('CMIP5 Co2e RCP85 range of R16 1.5deg pdf %ile') 
print(perc(samp=R16_1.5deg, thres = range(RCP60_1_5deg_CO2e)) * 100)
print('CMIP5 Co2e RCP85 range of R16 1.5deg pdf %ile') 
print(perc(samp=R16_1.5deg, thres = range(RCP45_1_5deg_CO2e)) * 100)
print('CMIP5 Co2e RCP85 range of R16 1.5deg pdf %ile') 
print(perc(samp=R16_1.5deg, thres = range(RCP26_1_5deg_CO2e)) * 100)


print('Emissions driven RCP85 range of AR5 1.5deg pdf %ile') 
print(perc(samp=samp_1.5deg, thres = range(passyears$conc_1.5)) *100)
print('Emissions driven RCP85 range of AR5 2deg pdf %ile') 
print(perc(samp=samp_2deg, thres = range(passyears$conc_2)) *100)

print('Emissions driven RCP85 range of R16 1.5deg pdf %ile') 
print(perc(samp=R16_1.5deg, thres = range(passyears$conc_1.5)) *100)
print('Emissions driven RCP85 range of AR5 2deg pdf %ile') 
print(perc(samp=R16_2deg, thres = range(passyears$conc_2)) *100)


Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

print(paste('AR5 2 degrees mode is ', Mode(samp_2deg) ))
print(paste('AR5 1.5 degrees mode is ', Mode(samp_1.5deg) ))

print(paste('R16 2 degrees mode is ', Mode(R16_2deg) ))
print(paste('R16 1.5 degrees mode is ', Mode(R16_1.5deg) ))






