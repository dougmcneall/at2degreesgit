# uncertaintyTCR.R
# Uncertainty analysis of Transient Climate Response (TCR)
# What is the probability distribution of CO2 concentration
# at a particular temperature change, given a distribution of TCR?

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
plot(tcr_range, tcrdens, type = 'l', lwd = 2, main = "pr(TCR)", xlab = "TCR", ylab = "Probability density" )

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
plot(tcr_range, tcrdens, type = 'l')
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
# Plot CO2 conc. when CMIP5 models cross warming thresholds
# --------------------------------------------------------------------


modco2 = read.csv("CMIP5_CO2_warming_summary - Sheet1.csv")

RCP85_2deg = subset(modco2, RCP == "RCP85" & SWL == 2)
RCP85_1_5deg = subset(modco2, RCP == "RCP85" & SWL == 1.5)

RCP6_2deg = subset(modco2, RCP == "RCP6" & SWL == 2)
RCP6_1_5deg = subset(modco2, RCP == "RCP6" & SWL == 1.5)

RCP45_2deg = subset(modco2, RCP == "RCP45" & SWL == 2)
RCP45_1_5deg = subset(modco2, RCP == "RCP45" & SWL == 1.5)

RCP26_2deg = subset(modco2, RCP == "RCP26" & SWL == 2)
RCP26_1_5deg = subset(modco2, RCP == "RCP26" & SWL == 1.5)

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

col2 = 'black'
col1_5 = 'blue'

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










