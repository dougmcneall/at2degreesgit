# uncertaintyTCR.R
# Uncertainty analysis of Transient Climate Response (TCR)
# What is the probability distribution of CO2 concentration
# at a particular temperature change, given a distribution of TCR?

tfunc  = function(tcr, c0=280, dT=2, dF2co2=3.7){
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
tcr_range = seq(from = 0.5, to = 3, by = 1/dx)

# What does the transfer function between TCR and co2 concentration look like?
c_at_2deg =  tfunc(tcr = tcr_range, c0 = 280, dT = 2, dF2co2=3.7)
c_at_1.5deg =  tfunc(tcr = tcr_range, c0 = 280, dT = 1.5, dF2co2=3.7)

lwd = 2
pdf(width = 10, height = 6, file = 'probs.pdf')
par(mfrow = c(2,2))
plot(tcr_range, c_at_2deg, type = 'l', lwd = lwd, ylim = c(300, 5000),
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
tcrsd = 0.5     # The final result is very sensitive to lower tail of tcr distribution

# A plot of what the assumed tcr distribution looks like
tcrdens = dnorm(tcr_range, mean = tcrmean, sd = tcrsd)
plot(tcr_range, tcrdens, type = 'l', lwd = 2, main = "pr(TCR)", xlab = "TCR", ylab = "Probability density" )

# Monte Carlo sample from the tcr distribution and push it through the 
# transfer function
tcr_samp = rnorm(10000, mean = tcrmean, sd = tcrsd)

samp_2deg = tfunc(tcr = tcr_samp, c0 = 280, dT = 2, dF2co2=3.7)
# truncate because you get some very large values at low tcr
samp_2deg.trunc = samp_2deg[samp_2deg<1500]
hist(samp_2deg.trunc, freq = FALSE, breaks = 30, col = 'lightgrey',
     main = "pr(CO2 conc.) at 2 degrees",
     xlab = "CO2 concentration",
     ylab = "probability density")

samp_1.5deg = tfunc(tcr = tcr_samp, c0 = 280, dT = 1.5, dF2co2=3.7)
samp_1.5deg.trunc = samp_1.5deg[samp_1.5deg<1500]
hist(samp_1.5deg.trunc, freq = FALSE, breaks = 30, col = 'lightgrey',
     main = "pr(CO2 conc.) at 1.5 degrees",
     xlab = "CO2 concentration",
     ylab = "probability density")
dev.off()

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

Mode(samp_1.5deg.trunc)
Mode(samp_2deg.trunc)

# Might do better with lower standard deviation
tcrdens = dnorm(tcr_range, mean = 1.75, sd = 0.47)
sum(tcrdens)/dx
sum((tcrdens/dx)[tcr_range<1])
sum((tcrdens/dx)[tcr_range<2.5])








# # looking for the probability density of C
# 
# plot(c_at_2deg , w*c_at_2deg, type = 'l')
# lines(c_at_1.5deg , w*c_at_1.5deg, col = 'red')
# 
# 
# tfuncprob  = function(tcr, c0=280, dT=2, dF2co2=3.7, w){
#   
#   out = c0 * exp( (dT * dF2co2) / (5.35 * tcr * w))
#   out
#   
# }
# 
# pc_at_2deg =  tfuncprob(tcr = tcr_range, c0 = 280, dT = 2, dF2co2=3.7, w = w)







