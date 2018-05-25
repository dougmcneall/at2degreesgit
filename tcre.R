# tcre.R

# Experiments in uncertainty propogation for Andy Wiltshire

# What is the sensitivity of our estimate of tcre to constraints in
# alpha, beta and gamma?
# How does

# alpha = dtas / dco2
# beta = dC / dCo2
# gamma = dC / dtas

# C = land and ocean carbon stores
# dtas = change in global mean surface air temperature

# tcre is transient climate response to emissions
# tcre = alpha / (k + beta + (alpha*gamma))

# Saltelli 2010 is pretty good on variance based SA
# http://doc.openturns.org/papers/saltelli2010.pdf

tcre = function(alpha, beta, gamma, k = 2.12, fac = 1000){
  out = (alpha / (k + beta + (alpha*gamma))) * fac
  out
}

n = 1000
fac = 1000

mu_alpha = 0.006
sd_alpha = 0.001

mu_beta = 2.2
sd_beta = 0.63

mu_gamma = -91
sd_gamma = 57

k = 2.12

alpha_samp = rnorm(n, mean = mu_alpha, sd_alpha)
beta_samp = rnorm(n, mean = mu_beta, sd = sd_beta)
gamma_samp = rnorm(n, mean = mu_gamma, sd = sd_gamma)

allvar_samp = tcre(alpha=alpha_samp, beta=beta_samp, gamma=gamma_samp)

hist(allvar_samp, xlim = c(0,6), breaks = seq(from = 0, to = 100, by = 0.1))

# Variance due to variation in all parameters
vY = var(allvar_samp)

# Need the cumulative probability of a particular carbon budget
# to hit 2 degrees, and the effects of constraints of alpha, beta, gamma on these.

# 2 degree budget is 2 /tcre
# We care about the carbon budget which gives us a 66% chance of staying below 2 degrees,
# and how our uncertainty about each of the constraints feeds into that.

# Non-linearity in the tail means that you might have a strong tail risk if you go over budget.

#plot(cumsum(2/test))
#length(test>3)/ length(test)


# Calculate the first-order effects index by hand
n = 10000
alpha_samp = rnorm(n, mean = mu_alpha, sd_alpha)
beta_samp = rnorm(n, mean = mu_beta, sd = sd_beta)
gamma_samp = rnorm(n, mean = mu_gamma, sd = sd_gamma)

alpha_fix_mu = rep(NA, n)
for(i in 1:n){
  alpha_fix_mu[i] = mean(tcre(alpha = alpha_samp[i], beta = beta_samp, gamma = gamma_samp))
}

for(i in 1:n){
  beta_fix_mu[i] = mean(tcre(alpha = alpha_samp, beta = beta_samp[i], gamma = gamma_samp))
}

gamma_fix_mu = rep(NA, n)
for(i in 1:n){
  gamma_fix_mu[i] = mean(tcre(alpha = alpha_samp, beta = beta_samp, gamma = gamma_samp[i]))
}

var(alpha_fix_mu)
var(beta_fix_mu)
var(gamma_fix_mu)

# 
var(allvar_samp)
var(alpha_fix_mu) + var(beta_fix_mu) + var(gamma_fix_mu)


library(boot)
# Sobol indices
n = 10000

X1 = cbind(alpha=rnorm(n, mean = mu_alpha, sd_alpha),
           beta=rnorm(n, mean = mu_beta, sd = sd_beta),
           gamma=rnorm(n, mean = mu_gamma, sd = sd_gamma)
           )

X2 = cbind(alpha=rnorm(n, mean = mu_alpha, sd_alpha),
           beta=rnorm(n, mean = mu_beta, sd = sd_beta),
           gamma=rnorm(n, mean = mu_gamma, sd = sd_gamma)
)

sb2002 = sobol2002(model = NULL, X1 = X1, X2 = X2, nboot = 1000)
sb2002.pred = tcre(alpha=sb2002$X[,'alpha'], beta=sb2002$X[,'beta'], gamma=sb2002$X[,'gamma'])

sb2002.out = tell(sb2002, sb2002.pred)
plot(sb2002.out)

# jansen 
sbj = soboljansen(model = NULL, X1 = X1, X2 = X2, nboot = 1000)
sbj.pred = tcre(alpha=sbj$X[,'alpha'], beta=sbj$X[,'beta'], gamma=sbj$X[,'gamma'])

sbj.out = tell(sbj, sbj.pred)
pdf(file = 'Sobol_Jansen.pdf', width = 5, height = 5)
plot(sbj.out)
dev.off()

# martinez
sbm = sobolmartinez(model = NULL, X1 = X1, X2 = X2, nboot = 1000)
sbm.pred = tcre(alpha=sbm$X[,'alpha'], beta=sbm$X[,'beta'], gamma=sbm$X[,'gamma'])

sbm.out = tell(sbm, sbm.pred)
plot(sbm.out)

# Plot the Jansen Sobol indices
print(sbj.out$T[,1])
print(sbj.out$T[,1] * var(allvar_samp) )

# sum of total effects index can exceed one, if there are interactions.
# https://stats.stackexchange.com/questions/70930/difference-between-sobol-indices-and-total-sobol-indices
# sum(sbj.out$T[,1])

# The first order effect (* var(Y)) is the reduction in variance, if an Xi is fixed.
#sbj.out$S[,1]*vY 
# The total effect is the variance left if all but Xi are fixed.
#sbj.out$T[,1]*vY 

pdf(file = 'Sobol_indices.pdf', width = 5, height = 5)
pcols = brewer.pal(3, 'Paired')
par(mar = c(5,6,3,2))
barplot(as.matrix(cbind(sbj.out$T[,1],sbj.out$S[,1]) , nrow = 3),
        horiz = TRUE,
        names.arg = c('Total effect', 'Main effect'),
        las =1,
        col = pcols,
        space = c(0.5,0.5) 
)

reset <- function() {
  par(mfrow=c(1, 1), oma=rep(0, 4), mar=rep(0, 4), new=TRUE)
  plot(0:1, 0:1, type="n", xlab="", ylab="", axes=FALSE)
}

reset()
legend('top',
       legend = c('alpha', 'beta', 'gamma'), 
       fill = pcols,
       horiz = TRUE)
dev.off()




    

stop()


# Replaces Normal with Uniform distributions
# This works but is a little horrible
# Need truncated normal distributions
f <-fast99(model=NULL,factors=c("alpha","beta", "gamma"),
           n=100,
           M=4,
           q=c("qunif","qunif", "qunif"),
           q.arg=list(alpha=list(min=min(alpha_samp),max=max(alpha_samp)),
                      beta=list(min=min(beta_samp),max=max(beta_samp)),
                      gamma=list(min=min(gamma_samp),max=max(gamma_samp)))
)

f.pred = tcre(alpha = f$X[,'alpha'], beta = f$X[,'beta'], gamma = f$X[,'gamma'] )
fast <- tell(f, f.pred)


f <-fast99(model=NULL,factors=c("alpha","beta", "gamma"),
           n=100,
           M=4,
           q=c("qnorm","qnorm", "qnorm"),
           q.arg=list(alpha=list(mean=mu_alpha,sd=sd_alpha),
                      beta=list(mean=mu_beta,sd=sd_beta),
                      gamma=list(mean=mu_beta,sd=sd_beta))
)# fast code
# Compare with a FAST sens
library(sensitivity)
xfast = fast99(model = NULL, factors = colnames(lhs.norm), n = 5000,
               q = "qunif", q.arg = list(min = 0, max = 1))
fast.pred = predict(fit, newdata = xfast$X, type = 'UK')
fast <- tell(xfast, fast.pred$mean)
#pdf(file = 'fast.pdf', width = 12, height = 6)
par(las = 2, mar = c(10,4,2,1))
plot(fast)
#dev.off()




