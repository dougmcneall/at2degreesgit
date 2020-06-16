# Plot_RCPs.R
# Plotting the annual globl mean temperature
# for all the models in the RCPs. Years 1850 - 2100


col.rcp85 = 'red'
col.rcp60 = 'orange'
col.rcp45 = 'skyblue1'
col.rcp26 = 'royalblue4'

datdir = 'knmi_climate_explorer'
#rcpdir = 'RCP26'

#dirlist = dir(paste(datdir,rcpdir, sep = '/'))
#nfiles = length(dirlist)

years = 1850:2100

anomalize_ensemble = function(dat, ix){
  baseline.mean = apply(dat[, ix], 1, mean, na.rm = TRUE)
  out = sweep(dat, 1, baseline.mean)
  out
}

#ix = which(years %in%1861:1880) # corresponding to 1861:1880
#RCP26_tas.anom = anomalize_ensemble(RCP26_tas, ix = ix)
#matplot(years, t(RCP26_tas.anom), type = 'l', col = 'grey', lty = 'solid')

load_rcp = function(datdir, rcpdir, years = 1850:2100){
  
  dirlist = dir(paste(datdir,rcpdir, sep = '/'))
  nfiles = length(dirlist)
  dat = matrix(NA, nrow = nfiles, ncol = length(years))
  
  for(i in 1:nfiles){
    fn = dirlist[i]
    tas = read.table(paste0(datdir,'/',rcpdir,'/',fn), skip = 9, head = FALSE)
    
    ensyears = tas[,1]
    in.ix = which(years%in%ensyears)
    
    #print(nrow(tas))
    dat[i,in.ix] = c(tas[,2], recursive = TRUE)
  }
  colnames(dat) = years
  dat
}

RCP26_tas = load_rcp(datdir = 'knmi_climate_explorer', rcpdir = 'RCP26')
ix = which(years %in%1861:1880) # corresponding to 1861:1880
RCP26_tas.anom = anomalize_ensemble(RCP26_tas, ix = ix)
matplot(years, t(RCP26_tas.anom), type = 'l', col = 'grey', lty = 'solid')

RCP45_tas = load_rcp(datdir = 'knmi_climate_explorer', rcpdir = 'RCP45')
RCP45_tas.anom = anomalize_ensemble(RCP45_tas, ix = ix)
matplot(years, t(RCP45_tas.anom), type = 'l', col = 'grey', lty = 'solid')

RCP60_tas = load_rcp(datdir = 'knmi_climate_explorer', rcpdir = 'RCP60')
RCP60_tas.anom = anomalize_ensemble(RCP60_tas, ix = ix)
matplot(years, t(RCP60_tas.anom), type = 'l', col = 'grey', lty = 'solid')

RCP85_tas = load_rcp(datdir = 'knmi_climate_explorer', rcpdir = 'RCP85')
RCP85_tas.anom = anomalize_ensemble(RCP85_tas, ix = ix)
matplot(years, t(RCP85_tas.anom), type = 'l', col = 'grey', lty = 'solid')


matplot(years, t(RCP85_tas.anom), type = 'l', col = 'tomato2', lty = 'solid')
matlines(years, t(RCP60_tas.anom), type = 'l', col = 'dodgerblue', lty = 'solid')
matlines(years, t(RCP45_tas.anom), type = 'l', col = 'gold', lty = 'solid')
matlines(years, t(RCP26_tas.anom), type = 'l', col = 'black', lty = 'solid')

# Looking at co2 concentration


# replace '85' with '3PD' '45' or '6' to look at the other rcps,

rcpyears = 1765:2500
getco2 = function(fn, skip = 38, startyear = 1850, endyear = 2100){
  rcp = read.table(file = fn,
                     skip = skip, header=TRUE)
  start_ix = which(rcp[,1]==startyear)
  end_ix = which(rcp[,1]==endyear)
  rcpconc = rcp[start_ix:end_ix,4]#
  rcpconc
}

rcp26conc = getco2(fn = 'knmi_climate_explorer/RCP3PD_MIDYEAR_CONCENTRATIONS.DAT')
rcp45conc = getco2('knmi_climate_explorer/RCP45_MIDYEAR_CONCENTRATIONS.DAT')
rcp60conc = getco2('knmi_climate_explorer/RCP6_MIDYEAR_CONCENTRATIONS.DAT')
rcp85conc = getco2('knmi_climate_explorer/RCP85_MIDYEAR_CONCENTRATIONS.DAT')


# Plot all RCPs on the same plot
pdf(file = 'all_rcps.pdf', width = 6, height = 7)
par(mfrow = c(2,1),las = 1, mar = c(0,5,2,1))

lwd = 3
xlim = c(1900, 2100)
ylim = c(280,940)
matplot(years, t(RCP85_tas.anom), type = 'l', lty = 'solid',
        col=col.rcp85,
        xlim = xlim,
        axes = FALSE, bty = 'n',
        ylab = expression(paste('Global mean temperature anomaly (',degree,'C)'))
        )
matlines(years, t(RCP60_tas.anom), type = 'l',lty = 'solid', col=col.rcp60)
matlines(years, t(RCP45_tas.anom), type = 'l',lty = 'solid', col=col.rcp45)
matlines(years, t(RCP26_tas.anom), type = 'l',lty = 'solid', col=col.rcp26)
abline(h = 1.5, col = 'black', lty = 'dotted', lwd = 2)
abline(h = 2, col = 'black', lty = 'dashed', lwd = 2)
text(1920, 1.7, labels = expression(paste('1.5',degree,'C')), 
     cex = 0.8, col = 'black',
     pos = 2)
text(1920, 2.2, labels = expression(paste('2',degree,'C')), cex = 0.8,
     col= 'black',
     pos = 2)
axis(2)
legend('top', legend = rev(c('RCP2.6', 'RCP4.5', 'RCP6.0', 'RCP8.5')),
       col = rev(c(col.rcp26, col.rcp45, col.rcp60, col.rcp85)), 
       text.col = rev(c(col.rcp26, col.rcp45, col.rcp60, col.rcp85)),
       lty = 'solid', 
       lwd = 2, bty = 'n')

par(mar = c(5,5,0,1))
plot(years, rcp85conc, type = 'l', col = col.rcp85, lwd = lwd,
     xlim = xlim,
     ylim = ylim,
     axes = FALSE, bty = 'n',
     xlab = 'Year',
     ylab = expression(paste('CO'[2],' conc. (ppm)'))
)
lines(years, rcp60conc, type = 'l', col = col.rcp60, lwd = lwd)
lines(years, rcp45conc, type = 'l', col = col.rcp45, lwd = lwd)
lines(years, rcp26conc, type = 'l', col = col.rcp26, lwd = lwd)
axis(1)
axis(2)

dev.off()


pdf(file = 'rcp26.pdf', width = 6, height = 7)
par(mfrow = c(2,1),las = 1, mar = c(0,5,2,1))

lwd = 3
xlim = c(1900, 2100)
matplot(years, t(RCP85_tas.anom),lty = 'solid',
        col=col.rcp85,
        xlim = xlim,
        axes = FALSE, bty = 'n',
        ylab = expression(paste('Global mean temperature anomaly (',degree,'C)')),
        type = 'n'
)
#matlines(years, t(RCP60_tas.anom), type = 'l',lty = 'solid', col=col.rcp60)
#matlines(years, t(RCP45_tas.anom), type = 'l',lty = 'solid', col=col.rcp45)
matlines(years, t(RCP26_tas.anom), type = 'l',lty = 'solid', col=col.rcp26)
abline(h = 1.5, col = 'black', lty = 'dotted', lwd = 2)
abline(h = 2, col = 'black', lty = 'dashed', lwd = 2)
text(1920, 1.7, labels = expression(paste('1.5',degree,'C')), 
     cex = 0.8, col = 'black',
     pos = 2)
text(1920, 2.2, labels = expression(paste('2',degree,'C')), cex = 0.8,
     col= 'black',
     pos = 2)
axis(2)
legend('top', legend = rev(c('RCP2.6', 'RCP4.5', 'RCP6.0', 'RCP8.5')),
       col = rev(c(col.rcp26, col.rcp45, col.rcp60, col.rcp85)), 
       text.col = rev(c(col.rcp26, col.rcp45, col.rcp60, col.rcp85)),
       lty = 'solid', 
       lwd = 2, bty = 'n')

par(mar = c(5,5,0,1))
plot(years, rcp85conc, type = 'n', col = col.rcp85, lwd = lwd,
     xlim = xlim,
     ylim = ylim,
     axes = FALSE, bty = 'n',
     xlab = 'Year',
     ylab = expression(paste('CO'[2],' conc. (ppm)'))
)
#lines(years, rcp60conc, type = 'l', col = col.rcp60, lwd = lwd)
#lines(years, rcp45conc, type = 'l', col = col.rcp45, lwd = lwd)
lines(years, rcp26conc, type = 'l', col = col.rcp26, lwd = lwd)
axis(1)
axis(2)

dev.off()



pdf(file = 'rcp45.pdf', width = 6, height = 7)
par(mfrow = c(2,1),las = 1, mar = c(0,5,2,1))

lwd = 3
xlim = c(1900, 2100)
matplot(years, t(RCP85_tas.anom),lty = 'solid',
        col=col.rcp85,
        xlim = xlim,
        axes = FALSE, bty = 'n',
        ylab = expression(paste('Global mean temperature anomaly (',degree,'C)')),
        type = 'n'
)
#matlines(years, t(RCP60_tas.anom), type = 'l',lty = 'solid', col=col.rcp60)
matlines(years, t(RCP45_tas.anom), type = 'l',lty = 'solid', col=col.rcp45)
#matlines(years, t(RCP26_tas.anom), type = 'l',lty = 'solid', col=col.rcp26)
abline(h = 1.5, col = 'black', lty = 'dotted', lwd = 2)
abline(h = 2, col = 'black', lty = 'dashed', lwd = 2)
text(1920, 1.7, labels = expression(paste('1.5',degree,'C')), 
     cex = 0.8, col = 'black',
     pos = 2)
text(1920, 2.2, labels = expression(paste('2',degree,'C')), cex = 0.8,
     col= 'black',
     pos = 2)
axis(2)
legend('top', legend = rev(c('RCP2.6', 'RCP4.5', 'RCP6.0', 'RCP8.5')),
       col = rev(c(col.rcp26, col.rcp45, col.rcp60, col.rcp85)), 
       text.col = rev(c(col.rcp26, col.rcp45, col.rcp60, col.rcp85)),
       lty = 'solid', 
       lwd = 2, bty = 'n')

par(mar = c(5,5,0,1))
plot(years, rcp85conc, type = 'n', col = col.rcp85, lwd = lwd,
     xlim = xlim,
     ylim = ylim,
     axes = FALSE, bty = 'n',
     xlab = 'Year',
     ylab = expression(paste('CO'[2],' conc. (ppm)'))
)
#lines(years, rcp60conc, type = 'l', col = col.rcp60, lwd = lwd)
lines(years, rcp45conc, type = 'l', col = col.rcp45, lwd = lwd)
#lines(years, rcp26conc, type = 'l', col = col.rcp26, lwd = lwd)
axis(1)
axis(2)

dev.off()



pdf(file = 'rcp60.pdf', width = 6, height = 7)
par(mfrow = c(2,1),las = 1, mar = c(0,5,2,1))

lwd = 3
xlim = c(1900, 2100)
matplot(years, t(RCP85_tas.anom),lty = 'solid',
        col=col.rcp85,
        xlim = xlim,
        axes = FALSE, bty = 'n',
        ylab = expression(paste('Global mean temperature anomaly (',degree,'C)')),
        type = 'n'
)
matlines(years, t(RCP60_tas.anom), type = 'l',lty = 'solid', col=col.rcp60)
#matlines(years, t(RCP45_tas.anom), type = 'l',lty = 'solid', col=col.rcp45)
#matlines(years, t(RCP26_tas.anom), type = 'l',lty = 'solid', col=col.rcp26)
abline(h = 1.5, col = 'black', lty = 'dotted', lwd = 2)
abline(h = 2, col = 'black', lty = 'dashed', lwd = 2)
text(1920, 1.7, labels = expression(paste('1.5',degree,'C')), 
     cex = 0.8, col = 'black',
     pos = 2)
text(1920, 2.2, labels = expression(paste('2',degree,'C')), cex = 0.8,
     col= 'black',
     pos = 2)
axis(2)
legend('top', legend = rev(c('RCP2.6', 'RCP4.5', 'RCP6.0', 'RCP8.5')),
       col = rev(c(col.rcp26, col.rcp45, col.rcp60, col.rcp85)), 
       text.col = rev(c(col.rcp26, col.rcp45, col.rcp60, col.rcp85)),
       lty = 'solid', 
       lwd = 2, bty = 'n')

par(mar = c(5,5,0,1))
plot(years, rcp85conc, type = 'n', col = col.rcp85, lwd = lwd,
     xlim = xlim,
     ylim = ylim,
     axes = FALSE, bty = 'n',
     xlab = 'Year',
     ylab = expression(paste('CO'[2],' conc. (ppm)'))
)
lines(years, rcp60conc, type = 'l', col = col.rcp60, lwd = lwd)
#lines(years, rcp45conc, type = 'l', col = col.rcp45, lwd = lwd)
#lines(years, rcp26conc, type = 'l', col = col.rcp26, lwd = lwd)
axis(1)
axis(2)

dev.off()


pdf(file = 'rcp85.pdf', width = 6, height = 7)
par(mfrow = c(2,1),las = 1, mar = c(0,5,2,1))

lwd = 3
xlim = c(1900, 2100)
matplot(years, t(RCP85_tas.anom),lty = 'solid',
        col=col.rcp85,
        xlim = xlim,
        axes = FALSE, bty = 'n',
        ylab = expression(paste('Global mean temperature anomaly (',degree,'C)')),
        type = 'l'
)
#matlines(years, t(RCP60_tas.anom), type = 'l',lty = 'solid', col=col.rcp60)
#matlines(years, t(RCP45_tas.anom), type = 'l',lty = 'solid', col=col.rcp45)
#matlines(years, t(RCP26_tas.anom), type = 'l',lty = 'solid', col=col.rcp26)
abline(h = 1.5, col = 'black', lty = 'dotted', lwd = 2)
abline(h = 2, col = 'black', lty = 'dashed', lwd = 2)
text(1920, 1.7, labels = expression(paste('1.5',degree,'C')), 
     cex = 0.8, col = 'black',
     pos = 2)
text(1920, 2.2, labels = expression(paste('2',degree,'C')), cex = 0.8,
     col= 'black',
     pos = 2)
axis(2)
legend('top', legend = rev(c('RCP2.6', 'RCP4.5', 'RCP6.0', 'RCP8.5')),
       col = rev(c(col.rcp26, col.rcp45, col.rcp60, col.rcp85)), 
       text.col = rev(c(col.rcp26, col.rcp45, col.rcp60, col.rcp85)),
       lty = 'solid', 
       lwd = 2, bty = 'n')

par(mar = c(5,5,0,1))
plot(years, rcp85conc, type = 'l', col = col.rcp85, lwd = lwd,
     xlim = xlim,
     ylim = ylim,
     axes = FALSE, bty = 'n',
     xlab = 'Year',
     ylab = expression(paste('CO'[2],' conc. (ppm)'))
)
#lines(years, rcp60conc, type = 'l', col = col.rcp60, lwd = lwd)
#lines(years, rcp45conc, type = 'l', col = col.rcp45, lwd = lwd)
#lines(years, rcp26conc, type = 'l', col = col.rcp26, lwd = lwd)
axis(1)
axis(2)

dev.off()

# Find where an ensemble crosses a threshold
findThresRolling = function(time, y, conc, k = 20, thres = 2, bp = 1861:1880){
  # smooth a timeseries with rollmean and find the point where
  # it crosses a threshold.
  # Baseline is calculated using only the years that are in the baseline period
  # - if not all those years are there, the mean is calculated using a shorter
  # baseline period.
  
  require(zoo)
  
  # find the index with the start of the baseline period
  bp.ix = which(round(time) %in% bp)
  bp.mean = mean(y[bp.ix], na.rm = TRUE)
  
  ysmooth = rollmean(y, k = k, na.pad = TRUE)
  #NonNAindex = which(!is.na(ysmooth))
  #firstNonNA = min(NonNAindex)
  #ysmooth.change = ysmooth - ysmooth[firstNonNA]
  ysmooth.change = ysmooth - bp.mean
  y.change = y - bp.mean
  #out = time[ min(which(ysmooth.change > thres))]
  thresyear = tryCatch(time[ min(which(ysmooth.change > thres))],
                       error=function(err) NA)
  
  thresconc = tryCatch(conc[ min(which(ysmooth.change > thres))],
                       error=function(err) NA)
  
  return(list(thresyear=thresyear, thresconc = thresconc, ysmooth = ysmooth,
              ysmooth.change = ysmooth.change,
              y.change = y.change))
}


time = c(as.numeric(colnames(RCP85_tas.anom)), recursive = TRUE)

test = findThresRolling(time = time, y= RCP85_tas.anom[3,],
                        conc = rcp85conc, k = 20, thres = 2, bp = 1861:1880 )

findThresRollingEnsemble = function(time, Y, conc, k = 20, thres = 2, bp = 1861:1880){
  
  nY = nrow(Y)
  thresyears = rep(NA, nY)
  thresconcs = rep(NA, nY)
  
  for(i in 1:nY){
    
    dat = findThresRolling(time = time, y = Y[i,],
                            conc = conc, k = k,
                            thres = thres, bp = bp)
    
    thresyears[i] = dat$thresyear
    thresconcs[i] = dat$thresconc
  }
  return(list(thresyears = thresyears, thresconcs = thresconcs) )
}
printthres = function(deg, max = FALSE){
  
  if(max) 
    ix = which.max(deg$thresyears) 
  else
    ix= which.min(deg$thresyears)
    
  print(deg$thresyears[ix])
  print(deg$thresconcs[ix])
  
}

savethres = function(deg, max = FALSE){
  if(max) 
    ix = which.max(deg$thresyears) 
  else
    ix= which.min(deg$thresyears)
  
  return(list(year = deg$thresyears[ix], conc = deg$thresconcs[ix]))
}

RCP85.thres2deg = findThresRollingEnsemble(time = time, Y= RCP85_tas.anom,
                        conc = rcp85conc, k = 20, thres = 2, bp = 1861:1880 )

RCP60.thres2deg = findThresRollingEnsemble(time = time, Y= RCP60_tas.anom,
                                       conc = rcp60conc, k = 20, thres = 2,
                                       bp = 1861:1880)

RCP45.thres2deg = findThresRollingEnsemble(time = time, Y= RCP45_tas.anom,
                                           conc = rcp45conc, k = 20, thres = 2,
                                           bp = 1861:1880)

RCP26.thres2deg = findThresRollingEnsemble(time = time, Y= RCP26_tas.anom,
                                           conc = rcp26conc, k = 20, thres = 2,
                                           bp = 1861:1880)

print('RCP85 2 degrees')
printthres(RCP85.thres2deg)
printthres(RCP85.thres2deg, max = TRUE)

print('RCP60 2 degrees')
printthres(RCP60.thres2deg)
printthres(RCP60.thres2deg, max = TRUE)

print('RCP45 2 degrees')
printthres(RCP45.thres2deg)
printthres(RCP45.thres2deg, max = TRUE)

print('RCP26 2 degrees')
printthres(RCP26.thres2deg)
printthres(RCP26.thres2deg, max = TRUE)


RCP85.thres1_5deg = findThresRollingEnsemble(time = time, Y= RCP85_tas.anom,
                                           conc = rcp85conc, k = 20, thres = 1.5, bp = 1861:1880 )

RCP60.thres1_5deg = findThresRollingEnsemble(time = time, Y= RCP60_tas.anom,
                                           conc = rcp60conc, k = 20, thres = 1.5,
                                           bp = 1861:1880)

RCP45.thres1_5deg = findThresRollingEnsemble(time = time, Y= RCP45_tas.anom,
                                           conc = rcp45conc, k = 20, thres = 1.5,
                                           bp = 1861:1880)

RCP26.thres1_5deg = findThresRollingEnsemble(time = time, Y= RCP26_tas.anom,
                                           conc = rcp26conc, k = 20, thres = 1.5,
                                           bp = 1861:1880)

print('RCP85 1.5 degrees')
printthres(RCP85.thres1_5deg)
printthres(RCP85.thres1_5deg, max = TRUE)

print('RCP60 1.5 degrees')
printthres(RCP60.thres1_5deg)
printthres(RCP60.thres1_5deg, max = TRUE)

print('RCP45 1.5 degrees')
printthres(RCP45.thres1_5deg)
printthres(RCP45.thres1_5deg, max = TRUE)

print('RCP26 1.5 degrees')
printthres(RCP26.thres1_5deg)
printthres(RCP26.thres1_5deg, max = TRUE)


pdf(file = 'rcp85_timing.pdf', width = 6, height = 7)
par(mfrow = c(2,1),las = 1, mar = c(0,5,2,1))

lwd = 3
xlim = c(1900, 2100)
matplot(years, t(RCP85_tas.anom),lty = 'solid',
        col=col.rcp85,
        xlim = xlim,
        axes = FALSE, bty = 'n',
        ylab = expression(paste('Global mean temperature anomaly (',degree,'C)')),
        type = 'l'
)

abline(v = savethres(RCP85.thres2deg)$year, col = 'black', lty = 'dashed', lwd = 2)
abline(v = savethres(RCP85.thres2deg, max = TRUE)$year, col = 'black', lty = 'dashed', lwd = 2)

abline(v = savethres(RCP85.thres1_5deg)$year, col = 'black', lty = 'dotted', lwd = 2)
abline(v = savethres(RCP85.thres1_5deg, max = TRUE)$year, col = 'black', lty = 'dotted', lwd = 2)

#matlines(years, t(RCP60_tas.anom), type = 'l',lty = 'solid', col=col.rcp60)
#matlines(years, t(RCP45_tas.anom), type = 'l',lty = 'solid', col=col.rcp45)
#matlines(years, t(RCP26_tas.anom), type = 'l',lty = 'solid', col=col.rcp26)
abline(h = 1.5, col = 'black', lty = 'dotted', lwd = 2)
abline(h = 2, col = 'black', lty = 'dashed', lwd = 2)
text(1920, 1.7, labels = expression(paste('1.5',degree,'C')), 
     cex = 0.8, col = 'black',
     pos = 2)
text(1920, 2.2, labels = expression(paste('2',degree,'C')), cex = 0.8,
     col= 'black',
     pos = 2)
axis(2)
legend('top', legend = rev(c('RCP2.6', 'RCP4.5', 'RCP6.0', 'RCP8.5')),
       col = rev(c(col.rcp26, col.rcp45, col.rcp60, col.rcp85)), 
       text.col = rev(c(col.rcp26, col.rcp45, col.rcp60, col.rcp85)),
       lty = 'solid', 
       lwd = 2, bty = 'n')

par(mar = c(5,5,0,1))
plot(years, rcp85conc, type = 'l', col = col.rcp85, lwd = lwd,
     xlim = xlim,
     ylim = ylim,
     axes = FALSE, bty = 'n',
     xlab = 'Year',
     ylab = expression(paste('CO'[2],' conc. (ppm)'))
)
abline(v = savethres(RCP85.thres2deg)$year, col = 'black', lty = 'dashed', lwd = 2)
abline(v = savethres(RCP85.thres2deg, max = TRUE)$year, col = 'black', lty = 'dashed', lwd = 2)

abline(v = savethres(RCP85.thres1_5deg)$year, col = 'black', lty = 'dotted', lwd = 2)
abline(v = savethres(RCP85.thres1_5deg, max = TRUE)$year, col = 'black', lty = 'dotted', lwd = 2)

abline(h = savethres(RCP85.thres2deg)$conc, col = 'black', lty = 'dashed', lwd = 2)
abline(h = savethres(RCP85.thres2deg, max = TRUE)$conc, col = 'black', lty = 'dashed', lwd = 2)

abline(h = savethres(RCP85.thres1_5deg)$conc, col = 'black', lty = 'dotted', lwd = 2)
abline(h = savethres(RCP85.thres1_5deg, max = TRUE)$conc, col = 'black', lty = 'dotted', lwd = 2)

#lines(years, rcp60conc, type = 'l', col = col.rcp60, lwd = lwd)
#lines(years, rcp45conc, type = 'l', col = col.rcp45, lwd = lwd)
#lines(years, rcp26conc, type = 'l', col = col.rcp26, lwd = lwd)
axis(1)
axis(2)

dev.off()



pdf(file = 'rcp60_timing.pdf', width = 6, height = 7)
par(mfrow = c(2,1),las = 1, mar = c(0,5,2,1))

lwd = 3
xlim = c(1900, 2100)
matplot(years, t(RCP85_tas.anom),lty = 'solid',
        col=col.rcp85,
        xlim = xlim,
        axes = FALSE, bty = 'n',
        ylab = expression(paste('Global mean temperature anomaly (',degree,'C)')),
        type = 'n'
)

matlines(years, t(RCP60_tas.anom), type = 'l',lty = 'solid', col=col.rcp60)
#matlines(years, t(RCP45_tas.anom), type = 'l',lty = 'solid', col=col.rcp45)
#matlines(years, t(RCP26_tas.anom), type = 'l',lty = 'solid', col=col.rcp26)

abline(v = savethres(RCP60.thres2deg)$year, col = 'black', lty = 'dashed', lwd = 2)
abline(v = savethres(RCP60.thres2deg, max = TRUE)$year, col = 'black', lty = 'dashed', lwd = 2)

abline(v = savethres(RCP60.thres1_5deg)$year, col = 'black', lty = 'dotted', lwd = 2)
abline(v = savethres(RCP60.thres1_5deg, max = TRUE)$year, col = 'black', lty = 'dotted', lwd = 2)


abline(h = 1.5, col = 'black', lty = 'dotted', lwd = 2)
abline(h = 2, col = 'black', lty = 'dashed', lwd = 2)
text(1920, 1.7, labels = expression(paste('1.5',degree,'C')), 
     cex = 0.8, col = 'black',
     pos = 2)
text(1920, 2.2, labels = expression(paste('2',degree,'C')), cex = 0.8,
     col= 'black',
     pos = 2)
axis(2)
legend('top', legend = rev(c('RCP2.6', 'RCP4.5', 'RCP6.0', 'RCP8.5')),
       col = rev(c(col.rcp26, col.rcp45, col.rcp60, col.rcp85)), 
       text.col = rev(c(col.rcp26, col.rcp45, col.rcp60, col.rcp85)),
       lty = 'solid', 
       lwd = 2, bty = 'n')

par(mar = c(5,5,0,1))
plot(years, rcp60conc, type = 'l', col = col.rcp60, lwd = lwd,
     xlim = xlim,
     ylim = ylim,
     axes = FALSE, bty = 'n',
     xlab = 'Year',
     ylab = expression(paste('CO'[2],' conc. (ppm)'))
)
abline(v = savethres(RCP60.thres2deg)$year, col = 'black', lty = 'dashed', lwd = 2)
abline(v = savethres(RCP60.thres2deg, max = TRUE)$year, col = 'black', lty = 'dashed', lwd = 2)

abline(v = savethres(RCP60.thres1_5deg)$year, col = 'black', lty = 'dotted', lwd = 2)
abline(v = savethres(RCP60.thres1_5deg, max = TRUE)$year, col = 'black', lty = 'dotted', lwd = 2)

abline(h = savethres(RCP60.thres2deg)$conc, col = 'black', lty = 'dashed', lwd = 2)
abline(h = savethres(RCP60.thres2deg, max = TRUE)$conc, col = 'black', lty = 'dashed', lwd = 2)

abline(h = savethres(RCP60.thres1_5deg)$conc, col = 'black', lty = 'dotted', lwd = 2)
abline(h = savethres(RCP60.thres1_5deg, max = TRUE)$conc, col = 'black', lty = 'dotted', lwd = 2)

#lines(years, rcp60conc, type = 'l', col = col.rcp60, lwd = lwd)
#lines(years, rcp45conc, type = 'l', col = col.rcp45, lwd = lwd)
#lines(years, rcp26conc, type = 'l', col = col.rcp26, lwd = lwd)
axis(1)
axis(2)

dev.off()


pdf(file = 'rcp45_timing.pdf', width = 6, height = 7)
par(mfrow = c(2,1),las = 1, mar = c(0,5,2,1))

lwd = 3
xlim = c(1900, 2100)
matplot(years, t(RCP85_tas.anom),lty = 'solid',
        col=col.rcp85,
        xlim = xlim,
        axes = FALSE, bty = 'n',
        ylab = expression(paste('Global mean temperature anomaly (',degree,'C)')),
        type = 'n'
)

#matlines(years, t(RCP60_tas.anom), type = 'l',lty = 'solid', col=col.rcp60)
matlines(years, t(RCP45_tas.anom), type = 'l',lty = 'solid', col=col.rcp45)
#matlines(years, t(RCP26_tas.anom), type = 'l',lty = 'solid', col=col.rcp26)

abline(v = savethres(RCP45.thres2deg)$year, col = 'black', lty = 'dashed', lwd = 2)
abline(v = savethres(RCP45.thres2deg, max = TRUE)$year, col = 'black', lty = 'dashed', lwd = 2)

abline(v = savethres(RCP45.thres1_5deg)$year, col = 'black', lty = 'dotted', lwd = 2)
abline(v = savethres(RCP45.thres1_5deg, max = TRUE)$year, col = 'black', lty = 'dotted', lwd = 2)


abline(h = 1.5, col = 'black', lty = 'dotted', lwd = 2)
abline(h = 2, col = 'black', lty = 'dashed', lwd = 2)
text(1920, 1.7, labels = expression(paste('1.5',degree,'C')), 
     cex = 0.8, col = 'black',
     pos = 2)
text(1920, 2.2, labels = expression(paste('2',degree,'C')), cex = 0.8,
     col= 'black',
     pos = 2)
axis(2)
legend('top', legend = rev(c('RCP2.6', 'RCP4.5', 'RCP6.0', 'RCP8.5')),
       col = rev(c(col.rcp26, col.rcp45, col.rcp60, col.rcp85)), 
       text.col = rev(c(col.rcp26, col.rcp45, col.rcp60, col.rcp85)),
       lty = 'solid', 
       lwd = 2, bty = 'n')

par(mar = c(5,5,0,1))
plot(years, rcp45conc, type = 'l', col = col.rcp45, lwd = lwd,
     xlim = xlim,
     ylim = ylim,
     axes = FALSE, bty = 'n',
     xlab = 'Year',
     ylab = expression(paste('CO'[2],' conc. (ppm)'))
)
abline(v = savethres(RCP45.thres2deg)$year, col = 'black', lty = 'dashed', lwd = 2)
abline(v = savethres(RCP45.thres2deg, max = TRUE)$year, col = 'black', lty = 'dashed', lwd = 2)

abline(v = savethres(RCP45.thres1_5deg)$year, col = 'black', lty = 'dotted', lwd = 2)
abline(v = savethres(RCP45.thres1_5deg, max = TRUE)$year, col = 'black', lty = 'dotted', lwd = 2)

abline(h = savethres(RCP45.thres2deg)$conc, col = 'black', lty = 'dashed', lwd = 2)
abline(h = savethres(RCP45.thres2deg, max = TRUE)$conc, col = 'black', lty = 'dashed', lwd = 2)

abline(h = savethres(RCP45.thres1_5deg)$conc, col = 'black', lty = 'dotted', lwd = 2)
abline(h = savethres(RCP45.thres1_5deg, max = TRUE)$conc, col = 'black', lty = 'dotted', lwd = 2)

#lines(years, rcp60conc, type = 'l', col = col.rcp60, lwd = lwd)
#lines(years, rcp45conc, type = 'l', col = col.rcp45, lwd = lwd)
#lines(years, rcp26conc, type = 'l', col = col.rcp26, lwd = lwd)
axis(1)
axis(2)

dev.off()


pdf(file = 'rcp26_timing.pdf', width = 6, height = 7)
par(mfrow = c(2,1),las = 1, mar = c(0,5,2,1))

lwd = 3
xlim = c(1900, 2100)
matplot(years, t(RCP85_tas.anom),lty = 'solid',
        col=col.rcp85,
        xlim = xlim,
        axes = FALSE, bty = 'n',
        ylab = expression(paste('Global mean temperature anomaly (',degree,'C)')),
        type = 'n'
)

#matlines(years, t(RCP60_tas.anom), type = 'l',lty = 'solid', col=col.rcp60)
#matlines(years, t(RCP45_tas.anom), type = 'l',lty = 'solid', col=col.rcp45)
matlines(years, t(RCP26_tas.anom), type = 'l',lty = 'solid', col=col.rcp26)

abline(v = savethres(RCP26.thres2deg)$year, col = 'black', lty = 'dashed', lwd = 2)
abline(v = savethres(RCP26.thres2deg, max = TRUE)$year, col = 'black', lty = 'dashed', lwd = 2)

abline(v = savethres(RCP26.thres1_5deg)$year, col = 'black', lty = 'dotted', lwd = 2)
abline(v = savethres(RCP26.thres1_5deg, max = TRUE)$year, col = 'black', lty = 'dotted', lwd = 2)


abline(h = 1.5, col = 'black', lty = 'dotted', lwd = 2)
abline(h = 2, col = 'black', lty = 'dashed', lwd = 2)
text(1920, 1.7, labels = expression(paste('1.5',degree,'C')), 
     cex = 0.8, col = 'black',
     pos = 2)
text(1920, 2.2, labels = expression(paste('2',degree,'C')), cex = 0.8,
     col= 'black',
     pos = 2)
axis(2)
legend('top', legend = rev(c('RCP2.6', 'RCP4.5', 'RCP6.0', 'RCP8.5')),
       col = rev(c(col.rcp26, col.rcp45, col.rcp60, col.rcp85)), 
       text.col = rev(c(col.rcp26, col.rcp45, col.rcp60, col.rcp85)),
       lty = 'solid', 
       lwd = 2, bty = 'n')

par(mar = c(5,5,0,1))
plot(years, rcp26conc, type = 'l', col = col.rcp45, lwd = lwd,
     xlim = xlim,
     ylim =ylim,
     axes = FALSE, bty = 'n',
     xlab = 'Year',
     ylab = expression(paste('CO'[2],' conc. (ppm)'))
)
abline(v = savethres(RCP26.thres2deg)$year, col = 'black', lty = 'dashed', lwd = 2)
abline(v = savethres(RCP26.thres2deg, max = TRUE)$year, col = 'black', lty = 'dashed', lwd = 2)

abline(v = savethres(RCP26.thres1_5deg)$year, col = 'black', lty = 'dotted', lwd = 2)
abline(v = savethres(RCP26.thres1_5deg, max = TRUE)$year, col = 'black', lty = 'dotted', lwd = 2)

abline(h = savethres(RCP26.thres2deg)$conc, col = 'black', lty = 'dashed', lwd = 2)
abline(h = savethres(RCP26.thres2deg, max = TRUE)$conc, col = 'black', lty = 'dashed', lwd = 2)

abline(h = savethres(RCP26.thres1_5deg)$conc, col = 'black', lty = 'dotted', lwd = 2)
abline(h = savethres(RCP26.thres1_5deg, max = TRUE)$conc, col = 'black', lty = 'dotted', lwd = 2)

#lines(years, rcp60conc, type = 'l', col = col.rcp60, lwd = lwd)
#lines(years, rcp45conc, type = 'l', col = col.rcp45, lwd = lwd)
#lines(years, rcp26conc, type = 'l', col = col.rcp26, lwd = lwd)
axis(1)
axis(2)

dev.off()


# Carbon Brief article
# This version of RCP8.5 for a carbon brief article
#
pdf(file = 'rcp85_timing_CarbonBrief.pdf', width = 6, height = 7)
par(mfrow = c(2,1),las = 1, mar = c(0,5,2,1))

lwd = 3
xlim = c(1900, 2100)
matplot(years, t(RCP85_tas.anom),lty = 'solid',
        col=col.rcp85,
        xlim = xlim,
        axes = FALSE, bty = 'n',
        ylab = expression(paste('Global mean temperature change (',degree,'C)')),
        type = 'l'
)

#abline(v = savethres(RCP85.thres2deg)$year, col = 'black', lty = 'dashed', lwd = 2)
#abline(v = savethres(RCP85.thres2deg, max = TRUE)$year, col = 'black', lty = 'dashed', lwd = 2)

abline(v = savethres(RCP85.thres1_5deg)$year, col = 'grey', lty = 'dashed', lwd = 2)
abline(v = savethres(RCP85.thres1_5deg, max = TRUE)$year, col = 'grey', lty = 'dashed', lwd = 2)

#matlines(years, t(RCP60_tas.anom), type = 'l',lty = 'solid', col=col.rcp60)
#matlines(years, t(RCP45_tas.anom), type = 'l',lty = 'solid', col=col.rcp45)
#matlines(years, t(RCP26_tas.anom), type = 'l',lty = 'solid', col=col.rcp26)
abline(h = 1.5, col = 'grey', lty = 'dashed', lwd = 2)
#abline(h = 2, col = 'black', lty = 'dashed', lwd = 2)
text(1920, 1.7, labels = expression(paste('1.5',degree,'C')), 
     cex = 0.8, col = 'black',
     pos = 2)
#text(1920, 2.2, labels = expression(paste('2',degree,'C')), cex = 0.8,
#     col= 'black',
#     pos = 2)
axis(2)

par(mar = c(5,5,0,1))
plot(years, rcp85conc, type = 'l', col = col.rcp85, lwd = lwd,
     xlim = xlim,
     ylim = ylim,
     axes = FALSE, bty = 'n',
     xlab = 'Year',
     ylab = expression(paste('CO'[2],' concentration (ppm)'))
)
#abline(v = savethres(RCP85.thres2deg)$year, col = 'black', lty = 'dashed', lwd = 2)
#abline(v = savethres(RCP85.thres2deg, max = TRUE)$year, col = 'black', lty = 'dashed', lwd = 2)

abline(v = savethres(RCP85.thres1_5deg)$year, col = 'grey', lty = 'dashed', lwd = 2)
abline(v = savethres(RCP85.thres1_5deg, max = TRUE)$year, col = 'grey', lty = 'dashed', lwd = 2)

#abline(h = savethres(RCP85.thres2deg)$conc, col = 'black', lty = 'dashed', lwd = 2)
#abline(h = savethres(RCP85.thres2deg, max = TRUE)$conc, col = 'black', lty = 'dashed', lwd = 2)

abline(h = savethres(RCP85.thres1_5deg)$conc, col = 'grey', lty = 'dashed', lwd = 2)
abline(h = savethres(RCP85.thres1_5deg, max = TRUE)$conc, col = 'grey', lty = 'dashed', lwd = 2)

#lines(years, rcp60conc, type = 'l', col = col.rcp60, lwd = lwd)
#lines(years, rcp45conc, type = 'l', col = col.rcp45, lwd = lwd)
#lines(years, rcp26conc, type = 'l', col = col.rcp26, lwd = lwd)
axis(1)
axis(2)

dev.off()








