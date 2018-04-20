# find_threshold_edr.R
# Find which year emissions driven runs of CMIP5 climate models
# pass thresholds of 1.5 and 2 degrees warming.

library(zoo)

findThres = function(time, x, f = 1/5, thres = 2, y.ix = 1:30){
  # smooth a timeseries and find the point where
  # it crosses a threshold.
  ysmooth = lowess(x = x, f = f)
  ysmooth.start = mean(ysmooth$y[y.ix])
  ysmooth.change = ysmooth$y - ysmooth.start
  out = time[ min(which(ysmooth.change > thres))]
  out
}

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

modlist = dir('booth')
nmod = length(modlist)

edr_15_year = rep(NA, length = nmod)
edr_15_conc = rep(NA, length = nmod)
edr_2_year  = rep(NA, length = nmod)
edr_2_conc  = rep(NA, length = nmod)


#tas = c(dat$time, recursive = TRUE)
tas = NULL

for (i in 1:nmod){
  dat = read.table(paste0('booth/', modlist[i]), header = TRUE)
  edr_15_year[i] = findThresRolling(time = dat$time,y = dat$tas, conc = dat$CO2, thres = 1.5)$thresyear
  edr_15_conc[i] = findThresRolling(time = dat$time, y = dat$tas, conc = dat$CO2, thres = 1.5)$thresconc
  edr_2_year[i] =  findThresRolling(time = dat$time, y = dat$tas, conc = dat$CO2, thres = 2)$thresyear
  edr_2_conc[i] =  findThresRolling(time = dat$time, y = dat$tas, conc = dat$CO2, thres = 2)$thresconc
  
}

# removes part of string after the dot
modname = gsub("\\..*","", modlist)

passyears = data.frame(cbind(modname,round(edr_15_year),edr_15_conc,
                             round(edr_2_year), edr_2_conc ))
colnames(passyears) = c('modname', "year_1.5","conc_1.5", "year_2", "conc_2")

print(passyears)

write.table(passyears, file = 'passyears.csv', row.names = FALSE, quote = FALSE, sep = ",")

alltime = 1851:2100

pdf(file = 'emission_driven_temps.pdf')
par(las = 1)
plot(alltime, seq(from = -1, to = 6, length.out = length(alltime)),
     ylim = c(-1, 6),
     type = 'n',
     ylab = 'degrees C')

for (i in 1:nmod){
  dat = read.table(paste0('booth/', modlist[i]), header = TRUE)
  #lines(dat$time, dat$tas, col = 'grey')
  sdat = findThresRolling(time = dat$time, y = dat$tas, thres = 1.5)

  yc = sdat$y.change
  ys = sdat$ysmooth.change
  lines(dat$time, yc, col = 'grey')
  lines(dat$time, ys, col = 'black')
  
}

abline(h = c(1.5, 2), col = 'tomato', lty = 'dashed')
dev.off()




