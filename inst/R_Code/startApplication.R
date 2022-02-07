# 00 Preparation ---------------------------------------------------------------
cat("System information:\n")
for (i in seq_along(sysinfo <- Sys.info()))
  cat("  ", names(sysinfo)[i], ":", sysinfo[i], "\n")

library("PlotR")
library("methods")

sessionInfo()

# futile.logger::flog.threshold(futile.logger::DEBUG)

startApplication(port = 3838)
