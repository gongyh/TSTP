#!/bin/env Rscript

if("QC" %in% isolate(listSteps)) {

  ttp <- colnames(transMatrix$val)
  ptp <- colnames(protMatrix$val)
  
  # get common time points
  commontp <- intersect(ttp, ptp)
  
  ct1 <- transMatrix$val[,commontp]
  cp1 <- protMatrix$val[,commontp]
  
  # filter nan rows
  ct2 <- na.omit(ct1)
  cp2 <- na.omit(cp1)
  
  # filter ***
  ctransMatrix$val <- ct2
  cprotMatrix$val <- cp2
  
  tg <- rownames(ctransMatrix$val)
  pg <- rownames(cprotMatrix$val)
  
  commong <- intersect(tg, pg)
  
  QCSummaryInfo$timepoints <- length(commontp)
  QCSummaryInfo$sameItems <- length(commong)

} else{
  
  ctransMatrix$val <- transMatrix$val
  cprotMatrix$val <- protMatrix$val
  
}

rownames(ctransMatrix$val) <- paste("trans", rownames(ctransMatrix$val), sep="-")
rownames(cprotMatrix$val) <- paste("prot", rownames(cprotMatrix$val), sep="-")
