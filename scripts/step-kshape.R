#!/bin/env Rscript

if("kshape" %in% isolate(listSteps)) {
  
  workers <- makeCluster(input$num_workers)
  invisible(clusterEvalQ(workers, library("dtwclust")))
  registerDoParallel(workers)
  
  # construct data
  data <- zscore(rbind(ctransMatrix$val,cprotMatrix$val))
  
  pc_ks <- tsclust(data, k=input$num_clusters, distance="sbd", centroid="shape", seed=NULL,trace=TRUE, 
                   control = partitional_control(pam.precompute = TRUE, iter.max = 1000L, nrep = input$num_iter))
  
  stopCluster(workers)
  registerDoSEQ()
  
  if(is.list(pc_ks)) {
    kshapeResult$val <- pc_ks
  } else {
    kshapeResult$val <- list()
    kshapeResult$val[[1]] <- pc_ks
  }
  
  y <- c()
  for (data in kshapeResult$val[[1]]@datalist) {
    y <- rbind(y, data)
  }
  rownames(y) <- names(kshapeResult$val[[1]]@datalist)
  ydf <- as.data.frame(y)
  ydf$cluster <- kshapeResult$val[[1]]@cluster
  ydf$id <- row.names(ydf)
  #cat(file=stderr(), str(ydf))
  dispResult$val <- melt(ydf,id.vars=c("id","cluster"))
  
} else {
  
}