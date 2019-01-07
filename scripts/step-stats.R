#!/bin/env Rscript

SameOrNot <- function (item, tid_index, pid_index) {
  clsi <- item@cluster
  if(clsi[tid_index] == clsi[pid_index]) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

if(!is.null(kshapeResult$val)) {
  # is a list or not
  klist <- kshapeResult$val
  protein_ids <- rownames(cprotMatrix$val)
  trans_ids <- rownames(ctransMatrix$val)
  results_names <- c()
  results_same <- c()
  results_tcls <- c()
  results_pcls <- c()
  for(pid in protein_ids) {
    name_tmp <- substr(pid,start=6,stop=nchar(pid))
    tid = paste("trans", name_tmp, sep="-")
    if(tid %in% trans_ids) { # judge if in the same cluster
      dln <- names(klist[[1]]@datalist)
      tid_index <- which(dln==tid)
      pid_index <- which(dln==pid)
      results <- sapply(klist, SameOrNot, tid_index, pid_index)
      same_tmp <- sum(results)/length(results)
      results_names <- c(results_names,name_tmp)
      results_same <- c(results_same,same_tmp)
      results_tcls <- c(results_tcls,klist[[1]]@cluster[tid_index])
      results_pcls <- c(results_pcls,klist[[1]]@cluster[pid_index])
    }
  }
  
  tmp <- data.frame(id=results_names,same=results_same,
                                trans_cluster=results_tcls,prot_cluster=results_pcls)
  
  ### annotation **************************
  if(!is.null(annotationTb$val)) {
    statsResult$val <- merge(tmp, annotationTb$val, by="id", all.x=T) 
  } else {
    statsResult$val <- tmp
  }

  #cat(file=stderr(), str(statsResult$val))
  
}
