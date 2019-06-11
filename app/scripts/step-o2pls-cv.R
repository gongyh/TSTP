#!/bin/env Rscript

t1 <- scale(t(as.matrix(transMatrix$val)), scale=F)
p1 <- scale(t(as.matrix(protMatrix$val)), scale=F)
cv <- crossval_o2m(t1, p1,
                   str_eval(paste0("c(",isolate({input$num_n}),")")),
                   str_eval(paste0("c(",isolate({input$num_nx}),")")),
                   str_eval(paste0("c(",isolate({input$num_ny}),")")),
                   isolate({input$num_folds}),isolate({input$num_cores}))

cat(file=stderr(),cv)
