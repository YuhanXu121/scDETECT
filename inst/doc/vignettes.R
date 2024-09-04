## ----knitr-options, include = FALSE-------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(scDETECT)

## -----------------------------------------------------------------------------
N <- 40
K <- 3
C <- 10
P <- 500
design <- data.frame(disease=factor(sample(0:1,size = N,replace=TRUE)))
Y = list()
res = list()
for (i in 1:K){
  y.pseudo = c()
  for (j in 1:N){
    y = matrix(rnbinom(P*C, size = 1, mu = 1), nrow = P, byrow = FALSE)
    y.pseudo = cbind(y.pseudo, rowSums(y))
  }
  Y[[i]] = y.pseudo
  rownames(Y[[i]]) <- paste0('gene',1:P)
}

## -----------------------------------------------------------------------------
res_scDETECT <- scDETECT(Y_raw = Y, 
                         design.1 = design,
                         design.2 = NULL,
                         factor.to.test = 'disease',
                         cutoff.tree = c('tstat',2.58),
                         cutoff.prior.prob = c('pval',0.01))

## -----------------------------------------------------------------------------
head(res_scDETECT$tree_res$full$pp)

## -----------------------------------------------------------------------------
res_scDETECT$tree_res$full$tree_structure

## -----------------------------------------------------------------------------
res_scDETECT$tree_res$single$tree_structure

## ----sessionInfo, echo=FALSE--------------------------------------------------
sessionInfo()

