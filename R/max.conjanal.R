#' Maximum utility method
#'
#' Maximum utility method to estimate market shares
#' @param conjanal A conjanal object
#' @param simp A data frame of scenarios
#' @return A matrix of scenarios and the estimated market shares
#' @export
maxconj<-function(conjanal,simp) {
  fit.conj<-conjanal$lm
  #predicted unility
  suppressWarnings(
    ut.conj<-t(t(predict(fit.conj,newdata=simp)))
  )
  #maximum utility method
  m.max<-matrix(rep(0,nrow(simp)*ncol(ut.conj)),
                nrow=nrow(simp),byrow=T)
  m.first<-apply(ut.conj,MARGIN=2,which.max)
  max.conj<-prop.table(
    table(factor(m.first, levels = 1:nrow(ut.conj))
    ))
  max.conj<- cbind(simp,share=as.vector(max.conj))
  return(max.conj)
}
