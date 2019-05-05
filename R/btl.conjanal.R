#' BTL method
#'
#' BTL method to estimate market shares
#' @param conjanal A conjanal object
#' @param simp A data frame of scenarios
#' @return A matrix of scenarios and the estimated market shares
#' @export
btlconj<-function(conjanal,simp) {
  fit.conj<-conjanal$lm
  #predicted unility
  suppressWarnings(
    ut.conj<-t(t(predict(fit.conj,newdata=simp)))
  )
  #BTL model
  m.btl<-t(t(ut.conj)/colSums(ut.conj))
  btl.conj<-rowMeans(m.btl)
  btl.conj<-cbind(simp,share=btl.conj)
  return(btl.conj)
}
