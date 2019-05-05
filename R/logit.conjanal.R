#' Logit method
#'
#' Logit method to estimate market shares
#' @param conjanal A conjanal object
#' @param simp A data frame of scenarios
#' @return A matrix of scenarios and the estimated market shares
#' @export
logitconj<-function(conjanal,simp) {
  fit.conj<-conjanal$lm
  #predicted unility
  suppressWarnings(
    ut.conj<-t(t(predict(fit.conj,newdata=simp)))
  )
  #logit model
  eu<-exp(ut.conj)
  m.logit<-t(t(eu)/colSums(eu))
  logit.conj<-rowMeans(m.logit)
  logit.conj<-cbind(simp,share=logit.conj)
  return(logit.conj)
}
