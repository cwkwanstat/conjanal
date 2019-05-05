
r.conj<-function(part) {
  if (is.vector(part)) {
    range.conj<-diff(range(part))
  }
  else {
    range.conj<-apply(part,MARGIN=1,FUN=function(x) diff(range(x)))
  }
  return(range.conj)
}


conj<-function(resp,profile,monotone,tor,maxit) {
  #monotone transformation
  if (monotone) {resp<-mono.t(resp,profile,tor,maxit)}
  iv<-paste(colnames(profile),collapse="+")
  model<-paste('resp~',iv)
  c.model<-as.formula(model)
  #linear model
  fit.conj<-lm(c.model,data=profile)

  #part-worth
  part.conj<-dummy.coef(fit.conj)
  part.conj[[1]]<-NULL

  #range of each factor
  range.conj<-lapply(part.conj,r.conj)

  m.range<-matrix(unlist(range.conj),ncol=ncol(resp),byrow=T)
  colnames(m.range)<-colnames(fit.conj$coefficients)
  rownames(m.range)<-names(fit.conj$xlevels)

  #relative importance
  m.imp<-t(t(m.range)/colSums(m.range))
  #part-worth into matrix
  #print(part.conj)
  if (ncol(resp)==1) {
    m.part<-t(t(do.call("c",part.conj)))
  } else {
    m.part<-t(do.call(cbind,part.conj))
  }

  conj<-list(lm=fit.conj,part=m.part,imp=m.imp)
  return(conj)
}

mono.t<-function(resp,profile,tor=1e-8,maxit=10) {
  # monotone transformation
  p<-ncol(resp)
  m.iv<-paste(colnames(profile),collapse="+")
  m.model<-paste('m.y~',m.iv)
  resp.m<-resp

  for (i in 1:p) {
    m.y<-resp[,i]
    m.formula<-as.formula(m.model)
    m.fit<-lm(m.formula,data=profile)
    est0<-m.fit$coefficients
    itn<-0
    repeat {
      itn<-itn+1
      m.y<-opscale(resp[,i],m.fit$fitted.values,level=2)$os
      m.formula<-as.formula(m.model)
      m.fit<-lm(m.formula,data=profile)
      err<-sum((est0-m.fit$coefficients)^2)/length(est0)
      if (err<tor | itn>maxit) {break}
      est0<-m.fit$coefficients
    }
    if (itn>=maxit) {print("convergence not met")}
    resp.m[,i]<-m.y
  }
  return(resp.m)
}

#' Conjoint analysis
#'
#' Metric and monotonic conjoint analysis for categorical factors
#'
#' @author Chi-wai Kwan
#'
#' @import optiscale
#'
#' @param resp Response matrix where rows are profiles and columnes are responses from respondents.
#' Small value for less preferable and large value for more preferable.
#' @param profile Profile matrix where rows are profiles and columens are factors.
#' @param monotone TRUE for Kruskal monotonic transformation. FALSE for metric scale.
#' @param tor Tolerane for monotonic transformation.
#' @param maxit Maximum number of itneractions for monotonic transformation.
#' @return A conjanal object
#' \item{lm }{lm object. Results of the linear regression models}
#' \item{part }{part worth for each level of each factor of each respondent}
#' \item{imp }{Relative importance of each factor for each respondent}
#' @details
#' Metric conjoint analysis:\cr
#' Linear regression model \cr \cr
#' Monotonic conjoint analysis: \cr
#' Kruskal monotonic transformation is applied to each response column by opscale(level=2,...) in optiscale package.
#' Linear regression is applied to the transformed responses.
#'
#' @examples
#' library(conjanal)
#' contrasts(mydata$f1)
#' contrasts(mydata$f2)
#' contrasts(mydata$f3)
#'
#' #define response matrix
#' resp<-as.matrix(mydata[,4:5])
#'
#' #define profiles
#' profile<-mydata[,1:3]
#'
#' fit.conj<-conjanal(resp=resp,profile=profile,monotone = TRUE)
#' summary(fit.conj$lm)
#'
#' #part-worth
#' fit.conj$part
#'
#' #relative importance
#' fit.conj$imp
#'
#' #predicted utilities
#' cbind(profile,pred=predict(fit.conj$lm,newdata=profile))
#'
#' #estimation of market shares
#' maxconj(fit.conj,csimp)
#' btlconj(fit.conj,csimp)
#' logitconj(fit.conj,csimp)
#' @export
conjanal<-function(resp,profile,monotone=F,tor=1e-10,maxit=50) {
  value<-conj(resp=resp,profile=profile,monotone=monotone,tor=tor,maxit=maxit)
  attr(value, "class") <- "conjanal"
  value
}
