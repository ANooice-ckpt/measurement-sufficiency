# Every feature gets the same additive nonlinear basis (three-df natural spline)
# plus a missingness indicator. Knots, imputation and scaling are training-only.
# No metric/transition-specific feature construction, selection or tuning.
rq2_risk_design <- function(tr, te, features) {
  a <- b <- list()
  for (p in features) {
    x <- as.numeric(tr[[p]]); y <- as.numeric(te[[p]])
    ok <- is.finite(x); if (sum(ok) < 3L || length(unique(x[ok])) < 2L) next
    med <- median(x[ok]); missing_x <- !ok; missing_y <- !is.finite(y)
    x[missing_x] <- med; y[missing_y] <- med
    knots <- unique(as.numeric(quantile(x,c(1/3,2/3))))
    knots <- knots[knots > min(x) & knots < max(x)]
    if (length(unique(x)) >= 5L && length(knots)) {
      basis <- splines::ns(x, knots = knots, Boundary.knots = range(x), intercept = FALSE)
      test <- predict(basis, y)
    } else {basis <- matrix(x, ncol=1); test <- matrix(y,ncol=1)}
    if (any(missing_x)) {basis <- cbind(basis, missing_x); test <- cbind(test, missing_y)}
    mu <- colMeans(basis); ss <- apply(basis,2,sd); keep <- is.finite(ss) & ss > 1e-8
    a[[p]] <- sweep(sweep(basis[,keep,drop=FALSE],2,mu[keep]),2,ss[keep],"/")
    b[[p]] <- sweep(sweep(test[,keep,drop=FALSE],2,mu[keep]),2,ss[keep],"/")
  }
  if (!length(a)) return(list(train=matrix(0,nrow(tr),0),test=matrix(0,nrow(te),0)))
  list(train=do.call(cbind,a),test=do.call(cbind,b))
}

# Fixed effective degrees of freedom: identical complexity across information sets.
# Multiresponse ridge learns mean risk and exceedance probabilities under squared
# loss (Brier loss for binary targets); no target-dependent hyperparameter search.
rq2_risk_predict <- function(design, targets, edf=10, return_train=FALSE) {
  x<-design$train; xt<-design$test; mu<-colMeans(targets)
  constant <- function() {
    test <- matrix(mu,nrow(xt),length(mu),byrow=TRUE)
    if(return_train)list(test=test,train=matrix(mu,nrow(x),length(mu),byrow=TRUE)) else test
  }
  if (!ncol(x)) return(constant())
  e<-eigen(crossprod(x)/nrow(x),symmetric=TRUE); val<-pmax(0,e$values)
  desired<-min(edf, sum(val > 1e-8)*.8)
  if (!any(val > 1e-8)) return(constant())
  penalty<-uniroot(function(l)sum(val/(val+l))-desired,c(1e-12,max(val)*1e6))$root
  beta<-e$vectors %*% ((t(e$vectors) %*% (crossprod(x,sweep(targets,2,mu))/nrow(x))) /(val+penalty))
  result <- sweep(xt %*% beta,2,mu,"+")
  if(return_train)list(test=result,train=sweep(x %*% beta,2,mu,"+")) else result
}

# Euclidean projection onto nonincreasing exceedance probabilities. The common
# tolerance grid defines a coherent CDF, without fitting another model.
rq2_risk_monotone <- function(p) {
  p[]<-pmax(0,pmin(1,as.vector(p))); n<-ncol(p); out<-p
  for(i in seq_len(n)) {
    outer<-rep(Inf,nrow(p))
    for(a in seq_len(i)) {
      inner<-rep(-Inf,nrow(p))
      for(b in seq.int(i,n))inner<-pmax(inner,rowMeans(p[,seq.int(a,b),drop=FALSE]))
      outer<-pmin(outer,inner)
    }
    out[,i]<-outer
  }
  out
}

# Separate, orthogonalized context block. This preserves the measurement fit
# exactly and uses the same ten-df rule on the part of C not represented by L.
rq2_risk_increment <- function(low, context, targets, edf=10) {
  base <- rq2_risk_predict(low,targets,edf,TRUE)
  if(!ncol(context$train))return(base$test)
  projected <- rq2_risk_predict(low,context$train,edf,TRUE)
  residual_design <- list(train=context$train-projected$train,
    test=context$test-projected$test)
  base$test + rq2_risk_predict(residual_design,targets-base$train,edf)
}

