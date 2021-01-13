library(tidyverse)
split_data <- function(data, s) {
  idx <- sample.int(s, nrow(data), replace = TRUE)
  data %>% split(idx)
}
blblm = function(formula,data,clusters,s,b){
  data_s = split_data(data,s)
  cl <- makeCluster(clusters)
  clusterExport(cl,varlist = list('lm_subsample','lm_boot','lm1','blbcoef','blbsigma'))
  estimates <- parLapply(cl, data_s,
                         function(data, formula,s,b) {
                           lm_subsample(formula, data = data, s=s, b=b)
                         },
                         formula = formula, s = nrow(data), b=b
  )
  stopCluster(cl)
  res <- list(estimates = estimates, formula = formula)
  
  class(res) <- "blblm"
  invisible(res)
}
lm_subsample <- function(formula, data, s, b) {
  replicate(b, lm_boot(formula, data, s), simplify = FALSE)
}
lm_boot <- function(formula, data, s) {
  freqs <- rmultinom(1, s, rep(1, nrow(data)))
  lm1(formula, data, freqs)
}
lm1 <- function(formula, data, freqs) {
  environment(formula) <- environment()
  fit <- lm(formula, data, weights = freqs)
  list(coef = blbcoef(fit), sigma = blbsigma(fit))
}

blbcoef <- function(fit) {
  coef(fit)
}
blbsigma <- function(fit) {
  p <- fit$rank
  y <- model.extract(fit$model, "response")
  e <- fitted(fit) - y
  w <- fit$weights
  sqrt(sum(w * (e^2)) / (sum(w) - p))
}
mean_lwr_upr <- function(x, level = 0.95) {
  alpha <- 1 - level
  c(fit = mean(x), quantile(x, c(alpha / 2, 1 - alpha / 2)) %>% set_names(c("lwr", "upr")))
}
# map then find mean of results
map_mean <- function(.x, .f, ...) {
  (map(.x, .f, ...) %>% reduce(`+`)) / length(.x)
}
# map and column bind
map_cbind <- function(.x, .f, ...) {
  map(.x, .f, ...) %>% reduce(cbind)
}
# map and rowbind
map_rbind <- function(.x, .f, ...) {
  map(.x, .f, ...) %>% reduce(rbind)
}
confint.blblm <- function(object, parm = NULL, level = 0.95, ...) {
  # function layout:
  # i)   obtain parameters
  # ii)  use map_rbind to obtain a confidence interval for parameters
  # iii) label confidence intervals with corresponding parameter
  alpha <- 1 - level
  est <- object$estimates
  out <- map_rbind(parm, function(p) {
    map_mean(est, ~ map_dbl(., list("coef", p)) %>% quantile(c(alpha / 2, 1 - alpha / 2)))
  })
  if (is.vector(out)) {
    out <- as.matrix(t(out))
  }
  dimnames(out)[[1]] <- parm
  out
}
coef.blblm <- function(object, ...) {
  # function layout
  # i) obtain estimates
  # ii) obtain means of coefficients
  est <- object$estimates
  map_mean(est, ~ map_cbind(., "coef") %>% rowMeans())
}

