## Implementations of Hsiao include:
## Eviews:
##   Khouiled, B (2018) Tests of Homogeneity in Panel Data with EViews. MPRA Paper
##   101001. https://mpra.ub.uni-muenchen.de/id/eprint/101001
## Stata:
##   https://github.com/TICSTAT/ticstat/blob/master/Hsiao_poolability.do
hsiao <- function(formula, data, index = NULL, ...){

  names_ellipsis <- names(list(...))

  if ("model" %in% names_ellipsis){
    stop("Remove 'model' arguement.")
  }

  if ("effect" %in% names_ellipsis){
    stop("Remove 'effect' arguement.")
  }

  if (!is(data, "pdata.frame") && is.data.frame(data)){
    if (is.null(index)){
      data <- pdata.frame(data)
    } else {
      data <- pdata.frame(data, index = index)
    }
  }

  pdims <- pdim(data)
  N     <- pdims$nT$n
  T     <- pdims$nT$T

  RSS_indi <- sum(by(data, plm::index(data)[,1],
                     function(x) deviance(plm(formula, x, model = "pooling",
                                              ...))))

  zz_pooling <- plm(formula, data, model = "pooling", ...)

  RSS_pool <- deviance(zz_pooling)

  cf <- coef(zz_pooling)
  K  <- length(cf[names(cf) != "(Intercept)"])

  zz_within <- plm(formula, data, model = "within", ...)

  RSS_with <- deviance(zz_within)

  F_statistic <- c(
    ((RSS_pool - RSS_indi) * (N * (T-(K+1)))) / (RSS_indi * (N-1) * (K+1)),
    ((RSS_with - RSS_indi) * (N * (T-(K+1)))) / (RSS_indi * (N-1) * K),
    ((RSS_pool - RSS_with) * (N * (T-1) - K)) / (RSS_with * (N-1))
  )

  res <-
    list(
      "Hypothesis"  = c("H1", "H2", "H3"),
      "F.statistic" = F_statistic,
      "p.value"     = c(
        pf(F_statistic[1], (K+1) * (N-1), (N*(T-(K+1))), lower.tail = FALSE),
        pf(F_statistic[2], K*(N-1),       (N*T-N*(K+1)), lower.tail = FALSE),
        pf(F_statistic[3], (N-1),         (N*(T-1)-K),   lower.tail = FALSE)
      ),
      "formula"     = formula
    )

  class(res) <- c("hsiao", 'list')

  return(res)

}

print.hsiao <- function(x, ...){
  x$p.value     <- ifelse(x$p.value < 0.001, "< 0.001", round(x$p.value, 5))
  x$F.statistic <- round(x$F.statistic, 4)
  form          <- Reduce(paste, deparse(x$formula))

  x <- data.frame(
    "Hypothesis"  = x$Hypothesis,
    "F-statistic" = x$F.statistic,
    "p-value"     = x$p.value,
    check.names   = FALSE
  )

  dnames <- dimnames(x)
  names(dnames) <- c("",
       paste0(
         "\n                  Hsiao Homogeneity Test\n",
         "\nHypothesis| Null |                Alternative                ",
         "\n----------+------+-------------------------------------------",
         "\n    H1    |Pooled|                    H2                     ",
         "\n    H2    |  H3  |      Heterogeneous intercept & slope      ",
         "\n    H3    |Pooled|Heterogeneous intercept & Homogeneous slope",
         "\n=============================================================\n",
         "\nformula: ", form, "\n"
       )
  )

  x <- as.matrix(x)
  dimnames(x) <- dnames

  names_width <- max(vapply(colnames(x), nchar, integer(1)))
  colnames(x) <- format(colnames(x), width = names_width, justify = "centre")

  print(noquote(format(x,
                       width = names_width,
                       justify = "centre")
  ), ...)
}
