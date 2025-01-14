## Implementations of Hsiao include:
## Eviews:
##   Khouiled, B (2018) Tests of Homogeneity in Panel Data with EViews. MPRA Paper
##   101001. https://mpra.ub.uni-muenchen.de/id/eprint/101001
## Stata:
##   https://github.com/TICSTAT/ticstat/blob/master/Hsiao_poolability.do

## Acknowledgements to Kevin Tappe for adding useful suggestions:
## - fix typos + re-use calc. degrees of freedom, print degrees of freedom
## https://github.com/cadam00/poobly/pull/1.


hsiao <- function(formula, data, index = NULL, ...){

  names_ellipsis <- names(list(...))

  if ("model" %in% names_ellipsis){
    stop("Remove 'model' argument.")
  }

  if ("effect" %in% names_ellipsis){
    stop("Remove 'effect' argument.")
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

  df1 <- c((N-1) * (K+1),
           (N-1) * K,
           (N-1))

  df2 <- c(N * (T-(K+1)),
           N * (T-(K+1)),
           N * (T-1) - K)

  F_statistic <- c(
    ((RSS_pool - RSS_indi) * df2[1]) / (RSS_indi * df1[1]),
    ((RSS_with - RSS_indi) * df2[2]) / (RSS_indi * df1[2]),
    ((RSS_pool - RSS_with) * df2[3]) / (RSS_with * df1[3])
  )

  p_value <- c(
    pf(F_statistic[1], df1[1], df2[1], lower.tail = FALSE),
    pf(F_statistic[2], df1[2], df2[2], lower.tail = FALSE),
    pf(F_statistic[3], df1[3], df2[3], lower.tail = FALSE)
  )

  res <-
    list(
      "Hypothesis"  = c("H1", "H2", "H3"),
      "F.statistic" = F_statistic,
      "p.value"     = p_value,
      "df1"         = df1,
      "df2"         = df2,
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
    "df1"         = x$df1,
    "df2"         = x$df2,
    "p-value"     = x$p.value,
    check.names   = FALSE
  )

  dnames <- dimnames(x)
  names(dnames) <- c("",
       paste0(
         "\n                    Hsiao Homogeneity Test\n",
         "\nHypothesis| Null |                 Alternative                 ",
         "\n----------+------+---------------------------------------------",
         "\n    H1    |Pooled|                    H2                       ",
         "\n    H2    |  H3  |      Heterogeneous intercepts & slopes      ",
         "\n    H3    |Pooled|Heterogeneous intercepts & homogeneous slopes",
         "\n===============================================================\n",
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
