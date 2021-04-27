#' Create plotly object for normalised stress-strain curves
#'
#' @description
#' Function that creates a plotly object showing the mobilisation
#' of tensile stress in the root as function of root tensile strain.
#' Stresses and strains are normalised by the stress and strain to
#' failure.
#'
#' @param dr dataframe with root classes. Should contains fields for
#'   the yield parameters (`try`, `epsry`) and peak parameters
#'   (`tru`, `epsru`)
#' @param du dataframe with unit system used
#' @param nsignif number of significant units used in plotly object
#' @return plotly object
#' @examples
#' dr <- data.frame(
#'   epsry = 0.05,
#'   epsru = 0.20,
#'   try = 5e6,
#'   tru = 10e6,
#'   Ere = 5e6 / 0.05,
#'   Erep = (10e6 - 5e6) / (0.20 - 0.05),
#'   kappat = 10
#' )
#' plotly_stressstrain(dr)
#' @export

#normalised root stress-strain curve + fraction of roots intact as function of current tensile strain
plotly_stressstrain <- function(dr, du = NULL, nsignif = 4){
  #R machine precision - smallest step in R
  dd <- .Machine$double.eps
  #max epsilon - normalised over tensile strain to failure
  epsrepsru_max <- 2
  #create stress-strain behaviour of single root, normalised by root tensile strength and strain to failure
  d1 <- data.frame(
    epsrepsru = c(0, dr$epsry[1]/dr$epsru[1], 1, 1+dd, epsrepsru_max),
    trtru  = c(0, dr$try[1]/dr$tru[1], 1, 0, 0)
  )
  #create hovertext
  d1$HoverText <- with(d1, paste(
    'Strain (normalised): ', signif(epsrepsru, nsignif), ' [-]', '<br>',
    'Stress (normalised): ', signif(trtru, nsignif), ' [-]',
    sep='')
  )
  #create weibull data
  n    <- 101
  kap  <- dr$kappat[1]
  lam  <- 1 / gamma(1 + 1/kap)
  eps1 <- seq(0, dr$epsry[1], l = n)
  eps2 <- seq(dr$epsry[1], (epsrepsru_max*dr$epsru[1]), l = n)
  tr1  <- eps1 * dr$Ere[1]
  tr2  <- tail(tr1,1) + (eps2-dr$epsry[1])*dr$Erep[1]
  d2   <- data.frame(
    epsrepsru = c(eps1, eps2)/dr$epsru[1],
    trtru = c(tr1, tr2)/dr$tru[1]
  )
  d2$fb <- with(d2, exp(-((trtru/lam)^kap)))
  d2$trtru  <- with(d2, trtru*fb)
  #create hovertext
  d2$HoverText <- with(d2, paste(
    'Strain (normalised): ', signif(epsrepsru, nsignif), ' [-]', '<br>',
    'Root fraction intact: ', signif(fb, nsignif), ' [-]',
    sep='')
  )
  #create plotly object
  p <- plotly::plot_ly()
  #add trace
  p <- plotly::add_trace(
    p,
    data = d1,
    x = ~epsrepsru,
    y = ~trtru,
    type = 'scatter',
    mode = 'lines+markers',
    name = 'Root stress-strain behaviour',
    text = ~HoverText,
    hoverinfo = 'text'
  )
  #add trace 2 - breakage
  p <- plotly::add_trace(
    p,
    data = d2,
    x = ~epsrepsru,
    y = ~fb,
    type = 'scatter',
    mode = 'lines',
    name = 'Root fraction intact (Weibull)',
    yaxis = 'y2',
    text = ~HoverText,
    hoverinfo = 'text'
  )
  #add layout
  p <- plotly::layout(
    p,
    title =  'Root tensile stress-strain behaviour',
    xaxis =  list(
      title = paste('Normalised root strain to failure [-]', sep=''),
      range = c(0, epsrepsru_max)
    ),
    yaxis =  list(
      title = paste('Normalised root tensile strength [-]', sep=''),
      range = c(0, 1)
    ),
    yaxis2 = list(
      overlaying = "y",
      side = "right",
      title = paste('Root fraction intact [-]',sep=''),
      range = c(0, 1)
    )
  )
  #return plot
  return(p)
}
