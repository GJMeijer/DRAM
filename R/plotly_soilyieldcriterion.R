#' Create plotly object for soil Mohr-Coulomb yield criterion
#'
#' @description
#' Function that creates a plotly object showing the peak soil
#' shear stress as function of the normal stress, following the
#' Mohr-Coulomb framework
#'
#' @param ds dataframe with soil properties
#' @param du dataframe with unit system used
#' @param nsignif number of significant units used in plotly object
#' @return plotly object
#' @examples
#' ds <- data.frame(
#'   c = 2,
#'   phi = 30/180*pi,
#'   sign = 5
#' )
#' plotly_soilyieldcriterion(ds)
#' @export

plotly_soilyieldcriterion <- function(ds, du = NULL, nsignif = 4) {
  #base label
  xlab <- 'Normal effective stress'
  ylab <- 'Soil shear strength'
  #shear strength
  ds$tausu <- ds$c + ds$sign*tan(ds$phi)
  #create dataframe with Mohr-Coulomb criterion
  dd <- data.frame(sign = seq(0, 1.25*ds$sign[1], l = 251))
  dd$tausu <- ds$c[1] + dd$sign*tan(ds$phi[1])
  #use unit system - and create plot labels
  if (!is.null(du)) {
    #convert units
    ds$sign <- ds$sign / du['sign','unit_factor']
    ds$tausu <- ds$tausu / du['tausu','unit_factor']
    #convert units
    dd$sign <- dd$sign / du['sign','unit_factor']
    dd$tausu <- dd$tausu / du['tausu','unit_factor']
    #get units
    xunit <- du['sign','unit_user']
    yunit <- du['tausu','unit_user']
    #specify units in axis plots
    xlab <- paste(xlab, ' [',  xunit, ']', sep='')
    ylab <- paste(ylab, ' [',  yunit, ']', sep='')
    #generate hover labels for criterion
    dd$HoverText <- with(dd, paste(
      'Normal effective stress: ', signif(sign, nsignif), ' ',xunit, '<br>',
      'Soil shear strength: ', signif(tausu, nsignif), ' ',yunit,
      sep='')
    )
    #generate hover labels for input
    ds$HoverText <- with(ds, paste(
      'Normal effective stress in shear zone: ', signif(sign, nsignif), ' ', xunit, '<br>',
      'Soil shear strength in shear zone: ',  signif(tausu, nsignif), ' ', yunit,
      sep='')
    )
  } else {
    #create dataframe with Mohr-Coulomb criterion
    dd <- data.frame(sign = seq(0, 1.25*ds$sign[1], l = 251))
    dd$tausu <- ds$c[1] + dd$sign*tan(ds$phi[1])
    #no unit system specified - create hover labels
    dd$HoverText <- with(dd, paste(
      'Normal effective stress: ', signif(sign, nsignif), '<br>',
      'Soil shear strength: ', signif(tausu, nsignif),
      sep='')
    )
    #generate hover labels for input
    ds$HoverText <- with(ds, paste(
      'Normal effective stress in shear zone: ', signif(sign, nsignif), '<br>',
      'Soil shear strength in shear zone: ',  signif((c + sign*tan(phi)), nsignif),
      sep='')
    )
  }
  #create plotly object
  p <- plotly::plot_ly()
  #add Mohr-Coulomb trace
  p <- plotly::add_trace(
    p,
    data = dd,
    x = ~sign,
    y = ~tausu,
    type = 'scatter',
    mode = 'lines',
    text = ~HoverText,
    hoverinfo = 'text'
  )
  #add layout
  p <- plotly::layout(
    p,
    title = 'Soil shear strength',
    xaxis = list(
      title = xlab,
      rangemode = 'tozero'
    ),
    yaxis = list(
      title = ylab,
      rangemode = 'tozero'
    )
  )
  #add annotations
  p <- plotly::add_annotations(
    p,
    x = ds$sign,
    y = ds$tausu,
    text = ds$HoverText,
    xref = "x",
    yref = "y",
    showarrow = TRUE,
    arrowhead = 7,
    ax = 20,
    ay = -40
  )
  #return plot object
  return(p)
}
