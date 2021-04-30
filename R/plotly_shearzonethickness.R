#' Create plotly object for shear zone thickness vs root reinforcement
#'
#' @description
#' Function that creates a plotly object showing the shear zone thickness
#' as function of direct shear displacement
#'
#' @param dsum dataframe with results. Should contain fields for the soil
#'   shear displacement (`us`) and the shear zone thickness (`h`)
#' @param du dataframe with unit system used
#' @param nsignif number of significant units used in plotly object
#' @return plotly object
#' @examples
#' dsum <- data.frame(
#'   us = seq(0, 10, l = 11),
#'   h = sqrt(seq(0, 10, l = 11))
#' )
#' plotly_shearzonethickness(dsum)
#' @export

plotly_shearzonethickness <- function(dsum, du = NULL, nsignif = 3){
  #base label
  xlab <- 'Soil shear displacement'
  ylab <- 'Shear zone thickness'
  #use unit system - and create plot labels
  if (!is.null(du)) {
    #convert units
    dsum$us <- dsum$us / du['us','unit_factor']
    dsum$h <- dsum$h / du['h','unit_factor']
    #get units
    xunit <- du['us','unit_user']
    yunit <- du['h','unit_user']
    #specify units in axis plots
    xlab <- paste(xlab, ' [',  xunit, ']', sep='')
    ylab <- paste(ylab, ' [',  yunit, ']', sep='')
    #generate hover labels
    dsum$HoverText <- paste(
      'Shear displacement: ', signif(dsum$us, nsignif), ' ',xunit, '<br>',
      'Shear zone thickness: ', signif(dsum$h, nsignif), ' ',yunit,
      sep=''
    )
  } else {
    #no unit system specified - create hover labels
    dsum$HoverText <- paste(
      'Shear displacement: ', signif(dsum$us, nsignif), '<br>',
      'Shear zone thickness: ', signif(dsum$h, nsignif),
      sep=''
    )
  }
  #create plotly object
  p <- plotly::plot_ly()
  #add trace
  p <- plotly::add_trace(
    p,
    data = dsum,
    x = ~us,
    y = ~h,
    type = 'scatter',
    mode = 'lines',
    text = ~HoverText,
    hoverinfo = 'text'
  )
  #add layout
  p <- plotly::layout(
    p,
    title = 'Shear zone thickness',
    xaxis = list(
      title = xlab
    ),
    yaxis = list(
      title = ylab,
      rangemode = 'tozero'
    )
  )
  #return plot object
  return(p)
}
