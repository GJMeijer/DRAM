#' Create plotly object for soil displacement vs root reinforcement
#'
#' @description
#' Function that creates a plotly object showing the root reinforcement
#' as function of direct shear displacement
#'
#' @param dsum dataframe with results. Should contain fields for the soil
#'   shear displacement (`us`) and the root reinforcement (`cr`)
#' @param du dataframe with unit system used
#' @param nsignif number of significant units used in plotly object
#' @return plotly object
#' @examples
#' dsum <- data.frame(
#'   us = seq(0, 10, l = 11),
#'   cr = sqrt(seq(0, 10, l = 11))
#' )
#' plotly_reinforcement(dsum)
#' @export

plotly_reinforcement <- function(dsum, du = NULL, nsignif = 3){
  #base label
  xlab <- 'Soil shear displacement'
  ylab <- 'Root reinforcement'
  #dataframe with annotations - peak
  dpeak <- dsum[which.max(dsum$cr),]
  #use unit system - and create plot labels
  if (!is.null(du)) {
    #convert units
    dsum$us <- dsum$us / du['us','unit_factor']
    dsum$cr <- dsum$cr / du['cr','unit_factor']
    dpeak$us <- dpeak$us / du['us','unit_factor']
    dpeak$cr <- dpeak$cr / du['cr','unit_factor']
    #get units
    xunit <- du['us','unit_user']
    yunit <- du['cr','unit_user']
    #specify units in axis plots
    xlab <- paste(xlab, ' [',  xunit, ']', sep='')
    ylab <- paste(ylab, ' [',  yunit, ']', sep='')
    #generate hover labels
    dsum$HoverText <- paste(
      'Shear displacement: ', signif(dsum$us, nsignif), ' ',xunit, '<br>',
      'Root reinforcement: ', signif(dsum$cr, nsignif), ' ',yunit,
      sep=''
    )
    #generate annotation for peak
    dpeak$HoverText <- paste(
      'Shear displacement: ', signif(dpeak$us, nsignif), ' ',xunit, '<br>',
      'Peak root-reinforcement: ', signif(dpeak$cr, nsignif), ' ',yunit,
      sep=''
    )
  } else {
    #no unit system specified - create hover labels
    dsum$HoverText <- paste(
      'Shear displacement: ', signif(dsum$us, nsignif), '<br>',
      'Root reinforcement: ', signif(dsum$cr, nsignif),
      sep=''
    )
    #generate annotation for peak
    dpeak$HoverText <- paste(
      'Shear displacement: ', signif(dpeak$us, nsignif), '<br>',
      'Peak root-reinforcement: ', signif(dpeak$cr, nsignif),
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
    y = ~cr,
    type = 'scatter',
    mode = 'lines',
    text = ~HoverText,
    hoverinfo = 'text'
  )
  #add layout
  p <- plotly::layout(
    p,
    title = 'Root-reinforcement',
    xaxis = list(
      title = xlab
    ),
    yaxis = list(
      title = ylab,
      rangemode = 'tozero'
    )
  )
  #add annotation for peak
  p <- plotly::add_annotations(
    p,
    x = dpeak$us,
    y = dpeak$cr,
    text = dpeak$HoverText,
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
