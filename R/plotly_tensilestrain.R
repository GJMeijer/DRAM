#' Create plotly object showing root tensile strain to failure
#'
#' @description
#' Function that creates a plotly object showing the tensile
#' strain to failure in roots as function of their diameter
#'
#' @param dr dataframe with root classes. Should contains fields for
#'   the representative root diameter in the class (`dr`) and the
#'   representative tensile strain (`epsru`).
#' @param du dataframe with unit system used
#' @param nsignif number of significant units used in plotly object
#' @return plotly object
#' @examples
#' dr <- data.frame(
#'   dr = c(1.5, 2.5, 3.5),
#'   epsru = c(0.1, 0.12, 0.11),
#' )
#' plotly_tensilestrain(dr)
#' @export

plotly_tensilestrain <- function(dr, du = NULL, nsignif = 3) {
  #base label
  xlab <- 'Root diameter'
  ylab <- 'Tensile strain to failure'
  #use unit system - and create plot labels
  if (!is.null(du)) {
    #convert units
    dr$dr <- dr$dr / du['dr','unit_factor']
    dr$epsru <- dr$epsru / du['epsru','unit_factor']
    #get units
    xunit <- du['dr','unit_user']
    yunit <- du['epsru','unit_user']
    #specify units in axis plots
    xlab <- paste(xlab, ' [',  xunit, ']', sep='')
    ylab <- paste(ylab, ' [',  yunit, ']', sep='')
    #generate hover labels
    dr$HoverText <- paste(
      'Average root diameter in diameter class: ', signif(dr$dr, nsignif), ' [',xunit,']', '<br>',
      'Root tensile strain to failure: ', signif(dr$epsru, nsignif), ' [',yunit,']',
      sep=''
    )
  } else {
    #no unit system specified - create hover labels
    dr$HoverText <- paste(
      'Average root diameter in diameter class: ', signif(dr$dr, nsignif), '<br>',
      'Root tensile strain to failure: ', signif(dr$epsru, nsignif),
      sep=''
    )
  }
  #create plotly object
  p <- plotly::plot_ly()
  #add trace
  p <- plotly::add_trace(
    p,
    data = dr,
    x = ~dr,
    y = ~epsru,
    type = 'scatter',
    mode = 'lines+markers',
    text = ~HoverText,
    hoverinfo = 'text'
  )
  #add layout
  p <- plotly::layout(
    p,
    title = 'Root diameters and tensile strain to failure',
    xaxis = list(
      title = xlab
    ),
    yaxis = list(
      title = ylab,
      rangemode = 'tozero'
    )
  )
  #return
  return(p)
}
