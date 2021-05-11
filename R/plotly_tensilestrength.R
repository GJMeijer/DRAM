#' Create plotly object showing root tensile strengths
#'
#' @description
#' Function that creates a plotly object showing the tensile
#' strength of roots as function of their diameters
#'
#' @param dr dataframe with root classes. Should contains fields for
#'   the representative root diameter in the class (`dr`) and the
#'   representative tensile strength (`tru`).
#' @param du dataframe with unit system used
#' @param nsignif number of significant units used in plotly object
#' @return plotly object
#' @examples
#' dr <- data.frame(
#'   dr = c(1.5, 2.5, 3.5),
#'   tru = c(1, 2, 3)
#' )
#' plotly_tensilestrength(dr)
#' @export

plotly_tensilestrength <- function(dr, du = NULL, nsignif = 3) {
  #base label
  xlab <- 'Root diameter'
  ylab <- 'Tensile strength'
  #use unit system - and create plot labels
  if (!is.null(du)) {
    #convert units
    dr$dr <- dr$dr / du['dr','unit_factor']
    dr$tru <- dr$tru / du['tru','unit_factor']
    #get units
    xunit <- du['dr','unit_user']
    yunit <- du['tru','unit_user']
    #specify units in axis plots
    xlab <- paste(xlab, ' [',  xunit, ']', sep='')
    ylab <- paste(ylab, ' [',  yunit, ']', sep='')
    #generate hover labels
    dr$HoverText <- paste(
      'Average root diameter in diameter class: ', signif(dr$dr, nsignif), ' ',xunit, '<br>',
      'Root tensile strength: ', signif(dr$tru, nsignif), ' ',yunit,
      sep=''
    )
  } else {
    #no unit system specified - create hover labels
    dr$HoverText <- paste(
      'Average root diameter in diameter class: ', signif(dr$dr, nsignif), '<br>',
      'Root tensile strength: ', signif(dr$tru, nsignif),
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
    y = ~tru,
    type = 'scatter',
    mode = 'lines+markers',
    text = ~HoverText,
    hoverinfo = 'text'
  )
  #add layout
  p <- plotly::layout(
    p,
    title = 'Root diameters and tensile strengths',
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
