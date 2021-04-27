#' Create plotly object showing root diameter classes
#'
#' @description
#' Function that creates a plotly object showing the discretised
#' root diameter classes used in the model
#'
#' @param dr dataframe with root classes. Should contains fields for
#'   the representative root diameter in the class (`dr`), the
#'   minimum (`drmin`) and the maximum root diameter (`drmax`) in each
#'   class as well as the root area ratio in each class (`phir`)
#' @param du dataframe with unit system used
#' @param nsignif number of significant units used in plotly object
#' @return plotly object
#' @examples
#' dr <- data.frame(
#'   dr = c(1.5, 2.5, 3.5),
#'   drmin = c(1, 2, 3),
#'   drmax = c(2, 3, 4),
#'   phir = c(0.01, 0.02, 0.03)
#' )
#' plotly_diameterclasses(dr)
#' @export

plotly_diameterclasses <- function(dr, du = NULL, nsignif = 3) {
  #base label
  xlab <- 'Root diameter'
  ylab <- 'Root area ratio'
  #use unit system - and create plot labels
  if (!is.null(du)) {
    #convert units
    dr$dr <- dr$dr / du['dr','unit_factor']
    dr$drmin <- dr$drmin / du['drmin','unit_factor']
    dr$drmax <- dr$drmax / du['drmax','unit_factor']
    dr$phir <- dr$phir / du['phir','unit_factor']
    #get units
    xunit <- du['dr','unit_user']
    yunit <- du['phir','unit_user']
    #specify units in axis plots
    xlab <- paste(xlab, ' [',  xunit, ']', sep='')
    ylab <- paste(ylab, ' [',  yunit, ']', sep='')
    #generate hover labels
    dr$HoverText <- paste(
      'Root diameter (average): ', signif(dr$dr, nsignif), ' ', xunit, '<br>',
      'Root diameter (range): ', signif(dr$drmin, nsignif), '-', signif(dr$drmax, nsignif), ' ', xunit, '<br>',
      'Root area ratio in class: ', signif(dr$phir, nsignif), ' ', yunit,
      sep=''
    )
  } else {
    #no unit system specified - create hover labels
    dr$HoverText <- paste(
      'Root diameter (average): ', signif(dr$dr, nsignif), '<br>',
      'Root diameter (range): ', signif(dr$drmin, nsignif), '-', signif(dr$drmax, nsignif), '<br>',
      'Root area ratio in class: ', signif(dr$phir, nsignif),
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
    y = ~phir,
    type = 'scatter',
    mode = 'lines+markers',
    text = ~HoverText,
    hoverinfo = 'text'
  )
  #add layout
  p <- plotly::layout(
    p,
    title = 'Root diameters and root area ratio distribution',
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
