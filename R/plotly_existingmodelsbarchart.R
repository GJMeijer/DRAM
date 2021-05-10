#' Create plotly with peak root reinforcement predictions
#'
#' @description
#' Function that creates a plotly object showing the peak root
#' reinforcement according to various existing models
#'
#' @param dcru dataframe with predictiondroot classes. Should contains
#'   fields for the model name (`model`) and the peak root-reinforcement
#'   prediction (`cru`).
#' @param du dataframe with unit system used
#' @param nsignif number of significant units used in plotly object
#' @param ylim array with minimum and maximum y-axis limits
#' @return plotly object
#' @examples
#' dcru <- data.frame(
#'   model = c('WWM', 'FBM0'),
#'   cru = c(20, 10)
#' )
#' plotly_existingmodelsbarchart(dcru)
#' @export

plotly_existingmodelsbarchart <- function(dcru, du = NULL, nsignif = 3, ylim = c(0,NA)) {
  #base label
  xlab <- 'Model'
  ylab <- 'Peak root reinforcement'
  #use unit system - and create plot labels
  if (!is.null(du)) {
    #convert units
    dcru$cru <- dcru$cru / du['cru','unit_factor']
    #convert axes
    ylim <- ylim / du['cru','unit_factor']
    #get units
    yunit <- du['cru','unit_user']
    #specify units in axis plots
    ylab <- paste(ylab, ' [',  yunit, ']', sep='')
    #generate hover labels
    dcru$HoverText <- paste(
      'Model: ', dcru$model, '<br>',
      'Peak root reinforcement: ', signif(dcru$cru, nsignif), ' ', yunit,
      sep=''
    )
  } else {
    #no unit system specified - create hover labels
    dcru$HoverText <- paste(
      'Model: ', dcru$model, '<br>',
      'Peak root reinforcement: ', signif(dcru$cru, nsignif),
      sep=''
    )
  }
  #create plotly object
  p <- plotly::plot_ly()
  #add trace
  p <- plotly::add_trace(
    p,
    data = dcru,
    x = ~model,
    y = ~cru,
    type = 'bar',
    text = ~HoverText,
    hoverinfo = 'text'
  )
  #add layout
  p <- plotly::layout(
    p,
    title = 'Peak root reinforcement predictions',
    xaxis = list(
      title = xlab
    ),
    yaxis = list(
      title = ylab,
      range = ylim
    )
  )
  #return
  return(p)
}
