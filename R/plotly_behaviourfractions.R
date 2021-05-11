#' Create plotly object for root behaviour types
#'
#' @description
#' Function that creates a plotly object showing how much of the root area
#' ratio behaves according to a certain type ("anchored, elastic", "not in
#' tension", "slip, elastoplastic" etc) for each soil shear displacement
#'
#' @param dsum dataframe with results. Should contain fields for the soil
#'   shear displacement (`us`) fields for the fraction of roots that behave
#'   not in tension (`frac_notintension`),
#'   anchored elastic (`frac_anchoredelastic`),
#'   anchored elastoplastic (`frac_anchoredelastoplastic`),
#'   slipping elastic (`frac_slipelastic`),
#'   slipping elastoplastic (`frac_slipelastoplastic`) or
#'   broken (`frac_broken`)
#' @param du dataframe with unit system used
#' @param nsignif number of significant units used in plotly object
#' @param plot_percentage if `TRUE`, show fractions in percentages`
#' @return plotly object
#' @export

plotly_behaviourfractions <- function(dsum, du = NULL, nsignif = 3, plot_percentage = FALSE){
  #base label
  xlab <- 'Soil shear displacement'
  ylab <- 'Fraction of root area ratio'
  #use unit system - and create plot labels
  if (!is.null(du)) {
    #convert units
    dsum$us <- dsum$us / du['us','unit_factor']
    #get units
    xunit <- du['us','unit_user']
    #specify units in axis plots
    xlab <- paste(xlab, ' [',  xunit, ']', sep='')
  }
  #percentages or not
  if (plot_percentage == T){
    fac <- 100
    ylab <- paste(ylab, ' [%]')
  } else {
    fac <- 1
    ylab <- paste(ylab, ' [-]')
  }
  #create some colors with RColorBrewer
   #colo <- brewer.pal(6, "Set3")
  colo <- c("#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462")
  #create plotly object
  p <- plotly::plot_ly()
  #add trace for "not in tension"
  p <- plotly::add_trace(
    p,
    data = dsum,
    x = ~us,
    y = ~fraction_notintension * fac,
    name = "Not in tension",
    type = 'scatter',
    mode = 'none',
    stackgroup = 'one',
    fillcolor = colo[1]
  )
  #add anchored, elastic
  p <- plotly::add_trace(
    p,
    data = dsum,
    x = ~us,
    y = ~fraction_anchoredelastic * fac,
    name = "Anchored, elastic",
    type = 'scatter',
    mode = 'none',
    stackgroup = 'one',
    fillcolor = colo[2]
  )
  #add anchored, elasto-plastic
  p <- plotly::add_trace(
    p,
    data = dsum,
    x = ~us,
    y = ~fraction_anchoredelastoplastic * fac,
    name = "Anchored, elastoplastic",
    type = 'scatter',
    mode = 'none',
    stackgroup = 'one',
    fillcolor = colo[3]
  )
  #add slipping, elastic
  p <- plotly::add_trace(
    p,
    data = dsum,
    x = ~us,
    y = ~fraction_slipelastic * fac,
    name = "Slipping, elastic",
    type = 'scatter',
    mode = 'none',
    stackgroup = 'one',
    fillcolor = colo[4]
  )
  #add slipping, elasto-plastic
  p <- plotly::add_trace(
    p,
    data = dsum,
    x = ~us,
    y = ~fraction_slipelastoplastic * fac,
    name = "Slipping, elastoplastic",
    type = 'scatter',
    mode = 'none',
    stackgroup = 'one',
    fillcolor = colo[5]
  )
  #add broken
  p <- plotly::add_trace(
    p,
    data = dsum,
    x = ~us,
    y = ~fraction_broken * fac,
    name = "Broken",
    type = 'scatter',
    mode = 'none',
    stackgroup = 'one',
    fillcolor = colo[6]
  )
  #add layout
  p <- plotly::layout(
    p,
    title = 'Root behaviour type',
    xaxis = list(
      title = xlab
    ),
    yaxis = list(
      title = ylab
    ),
    hovermode = 'compare'
  )
  #return plot object
  return(p)
}
