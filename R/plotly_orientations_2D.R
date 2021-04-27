#' Create plotly with discrete root orientation (polar coordinates)
#'
#' @description
#' Function that creates a plotly object graphically showing the assumed
#' discrete root orientations in the model, in 2D polar coordinates
#' (azimuth, elevation).
#'
#' @param do dataframe with details of discrete root orientations. Should
#'   contain fields for the azimuth (`alpha`), the elevation (`beta`) and
#'   the root area ratio assigned to this orientation (`phir`)
#' @param ndimension The number of dimensions to consider. `1` for 1D root
#'   orientations (single orientation), `2` for 2-D orienations (orientation
#'   all located on the same arc) or `3` for 3-D orientations (orientation
#'   distributed on a spherical cap).
#' @param betamax the maximum (limiting) elevation angle for all root
#'   orientations
#' @param alphaoffset azimuth of the (rotated) coordinate system used for initial
#'   definition of orientations.
#' @param betaoffset elevation of the (rotated) coordinate system used for initial
#'   definition of orientations.
#' @param dgrid optional dataframe for drawing the boundaries of each grid
#'   cell. If provided, should contain the fields `alpha` and `beta`.
#'   line segments should be separated by `NA` values to plot a large number
#'   of line segments in one command.
#' @param nsignif number of significant digits to use on Plotly hoverlabel
#' @return plotly object
#' @examples
#' do <- data.frame(
#'   alpha = c(0, 1, 2),
#'   beta = c(0.5, 0.5, 0.7),
#'   phir = c(0.1, 0.1, 0.1)
#' )
#' plotly_orientations_2D(do, 3, 1)
#' plotly_orientations_2D(do, 3, 1, alphaoffset = 0.2, betaoffset = 0.3)
#' @export

plotly_orientations_2D <- function(do, ndimension, betamax, alphaoffset = 0, betaoffset = 0, dgrid = NULL, nsignif = 3){
  ## create boundary of orientations
  if (ndimension==1){
    #1D --> single point
    db <- data.frame(
      alpha = alphaoffset,
      beta = betaoffset
    )
  } else if (ndimension==2){
    #2D --> line
    db <- polar2polar(
      data.frame(
        alpha = 0,
        beta = c(-betamax, betamax)
      ),
      alphaoffset = alphaoffset,
      betaoffset = betaoffset
    )
  } else if (ndimension==3){
    #3D --> circle
    db <- polar2polar(
      data.frame(
        alpha = seq(-pi, pi, l = 361),
        beta = betamax
      ),
      alphaoffset = alphaoffset,
      betaoffset = betaoffset
    )
  }
  #draw boundary in plotly
  p <- plotly::plot_ly(
    data = db,
    type = 'scatterpolar',
    mode = 'lines',
    fill = 'toself',
    r = ~beta*180/pi,
    theta = ~alpha,
    thetaunit = 'radians',
    name = 'Orientation range'
  )
  ## draw points in plotly
  #create hovertext
  do$HoverText <- with(do, paste(
    'Azimuth angle: ', signif(alpha*180/pi, nsignif), ' [deg]',
    '<br>', 'Elevation angle: ', signif(beta*180/pi, nsignif), ' [deg]',
    '<br>', 'Root area ratio: ', signif(phir*100, nsignif), ' [%]',
    sep=''))
  #add to plotly
  p <- plotly::add_trace(
    p,
    data = do,
    type = 'scatterpolar',
    r = ~beta*180/pi,
    theta = ~alpha,
    mode = 'markers',
    fill = 'none',
    thetaunit = 'radians',
    name = 'Discrete orientations used',
    text = ~HoverText,
    hoverinfo = 'text'
  )
  ## generate cell boundaries (if 3D)
  if ((ndimension==3) & !is.null(dgrid)){
    p <- plotly::add_trace(
      p,
      data = dgrid,
      type = 'scatterpolar',
      mode = 'lines',
      fill = 'none',
      r = ~beta*180/pi,
      theta = ~alpha,
      thetaunit = 'radians',
      name = 'Grid',
      hoverinfo = 'skip',
      showlegend = FALSE,
      line = list(
        width = 1,
        color='#444444',
        opacity = 0.6
      )
    )
  }
  ## add layour
  p <- plotly::layout(
    p,
    title = 'Root orientations (polar)',
    polar=list(
      radialaxis =  list(title='Elevation angle [deg]'), #, range=c(0,90)
      angularaxis = list(title='Azimuth angle [deg]')
    )
  )
  ##return
  return(p)
}
