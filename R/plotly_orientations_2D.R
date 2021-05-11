#' Create plotly with discrete root orientation (polar coordinates)
#'
#' @description
#' Function that creates a plotly object graphically showing the assumed
#' discrete root orientations in the model, in 2D polar coordinates
#' (azimuth, elevation).
#'
#' @param do dataframe with details of discrete root orientations. Should
#'   contain fields for the azimuth (`alpha0`), the elevation (`beta0`) and
#'   the fraction of roots with this orientation (`weight`)
#' @param ndimension The number of dimensions to consider. `1` for 1D root
#'   orientations (single orientation), `2` for 2-D orienations (orientation
#'   all located on the same arc) or `3` for 3-D orientations (orientation
#'   distributed on a spherical cap).
#' @param beta0max the maximum (limiting) elevation angle for all root
#'   orientations
#' @param alphaoffset azimuth of the (rotated) coordinate system used for initial
#'   definition of orientations.
#' @param betaoffset elevation of the (rotated) coordinate system used for initial
#'   definition of orientations.
#' @param dgrid optional dataframe for drawing the boundaries of each grid
#'   cell. If provided, should contain the fields `alpha0` and `beta0`.
#'   line segments should be separated by `NA` values to plot a large number
#'   of line segments in one command.
#' @param du dataframe with unit conventions
#' @param nsignif number of significant digits to use on Plotly hoverlabel
#' @return plotly object
#' @examples
#' do <- data.frame(
#'   alpha0 = c(0, 1, 2),
#'   beta0 = c(0.5, 0.5, 0.7),
#'   weight = c(0.3, 0.4, 0.3),
#'   phir = c(0.01, 0.01, 0.01)
#' )
#' plotly_orientations_2D(do, 3, 1)
#' plotly_orientations_2D(do, 3, 1, alphaoffset = 0.2, betaoffset = 0.3)
#' @export

plotly_orientations_2D <- function(do, ndimension, beta0max, alphaoffset = 0, betaoffset = 0, dgrid = NULL, du = NULL, nsignif = 3){
  ## create boundary of orientations
  if (ndimension==1){
    #1D --> single point
    db <- data.frame(
      alpha0 = alphaoffset,
      beta0 = betaoffset
    )
  } else if (ndimension==2){
    #2D --> line
    db <- polar2polar(
      data.frame(
        alpha0 = c(-pi, 0),
        beta0 = c(beta0max, beta0max)
      ),
      alphaoffset = alphaoffset,
      betaoffset = betaoffset
    )
  } else if (ndimension==3){
    #3D --> circle
    db <- polar2polar(
      data.frame(
        alpha0 = seq(-pi, pi, l = 361),
        beta0 = beta0max
      ),
      alphaoffset = alphaoffset,
      betaoffset = betaoffset
    )
  }
  #hoverlabels
  if (!is.null(du)){
    do$HoverText <- with(do, paste(
      'Azimuth angle: ', signif(alpha0*180/pi, nsignif), ' deg',
      '<br>', 'Elevation angle: ', signif(beta0*180/pi, nsignif), ' deg',
      '<br>', 'Weight: ', signif(weight*100, nsignif), ' %',
      '<br>', 'Root area ratio: ', signif(phir/du['phir','unit_factor'], nsignif), ' ', du['phir','unit_user'],
      sep='')
    )
  } else {
    do$HoverText <- with(do, paste(
      'Azimuth angle: ', signif(alpha0*180/pi, nsignif), ' deg',
      '<br>', 'Elevation angle: ', signif(beta0*180/pi, nsignif), ' deg',
      '<br>', 'Weight: ', signif(weight*100, nsignif), ' %',
      '<br>', 'Root area ratio: ', signif(phir*100, nsignif), ' %',
      sep='')
    )
  }
  #plotly object
  p <- plotly::plot_ly()
  #draw boundary in plotly
  p <- plotly::add_trace(
    p,
    data = db,
    type = 'scatterpolar',
    mode = 'lines',
    fill = 'toself',
    r = ~beta0*180/pi,
    theta = ~alpha0,
    thetaunit = 'radians',
    name = 'Orientation range'
  )
  ## draw points in plotly
  p <- plotly::add_trace(
    p,
    data = do,
    type = 'scatterpolar',
    r = ~beta0*180/pi,
    theta = ~alpha0,
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
      r = ~beta0*180/pi,
      theta = ~alpha0,
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
      radialaxis = list(
        title = 'Elevation angle [deg]'
      ),
      angularaxis = list(
        title = 'Azimuth angle [deg]'
      )
    )
  )
  ##return
  return(p)
}
