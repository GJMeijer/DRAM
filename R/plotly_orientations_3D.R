#' Create plotly with discrete root orientation (cartesian coordinates)
#'
#' @description
#' Function that creates a plotly object graphically showing the assumed
#' discrete root orientations in the model, in 3D cartesian coordinates
#' (x, y, z).
#'
#' @param do dataframe with details of discrete root orientations. Should
#'   contain fields for the azimuth (`alpha0`), the elevation (`beta0`) and
#'   the percentage ('weight') of roots with this orientation (`weight`)
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
#' plotly_orientations_3D(do, 3, 1)
#' plotly_orientations_3D(do, 3, 1, alphaoffset = 0.2, betaoffset = 0.3)
#' @export

plotly_orientations_3D <- function(do, ndimension, beta0max, alphaoffset = 0, betaoffset = 0, dgrid = NULL, du = NULL, nsignif = 3){
  ## CREATE EMPTY PLOTLY OBJECT
  p <- plotly::plot_ly()
  ## ADD ORIENTATIONS AS POINTS AND LINES
  dpp <- polar2cartesian(data = do)
  if (!is.null(du)){
    dpp$HoverText <- with(dpp, paste(
      'Azimuth angle: ', signif(alpha0*180/pi, nsignif), ' deg',
      '<br>', 'Elevation angle: ', signif(beta0*180/pi, nsignif), ' deg',
      '<br>', 'Weight: ', signif(weight*100, nsignif), ' %',
      '<br>', 'Root area ratio: ', signif(phir/du['phir','unit_factor'], nsignif), ' ', du['phir','unit_user'],
      sep='')
    )
  } else {
    dpp$HoverText <- with(dpp, paste(
      'Azimuth angle: ', signif(alpha0*180/pi, nsignif), ' deg',
      '<br>', 'Elevation angle: ', signif(beta0*180/pi, nsignif), ' deg',
      '<br>', 'Weight: ', signif(weight*100, nsignif), ' %',
      '<br>', 'Root area ratio: ', signif(phir*100, nsignif), ' %',
      sep='')
    )
  }
  #repeat to make discrete line segments
  dppl <- dpp[rep(seq(nrow(dpp)),each=3),]
  dppl$x <- dppl$x * rep(c(1,-1,NA), nrow(dpp))
  dppl$y <- dppl$y * rep(c(1,-1,NA), nrow(dpp))
  dppl$z <- dppl$z * rep(c(1,-1,NA), nrow(dpp))
  #add lines to plotly
  p <- plotly::add_trace(
    p,
    x = dppl$x,
    y = dppl$y,
    z = dppl$z,
    type = 'scatter3d',
    mode = 'lines+markers',
    text = dppl$HoverText,
    hoverinfo = 'text',
    name = 'Root orientations',
    marker = list(
      color = '#ff7f0e'
    ),
    line = list(
      width = 3,
      color = '#ff7f0e'
    )
  )
  ## ADD SHEAR PLANE
  p <- plotly::add_surface(
    p,
    x = c(-1, 1) * 1.2,
    y = c(-1, 1) * 1.2,
    z = matrix(c(0, 0, 0, 0), nrow = 2),
    opacity = 0.8,
    hoverinfo = 'skip',
    showscale = F,
    name = 'Shear plane'
  )
  ## ADD SHEAR PLANE ARROWS
  arrowx  <- 0.4
  arrowz  <- 0.2
  arrowdz <- 0.1
  p <- plotly::add_trace(
    p,
    x = c(-arrowx, arrowx, arrowx-arrowdz, NA, arrowx, -arrowx, -arrowx+arrowdz),
    y = c(0,0,0, NA, 0,0,0),
    z = c(arrowz, arrowz, arrowz+arrowdz, NA, -arrowz, -arrowz, -arrowz-arrowdz),
    type = 'scatter3d',
    mode = 'lines',
    line = list(width = 6),
    name = 'Shear displacement direction'
  )
  ## PLOT SPHERE
  ds <- cappedsphere_vertices(beta0 = c(0, pi))
  p <- plotly::add_trace(
    p,
    x = ds$coordinates$x,
    y = ds$coordinates$y,
    z = ds$coordinates$z,
    i = ds$vertices$i,
    j = ds$vertices$j,
    k = ds$vertices$k,
    opacity = 0.3,
    type = 'mesh3d',
    facecolor = rep('#7f7f7f', dim(ds$vertices)[1]),
    flatshading = TRUE,
    hoverinfo = 'skip',
    name = 'Unit sphere'
  )
  ## create boundary of orientations
  if (ndimension == 2){
    #2D --> arc
    db <- polar2cartesian(
      data = polar2polar(
        data.frame(
          alpha0 = 0,
          beta0 = seq(-beta0max, beta0max, l = 25)
        ),
        alphaoffset = alphaoffset,
        betaoffset = betaoffset
      )
    )
    #add to plotly - top half of plot
    p <- plotly::add_trace(
      p,
      x = db$x,
      y = db$y,
      z = db$z,
      type = 'scatter3d',
      mode = 'lines',
      line = list(width = 6, color='#1f77b4'),
      hoverinfo = 'skip',
      name = 'Orientation',
      showlegend = T
    )
    #add to plotly - bottom half of plot
    p <- plotly::add_trace(
      p,
      x = -db$x,
      y = -db$y,
      z = -db$z,
      type = 'scatter3d',
      mode = 'lines',
      line = list(width = 6, color='#1f77b4'),
      hoverinfo = 'skip',
      name = 'Orientation range',
      showlegend = F
    )
  } else if (ndimension==3){
    #3D --> spherical cap
    db <- cappedsphere_vertices(beta0 = c(0, beta0max), alphaoffset = alphaoffset, betaoffset = betaoffset)
    #add to plotly - top half of plot
    p <- plotly::add_trace(   #sphere - blue
      p,
      data = db,
      x = ~coordinates$x,
      y = ~coordinates$y,
      z = ~coordinates$z,
      i = ~vertices$i,
      j = ~vertices$j,
      k = ~vertices$k,
      opacity = 0.6,
      type = 'mesh3d',
      facecolor = rep('#1f77b4', dim(db$vertices)[1]),
      flatshading = T,
      hoverinfo = 'skip',
      name = 'Orientation range',
      showlegend = TRUE
    )
    #add to plotly - bottom half of plot
    p <- plotly::add_trace(   #sphere - blue
      p,
      data = db,
      x = -db$coordinates$x,
      y = -db$coordinates$y,
      z = -db$coordinates$z,
      i = db$vertices$i,
      j = db$vertices$j,
      k = db$vertices$k,
      opacity = 0.6,
      type = 'mesh3d',
      facecolor = rep('#1f77b4', dim(db$vertices)[1]),
      flatshading = T,
      hoverinfo = 'skip',
      name = 'Orientation range'
    )
  }
  ## ADD LAYOUT
  p <- plotly::layout(
    p,
    title = 'Root orientations (3D)',
    scene = list(
      xaxis  = list(
        title = paste('x', sep='')
      ),
      yaxis  = list(
        title = paste('y', sep=''),
        scaleanchor = "x"
      ),
      zaxis  = list(
        title = paste('z', sep=''),
        scaleanchor = "x"
      ),
      camera = list(
        eye = list(x=0, y=-2, z=0.5),
        center = list(x=0, y=0, z=0)
      )
    )
  )
  ## RETURN
  return(p)
}
