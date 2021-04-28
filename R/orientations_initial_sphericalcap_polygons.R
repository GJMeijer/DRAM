#' Create polygons for each discrete cell on spherical cap
#'
#' @description
#' Discretises a spherical cap, bescribed by elevation angle `betamax`,
#' into a number of discrete cells. For each cell, the azimuth and
#' elevation angles are returned in a single dataframe
#'
#' @param beta0max the maximum elevation angle describing the spherical cap
#' @param n number of discrete orientation requested
#' @param resolution the number of polygon points to use per 1 radians of
#'   rotation
#' @param add an array of values to add to the coordinates for each cell.
#'   This may be useful for quickly plotting traces - many traces can be
#'   plotted in one command by seperating the coordinates for each cell by
#'   a single `NA`, when all coordinates are in a single vector
#' @param full if TRUE, the coordinates for the full cell are returned.
#'   if FALSE, only those for the bottom (`beta0_1`) and left side (`alpha0_0`)
#'   are returned.
#' @return dataframe with an entry for each cell. For each, the band it belongs
#'   to (`band`), the cell number on the band (`cell`), the total number of cells
#'   in the current band (`nband`), and the lower and upper azimuth
#'   (`alpha0_0`, `alpha0_1`) and elevation angles (`beta0_0`, `beta0_1`) are returned
#' @examples
#' orientations_initial_sphericalcap_polygons(pi/4, 15)
#' @export

orientations_initial_sphericalcap_polygons <- function(beta0max, n, resolution = 5*pi/180, add = NULL, full = TRUE){
  #create all grid cells
  do <- orientations_initial_sphericalcap_discretise(beta0max, n)
  #function to generate a single dataframe per cells
  if (full == TRUE){
    #function return full circumference for each cell
    f_temp <- function(band, cell, a0, a1, b0, b1){
      return(
        data.frame(
          band = band,
          cell = cell,
          alpha0 = c(
            seq(a0, a1, l = ceiling((a1-a0)/resolution)),
            rep(a1, ceiling((b1-b0)/resolution)),
            seq(a1, a0, l = ceiling((a1-a0)/resolution)),
            rep(a0, ceiling((b1-b0)/resolution)),
            add
          ),
          beta0 = c(
            rep(b0, ceiling((a1-a0)/resolution)),
            seq(b0, b1, l = ceiling((b1-b0)/resolution)),
            rep(b1, ceiling((a1-a0)/resolution)),
            seq(b1, b0, l = ceiling((b1-b0)/resolution)),
            add
          ),
          row.names = NULL
        )
      )
    }
  } else {
    #function only returns the bottom and left boundary
    f_temp <- function(band, cell, a0, a1, b0, b1){
      return(
        data.frame(
          band = band,
          cell = cell,
          alpha0 = c(
            seq(a0, a1, l = ceiling((a1-a0)/resolution)),
            rep(a1, ceiling((b1-b0)/resolution)),
            add
          ),
          beta0 = c(
            rep(b0, ceiling((a1-a0)/resolution)),
            seq(b0, b1, l = ceiling((b1-b0)/resolution)),
            add
          ),
          row.names = NULL
        )
      )
    }
  }
  #apply to each
  dpoly <- do.call(
    'rbind',
    apply(
      do[,c('band','cell','alpha0_0','alpha0_1','beta0_0','beta0_1')],
      1,
      function(x) f_temp(x[1],x[2],x[3],x[4],x[5],x[6])
    )
  )
  #delete sides if cell is in apex
  if (min(do$ncell) == 1){
    beta0_1 <- do$beta0_1[(do$band==1)]
    dpoly <- dpoly[!((dpoly$band==1) & (dpoly$beta0<beta0_1)), ]
  }
  #return
  return(dpoly)
}

