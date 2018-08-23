#' Fit an ellipse to the boundary contour points
#'
#' @param contour_points a cimg or data frame of contour points.
#'          If a cimg, must be a pixset (0s and 1s). If a data frame, must have
#'          columns x and y
#' @param chull use the convex hull of the points?
#' @return a data frame of ellipse parameters: X, Y for the center, A, B for the
#'           major and minor axes, and angle for the rotation angle.
#' @export
#' @importFrom conicfit AtoG EllipseDirectFit
#' @importFrom dplyr '%>%'
#' @importFrom grDevices chull
#' @import assertthat
contour_ellipse_fit <- function(contour_points, chull = F) {
  if ("cimg" %in% class(contour_points)) {
    cp <- contour_points %>%
      as.data.frame

    assertthat::assert_that(
      assertthat::has_name(cp, "value")
    )

    cp <- cp[cp$value > 0,]
  } else {
    cp <- contour_points
  }

  if (is.data.frame(cp)) {
    cp <- data.frame(x = cp$x, y = cp$y)
  } else {
    tmp <- grDevices::xy.coords(cp)
    cp <- data.frame(x = tmp$x, y = tmp$y)
  }

  assertthat::assert_that(
    is.data.frame(cp),
    assertthat::has_name(cp, "x"),
    assertthat::has_name(cp, "y")
  )

  if (chull) {
    ch_cp <- chull(cp)
    cp <- cp[ch_cp,]
  } else {
    cp <- cp
  }

  efit <- conicfit::EllipseDirectFit(cp)

  # if (!quiet) {
  #   switch(efitG$exitCode,
  #          "-1" = "degenerate",
  #          "0" = "imaginary ellipse",
  #          "4" = "imaginary parallel lines",
  #          "1" = "ellipse",
  #          "2" = "hyperbola",
  #          "3" = "parabola"
  # }

  efitG <- conicfit::AtoG(efit)

  tmp <- efitG$ParG %>%
    t() %>%
    as.data.frame()

  names(tmp) <- c("CenterX", "CenterY", "AxisA", "AxisB", "Angle")
  tmp$Angle <- tmp$Angle * 180/pi

  return(tmp)
}

#' Return the boundary points of an ellipse
#'
#' @param ellipse output from contour_ellipse_fit, or a data frame with the
#'          following columns: CenterX, CenterY, AxisA, AxisB, Angle
#' @param n number of points
#' @param plot_lines return call to lines?
#' @param ... additional parameters for lines call
#' @return two-column data frame of x and y points
#' @export
#' @importFrom conicfit calculateEllipse
#' @import assertthat
ellipse_points <- function(ellipse, n = 300, plot_lines = T, ...) {
  assertthat::assert_that(
    assertthat::has_name(ellipse, "CenterX"),
    assertthat::has_name(ellipse, "CenterY"),
    assertthat::has_name(ellipse, "AxisA"),
    assertthat::has_name(ellipse, "AxisB"),
    assertthat::has_name(ellipse, "Angle"),
    is.numeric(ellipse$CenterX),
    is.numeric(ellipse$CenterY),
    is.numeric(ellipse$AxisA),
    is.numeric(ellipse$AxisB),
    is.numeric(ellipse$Angle)
  )

  tmp <- conicfit::calculateEllipse(x = ellipse$CenterX, y = ellipse$CenterY,
                                    a = ellipse$AxisA, b = ellipse$AxisB,
                                    angle = ellipse$Angle, steps = n) %>%
    as.data.frame()
  names(tmp) <- c("x", "y")

  if (plot_lines) {
    graphics::lines(tmp$x, tmp$y, ...)
    return(invisible(tmp))
  }
  return(tmp)
}
