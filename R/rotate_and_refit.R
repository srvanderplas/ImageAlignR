#' Rotate the image and re-fit the bounding ellipse
#'
#' This should ensure that the major axis of the ellipse is vertical.
#' @param img a cimg of the object to be rotated
#' @param show_plot should plot of image and ellipse be displayed?
#' @param ... additional parameters to pass to imrotate
#' @importFrom assertthat assert_that
#' @importFrom imager pad bucketfill rotate_xy autocrop
#' @importFrom stats spectrum
#' @export
img_rotate_refit <- function(img, show_plot = F, ...) {
  . <- idx <- theta <- width <- NULL
  
  # Clean up inputs and check
  img <- img_check(img, keep_alpha = T, keep_color = T)
  
  ellipse <- img %>%
    outer_contour() %>%
    thin_contour(img = img) %>%
    contour_ellipse_fit()
  
  ellipse_check(ellipse)

  theta <- -ellipse$Angle
  if (theta > 90) {
    theta <- theta - 180
  } else if (theta > 45) {
    theta <- theta - 90
  } else if (theta > -45) {
    theta <- theta
  } else if (theta > -90) {
    theta <- theta + 90
  } else {
    theta <- theta + 180
  }
  
  n <- 250
  imgrot <-
    img %>%
    imager::pad(n, "xy", pos = 1, val = rep(1, imager::spectrum(.))) %>%
    imager::pad(n, "xy", pos = -1, val = rep(1, imager::spectrum(.))) %>%
    imager::bucketfill(1, 1, color = c(1, 1, 1), sigma = .1) %>% 
    imager::rotate_xy(angle = theta, cx = ellipse$CenterX + n,
                      cy = ellipse$CenterY + n,
                      interpolation = 1, boundary = 2) %>%
    imager::autocrop()

  refit <- imgrot %>%
    outer_contour() %>%
    thin_contour(img = imgrot) %>%
    contour_ellipse_fit()
  
  if (show_plot) {
    graphics::plot(imgrot)
    ellipse_points(refit, col = "orange")
  }
  
  list(img = imgrot, ellipse = refit)
}
