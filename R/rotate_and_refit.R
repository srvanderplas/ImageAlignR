#' Rotate the image and re-fit the bounding ellipse
#'
#' This should ensure that the major axis of the ellipse is vertical.
#' @param img a cimg of the object to be rotated
#' @param ellipse the original (potentially tilted) ellipse
#' @param ... additional parameters to pass to imrotate
#' @importFrom assertthat assert_that
#' @export
img_rotate_refit <- function(img, ellipse, ...) {

  # Clean up inputs and check
  img <- img_check(img, keep_alpha = T, keep_color = T)
  ellipse_check(ellipse)

  n <- 250
  imgrot <-
    img %>%
    imager::pad(n, "xy", pos = 1, val = rep(1, spectrum(.))) %>%
    imager::pad(n, "xy", pos = -1, val = rep(1, spectrum(.))) %>%
    imager::bucketfill(1, 1, color = c(1, 1, 1), sigma = .1) %>% 
    imager::rotate_xy(angle = -ellipse$Angle, cx = ellipse$CenterX + n,
                      cy = ellipse$CenterY + n,
                      interpolation = 1, boundary = 2) %>%
    autocrop

  refit <- imgrot %>%
    outer_contour() %>%
    thin_contour(img = imgrot) %>%
    contour_ellipse_fit()
  
  plot(imgrot)
  ellipse_points(rot_ellipse, col = "orange")
  
  list(img = imgrot, ellipse = refit)
}
