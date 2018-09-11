#' Harris Keypoint detector
#'
#' The value of the Harris transformation is high only in well defined corners
#' on the image.
#'
#' Source: https://www.kaggle.com/vicensgaitan/image-registration-the-r-way/notebook
#'
#' @param im image
#' @param sigma sigma for gaussian blur function
#' @param eps threshold (defaults to 1e-10)
#' @return corner function value
#' @export
#' @importFrom imager imgradient isoblur
harris_corners <- function(im, sigma = 2, eps = 1e-10) {
  ix <- imager::imgradient(im, "x")
  iy <- imager::imgradient(im, "y")
  ix2 <- imager::isoblur(ix * ix, sigma, gaussian = T)
  iy2 <- imager::isoblur(iy * iy, sigma, gaussian = T)
  ixy <- imager::isoblur(ix * iy, sigma, gaussian = T)
  (ix2 * iy2 - ixy * ixy) / (ix2 + iy2 + eps)
}

#' Get centers of a set of labeled regions within an image
#'
#' @param bord border size
#' @return a data frame of coordinates
#' @export
#' @importFrom dplyr group_by summarize filter
#' @export
region_centers <- function(im, bord = 10) {
  as.data.frame(im) %>%
    dplyr::filter(value > 0) %>%
    dplyr::group_by(value) %>%
    dplyr::summarize(
      mx = round(mean(x)),
      my = round(mean(y))
    ) %>%
    dplyr::filter(
      mx > bord, mx < (imager::width(im) - bord),
      my > bord, my < (imager::height(im) - bord)
    )
}

#' Detect keypoints using Harris corner detector, and find centers of associated regions
#'
#' Runs the harris_corners function, then selects as keypoints the center of
#' connected regions with Harris over a certain threshod.
#'
#' @param im image
#' @param thr threshold, as passed to imager::threshold
#' @param sigma blur used for corner detection
#' @param bord border region
#' @return a list, with an image labeled by region and a data frame of the region's centers.
#' @importFrom imager threshold label
#' @export
harris_keypoints <- function(im, thr = "99%", sigma = 3, bord = 10) {
  # Harris keypoints
  kp <- harris_corners(im, sigma) %>%
    imager::threshold(thr) %>%
    imager::label()

  list(regions = kp, centers = region_centers(kp, bord)[, 2:3])
}

#' Histogram of oriented gradients
#'
#' @param im image
#' @return a list of one or two critical orientations
#' @export
hog <- function(im) {
  ix <- imgradient(im, "x")
  iy <- imgradient(im, "y")
  ita <- atan(iy / ix) * 180 / pi
  iga <- table(sample(round(ita * 2) / 2, 200000))
  # plot(iga)
  ma1 <- max(iga)[1]
  m1 <- which(iga == ma1)
  theta_1 <- (as.numeric(names(m1)))
  iga[max((m1 - 20), 0):min((m1 + 20), length(iga))] <- 0
  # plot(iga)
  ma2 <- max(iga)[1]
  m2 <- which(iga == ma2)
  theta_2 <- (as.numeric(names(m2)))
  if (theta_1 > 45) theta_1 <- theta_1 - 90
  if (theta_1 < (-45)) theta_1 <- theta_1 + 90
  if (theta_2 > 45) theta_2 <- theta_2 - 90
  if (theta_2 < (-45)) theta_2 <- theta_2 + 90
  if (abs(theta_1 - theta_2) > 5) {
    return(c(theta_1, theta_2))
  }
  else {
    return(theta_1)
  }
}
