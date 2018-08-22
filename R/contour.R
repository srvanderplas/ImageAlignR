#' Get outer contour of the object
#'
#' This function scans each row and column of the image and finds the minimum
#' and maximum coordinates which are less than thr (by default, the mean value
#' of the image).
#' @param img a cimg object
#' @param thr threshold to use for the image
#' @param as_cimg return the points as a cimg object?
#' @return either a cimg of points or a data frame of points indicating the boundary
#' @importFrom dplyr data_frame mutate filter select bind_rows group_by arrange summarize '%>%' ungroup
#' @export
outer_contour <- function(img, thr = mean(img), as_cimg = TRUE) {
  x <- y <- yidx <- type <- coord <- xidx <- value <- NULL

  if (!"cimg" %in% class(img)) {
    message("img is not a cimg object. Attempting to convert")
    img <- imager::as.cimg(img)
  }
  assertthat::assert_that("cimg" %in% class(img))

  if (length(imager::color.at(img, 1, 1)) > 1) {
    message("converting image to grayscale")
    img <- imager::grayscale(img)
  }

  # Contour point detection
  ypoints <- img %>%
    imager::imsplit(axis = "y")
  ypointdf <- lapply(1:length(ypoints), function(x) {
    idx <- which(as.numeric(ypoints[[x]]) < thr)
    if (length(idx) > 0) {
      data_frame(x = c(min(idx), max(idx)),
                 type = c("min", "max"),
                 yidx = names(ypoints)[x])
    }
  }) %>%
    bind_rows()
  ypointdf <- ypointdf %>%
    mutate(y = gsub(x = yidx, pattern = "y = ", replacement = "") %>%
             as.numeric(),
           coord = "y") %>%
    select(x, y, type, coord)

  xpoints <- img %>%
    imager::imsplit(axis = "x")
  xpointdf <- lapply(1:length(xpoints), function(x) {
    idx <- which(as.numeric(xpoints[[x]]) < thr)
    if (length(idx) > 0) {
      return(data_frame(y = c(min(idx), max(idx)),
                        type = c("min", "max"),
                        xidx = names(xpoints)[x]))
    }
  }) %>%
    bind_rows()
  xpointdf <- xpointdf %>%
    mutate(x = gsub(x = xidx, pattern = "x = ", replacement = "") %>%
             as.numeric(),
           coord = "x") %>%
    select(x, y, type, coord)

  contour_points <- bind_rows(xpointdf, ypointdf) %>%
    group_by(x, y) %>%
    arrange(coord, type) %>%
    summarize(type = paste(unique(type), collapse = ","),
              coord = paste(unique(coord), collapse = ",")) %>%
    ungroup()

  if (as_cimg) {
    contour_points <- contour_points  %>%
      mutate(value = 1) %>%
      select(x, y, value) %>%
      imager::as.cimg(dim = dim(img))
  }

}

#' Thin outer contour by removing points overlappling radially from the centroid
#'
#' @param contour results from outer_contour
#' @param img thresholded image (used to calculate the centroid)
#' @param centroid centroid of the image
#' @param n_angles number of unique angles to use
#' @param as_cimg return the points as a cimg object?
#' @return either a cimg of points or a data frame of points indicating the boundary
#' @importFrom dplyr mutate filter group_by arrange '%>%' ungroup desc row_number select
#' @export
thin_contour <- function(contour, img = NULL, centroid = NULL, n_angles = 1800, as_cimg = TRUE) {
  value <- y <- y1 <- x <- x1 <- angle <- radius <- ard <- NULL
  calcCentroid <- function(mat) {
    # From the IM:::calcCentroid method:
    # selectMethod(IM:::calcCentroid, "matrix")
    m <- mat.or.vec(3, 1)
    NY <- dim(mat)[1]
    NX <- dim(mat)[2]
    m[1] <- sum(mat)
    m[2] <- sum(t(mat) %*% (1:NY))
    m[3] <- sum(mat %*% (1:NX))
    c(m[2]/m[1], m[3]/m[1])
  }

  stopifnot(!(is.null(img) & is.null(centroid)))

  if (!is.null(img)) {
    if (!"cimg" %in% class(img)) {
      message("img is not a cimg object. Attempting to convert")
      img <- imager::as.cimg(img)
    }
    assertthat::assert_that("cimg" %in% class(img))
  }

  if (is.null(centroid)) {
    if (dim(img)[4] > 3) {
      img <- imager::rm.alpha(img)
    }
    imgmat <- imager::grayscale(img)[,]

    centroid <- imgmat %>%
      calcCentroid() %>%
      round()
  }

  stopifnot(length(centroid) == 2)
  stopifnot(is.numeric(centroid))

  if (imager::is.cimg(contour)) {
    contour <- as.data.frame(contour) %>%
      filter(value > 0)
  }

  stopifnot("x" %in% names(contour))
  stopifnot("y" %in% names(contour))

  contour_points <- contour %>%
    mutate(y1 = centroid[1], x1 = centroid[2],
           angle = atan2(y - y1, x - x1),
           radius = sqrt((y - y1)^2 + (x - x1)^2)) %>%
    mutate(ard = round(angle * n_angles) / n_angles) %>%
    arrange(desc(radius)) %>%
    group_by(ard) %>%
    filter(row_number() == 1) %>%
    ungroup()

  if (as_cimg) {
    contour_points <- contour_points  %>%
      mutate(value = 1) %>%
      select(x, y, value) %>%
      imager::as.cimg(dim = dim(img))
  }
  contour_points

}
