#' Get outer contour of the object
#'
#' This function scans each row and column of the image and finds the minimum
#' and maximum coordinates which are less than thr (by default, the mean value
#' of the image).
#' @param img a cimg object
#' @param thr threshold to use for the image
#' @param as_cimg return the points as a cimg object?
#' @return either a cimg of points or a data frame of points indicating the boundary
#' @importFrom purrr map_df
#' @importFrom dplyr data_frame mutate filter select bind_rows group_by arrange summarize '%>%' ungroup
#' @importFrom imager imsplit as.cimg
#' @importFrom stringr str_replace
#' @export
outer_contour <- function(img, thr = mean(img), as_cimg = TRUE) {

  # Contour point detection
  ypoints <- img %>%
    imager::imsplit(axis = "y") %>%
    purrr::map_df(function(x) {
      idx <- which(as.numeric(x) < thr)
      if (length(idx) > 0) {
        return(dplyr::data_frame(x = c(min(idx), max(idx)),
                                 type = c("min", "max")))
      }
    }, .id = "yidx") %>%
    dplyr::mutate(y = str_replace(yidx, "y = ", "") %>% as.numeric(),
                  coord = "y") %>%
    dplyr::select(x, y, type, coord)

  xpoints <- img %>%
    imager::imsplit(axis = "x") %>%
    purrr::map_df(function(x) {
      idx <- which(as.numeric(x) < thr)
      if (length(idx) > 0) {
        return(dplyr::data_frame(y = c(min(idx), max(idx)),
                                 type = c("min", "max")))
      }
    }, .id = "xidx") %>%
    dplyr::mutate(x = str_replace(xidx, "x = ", "") %>% as.numeric(),
                  coord = "x") %>%
    dplyr::select(x, y, type, coord)

  contour_points <- dplyr::bind_rows(xpoints, ypoints) %>%
    dplyr::group_by(x, y) %>%
    dplyr::arrange(coord, type) %>%
    dplyr::summarize(type = paste(unique(type), collapse = ","),
                     coord = paste(unique(coord), collapse = ",")) %>%
    dplyr::ungroup()

  if (as_cimg) {
    contour_points <- contour_points  %>%
      dplyr::mutate(value = 1) %>%
      dplyr::select(x, y, value) %>%
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
#' @importFrom dplyr mutate filter group_by arrange '%>%' ungroup desc row_number
#' @importFrom magrittr multiply_by divide_by
#' @importFrom IM calcCentroid
#' @export
thin_contour <- function(contour, img = NULL, centroid = NULL, n_angles = 1800, as_cimg = TRUE) {
  stopifnot(!(is.null(img) & is.null(centroid)))

  if (is.null(centroid)) {
    centroid <- img %>%
      as.matrix %>%
      IM::calcCentroid() %>%
      round()
  }

  stopifnot(length(centroid) == 2)
  stopifnot(is.numeric(centroid))

  if (is.cimg(contour)) {
    contour <- as.data.frame(contour) %>%
      dplyr::filter(value > 0)
  }

  stopifnot("x" %in% names(contour))
  stopifnot("y" %in% names(contour))

  contour_points <- contour %>%
    dplyr::mutate(y1 = centroid[1], x1 = centroid[2],
           angle = atan2(y - y1, x - x1),
           radius = sqrt((y - y1)^2 + (x - x1)^2)) %>%
    dplyr::mutate(ard = angle %>% magrittr::multiply_by(n_angles) %>%
             round() %>% magrittr::divide_by(n_angles)) %>%
    dplyr::arrange(dplyr::desc(radius)) %>%
    dplyr::group_by(ard) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%
    dplyr::ungroup()

  if (as_cimg) {
    contour_points <- contour_points  %>%
      dplyr::mutate(value = 1) %>%
      dplyr::select(x, y, value) %>%
      imager::as.cimg(dim = dim(img))
  }
  contour_points

}
