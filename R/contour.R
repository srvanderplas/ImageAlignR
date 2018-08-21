#' Get outer contour of the object
#'
#' This function scans each row and column of the image and finds the minimum
#' and maximum coordinates which are less than thr (by default, the mean value
#' of the image).
#' @param img a cimg object
#' @param thr threshold to use for the image
#' @return data frame of points which make up the boundary
#' @importFrom purrr map_df
#' @importFrom dplyr data_frame mutate filter select bind_rows group_by arrange summarize '%>%' ungroup
#' @importFrom imager imsplit
#' @importFrom stringr str_replace
outer_contour <- function(img, thr = mean(img)) {

  # Contour point detection
  ypoints <- img %>%
    imager::imsplit(axis = "y") %>%
    purrr::map_df(function(x)
      dplyr::data_frame(x = c(min(which(as.numeric(x) < thr)),
                              max(which(as.numeric(x) < thr))),
                        type = c("min", "max")),
      .id = "yidx") %>%
    dplyr::mutate(y = str_replace(yidx, "y = ", "") %>% as.numeric(),
                  coord = "y") %>%
    dplyr::filter(is.finite(x)) %>%
    dplyr::select(x, y, type, coord)

  xpoints <- img %>%
    imager::imsplit(axis = "x") %>%
    purrr::map_df(function(x)
      dplyr::data_frame(y = c(min(which(as.numeric(x) < thr)),
                              max(which(as.numeric(x) < thr))),
                        type = c("min", "max")),
      .id = "xidx") %>%
    dplyr::mutate(x = str_replace(xidx, "x = ", "") %>% as.numeric(),
                  coord = "x") %>%
    dplyr::filter(is.finite(y)) %>%
    dplyr::select(x, y, type, coord)

  contour_points <- dplyr::bind_rows(xpoints, ypoints) %>%
    dplyr::group_by(x, y) %>%
    dplyr::arrange(coord, type) %>%
    dplyr::summarize(type = paste(unique(type), collapse = ","),
                     coord = paste(unique(coord), collapse = ",")) %>%
    dplyr::ungroup()

}

#' Thin outer contour by removing points overlappling radially from the centroid
#'
#' @param contour results from outer_contour
#' @param img thresholded image (used to calculate the centroid)
#' @param centroid centroid of the image
#' @param n_angles number of unique angles to use
#' @return data frame of points which make up the boundary
#' @importFrom dplyr mutate filter group_by arrange '%>%' ungroup desc row_number
#' @importFrom magrittr multiply_by divide_by
#' @importFrom IM calcCentroid
thin_contour <- function(contour, img = NULL, centroid = NULL, n_angles = 1800) {
  stopifnot(!(is.null(img) & is.null(centroid)))

  if (is.null(centroid)) {
    centroid <- img %>%
      as.matrix %>%
      IM::calcCentroid() %>%
      round()
  }

  stopifnot(length(centroid) == 2)
  stopifnot(is.numeric(centroid))

  stopifnot("x" %in% names(contour))
  stopifnot("y" %in% names(contour))

  contour %>%
    dplyr::mutate(y1 = centroid[1], x1 = centroid[2],
           angle = atan2(y - y1, x - x1),
           radius = sqrt((y - y1)^2 + (x - x1)^2)) %>%
    dplyr::mutate(ard = angle %>% magrittr::multiply_by(n_angles) %>%
             round() %>% magrittr::divide_by(n_angles)) %>%
    dplyr::arrange(dplyr::desc(radius)) %>%
    dplyr::group_by(ard) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%
    dplyr::ungroup()
}
