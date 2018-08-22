
#' Split an image in half along an axis, mirror the 2nd half of the image, and return the image list
#'
#' @param img a cimg
#' @param axis "x" or "y"
#' @return an imlist
#' @export
halfimg_split <- function(img, axis = "y") {
  if (!"cimg" %in% class(img)) {
    message("img is not a cimg object. Attempting to convert")
    img <- imager::as.cimg(img)
  }
  assertthat::assert_that("cimg" %in% class(img))
  assertthat::assert_that(axis %in% c("x", "y"), msg = "axis must be 'x' or 'y'")
  workimg <- imager::imsplit(img, axis = axis, nb = 2)
  workimg[[2]] <- imager::mirror(workimg[[2]], axis = axis)
  workimg
}

#' Paste a split image back together, first mirroring the 2nd entry in the imlist
#'
#' @param imlist an imlist
#' @param axis "x" or "y"
#' @return an imlist
#' @importFrom assertthat assert_that
#' @export
halfimg_unsplit <- function(imlist, axis = "y") {
  assertthat::assert_that(axis %in% c("x", "y"), msg = "axis must be 'x' or 'y'")
  assertthat::assert_that(length(imlist) == 2, msg = "imlist must have length 2")
  imlist[[2]] <- imager::mirror(imlist[[2]], axis = axis)
  imager::imappend(imlist, axis = axis)
}

#' Split an image in half along one or more axes, apply a function, and paste it back together.
#'
#' @param img a cimg object
#' @param fun a function that operates on a cimg
#' @param axes "x", "y", or "xy"
#' @return an imlist
#' @importFrom imager map_il
#' @importFrom dplyr '%>%'
#' @export
map_halfimg <- function(img, fun = function(x) x, axes = "xy") {
  if (!"cimg" %in% class(img)) {
    message("img is not a cimg object. Attempting to convert")
    img <- imager::as.cimg(img)
  }
  assertthat::assert_that("cimg" %in% class(img))

  assertthat::assert_that(is.function(fun), msg = "fun must be a function")
  workimg <- img
  axeslist <- strsplit(axes, "") %>% unlist()

  assertthat::assert_that(sum(axeslist %in% c("x", "y")) == length(axeslist),
                          msg = "axes must be 'x', 'y', or 'xy'")

  for (i in axeslist) {
    if (i %in% c("x", "y")) {
      if (!"axes" %in% methods::formalArgs(fun)) {
        workimg <- halfimg_split(workimg, i) %>%
          map_il(fun) %>%
          halfimg_unsplit(axis = i)
      } else {
        workimg <- halfimg_split(workimg, i) %>%
          map_il(fun, axes = i) %>%
                   halfimg_unsplit(axis = i)
      }
    }
  }

  workimg
}


#' Limit the number of colors in the image
#'
#' From https://lumiamitie.github.io/r/imager-color-quantization/
#' @param img cimage
#' @param n number of colors in returned image
#' @return a cimage with only n colors
#' @importFrom imager renorm as.cimg
#' @importFrom dplyr select '%>%' row_number left_join mutate starts_with
#' @importFrom tidyr gather
#' @export
quantize_colors <- function(img, n = 16) {
  x <- y <- label <- cc <- NULL

  if (!"cimg" %in% class(img)) {
    message("img is not a cimg object. Attempting to convert")
    img <- imager::as.cimg(img)
  }
  assertthat::assert_that("cimg" %in% class(img))


  if (max(img) > 255) {
    img <- renorm(img)
  }

  shoe_df <- img %>%
    as.data.frame(wide = 'c')

  img_cluster <- suppressMessages(
    shoe_df %>% dplyr::select(-x, -y) %>% stats::kmeans(centers = n)
  )

  shoe_df %>%
    dplyr::mutate(label = as.character(img_cluster$cluster)) %>%
    dplyr::select(x, y, label) %>%
    dplyr::left_join(
      img_cluster$centers %>%
        as.data.frame() %>%
        dplyr::mutate(label = as.character(dplyr::row_number())),
      by = "label") %>%
    dplyr::select(-label) %>%
    tidyr::gather(key = 'cc', value = 'value', dplyr::starts_with('c.')) %>%
    dplyr::mutate(cc = as.integer(gsub('c\\.', '', cc))) %>%
    as.cimg(dim = dim(img))
}
