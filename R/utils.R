
#' Split an image in half along an axis, mirror the 2nd half of the image, and return the image list
#'
#' @param img a cimg
#' @param axis "x" or "y"
#' @return an imlist
#' @importFrom imager mirror imsplit
#' @export
halfimg_split <- function(img, axis = "y") {
  stopifnot(axis %in% c("x", "y"))
  workimg <- imsplit(img, axis = axis, nb = 2)
  workimg[[2]] <- mirror(workimg[[2]], axis = axis)
  workimg
}

#' Paste a split image back together, first mirroring the 2nd entry in the imlist
#'
#' @param imlist an imlist
#' @param axis "x" or "y"
#' @return an imlist
#' @importFrom imager mirror imappend
#' @export
halfimg_unsplit <- function(imlist, axis = "y") {
  stopifnot(axis %in% c("x", "y"))
  stopifnot(length(imlist) == 2)
  imlist[[2]] <- mirror(imlist[[2]], axis = axis)
  imappend(imlist, axis = axis)
}

#' Paste a split image back together, first mirroring the 2nd entry in the imlist
#'
#' @param imlist an imlist
#' @param axes "x", "y", or "xy"
#' @param fun a function that operates on a cimg
#' @return an imlist
#' @importFrom imager map_il
#' @export
map_halfimg <- function(img, fun, axes = "xy") {
  workimg <- img
  axeslist <- strsplit(axes, "")

  for (i in axeslist) {
    if (i %in% c("x", "y")) {
      if (!"axes" %in% formals(fun)) {
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
#' @param img cimage
#' @param n number of colors in returned image
#' @param return cimg
#' @importFrom imager renorm as.cimg
#' @importFrom dplyr select '%>%' row_number left_join mutate starts_with
#' @importFrom tidyr gather
#' @export
quantize_colors <- function(img, n = 16) {
  if (max(img) > 255) {
    img <- renorm(img)
  }
  # https://lumiamitie.github.io/r/imager-color-quantization/

  shoe_df <- img %>%
    as.data.frame(wide = 'c')

  img_cluster <- suppressMessages(
    shoe_df %>% dplyr::select(-x, -y) %>% kmeans(centers = n)
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
