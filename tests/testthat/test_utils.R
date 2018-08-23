context("utils")

testthat::setup({
  `%>%` <- dplyr::`%>%`

  if (!file.exists("parrot_sub.rds")) {
    parrotpath <- system.file("extdata", "parrots.png", package = "imager")
    parrots <- imager::load.image(parrotpath) %>%
      imager::crop.bbox(., (imager::Xc(.) > 190 & imager::Yc(.) > 190 &
                              imager::Xc(.) <= 200 & imager::Yc(.) <= 200))

    saveRDS(parrots, "parrot_sub.rds")
  }
})

testthat::teardown({
  if (file.exists("parrot_sub.rds")) {
    file.remove("parrot_sub.rds")
  }
})


img <- readRDS("parrot_sub.rds")
imgmat <- img[, , 1, ]
splitres <- halfimg_split(img)

test_that("halfimg_split works as expected", {
  expect_warning(
    expect_message(halfimg_split(imgmat),
                   "img is not a cimg object. Attempting to convert"),
    "Assuming third dimension corresponds to colour")
  expect_error(halfimg_split(img, "c"), "axis must be .x. or .y.")
  expect_equal(names(splitres), c("y = 1 - 5", "y = 6 - 10"))
  expect_equal(length(splitres), 2)
  expect_equivalent(dim(splitres[[1]]), c(10, 5, 1, 3))
})

test_that("halfimg_unsplit works as expected", {
  expect_error(halfimg_unsplit(splitres, "c"), "axis must be .x. or .y.")
  expect_error(halfimg_unsplit(c(splitres, splitres)),
               "imlist must have length 2")
  expect_equivalent(halfimg_unsplit(splitres), img)
})

test_that("map_halfimg works as expected", {
  expect_warning(
    expect_message(map_halfimg(imgmat),
                   "img is not a cimg object. Attempting to convert"),
    "Assuming third dimension corresponds to colour")
  expect_error(map_halfimg(img, axes = "c"), "axes must be 'x', 'y', or 'xy'")
  expect_error(map_halfimg(img, "c"), "fun must be a function")
  expect_equivalent(img, map_halfimg(img))
  expect_equivalent(img, map_halfimg(img, fun = function(x, axes = "xy") x))

})

test_that("quantize_colors works as expected", {
  expect_warning(
    expect_message(quantize_colors(imgmat, 4),
                   "img is not a cimg object. Attempting to convert"),
    "Assuming third dimension corresponds to colour")
  qimg <- quantize_colors(img, n = 4) %>% as.data.frame(wide = 'c')
  expect_equal(nrow(unique(qimg[,3:5])), 4)
})
