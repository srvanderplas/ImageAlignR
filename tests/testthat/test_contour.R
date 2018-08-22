context("contour")

testthat::setup({
  `%>%` <- dplyr::`%>%`

  if (!file.exists("poo.rds")) {
    poopath <- "https://upload.wikimedia.org/wikipedia/commons/thumb/9/92/Emojione_1F4A9.svg/240px-Emojione_1F4A9.svg.png"
    poo <- imager::load.image(poopath) %>%
      imager::rm.alpha() %>%
      bucketfill(x = 1, y = 1, color = c(1,1,1), sigma = 0)

    saveRDS(poo, "poo.rds")
  }
})

testthat::teardown({
  if (file.exists("poo.rds")) {
    file.remove("poo.rds")
  }
})


img <- readRDS("poo.rds")
imgmat <- img[, , 1, ]

test_that("outer_contour works as expected", {
  expect_warning(
    expect_message(outer_contour(imgmat),
                   "img is not a cimg object. Attempting to convert"),
    "Assuming third dimension corresponds to colour")
  expect_message(poo_oc <- outer_contour(img), "converting image to grayscale")

})

test_that("thin_contour works as expected", {


})
