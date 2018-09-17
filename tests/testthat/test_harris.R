context("harris")

library(imager)
library(dplyr)

img <- matrix(1, nrow = 5, ncol = 5) %>% as.cimg()
img[3,3] <- 0

test_that("harris_corners works as expected", {
  hc <- harris_corners(img, sigma = 1)
  expect_equal(sum(hc > 0), 25)
  expect_equivalent(colSums(hc < .001), c(2, 0, 0, 0, 2))
  expect_equivalent(rowSums(hc < .001), c(2, 0, 0, 0, 2))
  expect_equivalent(as.matrix(hc), t(as.matrix(hc)))
})

test_that("region_centers works as expected", {
  rc <- hc %>% imager::threshold("80%") %>% imager::label() %>% region_centers(bord = 1)
  expect_equivalent(rc, data_frame(value = 1, mx = 3, my = 3))
})

test_that("harris_keypoints works as expected", {
  hk <- harris_keypoints(img, thr = "80%", sigma = 1, bord = 1)
  expect_equivalent(hk$centers, rc[,2:3])
})

img2 <- img
img2[2, 2] <- img2[4, 4] <- 0
img3 <- imager::load.image(system.file("extdata", "parrots.png", package = "imager"))
test_that("oriented_gradients works as expected", {
  expect_equal(oriented_gradients(img2, show_plot = F), -45)
  expect_equivalent(oriented_gradients(img3, show_plot = F), c(0, 45))
})

do <- descriptor_orientation(img3, theta = 0, v = c(50, 50))
test_that("descriptor_orientation works as expected", {
  expect_equivalent(length(do), 243)
})
