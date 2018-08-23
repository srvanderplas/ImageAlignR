context("contour")

testthat::setup({
  `%>%` <- dplyr::`%>%`

  if (!file.exists("poo.rds")) {
    poopath <- "https://upload.wikimedia.org/wikipedia/commons/thumb/9/92/Emojione_1F4A9.svg/240px-Emojione_1F4A9.svg.png"
    temp_poo <- tempfile(fileext = ".png")
    download.file(poopath, destfile = temp_poo, quiet = T)
    poo <- suppressMessages(imager::load.image(temp_poo))

    saveRDS(poo, "poo.rds")
  }
})

testthat::teardown({
  if (file.exists("poo.rds")) {
    file.remove("poo.rds")
  }
})


imgorig <- readRDS("poo.rds")
img <- imgorig  %>%
  imager::rm.alpha() %>%
  imager::bucketfill(x = 1, y = 1, color = c(1,1,1), sigma = 0)
imgmat <- imager::grayscale(img)[, , 1, ]
poo_oc <- suppressMessages(outer_contour(img))
poo_oc2 <- suppressMessages(outer_contour(img, as_cimg = F))

# ------------------------------------------------------------------------------
test_that("outer_contour works as expected", {
  expect_s3_class(poo_oc, "cimg")
  expect_equivalent(as.numeric(poo_oc) %>% unique, c(0, 1))
  expect_equal(names(poo_oc2), c("x", "y", "type", "coord"))
  expect_equal(sort(unique(poo_oc2$type)), c("max", "max,min", "min"))
  expect_equal(sort(unique(poo_oc2$coord)), c("x", "y"))
})

# ------------------------------------------------------------------------------
test_that("thin_contour works as expected", {
  poo_oc <- outer_contour(imager::grayscale(img))
  expect_error(thin_contour(poo_oc),
               "Either img or centroid must not be null")
  expect_s3_class(thin_contour(poo_oc, img = img, n_angles = 36), "cimg")
  expect_s3_class(poo_tc_df <- thin_contour(poo_oc, img = img, n_angles = 12, as_cimg = F), "data.frame")
  expect_equal(length(unique(poo_tc_df$ard)), nrow(poo_tc_df))
})



# ------------------------------------------------------------------------------
test_that("ellipse_check works as expected", {
  expect_message(img_checkmat <- img_check(imgmat, T, T),
                 "img is not a cimg object. Attempting to convert")
  expect_message(img_noalpha <- img_check(imgorig, F, T),
                 "removing alpha channel")
  msgs <- capture_messages(
    expect_warning(
      img_check(imgorig, T, F),
      "Cannot keep alpha channel and remove color channels. Alpha channel will be removed"))
  expect_match(msgs[1], "removing alpha channel")
  expect_match(msgs[2], "converting image to grayscale")
  msgs <- capture_messages(
    img_nocolor <- img_check(imgorig)
  )
  expect_match(msgs[1], "removing alpha channel")
  expect_match(msgs[2], "converting image to grayscale")
})
