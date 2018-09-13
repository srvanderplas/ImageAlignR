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
#' @importFrom imager imgradient isoblur
#' @export
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
#' Source: https://www.kaggle.com/vicensgaitan/image-registration-the-r-way/notebook
#'
#' @param im image returned from harris_corners()
#' @param bord border size
#' @return a data frame of coordinates
#' @importFrom dplyr group_by summarize filter
#' @export
region_centers <- function(im, bord = 30) {
  value <- x <- y <- mx <- my <- NULL
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
#' Source: https://www.kaggle.com/vicensgaitan/image-registration-the-r-way/notebook
#'
#' @param im image
#' @param thr threshold, as passed to imager::threshold
#' @param sigma blur used for corner detection
#' @param bord border region
#' @return a list, with an image labeled by region and a data frame of the region's centers.
#' @importFrom imager threshold label
#' @export
harris_keypoints <- function(im, thr = "99%", sigma = 3, bord = 30) {
  # Harris keypoints
  kp <- harris_corners(im, sigma) %>%
    imager::threshold(thr) %>%
    imager::label()

  list(regions = kp, centers = region_centers(kp, bord)[, 2:3])
}

#' Find most common gradients
#'
#' Source: https://www.kaggle.com/vicensgaitan/image-registration-the-r-way/notebook
#'
#' @param im image
#' @param sigma blur to use for hog image
#' @param show_plot show histogram of gradients?
#' @return a list of one or two critical orientations
#' @importFrom imager imgradient isoblur
#' @export
oriented_gradients <- function(im, sigma = 0, show_plot = T) {
  if (sigma > 0) {
    imblur <- imager::isoblur(im, sigma = sigma, gaussian = T)
  } else {
    imblur <- im
  }
  n <- length(im)
  ix <- imager::imgradient(imblur, "x")
  iy <- imager::imgradient(imblur, "y")
  ita <- atan(iy / ix) * 180 / pi
  iga <- table(sample(round(ita * 2) / 2, pmin(n, 200000)))

  if (show_plot) plot(iga)

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

#' Calculate orientation of keypoint descriptors
#'
#' Source: https://www.kaggle.com/vicensgaitan/image-registration-the-r-way/notebook
#'
#' @param im image
#' @param stencil_ext stencil used to get a patch around the keypoint
#' @param stencil stencil used to calculate the descriptor
#' @param theta angle to rotate the image
#' @param v numeric vector of length 2 giving x and y coordinates of the keypoint
#' @importFrom imager get.stencil as.cimg imrotate width grayscale
#' @export
descriptor_orientation <- function(im, stencil_ext = NULL, stencil = NULL, theta, v) {
  imdims <- dim(im)
  nx <- imdims[1]
  ny <- imdims[2]

  stencil_x <- pmin(floor(nx/8), 20)
  stencil_y <- pmin(floor(ny/8), 20)
  stencil_ext_x <- pmin(floor(nx/5), 30)
  stencil_ext_y <- pmin(floor(ny/5), 30)

  if (is.null(stencil)) {
    stencil <- expand.grid(
      dx = round(seq(-stencil_x, stencil_x, length.out = 9)),
      dy = round(seq(-stencil_y, stencil_y, length.out = 9)))
  }
  if (is.null(stencil_ext)) {
    stencil_ext <- expand.grid(dx = round(seq(-stencil_ext_x, stencil_ext_x, 1)),
                               dy = round(seq(-stencil_ext_y, stencil_ext_y, 1)))
  }

  # if (dim(im)[4] != 1) {
  #   message("Converting to grayscale")
  #   im <- imager::grayscale(im)
  # }
  ccs <- seq(1, pmin(3, imdims[4]), by = 1)

  im_cc <- imager::imsplit(im, 'cc')
  v <- as.numeric(v)
  pm <- map_il(im_cc, function(.) {
    im.st <- imager::get.stencil(., stencil_ext, x = v[1], y = v[2])
    w <- sqrt(length(im.st))
    im.st <- imager::as.cimg(im.st, x = w, y = w)
    imr <- imager::imrotate(im.st, -theta)
    ww <- round(imager::width(imr) / 2)
    ims <- imager::get.stencil(imr, stencil, x = ww, y = ww)
    www <- sqrt(length(ims))
    ims <- imager::as.cimg(ims, x = www, y = www)
    ims
  })
  imager::imappend(pm, 'cc') %>% as.numeric()
}

#' Get KNN for feature point matches
#'
#' Source: https://www.kaggle.com/vicensgaitan/image-registration-the-r-way/notebook
#'
#' @param kpf_a matrix of features for image a
#' @param kpf_b matrix of features for image b
#' @param centers_a matrix of points for image a
#' @param centers_b matrix of points for image b
#' @param n number of neighbors to keep
#' @param ratio ratio of first neighbor to second neighbor distance. Values smaller than this will be kept
#' @param show_plot show a plot of matches kept?
#' @export
#' @importFrom FNN get.knnx
knn_points <- function(kpf_a, kpf_b, centers_a, centers_b, n = 2, ratio = .8, show_plot = F) {
  kk <- FNN::get.knnx(data = kpf_a, query = kpf_b, k = n, algorithm = "kd_tree")
  kpf_b_idx_vec <- 1:nrow(kpf_b)

  mask <- kk$nn.dist[, 1] / kk$nn.dist[, 2] < ratio
  match <- data_frame(a = kk$nn.index[mask, 1], b = kpf_b_idx_vec[mask])

  avec <- if (dim(kpf_a)[1]/dim(centers_a)[1] == 2) rbind(centers_a, centers_a) else centers_a
  bvec <- if (dim(kpf_b)[1]/dim(centers_b)[1] == 2) rbind(centers_b, centers_b) else centers_b

  p1 <- as.matrix(avec[match$a, ])
  p2 <- as.matrix(bvec[match$b, ])

  if (show_plot) {
    graphics::plot(kk$nn.dist[, 1], kk$nn.dis[, 2], pch = ".")
    graphics::points(kk$nn.dist[mask, 1], kk$nn.dis[mask, 2], pch = "o", col = "red")
  }
  return(list(points_a = p1, points_b = p2))
}

#' Estimate homography from points in P to points in p
#'
#' Source: https://www.kaggle.com/vicensgaitan/image-registration-the-r-way/notebook
#'
#' @param P set of points
#' @param p set of points in different space
est_homograph <- function(P, p) {
  n <- nrow(P)
  hh <- NULL
  for (i in 1:n) {
    a <- t(c(p[i, ], 1))
    b <- t(c(0, 0, 0))
    c <- P[i, ]
    d <- -c %*% a
    hh <- rbind(hh, cbind(rbind(c(a, b), c(b, a)), d))
  }
  h <- t(matrix(svd(hh, nv = ncol(hh))$v[, 9], nrow = 3, ncol = 3))
}

#' Apply homography to points in p
#'
#' Source: https://www.kaggle.com/vicensgaitan/image-registration-the-r-way/notebook
#'
#' @param h homography
#' @param p points
apply_homograph <- function(h, p) {
  p1 <- t(cbind(p, 1))
  q1 <- t(h %*% p1)
  q1 <- q1 / q1[, 3]
  q1[, 1:2]
}

#' Robust homography estimation from p1 to p2.
#'
#' Source: https://www.kaggle.com/vicensgaitan/image-registration-the-r-way/notebook
#'
#' Returns h and the list of inliers
#' @param p1 point set
#' @param p2 point set
#' @param thresh threshold value
#' @param N samples to use
#' @export
ransac <- function(p1, p2, thresh = 100, N = 1000) {
  n <- nrow(p1)

  sn <- c(1:n)
  flag <- matrix(0, nrow = N, ncol = n)
  for (i in 1:N) {
    smpl <- sample(sn, 4)
    pp1 <- p1[smpl, ]
    pp2 <- p2[smpl, ]
    h <- est_homograph(pp2, pp1)
    p <- apply_homograph(h, p1)
    d <- rowSums((p - p2)^2)
    flag[i, ] <- as.numeric(d < thresh)
  }
  sinliers <- rowSums(flag)
  sinliers <- sinliers[!is.na(sinliers)]
  imax <- which(sinliers == max(sinliers))[1]
  inliers <- sn[flag[imax, ] == 1]
  h <- est_homograph(p2[inliers, ], p1[inliers, ])
  list(homography = h, inliers = inliers)
}

#' Function to generate an affine transform
#'
#' Returns a function taking arguments x and y, for use in imager::imwarp
#' @param homography homograph matrix from ransac (unsolved)
#' @export
map_affine_gen <- function(homography) {
  h1 <- solve(homography)
  function(x, y) {
    p <- apply_homograph(h1, cbind(x, y))
    list(x = p[,1], y = p[,2])
  }
}

#' Function to make two images the same size by padding one image
#'
#' @param img1 image 1
#' @param img2 image 2
#' @export
#' @importFrom imager pad imlist imresize
images_resize <- function(img1, img2) {
  img1dim <- dim(img1)
  img2dim <- dim(img2)

  scale_x <- img2dim[1]/img1dim[1]
  scale_y <- img2dim[2]/img1dim[2]

  img1 <- imager::imresize(img1, scale = pmin(scale_x, scale_y))
  img1dim <- dim(img1)

  xdim <- max(img1dim[1], img2dim[1])
  ydim <- max(img1dim[2], img2dim[2])

  img_a <- img1
  img_b <- img2

  img_a_max_val <- img_a %>% imsplit('c') %>% map_dbl(max)
  img_b_max_val <- img_b %>% imsplit('c') %>% map_dbl(max)

  if (xdim > img1dim[1]) {
    img_a <- imager::pad(img_a, nPix = xdim - img1dim[1], axes = "x", pos = 1, val = img_a_max_val)
  }
  if (xdim > img2dim[1]) {
    img_b <- imager::pad(img_b, nPix = xdim - img2dim[1], axes = "x", pos = 1, val = img_b_max_val)
  }
  if (ydim > img1dim[2]) {
    img_a <- imager::pad(img_a, nPix = ydim - img1dim[2], axes = "y", pos = 1, val = img_a_max_val)
  }
  if (ydim > img2dim[2]) {
    img_b <- imager::pad(img_b, nPix = ydim - img2dim[2], axes = "y", pos = 1, val = img_b_max_val)
  }
  return(imager::imlist(img_a, img_b))
}

#' Align two images
#'
#' @param img1 image 1
#' @param img2 image 2
#' @return an image list with image 1 and image 2 on the same coordinate system
#' @export
#' @import dplyr
#' @importFrom tidyr unnest nest
#' @importFrom purrr pmap
#' @importFrom imager grayscale
align_images <- function(img1, img2) {
  theta <- idx <- . <- NULL

  imgs <- images_resize(img1, img2)
  img_a <- imgs[[1]]
  img_b <- imgs[[2]]

  hkp_a <- harris_keypoints(img_a, sigma = 6)
  hkp_b <- harris_keypoints(img_b, sigma = 6)

  angles_a <- img_a %>% oriented_gradients(sigma = 3)
  angles_b <- img_b %>% oriented_gradients(sigma = 3)

  get_kpf <- function(angles, hkp, im) {
    v <- angle <- mx <- my <- n
    kpa <- dplyr::data_frame(angle = angles, v = list(hkp$centers)) %>%
      tidyr::unnest(v) %>%
      dplyr::rename(theta = angle, x = mx, y = my) %>%
      dplyr::mutate(idx = 1:dplyr::n()) %>%
      dplyr::rowwise() %>%
      tidyr::nest(-theta, -idx, .key = "v") %>%
      dplyr::select(-idx)
    purrr::pmap(list(theta = kpa$theta, v = kpa$v), descriptor_orientation,
                im = imager::grayscale(im)) %>%
      do.call("rbind", .)
  }

  kpf_a <- get_kpf(angles_a, hkp_a, img_a)
  kpf_b <- get_kpf(angles_b, hkp_b, img_b)

  match_points <- knn_points(kpf_a, kpf_b, hkp_a$centers, hkp_b$centers, show_plot = T)

  ransac_points <- ransac(match_points$points_a, match_points$points_b)

  map_fcn <- map_affine_gen(ransac_points$homography)

  imager::imlist(
    imager::imwarp(img_a, map_fcn, direction = "backward", boundary = "neumann"),
    img_b
  )
}
