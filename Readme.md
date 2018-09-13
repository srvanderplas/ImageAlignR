[![CRAN
status](https://www.r-pkg.org/badges/version/ImageAlignR)](https://cran.r-project.org/package=ImageAlignR)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Travis build
status](https://travis-ci.org/srvanderplas/ImageAlignR.svg?branch=master)](https://travis-ci.org/srvanderplas/ImageAlignR)
[![Coverage
status](https://codecov.io/gh/srvanderplas/ImageAlignR/branch/master/graph/badge.svg)](https://codecov.io/github/srvanderplas/ImageAlignR?branch=master)

Image Alignment Methods included in this package:
=================================================

1.  Single-Image Alignment
    -   [Ellipse-based](https://srvanderplas.github.io/ImageAlignR/articles/ellipse-alignment.html):
        Align an image so that its main axes are the y and x axis of the
        cartesian coordinate system. Rotates an image so that it is
        straight up-and-down.  
        Gwo C and Wei C (2016). “Shoeprint Retrieval: Core Point
        Alignment, for Pattern Comparison.” *Science & Justice*,
        *56*(5), pp., 341-350. ISSN 1355-0306, doi:
        10.1016/j.scijus.2016.06.004 (URL:,
        <http://doi.org/10.1016/j.scijus.2016.06.004>), 00000, &lt;URL:,
        <http://www.sciencedirect.com/science/article/pii/S1355030616300521>&gt;.
2.  Dual-Image Alignment
    -   [Keypoint
        Alignment](https://srvanderplas.github.io/ImageAlignR/articles/keypoint-alignment.html):
        Keypoint detection using the following steps:
        1.  Harris corner detector
        2.  Calculate key image orientations
        3.  Feature detection
        4.  Point matching using KNN to find similar points
        5.  RANSAC to get a consistent homology
        6.  Image transformation
