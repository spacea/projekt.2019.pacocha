
<!-- README.md is generated from README.Rmd. Please edit that file -->
Operations of figures
====================

<!-- badges: start -->
<!-- badges: end -->
Authors: Błażej Kościański, Patryk Połomski, Jakub Pacocha

Operations on figures is a project co-created by three students of WNGiG UAM which consists of creating a package containing functions enumerating the basic parameters of the most important geometric figures in two-dimensional Euclidean space and sketches them in this space. The goal of projekt.2019.pacocha is to provide the user with functions that are both easy and ready to use, including:

-   `fo_area_hex()` - calculating an area of a hexagon based on its apexes.
-   `fo_area_par()` - calculating an area of a parallelogram.
-   `fo_area_rec()` - calculating an area of a rectangle.
-   `fo_area_rho()` - calculating an area of a rhombus.
-   `fo_area_sqr()` - calculating an area of a square.
-   `fo_area_tri()` - calculating an area of a triangle.
-   `fo_can_rec()` - testing whether the figure is a rectangle.
-   `fo_can_sqr()` - testing whether the figure is a square.
-   `fo_cir_cir()` - calculating the circumference of a circle.
-   `fo_dia_rec()` - calculating the diameter of a rectangle.
-   `fo_dia_sqr()` - calculating the diameter of a square.
-   `fo_hgh_tri()` - calculating the heights of a triangle.
-   `fo_per_hex()` - calculating the perimeter of a hexagon.
-   `fo_per_par()` - calculating the perimeter of a parallelogram.
-   `fo_per_quad()` - calculating the perimeter of a quadrangle
-   `fo_per_rec()` - calculating the perimeter of a rectangle.
-   `fo_per_rho()` - calculating the perimeter of a rhombus.
-   `fo_per_sqr()` - calculating the perimeter of a square.
-   `fo_per_tri()` - calculating the perimeter of a triangle.
-   `fo_plot_hex()` - plotting a hexagon.
-   `fo_centerplot_hex()` - plotting a regular hexagon using it's center point.
-   `fo_plot_par()` - plotting a parallelogram.
-   `fo_plot_quad()` - plotting a quadrangle.
-   `fo_plot_rec()` - plotting a rectangle.
-   `fo_plot_rho()` - plotting a rhombus.
-   `fo_plot_sqr()` - plotting a square.
-   `fo_plot_tri()` - plotting a triangle.
-   `fo_rad_cir()` - calculating the radius of a circle.

Installation
------------

You can install the development version of **projekt.2019.pacocha** from [GitHub](https://github.com/spacea/projekt.2019.pacocha) with:

``` r
remotes::install_github("https://github.com/spacea/projekt.2019.pacocha")
```

Example
-------

This is a basic example which shows you how to solve a common problem:

``` r
library("projekt.2019.pacocha")
fo_area_tri(2,0,10,10,5,3)
#> [1] 3
```

The `fo_area_tri()` function returns a simple numeric vector equal to the value of the chosen triangle's area.

``` r
library("projekt.2019.pacocha")
fo_per_par(0,0,3,3,45,150)
#> [1] 12
```

The `fo_per_par()` function returns a simple numeric vector equal to the perimeter of the chosen parallelogram's area.

``` r
library("projekt.2019.pacocha")
fo_centerplot_hex(0,0,3,45)
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

The `fo_centerplot_hex()` function returns a plot of the chosen regular hexagon's area.
