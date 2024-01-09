# fplot
A Fortran library providing a convenient interface for plotting with Gnuplot.


## Status
![Build Status](https://github.com/jchristopherson/fplot/actions/workflows/cmake.yml/badge.svg)
[![Actions Status](https://github.com/jchristopherson/fplot/workflows/fpm/badge.svg)](https://github.com/jchristopherson/fplot/actions)

## GNUPLOT
This library is tailored to write script files for GNUPLOT.  As such, GNUPLOT is required to make use of the output of this library.  GNUPLOT can be found [here](http://www.gnuplot.info/).

## Documentation
Documentation can be found [here](https://jchristopherson.github.io/fplot/)

## Example 1
This example illustrates how to plot two-dimensional data.
```fortran
program example
    use, intrinsic :: iso_fortran_env
    use fplot_core
    implicit none

    ! Parameters
    integer(int32), parameter :: n = 1000

    ! Local Variables
    real(real64), dimension(n) :: x, y1, y2
    type(plot_2d) :: plt
    type(plot_data_2d) :: d1, d2
    class(plot_axis), pointer :: xAxis, yAxis
    type(legend), pointer :: leg
    
    ! Initialize the plot object
    call plt%initialize()

    ! Define titles
    call plt%set_title("Example Plot")
    call plt%set_font_size(14)

    xAxis => plt%get_x_axis()
    call xAxis%set_title("X Axis")

    yAxis => plt%get_y_axis()
    call yAxis%set_title("Y Axis")

    ! Establish legend properties
    leg => plt%get_legend()
    call leg%set_is_visible(.true.)
    call leg%set_draw_inside_axes(.false.)
    call leg%set_horizontal_position(LEGEND_CENTER)
    call leg%set_vertical_position(LEGEND_BOTTOM)
    call leg%set_draw_border(.false.)

    ! Define the data, and then add it to the plot
    x = linspace(0.0d0, 10.0d0, n)
    y1 = sin(5.0d0 * x)
    y2 = 2.0d0 * cos(2.0d0 * x)

    call d1%define_data(x, y1)
    call d2%define_data(x, y2)

    ! Define properties for each data set
    call d1%set_name("Data Set 1")
    call d1%set_draw_markers(.true.)
    call d1%set_marker_frequency(10)
    call d1%set_marker_style(MARKER_EMPTY_CIRCLE)
    call d1%set_marker_scaling(2.0)

    call d2%set_name("Data Set 2")
    call d2%set_line_style(LINE_DASHED)
    call d2%set_line_width(2.0)

    ! Add the data sets to the plot
    call plt%push(d1)
    call plt%push(d2)

    ! Let GNUPLOT draw the plot
    call plt%draw()
end program
```
This is the plot resulting from the above program.
![](images/example_2d_plot_1.png?raw=true)

## Example 2
Another example of a similar two-dimensional plot to the plot in example 1 is given below.  This plot shifts the x-axis to the zero point along the y-axis.
```fortran
! fplot_2d_2.f90

program example
    use, intrinsic :: iso_fortran_env
    use fplot_core
    implicit none

    ! Parameters
    integer(int32), parameter :: n = 1000

    ! Local Variables
    real(real64), dimension(n) :: x, y1, y2
    type(plot_2d) :: plt
    type(plot_data_2d) :: d1, d2
    class(plot_axis), pointer :: xAxis, yAxis
    type(legend), pointer :: lgnd
    
    ! Initialize the plot object
    call plt%initialize()
    
    ! Set plot properties
    call plt%set_draw_border(.false.)
    call plt%set_show_gridlines(.false.)

    ! Define the legend location
    lgnd => plt%get_legend()
    call lgnd%set_is_visible(.true.)
    call lgnd%set_draw_inside_axes(.false.)

    ! Define titles
    call plt%set_title("2D Example Plot 2")

    xAxis => plt%get_x_axis()
    call xAxis%set_title("X Axis")
    call xAxis%set_zero_axis(.true.)
    call xAxis%set_zero_axis_line_width(1.0)

    yAxis => plt%get_y_axis()
    call yAxis%set_title("Y Axis")

    ! Define the data, and then add it to the plot
    x = linspace(0.0d0, 10.d0, n)
    y1 = sin(5.0d0 * x)
    y2 = 2.0d0 * cos(2.0d0 * x)

    call d1%define_data(x, y1)
    call d2%define_data(x, y2)

    ! Define properties for each data set
    call d1%set_name("Data Set 1")
    call d1%set_line_width(1.0)

    call d2%set_name("Data Set 2")
    call d2%set_line_style(LINE_DASHED)
    call d2%set_line_width(2.0)

    ! Add the data sets to the plot
    call plt%push(d1)
    call plt%push(d2)

    ! Let GNUPLOT draw the plot
    call plt%draw()
end program
```
This is the plot resulting from the above program.
![](images/example_2d_plot_2.png?raw=true)

## Example 3
The following example illustrates how to create a three-dimensional surface plot.  The plot also leverages the [FORCOLORMAP](https://github.com/vmagnin/forcolormap) library to provide the colormap.
```fortran
program example
    use fplot_core
    use iso_fortran_env
    use forcolormap, cmap => Colormap
    use forcolormap, only : colormaps_list
    implicit none

    ! Parameters
    integer(int32), parameter :: m = 50
    integer(int32), parameter :: n = 50

    ! Local Variables
    real(real64), dimension(m, n, 2), target :: xy
    real(real64), pointer, dimension(:,:) :: x, y
    real(real64), dimension(m, n) :: z
    type(surface_plot) :: plt
    type(surface_plot_data) :: d1
    class(plot_axis), pointer :: xAxis, yAxis, zAxis
    type(custom_colormap) :: map
    type(cmap) :: colors

    ! Set up the colormap
    call colors%set("glasgow", -8.0d0, 8.0d0)
    call map%set_colormap(colors)

    ! Define the data
    xy = meshgrid(linspace(-5.0d0, 5.0d0, n), linspace(-5.0d0, 5.0d0, m))
    x => xy(:,:,1)
    y => xy(:,:,2)

    ! Initialize the plot
    call plt%initialize()
    call plt%set_colormap(map)

    ! Establish lighting
    call plt%set_use_lighting(.true.)

    ! Set the orientation of the plot
    call plt%set_elevation(20.0d0)
    call plt%set_azimuth(30.0d0)
    
    ! Define titles
    call plt%set_title("Example Plot")
    
    xAxis => plt%get_x_axis()
    call xAxis%set_title("X Axis")

    yAxis => plt%get_y_axis()
    call yAxis%set_title("Y Axis")

    zAxis => plt%get_z_axis()
    call zAxis%set_title("Z Axis")

    ! Define the function to plot
    z = sqrt(x**2 + y**2) * sin(x**2 + y**2)
    call d1%define_data(x, y, z)
    call plt%push(d1)

    ! Draw the plot
    call plt%draw()
end program
```
This is the plot resulting from the above program.
![](images/custom_colormap.png?raw=true)

## Example 4
The following example illustrates how to create a vector-field plot.  This example illustrates using one of the built-in colormaps to to help illustrate vector magnitude.
```fortran
program example
    use iso_fortran_env
    use fplot_core
    implicit none

    ! Local Variables
    type(plot_2d) :: plt
    type(vector_field_plot_data) :: ds1
    class(plot_axis), pointer :: xAxis, yAxis
    type(rainbow_colormap) :: cmap
    real(real64), allocatable, dimension(:,:,:) :: pts
    real(real64), allocatable, dimension(:,:) :: dx, dy
    real(real64) :: dxdt(2)
    integer(int32) :: i, j

    ! Create a grid of points defining the vector locations
    pts = meshgrid( &
        linspace(-2.0d0, 2.0d0, 20), &
        linspace(-5.0d0, 5.0d0, 20))

    ! Compute the values of each derivative
    allocate(dx(size(pts, 1), size(pts, 2)))
    allocate(dy(size(pts, 1), size(pts, 2)))
    do j = 1, size(pts, 2)
        do i = 1, size(pts, 1)
            call eqn([pts(i,j,1), pts(i,j,2)], dxdt)
            dx(i,j) = dxdt(1)
            dy(i,j) = dxdt(2)
        end do
    end do

    ! Define arrow properties
    call ds1%set_arrow_size(0.1d0)  ! 1.0 by default
    call ds1%set_fill_arrow(.true.) ! .false. by default

    ! Create the plot
    call plt%initialize()
    call plt%set_font_size(14)
    xAxis => plt%get_x_axis()
    yAxis => plt%get_y_axis()

    ! Define axis labels
    call xAxis%set_title("x(t)")
    call yAxis%set_title("dx/dt")

    ! Set plot style information
    call xAxis%set_zero_axis(.true.)
    call yAxis%set_zero_axis(.true.)
    call plt%set_draw_border(.false.)
    call plt%set_show_gridlines(.false.)

    ! Define the colormap
    call plt%set_colormap(cmap)

    ! Add the data to the plot - color by the magnitude of gradient
    call ds1%define_data(pts(:,:,1), pts(:,:,2), dx, dy, sqrt(dx**2 + dy**2))
    call plt%push(ds1)

    call plt%draw()
contains
    ! Van der Pol Equation
    ! x" - mu * (1 - x^2) * x' + x = 0
    subroutine eqn(x, dxdt)
        real(real64), intent(in) :: x(2)
        real(real64), intent(out) :: dxdt(2)

        real(real64), parameter :: mu = 2.0d0

        dxdt(1) = x(2)
        dxdt(2) = mu * (1.0d0 - x(1)**2) * x(2) - x(1)
    end subroutine
end program
```
This is the plot resulting from the above program.
![](images/vector_plot_2.png?raw=true)

## Example 5
The following example illustrates how to create a polar plot.
```fortran
program example
    use iso_fortran_env
    use fplot_core

    ! Local Variables
    integer(int32), parameter :: npts = 1000
    real(real64), parameter :: pi = 2.0d0 * acos(0.0d0)
    real(real64) :: t(npts), x(npts)
    type(plot_polar) :: plt
    type(plot_data_2d) :: pd

    ! Create a function to plot
    t = linspace(-2.0d0 * pi, 2.0d0 * pi, npts)
    x = t * sin(t)

    ! Plot the function
    call plt%initialize()
    call plt%set_font_size(14)
    call plt%set_title("Polar Plot Example")
    call plt%set_autoscale(.false.)
    call plt%set_radial_limits([0.0d0, 6.0d0])

    call pd%define_data(t, x)
    call pd%set_line_width(2.0)
    call plt%push(pd)
    call plt%draw()
end program
```
This is the plot resulting from the above program.
![](images/polar_example_1.png?raw=true)

## Building FPLOT
[CMake](https://cmake.org/)This library can be built using CMake.  For instructions see [Running CMake](https://cmake.org/runningcmake/).

[FPM](https://github.com/fortran-lang/fpm) can also be used to build this library using the provided fpm.toml.
```txt
fpm build
```
The FPLOT library can be used within your FPM project by adding the following to your fpm.toml file.
```toml
[dependencies]
fplot = { git = "https://github.com/jchristopherson/fplot" }
```

## External Libraries
The FPLOT library depends upon the following libraries.
- [FERROR](https://github.com/jchristopherson/ferror)
- [COLLECTIONS](https://github.com/jchristopherson/collections)
- [ISO_VARYING_STRING](https://gitlab.com/everythingfunctional/iso_varying_string)
- [GEOMPACK](https://github.com/jchristopherson/geompack)
- [FORCOLORMAP](https://github.com/vmagnin/forcolormap)
