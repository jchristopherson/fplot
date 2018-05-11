# fplot
A Fortran library providing a convenient interface for plotting with Gnuplot.

## Status
![Build Status](https://travis-ci.org/jchristopherson/fplot.svg?branch=master)

## Gnuplot
This library is tailored to write script files for Gnuplot.  As such, Gnuplot is required to make use of the output of this library.  Gnuplot can be found [here](http://www.gnuplot.info/).

## Example 1
This example illustrates how to plot two-dimensional data.
```fortran
program example
    use, intrinsic :: iso_fortran_env
    use fplot_core
    implicit none

    ! Parameters
    integer(int32), parameter :: n = 1000
    real(real64), parameter :: dx = 1.0d-2

    ! Local Variables
    integer(int32) :: i
    real(real64), dimension(n) :: x, y1, y2
    type(plot_2d) :: plt
    type(plot_data_2d) :: d1, d2
    class(plot_axis), pointer :: xAxis, yAxis
    
    ! Initialize the plot object
    call plt%initialize()

    ! Define titles
    call plt%set_title("2D Example Plot 1")

    xAxis => plt%get_x_axis()
    call xAxis%set_title("X Axis")

    yAxis => plt%get_y_axis()
    call yAxis%set_title("Y Axis")

    ! Define the data, and then add it to the plot
    x(1) = 0.0d0
    do i = 2, n
        x(i) = x(i-1) + dx
    end do
    y1 = sin(5.0d0 * x)
    y2 = 2.0d0 * cos(2.0d0 * x)

    call d1%define_data(x, y1)
    call d2%define_data(x, y2)

    ! Define properties for each data set
    call d1%set_name("Data Set 1")
    call d1%set_use_auto_color(.false.)
    call d1%set_line_color(CLR_BLUE)
    call d1%set_draw_markers(.true.)
    call d1%set_marker_frequency(10)
    call d1%set_marker_style(MARKER_EMPTY_CIRCLE)
    call d1%set_marker_scaling(2.0)

    call d2%set_name("Data Set 2")
    call d2%set_use_auto_color(.false.)
    call d2%set_line_color(CLR_GREEN)
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
program example
    use, intrinsic :: iso_fortran_env
    use fplot_core
    implicit none

    ! Parameters
    integer(int32), parameter :: n = 1000
    real(real64), parameter :: dx = 1.0d-2

    ! Local Variables
    integer(int32) :: i
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
    x(1) = 0.0d0
    do i = 2, n
        x(i) = x(i-1) + dx
    end do
    y1 = sin(5.0d0 * x)
    y2 = 2.0d0 * cos(2.0d0 * x)

    call d1%define_data(x, y1)
    call d2%define_data(x, y2)

    ! Define properties for each data set
    call d1%set_name("Data Set 1")
    call d1%set_use_auto_color(.false.)
    call d1%set_line_color(CLR_BLUE)
    call d1%set_line_width(1.0)

    call d2%set_name("Data Set 2")
    call d2%set_use_auto_color(.false.)
    call d2%set_line_color(CLR_GREEN)
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
The following example illustrates how to create a three-dimensional surface plot.
```fortran
program example
    use, intrinsic :: iso_fortran_env
    use fplot_core
    implicit none

    ! Parameters
    integer(int32), parameter :: m = 50
    integer(int32), parameter :: n = 50
    real(real64), parameter :: xMax = 5.0d0
    real(real64), parameter :: xMin = -5.0d0
    real(real64), parameter :: yMax = 5.0d0
    real(real64), parameter :: yMin = -5.0d0

    ! Local Variables
    real(real64), dimension(m, n) :: x, y, z
    real(real64) :: dx, dy
    integer(int32) :: i, j
    type(surface_plot) :: plt
    type(surface_plot_data) :: d1
    type(rainbow_colormap) :: map
    class(plot_axis), pointer :: xAxis, yAxis, zAxis

    ! Define the data
    dx = (xMax - xMin) / (n - 1.0d0)
    x(:,1) = xMin
    do j = 2, n
        x(:,j) = x(:,j-1) + dx
    end do

    dy = (yMax - yMin) / (m - 1.0d0)
    y(1,:) = yMax
    do i = 2, m
        y(i,:) = y(i-1,:) - dy
    end do

    z = sin(sqrt(x**2 + y**2))

    ! Create the plot
    call plt%initialize()
    call plt%set_colormap(map)

    ! Define titles
    call plt%set_title("Surface Example Plot 1")

    xAxis => plt%get_x_axis()
    call xAxis%set_title("X Axis")

    yAxis => plt%get_y_axis()
    call yAxis%set_title("Y Axis")

    zAxis => plt%get_z_axis()
    call zAxis%set_title("Z Axis")

    ! Define the data set
    call d1%define_data(x, y, z)
    call d1%set_name("sin(sqrt(x**2 + y**2))")
    call plt%push(d1)

    ! Let GNUPLOT draw the plot
    call plt%draw()
end program
```
This is the plot resulting from the above program.
![](images/example_surf_plot_1.png?raw=true)

## Building FPLOT
This library can be built using CMake.  For instructions see [Running CMake](https://cmake.org/runningcmake/).

## Documentation
Documentation can be found [here](http://htmlpreview.github.io/?https://github.com/jchristopherson/fplot/blob/master/doc/html/index.html)

## External Libraries
The FPLOT library depends upon the following libraries.
- [FERROR](https://github.com/jchristopherson/ferror)
- [STRINGS](https://github.com/jchristopherson/strings)
- [COLLECTIONS](https://github.com/jchristopherson/collections)

## Using FPLOT
Using fplot in an application utilizing CMake is straight forward.  The following CMake script illustrates a bare bones implementation that generates and runs an executable by the name of "plot."
```text
# Master CMAKE Build Script
cmake_minimum_required(VERSION 3.7)
project(plot Fortran)

# Get FPLOT
find_package(fplot)
find_package(ferror)
find_package(collections)

# Build the executable
add_executable(plot plot.f90)
target_link_libraries(plot fplot)

# Copy the necessary DLL's to the application directory - Assuming Windows for an OS
get_target_property(ferror_LibLocation ferror LOCATION)
add_custom_command(TARGET plot POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
    ${ferror_LibLocation} $<TARGET_FILE_DIR:plot>
)

get_target_property(fplot_LibLocation fplot LOCATION)
add_custom_command(TARGET plot POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
    ${fplot_LibLocation} $<TARGET_FILE_DIR:plot>
)

get_target_property(collections_LibLocation collections LOCATION)
add_custom_command(TARGET plot POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
    ${collections_LibLocation} $<TARGET_FILE_DIR:plot>
)

# Run the executable
add_custom_command(
    OUTPUT plot_output
    COMMAND plot
)
add_custom_target(run_plot ALL DEPENDS plot_output)

```
