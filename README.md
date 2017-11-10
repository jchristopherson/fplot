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
    type(scatter_plot) :: plt
    type(scatter_plot_data) :: d1, d2
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
    do i = 1, n
        if (i == 1) then
            x(i) = 0.0d0
        else
            x(i) = x(i-1) + dx
        end if
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
This is the graph resulting from the above program.
![](images/example_2d_plot_1.png?raw=true)