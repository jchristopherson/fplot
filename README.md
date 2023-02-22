# fplot
A Fortran library providing a convenient interface for plotting with Gnuplot.

## Gnuplot
This library is tailored to write script files for Gnuplot.  As such, Gnuplot is required to make use of the output of this library.  Gnuplot can be found [here](http://www.gnuplot.info/).

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
    real(real64), parameter :: dx = 1.0d-2

    ! Local Variables
    integer(int32) :: i
    real(real64), dimension(n) :: x, y1, y2
    type(plot_2d) :: plt
    type(plot_data_2d) :: d1, d2
    class(plot_axis), pointer :: xAxis, yAxis
    class(legend), pointer :: lgnd
    
    ! Initialize the plot object
    call plt%initialize()

    ! Define titles
    call plt%set_title("2D Example Plot 1")

    xAxis => plt%get_x_axis()
    call xAxis%set_title("X Axis")

    yAxis => plt%get_y_axis()
    call yAxis%set_title("Y Axis")

    lgnd => plt%get_legend()
    call lgnd%set_is_visible(.true.)

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
    call d1%set_line_width(1.0)

    call d2%set_name("Data Set 2")
    call d2%set_use_auto_color(.false.)
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

## Example 4
The following example illustrates how to create a vector-field plot.
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
This library can be built using CMake.  For instructions see [Running CMake](https://cmake.org/runningcmake/).

## External Libraries
The FPLOT library depends upon the following libraries.
- [FERROR](https://github.com/jchristopherson/ferror)
- [COLLECTIONS](https://github.com/jchristopherson/collections)
- [ISO_VARYING_STRING](https://gitlab.com/everythingfunctional/iso_varying_string)
