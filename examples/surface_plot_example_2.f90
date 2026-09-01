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
    real(real64), dimension(n) :: xdata
    real(real64), dimension(m) :: ydata
    real(real64), dimension(:,:), pointer :: x, y
    real(real64), dimension(m, n, 2), target :: xy
    real(real64), dimension(m, n) :: z
    type(surface_plot) :: plt
    type(surface_plot_data) :: d1
    type(rainbow_colormap) :: map

    ! Define the data
    xdata = linspace(xMin, xMax, n)
    ydata = linspace(yMin, yMax, m)
    xy = meshgrid(xdata, ydata)
    x => xy(:,:,1)
    y => xy(:,:,2)

    ! Define the function to plot
    z = sin(sqrt(x**2 + y**2))

    ! Define colormap settings
    call map%set_show_tics(.false.)

    ! Create the plot
    call plt%initialize()
    call plt%set_colormap(map)
    call plt%set_azimuth(20.0d0)

    ! Define titles
    call plt%set_title("Example Plot")
    call plt%set_x_axis_title("X Axis")
    call plt%set_y_axis_title("Y Axis")
    call plt%set_z_axis_title("Z Axis")

    ! Define the data set
    call d1%define_data(x, y, z)
    call plt%push(d1)

    ! Let GNUPLOT draw the plot
    call plt%draw()
end program