! fplot_surf_1.f90

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
