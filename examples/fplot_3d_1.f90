! fplot_3d_1.f90

program example
    use, intrinsic :: iso_fortran_env
    use fplot_core
    implicit none

    ! Parameters
    integer(int32), parameter :: n = 1000
    real(real64), parameter :: dt = 1.0d-2

    ! Local Variables
    integer(int32) :: i
    real(real64), dimension(n) :: t, x, y, z
    type(plot_3d) :: plt
    type(plot_data_3d) :: d1
    class(plot_axis), pointer :: xAxis, yAxis, zAxis

    ! Initialize the plot object
    call plt%initialize()

    ! Define titles
    call plt%set_title("3D Example Plot 1")

    xAxis => plt%get_x_axis()
    call xAxis%set_title("X Axis")

    yAxis => plt%get_y_axis()
    call yAxis%set_title("Y Axis")

    zAxis => plt%get_z_axis()
    call zAxis%set_title("Z Axis")

    ! Define the data
    t(1) = 0.0d0
    do i = 2, n
        t(i) = t(i-1) + dt
    end do
    x = cos(5.0d0 * t)
    y = sin(5.0d0 * t)
    z = 2.0d0 * t

    call d1%define_data(x, y, z)

    ! Set up the data set
    call d1%set_name("Helix")
    call d1%set_use_auto_color(.false.)
    call d1%set_line_color(CLR_BLUE)
    call d1%set_line_width(2.0)

    ! Add the data to the plot
    call plt%push(d1)

    ! Let GNUPLOT draw the plot
    call plt%draw()
end program