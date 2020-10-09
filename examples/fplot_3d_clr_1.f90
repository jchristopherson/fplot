! fplot_3d_clr_1.f90

program example
    use, intrinsic :: iso_fortran_env
    use fplot_core
    implicit none

    ! Parameters
    integer(int32), parameter :: n = 1000

    ! Local Variables
    real(real64), dimension(n) :: t, x, y, z
    type(plot_3d) :: plt
    type(plot_data_3d) :: d1
    class(plot_axis), pointer :: xAxis, yAxis, zAxis

    ! Initialize the plot object
    call plt%initialize()
    call plt%set_font_size(14)

    ! Define titles
    call plt%set_title("Example Plot")

    xAxis => plt%get_x_axis()
    call xAxis%set_title("X Axis")

    yAxis => plt%get_y_axis()
    call yAxis%set_title("Y Axis")

    zAxis => plt%get_z_axis()
    call zAxis%set_title("Z Axis")

    ! Define the data
    t = linspace(0.0d0, 10.0d0, n)
    x = cos(5.0d0 * t)
    y = sin(5.0d0 * t)
    z = 2.0d0 * t

    call d1%define_data(x, y, z, x * y)

    ! Set up the data set
    call d1%set_line_color(CLR_BLUE)
    call d1%set_line_width(2.0)

    ! Add the data to the plot
    call plt%push(d1)

    ! Let GNUPLOT draw the plot
    call plt%draw()
end program
