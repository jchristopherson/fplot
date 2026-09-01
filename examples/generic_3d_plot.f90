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

    ! Initialize the plot object
    call plt%initialize()

    ! Define titles
    call plt%set_title("Example Plot")
    call plt%set_x_axis_title("X Axis")
    call plt%set_y_axis_title("Y Axis")
    call plt%set_z_axis_title("Z Axis")

    ! Define the data
    t = linspace(0.0d0, 10.0d0, n)
    x = cos(5.0d0 * t)
    y = sin(5.0d0 * t)
    z = 2.0d0 * t

    ! Adjust plot orientation
    call plt%set_elevation(40.0d0)
    call plt%set_azimuth(20.0d0)

    call d1%define_data(x, y, z)
    call d1%set_line_width(2.0)

    ! Add the data to the plot
    call plt%push(d1)

    ! Let GNUPLOT draw the plot
    call plt%draw()
end program