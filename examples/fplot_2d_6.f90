! fplot_2d_6.f90

program example
    use fplot_core
    use iso_fortran_env
    implicit none

    ! Local Variables & Parameters
    integer(int32), parameter :: npts = 1000
    real(real64), dimension(npts) :: x, y
    type(plot_2d) :: plt
    type(plot_data_2d) :: dataset
    class(plot_axis), pointer :: xAxis, yAxis
    type(legend), pointer :: leg

    ! Build a data set to plot
    x = linspace(0.0d0, 10.0d0, npts)
    y = exp(-0.5d0 * x) * sin(10.0d0 * x - 0.5d0)

    call dataset%define_data(x, y)

    ! Set up the plot
    call plt%initialize()
    call plt%set_title("Example Plot")

    xAxis => plt%get_x_axis()
    call xAxis%set_title("X Axis")

    yAxis => plt%get_y_axis()
    call yAxis%set_title("Y Axis")

    ! Hide the legend
    leg => plt%get_legend()
    call leg%set_is_visible(.false.)

    ! Add the data to the plot
    call plt%push(dataset)

    ! Save the plot to a file that can be opened by GNUPLOT at a later time
    call plt%save_file("example_gnuplot_file.plt")
end program