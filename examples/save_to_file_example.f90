program example
    use fplot_core
    use iso_fortran_env
    implicit none

    ! Local Variables & Parameters
    integer(int32), parameter :: npts = 1000
    real(real64), dimension(npts) :: x, y
    type(plot_2d) :: plt

    ! Build a data set to plot
    x = linspace(0.0d0, 10.0d0, npts)
    y = exp(-0.5d0 * x) * sin(10.0d0 * x - 0.5d0)

    ! Set up the plot
    call plt%initialize()
    call plt%set_title("Example Plot")
    call plt%set_x_axis_title("X Axis")
    call plt%set_y_axis_title("Y Axis")

    ! Add the data to the plot
    call plt%push(x, y)

    ! Save the plot to a file that can be opened by GNUPLOT at a later time
    call plt%save_file("save_to_file_example.plt")
end program