! fplot_2d_3.f90

program example
    use iso_fortran_env
    use fplot_core
    implicit none

    ! Local Variables & Parameters
    integer(int32), parameter :: npts = 1000
    real(real64), dimension(npts) :: x, y1, y2
    type(plot_2d) :: plt
    type(plot_data_2d) :: d1, d2
    class(plot_axis), pointer :: xAxis, yAxis
    type(legend), pointer :: leg

    ! Build a data set to plot
    x = linspace(0.0d0, 10.0d0, npts)
    y1 = sin(x) * cos(x)
    y2 = sqrt(x) * sin(x)

    call d1%define_data(x, y1)
    call d2%define_data(x, y2)

    ! Set up the plot
    call plt%initialize(GNUPLOT_TERMINAL_PNG, "example_plot.png") ! Save to file directly
    call plt%set_title("Example Plot")
    
    xAxis => plt%get_x_axis()
    call xAxis%set_title("X Axis")

    yAxis => plt%get_y_axis()
    call yAxis%set_title("Y Axis")

    ! Put the legend in the upper left corner of the plot
    leg => plt%get_legend()
    call leg%set_horizontal_position(LEGEND_LEFT)
    call leg%set_vertical_position(LEGEND_TOP)

    ! Set up line color and style properties to better distinguish each data set
    call d1%set_name("Data Set 1")
    call d1%set_line_color(CLR_BLUE)
    
    call d2%set_name("Data Set 2")
    call d2%set_line_color(CLR_GREEN)

    ! Add the data to the plot
    call plt%push(d1)
    call plt%push(d2)

    ! Draw the plot
    call plt%draw()
end program