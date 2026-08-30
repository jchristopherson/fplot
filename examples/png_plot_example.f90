program example
    use iso_fortran_env
    use fplot_core
    implicit none

    ! Local Variables & Parameters
    integer(int32), parameter :: npts = 1000
    real(real64), dimension(npts) :: x, y1, y2
    type(plot_2d) :: plt
    type(legend), pointer :: leg

    ! Build a data set to plot
    x = linspace(0.0d0, 10.0d0, npts)
    y1 = sin(x) * cos(x)
    y2 = sqrt(x) * sin(x)

    ! Set up the plot
    call plt%initialize(GNUPLOT_TERMINAL_PNG, "png_plot_example.png") ! Save to file directly
    call plt%set_title("Example Plot")
    call plt%set_x_axis_title("X Axis")
    call plt%set_y_axis_title("Y Axis")

    ! Put the legend in the upper left corner of the plot
    leg => plt%get_legend()
    call leg%set_is_visible(.true.)
    call leg%set_horizontal_position(LEGEND_LEFT)
    call leg%set_vertical_position(LEGEND_TOP)

    ! Set up line color and style properties to better distinguish each data set
    call plt%push(x, y1, lc = CLR_BLUE, name = "Data Set 1")
    call plt%push(x, y2, lc = CLR_GREEN, name = "Data Set 2")

    ! Draw the plot
    call plt%draw()
end program