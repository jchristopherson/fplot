! fplot_2d_1.f90

program example
    use, intrinsic :: iso_fortran_env
    use fplot_core
    implicit none

    ! Parameters
    integer(int32), parameter :: n = 1000

    ! Local Variables
    real(real64), dimension(n) :: x, y1, y2
    type(plot_2d) :: plt
    type(plot_data_2d) :: d1, d2
    class(plot_axis), pointer :: xAxis, yAxis
    type(legend), pointer :: leg
    
    ! Initialize the plot object
    call plt%initialize()

    ! Define titles
    call plt%set_title("Example Plot")
    call plt%set_font_size(14)

    xAxis => plt%get_x_axis()
    call xAxis%set_title("X Axis")

    yAxis => plt%get_y_axis()
    call yAxis%set_title("Y Axis")

    ! Establish legend properties
    leg => plt%get_legend()
    call leg%set_draw_inside_axes(.false.)
    call leg%set_horizontal_position(LEGEND_CENTER)
    call leg%set_vertical_position(LEGEND_BOTTOM)
    call leg%set_draw_border(.false.)

    ! Define the data, and then add it to the plot
    x = linspace(0.0d0, 10.0d0, n)
    y1 = sin(5.0d0 * x)
    y2 = 2.0d0 * cos(2.0d0 * x)

    call d1%define_data(x, y1)
    call d2%define_data(x, y2)

    ! Define properties for each data set
    call d1%set_name("Data Set 1")
    call d1%set_line_color(CLR_BLUE)
    call d1%set_draw_markers(.true.)
    call d1%set_marker_frequency(10)
    call d1%set_marker_style(MARKER_EMPTY_CIRCLE)
    call d1%set_marker_scaling(2.0)

    call d2%set_name("Data Set 2")
    call d2%set_line_color(CLR_GREEN)
    call d2%set_line_style(LINE_DASHED)
    call d2%set_line_width(2.0)

    ! Add the data sets to the plot
    call plt%push(d1)
    call plt%push(d2)

    ! Let GNUPLOT draw the plot
    call plt%draw()
end program