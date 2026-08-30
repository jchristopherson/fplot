program example
    use iso_fortran_env
    use fplot_core
    implicit none

    ! Local Variables & Parameters
    integer(int32), parameter :: npts = 1000
    real(real64), dimension(npts) :: x, y1, y2
    type(plot_2d) :: plt
    class(terminal), pointer :: term
    type(legend), pointer :: leg

    ! Build a data set to plot
    x = linspace(0.0d0, 10.0d0, npts)
    y1 = sin(x) * cos(x)
    y2 = sqrt(x) * sin(x)

    ! Set up the plot
    call plt%initialize(GNUPLOT_TERMINAL_PNG) ! Save to file directly
    call plt%set_title("Example Plot")
    call plt%set_x_axis_title("X Axis")
    call plt%set_y_axis_title("Y Axis")

    ! Put the legend outside the axes, and remove it's border
    leg => plt%get_legend()
    call leg%set_is_visible(.true.)
    call leg%set_draw_inside_axes(.false.)
    call leg%set_draw_border(.false.)

    ! Set up line color and style properties to better distinguish each data set
    call plt%push(x, y1, lc = CLR_BLUE, name = "Data Set 1")
    call plt%push(x, y2, lc = CLR_GREEN, name = "Data Set 2")

    ! Define the file to which the plot should be saved
    term => plt%get_terminal()
    select type (term)
    class is (png_terminal)
        call term%set_filename("png_plot_example_2.png")
    end select

    ! Draw the plot
    call plt%draw()
end program