program example
    use fplot_core
    use iso_fortran_env
    implicit none

    ! Local Variables & Parameters
    integer(int32), parameter :: npts = 1000
    real(real64), dimension(npts) :: x, y
    type(plot_2d) :: plt
    type(legend), pointer :: leg

    ! Build a data set to plot
    x = linspace(0.0d0, 10.0d0, npts)
    y = exp(-0.5d0 * x) * sin(10.0d0 * x - 0.5d0)

    ! Set up the plot
    call plt%initialize()
    call plt%set_title("Example Plot")
    call plt%set_x_axis_title("X Axis")
    call plt%set_y_axis_title("Y Axis")
    call plt%push(x, y, name = "Example")

    ! Show the legend
    leg => plt%get_legend()
    call leg%set_is_visible(.true.)
    call leg%set_is_opaque(.false.)
    call leg%set_draw_border(.false.)
    call leg%set_horizontal_position(LEGEND_RIGHT)
    call leg%set_vertical_position(LEGEND_BOTTOM)

    ! Draw the plot
    call plt%draw()
end program