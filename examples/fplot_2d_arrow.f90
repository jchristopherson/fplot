program example
    use fplot_core
    use iso_fortran_env
    implicit none

    ! Local Variables
    integer(int32), parameter :: npts = 1000
    real(real64), dimension(npts) :: x, y
    type(plot_2d) :: plt
    type(plot_data_2d) :: pd
    class(plot_axis), pointer :: xAxis, yAxis
    type(plot_arrow) :: a1, a2, a3

    ! Initialize the plot
    call plt%initialize()
    xAxis => plt%get_x_axis()
    yAxis => plt%get_y_axis()
    call xAxis%set_title("X Axis")
    call yAxis%set_title("Y Axis")

    ! Build a data set
    x = linspace(0.0d0, 1.0d1, npts)
    y = sin(1.0d1 * x) * sin(0.5d0 * x)
    call pd%define_data(x, y)
    call plt%push(pd)

    ! Define some arrows
    call a1%set_tail_location(0.5, 0.5)
    call a1%set_head_location(1.0, 1.0)
    call plt%push_arrow(a1)

    call a2%set_tail_location(0.5, 0.5)
    call a2%set_head_location(2.0, 0.5)
    call a2%set_color(CLR_RED)
    call a2%set_line_width(2.0)
    call a2%set_head_type(ARROW_HEADS)
    call a2%set_head_fill(ARROW_EMPTY)
    call a2%set_move_to_front(.false.)
    call plt%push_arrow(a2)

    call a3%set_tail_location(0.5, 0.5)
    call a3%set_head_location(0.5, 0.0)
    call a3%set_color(CLR_GREEN)
    call a3%set_line_width(2.0)
    call a3%set_head_type(ARROW_BACKHEAD)
    call a3%set_head_fill(ARROW_NO_FILL)
    call plt%push_arrow(a3)

    ! Draw the plot
    call plt%draw()
end program