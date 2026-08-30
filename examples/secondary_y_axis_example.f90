program example
    use fplot_core
    use iso_fortran_env
    implicit none

    ! Local Variables
    integer(int32), parameter :: npts = 1000
    real(real64), dimension(npts) :: x, y1, y2
    type(plot_2d) :: plt
    type(plot_data_2d) :: ds1, ds2

    ! Build a data set
    x = linspace(0.0d0, 10.0d0, npts)
    y1 = exp(-0.5d0 * x) * abs(sin(x))
    y2 = cos(0.5d0 * x) * sin(10.0d0 * x)

    call ds1%define_data(x, y1)
    call ds1%set_name("f(x) = exp(-x / 2) * |sin(x)|")

    call ds2%define_data(x, y2)
    call ds2%set_name("f(x) = cos(x / 2) * sin(10 x)")

    ! Make the ds2 line green and dashed
    call ds2%set_line_color(CLR_GREEN)
    call ds2%set_line_style(LINE_DASHED)
    
    ! Draw ds2 against the secondary y axis
    call ds2%set_draw_against_y2(.true.)

    ! Set up the plot
    call plt%initialize()
    call plt%set_use_y2_axis(.true.)
    call plt%set_title("Example Plot")
    call plt%set_x_axis_title("X Axis")
    call plt%set_y_axis_title("Y Axis")
    call plt%set_y2_axis_title("Secondary Y Axis")

    ! Add the data to the plot
    call plt%push(ds1)
    call plt%push(ds2)

    ! Draw
    call plt%draw()
end program