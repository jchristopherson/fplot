! fplot_err_1.f90

program example
    use iso_fortran_env
    use fplot_core
    implicit none

    ! Parameters
    integer(int32), parameter :: n = 50
    
    ! Local Variables
    real(real64) :: x(n), y(n), yerr(n)
    type(plot_2d) :: plt
    type(plot_data_2d) :: d1
    type(plot_data_error_bars) :: e1

    ! Initialization
    x = linspace(0.0d0, 1.0d1, n)
    y = sin(x)
    call random_number(yerr)
    yerr = 3.0d-1 * (yerr - 0.5d0)

    ! Create the plot
    call plt% initialize()

    call d1%define_data(x, y)
    call d1%set_name("Raw")

    call e1%define_y_error_data(x, y, yerr)
    call e1%set_name("Errors")
    call e1%set_color(CLR_BLUE)

    call plt%push(d1)
    call plt%push(e1)
    call plt%draw()
end program