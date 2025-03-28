program example
    use iso_fortran_env
    use fplot_core
    implicit none

    ! Parameters
    integer(int32), parameter :: n = 1000

    ! Local Variables
    real(real64) :: x(n), y1(n), y2(n)
    type(plot_2d) :: plt
    type(plot_data_2d) :: d1, d2

    ! Initialization
    x = linspace(0.0d0, 10.0d0, n)
    y1 = sin(x)
    y2 = abs(sin(x))

    ! Plot the data
    call plt%initialize()
    call d1%set_name("sin(x)")
    call d1%define_data(x, y1)
    call plt%push(d1)
    call plt%draw()

    ! Clear the first plot
    call plt%clear_all()

    ! Build the second plot
    call d2%set_name("|sin(x)|")
    call d2%define_data(x, y2)
    call plt%push(d2)
    call plt%draw()
end program