program example
    use iso_fortran_env
    use fplot_core
    implicit none

    ! Local Variables
    integer(int32), parameter :: npts = 100
    real(real64), parameter :: pi = 2.0d0 * acos(0.0d0)
    real(real64) :: x(npts), y(npts)
    type(plot_2d) :: plt
    type(plot_data_2d) :: pd1, pd2

    ! Generate the curve to plot
    x = linspace(0.0d0, 1.0d0, npts)
    y = sin(4.0d0 * pi * x)

    ! Plot the data
    call plt%initialize()
    
    call pd1%define_data(x, y)
    call pd1%set_fill_curve(.true.)
    call plt%push(pd1)

    call pd2%define_data(x, -0.25d0 * y)
    call pd2%set_fill_curve(.true.)
    call plt%push(pd2)

    call plt%draw()
end program
