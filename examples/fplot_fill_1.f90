! fplot_fill_1.f90

program example
    use iso_fortran_env
    use fplot_core
    implicit none

    ! Local Variables
    integer(int32), parameter :: npts = 100
    real(real64), parameter :: pi = 2.0d0 * acos(0.0d0)
    real(real64) :: x(npts), y(npts)
    type(plot_2d) :: plt
    type(plot_data_2d) :: pd

    ! Generate the curve to plot
    x = linspace(0.0d0, 1.0d0, npts)
    y = sin(4.0d0 * pi * x)

    ! Plot the data
    call plt%initialize()
    
    call pd%define_data(x, y)
    call pd%set_fill_curve(.true.)
    call plt%push(pd)

    call pd%define_data(x, -0.25d0 * y)
    call pd%set_fill_curve(.true.)
    call plt%push(pd)

    call plt%draw()
end program
