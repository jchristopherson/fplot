! fplot_fill_2.f90

program example
    use iso_fortran_env
    use fplot_core
    implicit none

    ! Local Variables
    integer(int32), parameter :: npts = 100
    real(real64), parameter :: pi = 2.0d0 * acos(0.0d0)
    real(real64) :: x(npts), y(npts)
    type(plot_2d) :: plt
    type(filled_plot_data) :: pd

    ! Generate the curve to plot
    x = linspace(0.0d0, 1.0d0, npts)
    y = sin(4.0d0 * pi * x)

    ! Plot the data
    call plt%initialize()
    
    call pd%define_data(x, y, 0.25d0 * y)
    
    call plt%push(pd)
    call plt%draw()
end program
