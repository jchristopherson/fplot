! fplot_polar_1.f90

program example
    use iso_fortran_env
    use fplot_core

    ! Local Variables
    integer(int32), parameter :: npts = 1000
    real(real64), parameter :: pi = 2.0d0 * acos(0.0d0)
    real(real64) :: t(npts), x(npts)
    type(plot_polar) :: plt
    type(plot_data_2d) :: pd

    ! Create a function to plot
    t = linspace(-2.0d0 * pi, 2.0d0 * pi, npts)
    x = t * sin(t)

    ! Plot the function
    call plt%initialize()
    call plt%set_font_size(14)
    call plt%set_title("Polar Plot Example")

    call pd%define_data(t, x)
    call plt%push(pd)
    call plt%draw()
end program