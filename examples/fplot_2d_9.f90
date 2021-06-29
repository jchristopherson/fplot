! fplot_2d_9.f90

program example
    use iso_fortran_env
    use fplot_core
    implicit none

    ! Local Variables
    integer(int32), parameter :: npts = 1000
    real(real64), parameter :: pi = 2.0d0 * acos(0.0d0)
    real(real64) :: t(npts), x(npts), y(npts)
    type(plot_2d) :: plt
    type(plot_data_2d) :: pd

    ! Generate the data
    t = linspace(0.0d0, 2.0d0 * pi, npts)
    x = cos(t)
    y = sin(t)

    ! Set up the plot
    call plt%initialize()
    call plt%set_font_size(14)
    call plt%set_title("Default Settings")

    call pd%define_data(x, y)
    call plt%push(pd)

    call plt%draw()

    ! Now show the effects of square axes
    call plt%set_axis_equal(.false.)
    call plt%set_square_axes(.true.)
    call plt%set_title("Square Axes")
    call plt%draw()
end program