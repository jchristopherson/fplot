program example
    use iso_fortran_env
    use fplot_core
    implicit none

    ! Parameters
    integer(int32), parameter :: npts = 1000

    ! Local Variables
    real(real64) :: x(npts), y(npts)
    type(plot_2d) :: plt
    type(plot_data_2d) :: ds
    type(cool_colormap) :: map

    ! Build the data set
    x = linspace(0.0d0, 1.0d1, npts)
    y = exp(-0.2 * x) * sin(10.0d0 * x) * cos(5.0d0 * x)

    ! Plot the data set
    call plt%initialize()
    call plt%set_colormap(map)
    call plt%set_font_size(14)
    call ds%define_data(x, y, y)
    call ds%set_line_width(3.0)
    call plt%push(ds)
    call plt%draw()
end program
