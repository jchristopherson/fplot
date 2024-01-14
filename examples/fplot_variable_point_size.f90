program example
    use iso_fortran_env
    use fplot_core
    implicit none

    ! Variables
    integer(int32), parameter :: npts = 10
    real(real64) :: x(npts), y(npts)
    type(plot_2d) :: plt
    type(plot_data_2d) :: pd
    type(parula_colormap) :: map

    ! Create the data to plot
    x = linspace(0.0d0, 5.0d0, npts)
    call random_number(y)

    ! Plot the results
    call plt%initialize()
    call plt%set_colormap(map)

    call pd%define_data(x, y, ps = 3.0d0 * y, c = y)
    call pd%set_draw_markers(.true.)
    call pd%set_draw_line(.false.)
    call plt%push(pd)

    call plt%draw()
end program