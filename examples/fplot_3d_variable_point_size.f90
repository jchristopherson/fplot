program example
    use iso_fortran_env
    use fplot_core
    implicit none

    ! Variables
    integer(int32), parameter :: npts = 10
    real(real64) :: x(npts), y(npts), z(npts)
    type(plot_3d) :: plt
    type(plot_data_3d) :: pd
    type(parula_colormap) :: map

    ! Create the data to plot
    x = linspace(0.0d0, 5.0d0, npts)
    y = linspace(0.0d0, 5.0d0, npts)
    call random_number(z)

    ! Plot the results
    call plt%initialize()
    call plt%set_colormap(map)

    call pd%define_data(x, z, z, ps = 5.0d0 * z, c = z)
    call pd%set_draw_markers(.true.)
    call pd%set_draw_line(.false.)
    call plt%push(pd)

    call plt%draw()
end program