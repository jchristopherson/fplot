! Contrast this example to polar_plot_example.f90 to see how the "add" 
! procedure simplifies creation of a plot.  The plot_data_2d object is not
! needed.  Behind the scenes, the plot_data_2d object is created and can still
! be accessed, but this approach simplifies the interface for most typical
! use cases.
program example
    use iso_fortran_env
    use fplot_core

    ! Local Variables
    integer(int32), parameter :: npts = 1000
    real(real64), parameter :: pi = 2.0d0 * acos(0.0d0)
    real(real64) :: t(npts), x(npts)
    type(plot_polar) :: plt

    ! Create a function to plot
    t = linspace(-2.0d0 * pi, 2.0d0 * pi, npts)
    x = t * sin(t)

    ! Plot the function
    call plt%initialize()
    call plt%set_font_size(14)
    call plt%set_title("Polar Plot Example")
    call plt%set_autoscale(.false.)
    call plt%set_radial_limits([0.0d0, 6.0d0])
    call plt%push(t, x, lw = 2.0)
    call plt%draw()
end program