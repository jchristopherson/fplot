program example
    use iso_fortran_env
    use fplot_core
    implicit none

    ! Local Variables
    type(plot_2d) :: plt
    type(plot_data_function) :: fcn

    ! Define the function to plot
    call fcn%define_data("sin(x)", "f(x)", 0.0d0, 10.0d0)

    ! Plot
    call plt%initialize()
    call plt%push(fcn)
    call plt%draw()
end program