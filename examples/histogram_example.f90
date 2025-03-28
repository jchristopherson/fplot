program example
    use iso_fortran_env
    use fplot_core
    implicit none

    ! Local Variables
    integer(int32), parameter :: n = 5000
    integer(int32), parameter :: nbins = 12
    real(real64) :: x(n), u(n), v(n)
    type(plot_bar) :: plt   ! can also be plot_2d
    type(plot_data_histogram) :: pd1

    ! Initialization
    call plt%initialize()

    ! Create some data
    call random_number(u)
    call random_number(v)
    v = v - 1.0d0
    x = u * u - v * v

    ! Plot the data
    call pd1%set_bin_count(nbins)   ! optiona, but must be done prior to define_data is used
    call pd1%define_data(x)
    call pd1%set_transparency(0.5)  ! optional - for illustration purposes
    call plt%push(pd1)
    call plt%draw()
end program