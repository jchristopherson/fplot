program example
    use iso_fortran_env
    use fplot_core
    implicit none

    ! Local Variables
    integer(int32), parameter :: n = 5000
    integer(int32), parameter :: nbins = 30
    real(real64) :: x(n), u(n), v(n)
    type(plot_2d) :: plt
    type(plot_data_histogram) :: pd1
    class(plot_axis), pointer :: xAxis

    ! Initialization
    call plt%initialize()
    xAxis => plt%get_x_axis()

    ! Create some data
    call random_number(u)
    call random_number(v)
    v = v - 1.0d0
    x = u * u - v * v

    ! Rotate the labels on the x axis
    call xAxis%set_tic_label_angle(90.0)
    call xAxis%set_tic_label_rotation_origin(GNUPLOT_ROTATION_ORIGIN_RIGHT)

    ! Plot the data
    call pd1%set_bin_count(nbins)   ! optional, but must be done prior to define_data is used
    call pd1%define_data(x)
    call plt%push(pd1)
    call plt%draw()
end program