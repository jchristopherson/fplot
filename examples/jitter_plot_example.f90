program example
    use iso_fortran_env
    use fplot_core
    implicit none

    ! Local Variables
    integer(int32), parameter :: n1 = 100
    integer(int32), parameter :: n2 = 100
    integer(int32), parameter :: n = n1 + n2
    real(real64) :: x(n), y(n)
    type(name_value_pair) :: labels(2)
    type(plot_2d) :: plt
    type(plot_data_2d) :: pd
    class(plot_axis), pointer :: xAxis

    ! Create the data sets
    call random_number(y)
    x(1:n1) = 1.0d0
    x(n1+1:n) = 2.0d0

    ! Define the labels
    labels(1)%name = "A"
    labels(1)%value = 1.0d0

    labels(2)%name = "B"
    labels(2)%value = 2.0d0

    ! Create the plot
    call plt%initialize()
    call plt%set_show_gridlines(.false.)
    call plt%set_use_jittering(.true.)
    xAxis => plt%get_x_axis()

    call xAxis%set_manual_tic_labels(labels)
    call xAxis%set_limits(0.5d0, 2.5d0)

    call pd%define_data(x, y)
    call pd%set_draw_line(.false.)
    call pd%set_draw_markers(.true.)
    call pd%set_marker_scaling(1.5)
    call plt%push(pd)
    call plt%draw()
end program