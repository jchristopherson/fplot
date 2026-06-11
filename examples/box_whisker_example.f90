program example
    use iso_fortran_env
    use fplot_core
    use strings
    implicit none

    ! Variables
    integer(int32), parameter :: n = 3
    integer(int32) :: i
    type(string) :: titles(n)
    real(real64) :: boxmin(n), boxmax(n), whiskermin(n), whiskermax(n)
    type(plot_2d) :: plt
    type(plot_data_box_whisker) :: pd

    ! Initialization
    do i = 1, n
        titles(i) = "Item-" // to_string(i)
    end do
    boxmin = [4.5d0, 5.0d0, 6.0d0]
    boxmax = [7.5d0, 8.0d0, 9.0d0]
    whiskermin = [3.0d0, 4.0d0, 5.0d0]
    whiskermax = [9.0d0, 10.0d0, 11.0d0]

    ! Create the plot
    call plt%initialize()

    call pd%define_data(titles, boxmin, boxmax, whiskermin, whiskermax)
    call pd%set_line_width(2.0)
    call plt%push(pd)
    call plt%draw()
end program