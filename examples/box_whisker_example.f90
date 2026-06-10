program example
    use iso_fortran_env
    use fplot_core
    use strings
    implicit none

    ! Variables
    integer(int32), parameter :: n = 5
    integer(int32) :: i
    type(string) :: titles(n)
    real(real64) :: boxmin(n), boxmax(n), whiskermin(n), whiskermax(n)
    type(plot_2d) :: plt
    type(plot_data_box_whisker) :: pd

    ! Initialization
    do i = 1, n
        titles(i) = "Item " // to_string(i)
    end do
    call random_number(whiskermin)
    call random_number(whiskermax)
    call random_number(boxmin)
    call random_number(boxmax)

    ! Ensure proper order
    boxmin = boxmin + whiskermin
    boxmax = boxmin + boxmax
    whiskermax = whiskermax + boxmax

    ! Create the plot
    call plt%initialize()

    call pd%define_data(titles, boxmin, boxmax, whiskermin, whiskermax)
    call plt%push(pd)
    call plt%draw()
end program