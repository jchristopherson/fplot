module fplot_plot_histogram
    use iso_fortran_env
    use fplot_plot_bar
    use fplot_plot_data
    use fplot_plot_data_histogram
    use strings
    implicit none
    private

    type, extends(plot_bar) :: plot_histogram
        !! Defines a 2D plot tailored towards histogram plotting.
    contains
    end type
contains
! ------------------------------------------------------------------------------
function ph_get_cmd(this) result(rst)
    !! Gets the GNUPLOT commands required to draw the plot.
    class(plot_histogram), intent(in) :: this
        !! The plot_histogram object.
    character(len = :), allocatable :: rst
        !! The command string.

    ! Local Variables
    integer(int32) :: i, n
    real(real64) :: minX, maxX
    type(string_builder) :: str
    class(plot_data), pointer :: pd

    ! Cycle over the data and determine the min and max values from the data
    n = this%get_count()
    maxX = -huge(1.0d0)
    minX = huge(1.0d0)
    do i = 1, n
        pd => this%get(i)
        if (.not.associated(pd)) cycle
        select type (pd)
        class is (plot_data_histogram)
            maxX = max(maxX, pd%get_maximum_value())
            minX = min(minX, pd%get_minimum_value())
        end select
    end do

    ! Define the x axis tic labels

    ! Call the base method
    call str%append(this%plot_bar%get_command_string())

    ! End
    rst = str%to_string()
end function

! ------------------------------------------------------------------------------
end module