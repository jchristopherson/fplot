! fplot_plot_bar.f90

module fplot_plot_bar
    use iso_fortran_env
    use fplot_plot_2d
    use strings
    implicit none
    private
    public :: plot_bar

    type, extends(plot_2d) :: plot_bar
        !! Defines a 2D plot tailored towards bar plotting.
        real(real32), private :: m_barWidth = 0.75d0
            !! A relative scaling of the width of a single bar.  The value
            !! must be between 0 and 1 with 1 being full width.
    contains
        procedure, public :: get_bar_width => pb_get_bar_width
        procedure, public :: set_bar_width => pb_set_bar_width
        procedure, public :: get_command_string => pb_get_cmd
    end type

contains
! ------------------------------------------------------------------------------
pure function pb_get_bar_width(this) result(x)
    !! Gets the bar width scaling factor.
    class(plot_bar), intent(in) :: this
        !! The plot_bar object.
    real(real32) :: x
        !! The scaling factor.
    x = this%m_barWidth
end function

! ------------------------------------------------------------------------------
subroutine pb_set_bar_width(this, x)
    !! Sets the bar width scaling factor.
    class(plot_bar), intent(inout) :: this
        !! The plot_bar object.
    real(real32), intent(in) :: x
        !! The scaling factor.  The value must be in the set [0, 1]; else, the
        !! value will be shifted accordingly.
    if (x > 1.0) then
        this%m_barWidth = 1.0
    else if (x < 0.0) then
        this%m_barWidth = 0.0
    else
        this%m_barWidth = x
    end if
end subroutine

! ------------------------------------------------------------------------------
function pb_get_cmd(this) result(x)
    !! Gets the GNUPLOT commands required to draw the plot.
    class(plot_bar), intent(in) :: this
        !! The plot_bar object.
    character(len = :), allocatable :: x
        !! The command string.

    ! Local Variables
    type(string_builder) :: str

    ! Initialization
    call str%initialize()

    ! Box Width
    call str%append(new_line('a'))
    call str%append("set boxwidth ")
    call str%append(to_string(this%get_bar_width()))
    call str%append(" relative")

    ! Call the base routine to establish the remainder of the plot
    call str%append(this%plot_2d%get_command_string())

    ! End
    x = char(str%to_string())
end function

! ------------------------------------------------------------------------------
! TO DO YET:
! - clustering
! - stacking
! - lighting
end module
