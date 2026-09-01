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
        real(real32), private :: m_barWidth = 1.0d0
            !! A relative scaling of the width of a single bar.  The value
            !! must be between 0 and 1 with 1 being full width.
        integer(int32), private :: m_gap = 1
            !! The default gap size between groups
        logical, private :: m_stacked = .false.
            !! True if bars should be stacked; else, false for side-by-side
    contains
        procedure, public :: get_bar_width => pb_get_bar_width
        procedure, public :: set_bar_width => pb_set_bar_width
        procedure, public :: get_command_string => pb_get_cmd
        procedure, public :: get_stacked => pb_get_stacked
        procedure, public :: set_stacked => pb_set_stacked
        procedure, public :: get_spacing => pb_get_spacing
        procedure, public :: set_spacing => pb_set_spacing
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

    ! Style
    call str%append(new_line('a'))
    call str%append("set style data histogram")

    if (this%get_stacked()) then
        call str%append(new_line('a'))
        call str%append("set style histogram rowstacked")
    else
        call str%append(new_line('a'))
        call str%append("set style histogram cluster gap ")
        call str%append(to_string(this%get_spacing()))
    end if

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
pure function pb_get_stacked(this) result(rst)
    !! Gets a value determining if data sets should be stacked (true) or
    !! left side-by-side (false).
    class(plot_bar), intent(in) :: this
        !! The [[plot_bar]] object.
    logical :: rst
        !! True if multiple data sets should be stacked; else, false.
    rst = this%m_stacked
end function

! --------------------
subroutine pb_set_stacked(this, x)
    !! Gets a value determining if data sets should be stacked (true) or
    !! left side-by-side (false).
    class(plot_bar), intent(inout) :: this
        !! The [[plot_bar]] object.
    logical, intent(in) :: x
        !! True if multiple data sets should be stacked; else, false.
    this%m_stacked = x
end subroutine

! ------------------------------------------------------------------------------
pure function pb_get_spacing(this) result(rst)
    !! Gets the spacing between clusters of data sets.
    class(plot_bar), intent(in) :: this
        !! The [[plot+bar]] object.
    integer(int32) :: rst
        !! The spacing.
    rst = this%m_gap
end function

! --------------------
subroutine pb_set_spacing(this, x)
    !! Sets the spacing between clusters of data sets.
    class(plot_bar), intent(inout) :: this
        !! The [[plot+bar]] object.
    integer(int32), intent(in) :: x
        !! The spacing.
    this%m_gap = x
end subroutine

! ------------------------------------------------------------------------------
! TO DO YET:
! - lighting
end module
