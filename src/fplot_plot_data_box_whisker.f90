module fplot_plot_data_box_whisker
    use iso_fortran_env
    use fplot_plot_data
    use fplot_errors
    use fplot_colors
    use ferror
    use strings
    implicit none
    private
    public :: plot_data_box_whisker

    type, extends(plot_data_colored) :: plot_data_box_whisker
        !! A container for box-whisker plot data.
        type(string), private, allocatable, dimension(:) :: m_x
            !! The x-coordinate data.
        real(real64), private, allocatable, dimension(:) :: m_boxMin
            !! The minimum y-values for each box.
        real(real64), private, allocatable, dimension(:) :: m_boxMax
            !! The maximum y-values for each box.
        real(real64), private, allocatable, dimension(:) :: m_whiskerMin
            !! The minimum y-values for each whisker.
        real(real64), private, allocatable, dimension(:) :: m_whiskerMax
            !! The maximum y-values for each whisker.
        logical, private :: m_useY2 = .false.
            !! Plot against the secondary y-axis?
        logical, private :: m_whiskerbars = .true.
            !! Use horizontal whisker bar caps?
        real(real32), private :: m_whiskerWidth = 1.0
            !! On a scale of 0 -> 1, the whiskerwidth.
        real(real32), private :: m_lineWidth = 1.0
            !! The line width.
        real(real32), private :: m_boxWidth = 0.05
            !! The box width.
    contains
        procedure, public :: define_data => pdbw_define_data_xstring
        procedure, public :: get_command_string => pdbw_get_cmd
        procedure, public :: get_data_string => pdbw_get_data_cmd
        procedure, public :: get_draw_against_y2 => pdbw_get_use_y2
        procedure, public :: set_draw_against_y2 => pdbw_set_use_y2
        procedure, public :: get_use_whiskerbars => pdbw_get_use_whiskerbars
        procedure, public :: set_use_whiskerbars => pdbw_set_use_whiskerbars
        procedure, public :: get_whiskerbar_width => pdbw_get_whiskerbar_width
        procedure, public :: set_whiskerbar_width => pdbw_set_whiskerbar_width
        procedure, public :: get_line_width => pdbw_get_line_width
        procedure, public :: set_line_width => pdbw_set_line_width
        procedure, public :: get_box_width => pdbw_get_box_width
        procedure, public :: set_box_width => pdbw_set_box_width
    end type

contains
! ------------------------------------------------------------------------------
subroutine pdbw_define_data_xstring(this, x, boxmin, boxmax, whiskermin, &
    whiskermax, err)
    !! Defines the data set to plot.
    class(plot_data_box_whisker), intent(inout) :: this
        !! The plot_data_box_whisker object.
    type(string), intent(in), dimension(:) :: x
        !! The x-coordinate data.
    real(real64), intent(in), dimension(size(x)) :: boxmin
        !! The minimum y-values for each box.
    real(real64), intent(in), dimension(size(x)) :: boxmax
        !! The maximum y-values for each box.
    real(real64), intent(in), dimension(size(x)) :: whiskermin
        !! The minimum y-values for each whisker.
    real(real64), intent(in), dimension(size(x)) :: whiskermax
        !! The maximum y-values for each whisker.
    class(errors), intent(inout), optional, target :: err
        !! An error handling object.

    ! Local Variables
    integer(int32) :: n, flag
    class(errors), pointer :: errmgr
    type(errors), target :: deferr
    
    ! Initialization
    if (present(err)) then
        errmgr => err
    else
        errmgr => deferr
    end if
    n = size(x)

    ! Allocations
    if (allocated(this%m_x)) deallocate(this%m_x)
    if (allocated(this%m_boxMin)) deallocate(this%m_boxMin)
    if (allocated(this%m_boxMax)) deallocate(this%m_boxMax)
    if (allocated(this%m_whiskerMin)) deallocate(this%m_whiskerMin)
    if (allocated(this%m_whiskerMax)) deallocate(this%m_whiskerMax)

    allocate(this%m_x(n), source = x, stat = flag)
    if (flag == 0) allocate(this%m_boxMin(n), source = boxmin, stat = flag)
    if (flag == 0) allocate(this%m_boxMax(n), source = boxmax, stat = flag)
    if (flag == 0) allocate(this%m_whiskerMin(n), source = whiskermin, stat = flag)
    if (flag == 0) allocate(this%m_whiskerMax(n), source = whiskermax, stat = flag)
    if (flag /= 0) then
        call report_memory_error(errmgr, "pdbw_define_data_xstring", flag)
        return
    end if
end subroutine

! ------------------------------------------------------------------------------
function pdbw_get_cmd(this) result(rst)
    !! Gets the GNUPLOT command string for this object.
    class(plot_data_box_whisker), intent(in) :: this
        !! The plot_data_box_whisker object.
    character(len = :), allocatable :: rst
        !! The command string.

    ! Local Variables
    type(string_builder) :: str
    integer(int32) :: n, nname
    type(color) :: clr

    ! Style
    ! call str%append(' "-" using ($0+1):2:3:4:5:xtic(1) with candlesticks')
    call str%append(' "-" using ($0+1):2:3:4:5:(')
    call str%append(to_string(this%get_box_width()))
    call str%append("):xtic(1) with candlesticks")

    ! Title
    nname = len_trim(this%get_name())
    if (n > 0) then
        call str%append(' title "')
        call str%append(this%get_name())
        call str%append('"')
    else
        call str%append(' notitle')
    end if

    ! Whisker bars
    if (this%get_use_whiskerbars()) then
        call str%append(" whiskerbars ")
        call str%append(to_string(this%get_whiskerbar_width()))
    end if

    ! Color
    clr = this%get_line_color()
    call str%append(' lc rgb "#')
    call str%append(clr%to_hex_string())
    call str%append('"')

    ! Line Width
    call str%append(" lw ")
    call str%append(to_string(this%get_line_width()))

    ! End
    rst = char(str%to_string())
end function

! ------------------------------------------------------------------------------
function pdbw_get_data_cmd(this) result(rst)
    !! Gets the GNUPLOT command string defining the data for this object.
    class(plot_data_box_whisker), intent(in) :: this
        !! The plot_data_box_whisker object.
    character(len = :), allocatable :: rst
        !! The command string.

    ! Local Variables
    type(string_builder) :: str
    integer(int32) :: i, n
    character :: delimiter, nl

    ! Initialization
    delimiter = achar(9)
    nl = new_line(nl)
    n = size(this%m_x)

    ! Process
    do i = 1, n
        call str%append(this%m_x(i))
        call str%append(delimiter)
        call str%append(to_string(this%m_boxMin(i)))
        call str%append(delimiter)
        call str%append(to_string(this%m_whiskerMin(i)))
        call str%append(delimiter)
        call str%append(to_string(this%m_whiskerMax(i)))
        call str%append(delimiter)
        call str%append(to_string(this%m_boxMax(i)))
        call str%append(nl)
    end do

    ! End
    rst = char(str%to_string())
end function

! ------------------------------------------------------------------------------
function pdbw_get_axes_cmd(this) result(rst)
    !! Gets the GNUPLOT command string defining which axes the data is to be
    !! plotted against.
    class(plot_data_box_whisker), intent(in) :: this
        !! The plot_data_box_whisker object.
    character(len = :), allocatable :: rst
        !! The command string.

    ! Define which axes the data is to be plotted against
    if (this%get_draw_against_y2()) then
        rst = "axes x1y2"
    else
        rst = "axes x1y1"
    end if
end function

! ------------------------------------------------------------------------------
pure function pdbw_get_use_y2(this) result(rst)
    !! Gets a value determining if the data is to be plotted against the
    !! secondary y axis.
    class(plot_data_box_whisker), intent(in) :: this
        !! The plot_data_box_whisker object.
    logical :: rst
        !! Returns true if the data is to be plotted against the secondary y 
        !! axis; else, false for the primary y axis.
    rst = this%m_useY2
end function

! --------------------
subroutine pdbw_set_use_y2(this, x)
    !! Sets a value determining if the data is to be plotted against the
    !! secondary y axis.
    class(plot_data_box_whisker), intent(inout) :: this
        !! The plot_data_box_whisker object.
    logical, intent(in) :: x
        !! Set to true if the data is to be plotted against the secondary y 
        !! axis; else, false for the primary y axis.
    this%m_useY2 = x
end subroutine

! ------------------------------------------------------------------------------
pure function pdbw_get_use_whiskerbars(this) result(rst)
    !! Gets a value determining if whiskerbars should be used.
    class(plot_data_box_whisker), intent(in) :: this
        !! The plot_data_box_whisker object.
    logical :: rst
        !! True if whiskerbars should be used; else, false.
    rst = this%m_whiskerbars
end function

! --------------------
subroutine pdbw_set_use_whiskerbars(this, x)
    !! Sets a value determining if whiskerbars should be used.
    class(plot_data_box_whisker), intent(inout) :: this
        !! The plot_data_box_whisker object.
    logical, intent(in) :: x
        !! Set to true if whiskerbars should be used; else, false.
    this%m_whiskerbars = x
end subroutine

! ------------------------------------------------------------------------------
pure function pdbw_get_whiskerbar_width(this) result(rst)
    !! Gets the width of whiskerbar.
    class(plot_data_box_whisker), intent(in) :: this
        !! The plot_data_box_whisker object.
    real(real32) :: rst
        !! The width of the whiskerbar on a scale of 0:1 with 1 being the full
        !! width.
    rst = this%m_whiskerWidth
end function

! --------------------
subroutine pdbw_set_whiskerbar_width(this, x)
    !! Sets the width of the whiskerbar.
    class(plot_data_box_whisker), intent(inout) :: this
        !! The plot_data_box_whisker object.
    real(real32), intent(in) :: x
        !! The width of the whiskerbar.  This value is clamped to [0, 1] with
        !! 1 representing full width.

    if (x < 0.0d0) then
        this%m_whiskerWidth = 0.0d0
    else if (x > 1.0d0) then
        this%m_whiskerWidth = 1.0d0
    else
        this%m_whiskerWidth = x
    end if
end subroutine

! ------------------------------------------------------------------------------
pure function pdbw_get_line_width(this) result(x)
    !! Gets the width of the line, in pixels.
    class(plot_data_box_whisker), intent(in) :: this
        !! The plot_data_box_whisker object.
    real(real32) :: x
        !! The line width.
    x = this%m_lineWidth
end function

! --------------------
subroutine pdbw_set_line_width(this, x)
    !! Sets the width of the line, in pixels.
    class(plot_data_box_whisker), intent(inout) :: this
        !! The plot_data_box_whisker object.
    real(real32), intent(in) :: x
        !! The line width.
    this%m_lineWidth = x
end subroutine

! ------------------------------------------------------------------------------
pure function pdbw_get_box_width(this) result(rst)
    !! Gets the box width.
    class(plot_data_box_whisker), intent(in) :: this
        !! The plot_data_box_whisker object.
    real(real32) :: rst
        !! The box width.
    rst = this%m_boxWidth
end function

! --------------------
subroutine pdbw_set_box_width(this, x)
    !! Sets the box width.
    class(plot_data_box_whisker), intent(inout) :: this
        !! The plot_data_box_whisker object.
    real(real32), intent(in) :: x
        !! The box width.
    this%m_boxWidth = x
end subroutine

! ------------------------------------------------------------------------------

! --------------------

! ------------------------------------------------------------------------------

! --------------------

! ------------------------------------------------------------------------------
end module