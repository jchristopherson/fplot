module fplot_arrow
    use iso_fortran_env
    use fplot_plot_object
    use fplot_colors
    use fplot_constants
    use strings
    implicit none
    private
    public :: plot_arrow

    type, extends(plot_object) :: plot_arrow
        !! Defines an arrow that can be drawn on a plot.
        logical, private :: m_visible = .true.
            !! Visible?
        real(real32), private, dimension(3) :: m_tail = [0.0, 0.0, 0.0]
            !! The x, y, z coordinates of the tail.
        real(real32), private, dimension(3) :: m_head = [0.0, 0.0, 0.0]
            !! The x, y, z coordinates of the head.
        type(color), private :: m_color = CLR_BLACK
            !! The arrow color.
        integer(int32), private :: m_linestyle = LINE_SOLID
            !! The line style.
        real(real32), private :: m_linewidth = 1.0
            !! The line width.
        integer(int32), private :: m_head_type = ARROW_HEAD
            !! The head configuration.
        integer(int32), private :: m_filling = ARROW_FILLED
            !! Arrow filling.
        logical, private :: m_front = .true.
            !! Move to front?
        real(real32), private :: m_size = 0.375
            !! Arrow head size.
        real(real32), private :: m_angle = 10.0
            !! Arrow head angle.
        real(real32), private :: m_backangle = 90.0
            !! Arrow head back angle.
        logical, private :: m_use_default_size = .true.
            !! Use default head size.
    contains
        procedure, public :: get_is_visible => par_get_is_visible
        procedure, public :: set_is_visible => par_set_is_visible
        procedure, public :: get_tail_location => par_get_tail
        generic, public :: set_tail_location => par_set_tail_1, &
            par_set_tail_2, par_set_tail_3
        procedure, private :: par_set_tail_1
        procedure, private :: par_set_tail_2
        procedure, private :: par_set_tail_3
        procedure, public :: get_head_location => par_get_head
        generic, public :: set_head_location => par_set_head_1, &
            par_set_head_2, par_set_head_3
        procedure, private :: par_set_head_1
        procedure, private :: par_set_head_2
        procedure, private :: par_set_head_3
        procedure, public :: get_color => par_get_color
        procedure, public :: set_color => par_set_color
        procedure, public :: get_line_style => par_get_line_style
        procedure, public :: set_line_style => par_set_line_style
        procedure, public :: get_line_width => par_get_line_width
        procedure, public :: set_line_width => par_set_line_width
        procedure, public :: get_head_type => par_get_head_type
        procedure, public :: set_head_type => par_set_head_type
        procedure, public :: get_head_fill => par_get_fill
        procedure, public :: set_head_fill => par_set_fill
        procedure, public :: get_move_to_front => par_get_move_to_front
        procedure, public :: set_move_to_front => par_set_move_to_front
        procedure, public :: get_head_size => par_get_head_size
        procedure, public :: set_head_size => par_set_head_size
        procedure, public :: get_head_angle => par_get_head_angle
        procedure, public :: set_head_angle => par_set_head_angle
        procedure, public :: get_head_back_angle => par_get_head_back_angle
        procedure, public :: set_head_back_angle => par_set_head_back_angle
        procedure, public :: get_use_default_size => par_get_use_default_size
        procedure, public :: set_use_default_size => par_set_use_default_size
        procedure, public :: get_command_string => par_get_cmd
    end type

contains
! ------------------------------------------------------------------------------
pure function par_get_is_visible(this) result(rst)
    !! Gets a value determining if the arrow is visible.
    class(plot_arrow), intent(in) :: this
        !! The plot_arrow object.
    logical :: rst
        !! True if the arrow is visible; else, false.
    rst = this%m_visible
end function

! --------------------
subroutine par_set_is_visible(this, x)
    !! Sets a value determining if the arrow is visible.
    class(plot_arrow), intent(inout) :: this
        !! The plot_arrow object.
    logical, intent(in) :: x
        !! True if the arrow is visible; else, false.
    this%m_visible = x
end subroutine

! ------------------------------------------------------------------------------
pure function par_get_tail(this) result(rst)
    !! Gets the coordinates of the arrow's tail.
    class(plot_arrow), intent(in) :: this
        !! The plot_arrow object.
    real(real32), dimension(3) :: rst
        !! A 3-element array containing the x, y, and z coordinates of the 
        !! arrow's tail.
    rst = this%m_tail
end function

! --------------------
subroutine par_set_tail_1(this, x)
    !! Sets the coordinates of the arrow's tail.
    class(plot_arrow), intent(inout) :: this
        !! The plot_arrow object.
    real(real32), intent(in) :: x(3)
        !! A 3-element array containing the x, y, and z coordinates of the 
        !! arrow's tail.
    this%m_tail = x
end subroutine

! --------------------
subroutine par_set_tail_2(this, x, y)
    !! Sets the coordinates of the arrow's tail.
    class(plot_arrow), intent(inout) :: this
        !! The plot_arrow object.
    real(real32), intent(in) :: x
        !! The x-coordinate of the arrow's tail.
    real(real32), intent(in) :: y
        !! !! The y-coordinate of the arrow's tail.
    this%m_tail = [x, y, 0.0]
end subroutine

! --------------------
subroutine par_set_tail_3(this, x, y, z)
    !! Sets the coordinates of the arrow's tail.
    class(plot_arrow), intent(inout) :: this
        !! The plot_arrow object.
    real(real32), intent(in) :: x
        !! The x-coordinate of the arrow's tail.
    real(real32), intent(in) :: y
        !! The y-coordinate of the arrow's tail.
    real(real32), intent(in) :: z
        !! The z-coordinate of the arrow's tail.
    this%m_tail = [x, y, z]
end subroutine

! ------------------------------------------------------------------------------
pure function par_get_head(this) result(rst)
    !! Gets the coordinates of the arrow's head.
    class(plot_arrow), intent(in) :: this
        !! The plot_arrow object.
    real(real32), dimension(3) :: rst
        !! A 3-element array containing the x, y, and z coordinates of the
        !! arrow's head.
    rst = this%m_head
end function

! --------------------
subroutine par_set_head_1(this, x)
    !! Sets the location of the arrow's head.
    class(plot_arrow), intent(inout) :: this
        !! The plot_arrow object.
    real(real32), intent(in) :: x(3)
        !! A 3-element array containing the x, y, and z coordinates of the
        !! arrow's head.
    this%m_head = x
end subroutine

! --------------------
subroutine par_set_head_2(this, x, y)
    !! Sets the location of the arrow's head.
    class(plot_arrow), intent(inout) :: this
        !! The plot_arrow object.
    real(real32), intent(in) :: x
        !! The x-coordinate of the arrow's head.
    real(real32), intent(in) :: y
        !! The y-coordinate of the arrow's head.
    this%m_head = [x, y, 0.0]
end subroutine

! --------------------
subroutine par_set_head_3(this, x, y, z)
    !! Sets the location of the arrow's head.
    class(plot_arrow), intent(inout) :: this
        !! The plot_arrow object.
    real(real32), intent(in) :: x
        !! The x-coordinate of the arrow's head.
    real(real32), intent(in) :: y
        !! The y-coordinate of the arrow's head.
    real(real32), intent(in) :: z
        !! The z-coordinate of the arrow's head.
    this%m_head = [x, y, z]
end subroutine

! ------------------------------------------------------------------------------
pure function par_get_color(this) result(rst)
    !! Gets the color of the arrow.
    class(plot_arrow), intent(in) :: this
        !! The plot_arrow object.
    type(color) :: rst
        !! The color.
    rst = this%m_color
end function

! --------------------
subroutine par_set_color(this, x)
    !! Sets the color of the arrow.
    class(plot_arrow), intent(inout) :: this
        !! The plot_arrow object.
    type(color), intent(in) :: x
        !! The color.
    this%m_color = x
end subroutine

! ------------------------------------------------------------------------------
pure function par_get_line_style(this) result(rst)
    !! Gets the line style used to draw the arrow.
    class(plot_arrow), intent(in) :: this
        !! The plot_arrow object.
    integer(int32) :: rst
        !! The line style.
    rst = this%m_linestyle
end function

! --------------------
subroutine par_set_line_style(this, x)
    !! Sets the line style used to draw the arrow.
    class(plot_arrow), intent(inout) :: this
        !! The plot_arrow object.
    integer(int32), intent(in) :: x
        !! The line style.  The value must be one of the following.
        !!
        !!  - LINE_SOLID
        !!
        !!  - LINE_DASHED
        !!
        !!  - LINE_DASH_DOTTED
        !!
        !!  - LINE_DASH_DOT_DOT
        !!
        !!  - LINE_DOTTED
        !!
        !! If the value is not one of the above, the command is ignored.
    if (x == LINE_DASHED .or. &
        x == LINE_DASH_DOTTED .or. &
        x == LINE_DASH_DOT_DOT .or. &
        x == LINE_DOTTED .or. &
        x == LINE_SOLID) then
        ! Only reset the line style if it is a valid type.
        this%m_linestyle = x
    end if
end subroutine

! ------------------------------------------------------------------------------
pure function par_get_line_width(this) result(rst)
    !! Gets the width of the lines used to draw the arrow.
    class(plot_arrow), intent(in) :: this
        !! The plot_arrow object.
    real(real32) :: rst
        !! The width of the line.
    rst = this%m_linewidth
end function

! --------------------
subroutine par_set_line_width(this, x)
    !! Sets the width of the lines used to draw the arrow.
    class(plot_arrow), intent(inout) :: this
        !! The plot_arrow object.
    real(real32), intent(in) :: x
        !! The width of the line.
    this%m_linewidth = x
end subroutine

! ------------------------------------------------------------------------------
pure function par_get_head_type(this) result(rst)
    !! Gets the type of arrow head.
    class(plot_arrow), intent(in) :: this
        !! The plot_arrow object.
    integer(int32) :: rst
        !! The arrow head type.
    rst = this%m_head_type
end function

! --------------------
subroutine par_set_head_type(this, x)
    !! Sets the type of arrow head.
    class(plot_arrow), intent(inout) :: this
        !! The plot_arrow object.
    integer(int32), intent(in) :: x
        !! The arrow head type.  It must be one of the following constants.
        !!
        !! - ARROW_HEAD
        !!
        !! - ARROW_BACKHEAD
        !!
        !! - ARROW_HEADS
        !!
        !! - ARROW_NO_HEAD
        !!
        !! If the value is not one of the above, the command is ignored.
    if (x == ARROW_BACKHEAD .or. &
        x == ARROW_HEAD .or. &
        x == ARROW_HEADS .or. &
        x == ARROW_NO_HEAD &
    ) then
        this%m_head_type = x
    end if
end subroutine

! ------------------------------------------------------------------------------
pure function par_get_fill(this) result(rst)
    !! Gets a flag denoting the head fill type.
    class(plot_arrow), intent(in) :: this
        !! The plot_arrow object.
    integer(int32) :: rst
        !! The flag denoting head fill.
    rst = this%m_filling
end function

! --------------------
subroutine par_set_fill(this, x)
    !! Sets a flag denoting the head fill type.
    class(plot_arrow), intent(inout) :: this
        !! The plot_arrow object.
    integer(int32), intent(in) :: x
        !! The flag denoting head fill.  It must be one of the following 
        !! constants.
        !!
        !! - ARROW_FILLED
        !!
        !! - ARROW_EMPTY
        !!
        !! - ARROW_NO_BORDER
        !!
        !! - ARROW_NO_FILL
        !!
        !! If the value is not one of the above, the command is ignored.
    if (x == ARROW_FILLED .or. &
        x == ARROW_EMPTY .or. &
        x == ARROW_NO_BORDER .or. &
        x == ARROW_NO_FILL &
    ) then
        this%m_filling = x
    end if
end subroutine

! ------------------------------------------------------------------------------
pure function par_get_move_to_front(this) result(rst)
    !! Gets a value determining if the arrow should be moved to the front.
    class(plot_arrow), intent(in) :: this
        !! The plot_arrow object.
    logical :: rst
        !! True if the arrow should be moved to the front; else, false.
    rst = this%m_front
end function

! --------------------
subroutine par_set_move_to_front(this, x)
    !! Sets a value determining if the arrow should be moved to the front.
    class(plot_arrow), intent(inout) :: this
        !! The plot_arrow object.
    logical, intent(in) :: x
        !! True if the arrow should be moved to the front; else, false.
    this%m_front = x
end subroutine

! ------------------------------------------------------------------------------
pure function par_get_head_size(this) result(rst)
    !! Gets the size of the arrow head.
    class(plot_arrow), intent(in) :: this
        !! The plot_arrow object.
    real(real32) :: rst
        !! The head size.
    rst = this%m_size
end function

! --------------------
subroutine par_set_head_size(this, x)
    !! Sets the size of the arrow head.
    class(plot_arrow), intent(inout) :: this
        !! The plot_arrow object.
    real(real32), intent(in) :: x
        !! The head size.
    this%m_size = x
end subroutine

! ------------------------------------------------------------------------------
pure function par_get_head_angle(this) result(rst)
    !! Gets the angle of the arrow head.
    class(plot_arrow), intent(in) :: this
        !! The plot_arrow object.
    real(real32) :: rst
        !! The angle, in degrees.
    rst = this%m_angle
end function

! --------------------
subroutine par_set_head_angle(this, x)
    !! Sets the angle of the arrow head.
    class(plot_arrow), intent(inout) :: this
        !! The plot_arrow object.
    real(real32), intent(in) :: x
        !! The angle, in degrees.
    this%m_angle = x
end subroutine

! ------------------------------------------------------------------------------
pure function par_get_head_back_angle(this) result(rst)
    !! Gets the angle of the back of the arrow head.
    class(plot_arrow), intent(in) :: this
        !! The plot_arrow object.
    real(real32) :: rst
        !! The angle, in degrees.
    rst = this%m_backangle
end function

! --------------------
subroutine par_set_head_back_angle(this, x)
    !! Sets the angle of the back of the arrow head.
    class(plot_arrow), intent(inout) :: this
        !! The plot_arrow object.
    real(real32), intent(in) :: x
        !! The angle, in degrees.
    this%m_backangle = x
end subroutine

! ------------------------------------------------------------------------------
pure function par_get_use_default_size(this) result(rst)
    !! Gets a value determining if arrow head sizing defaults should be used.
    class(plot_arrow), intent(in) :: this
        !! The plot_arrow object.
    logical :: rst
        !! True if the defaults should be used; else, false.
    rst = this%m_use_default_size
end function

! --------------------
subroutine par_set_use_default_size(this, x)
    !! Sets a value determining if arrow head sizing defaults should be used.
    class(plot_arrow), intent(inout) :: this
        !! The plot_arrow object.
    logical, intent(in) :: x
        !! True if the defaults should be used; else, false.
    this%m_use_default_size = x
end subroutine

! ------------------------------------------------------------------------------
function par_get_cmd(this) result(rst)
    !! Returns the appropriate GNUPLOT command string to establish appropriate 
    !! parameters.
    class(plot_arrow), intent(in) :: this
        !! The plot_arrow object.
    character(len = :), allocatable :: rst
        !! The GNUPLOT command string.

    ! Local Variables
    type(string_builder) :: str
    type(color) :: clr
    real(real32) :: tail(3), head(3)

    ! Quick Return
    if (.not.this%get_is_visible()) then
        rst = ""
        return
    end if

    ! Command
    call str%append("set arrow")

    ! Position Info
    tail = this%get_tail_location()
    head = this%get_head_location()
    call str%append(" from ")
    call str%append(to_string(tail(1)))
    call str%append(",")
    call str%append(to_string(tail(2)))
    call str%append(",")
    call str%append(to_string(tail(3)))

    call str%append(" to ")
    call str%append(to_string(head(1)))
    call str%append(",")
    call str%append(to_string(head(2)))
    call str%append(",")
    call str%append(to_string(head(3)))

    ! Head Type
    select case (this%get_head_type())
    case (ARROW_BACKHEAD)
        call str%append(" backhead")
    case (ARROW_HEAD)
        call str%append(" head")
    case (ARROW_HEADS)
        call str%append(" heads")
    case (ARROW_NO_HEAD)
        call str%append(" nohead")
    end select

    if (this%get_head_type() /= ARROW_NO_HEAD) then
        ! Fill Info
        select case (this%get_head_fill())
        case (ARROW_FILLED)
            call str%append(" filled")
        case (ARROW_EMPTY)
            call str%append(" empty")
        case (ARROW_NO_BORDER)
            call str%append(" noborder")
        case (ARROW_NO_FILL)
            call str%append(" nofilled")
        end select

        ! Size
        if (.not.this%get_use_default_size()) then
            call str%append(" size ")
            call str%append(to_string(this%get_head_size()))
            call str%append(",")
            call str%append(to_string(this%get_head_angle()))
            call str%append(",")
            call str%append(to_string(this%get_head_back_angle()))
        end if
    end if

    ! Front/Back
    if (this%get_move_to_front()) then
        call str%append(" front")
    else
        call str%append(" back")
    end if

    ! Line Color
    clr = this%get_color()
    call str%append(' lc rgb "#')
    call str%append(clr%to_hex_string())
    call str%append('"')

    ! Line Width
    call str%append(" lw ")
    call str%append(to_string(this%get_line_width()))

    ! Line Style
    call str%append(" lt ")
    call str%append(to_string(this%get_line_style()))
    if (this%get_line_style() /= LINE_SOLID) then
        call str%append(" dashtype ")
        call str%append(to_string(this%get_line_style()))
    end if

    ! End
    rst = char(str%to_string())
end function

! ------------------------------------------------------------------------------
! pure subroutine par_assign(x, y)
!     type(plot_arrow), intent(out) :: x
!     class(plot_arrow), intent(in) :: y
!     x%m_visible = y%m_visible
!     x%m_tail = y%m_tail
!     x%m_head = y%m_head
!     x%m_color = y%m_color
!     x%m_linestyle = y%m_linestyle
!     x%m_linewidth = y%m_linewidth
!     x%m_head_type = y%m_head_type
!     x%m_filling = y%m_filling
!     x%m_front = y%m_front
!     x%m_size = y%m_size
!     x%m_angle = y%m_angle
!     x%m_backangle = y%m_backangle
!     x%m_use_default_size = y%m_use_default_size
! end subroutine

! ------------------------------------------------------------------------------
end module