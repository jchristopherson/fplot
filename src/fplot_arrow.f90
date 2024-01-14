submodule (fplot_core) fplot_arrow
    implicit none
contains
! ------------------------------------------------------------------------------
pure module function par_get_is_visible(this) result(rst)
    class(plot_arrow), intent(in) :: this
    logical :: rst
    rst = this%m_visible
end function

! --------------------
module subroutine par_set_is_visible(this, x)
    class(plot_arrow), intent(inout) :: this
    logical, intent(in) :: x
    this%m_visible = x
end subroutine

! ------------------------------------------------------------------------------
pure module function par_get_tail(this) result(rst)
    class(plot_arrow), intent(in) :: this
    real(real32), dimension(3) :: rst
    rst = this%m_tail
end function

! --------------------
module subroutine par_set_tail_1(this, x)
    class(plot_arrow), intent(inout) :: this
    real(real32), intent(in) :: x(3)
    this%m_tail = x
end subroutine

! --------------------
module subroutine par_set_tail_2(this, x, y)
    class(plot_arrow), intent(inout) :: this
    real(real32), intent(in) :: x, y
    this%m_tail = [x, y, 0.0]
end subroutine

! --------------------
module subroutine par_set_tail_3(this, x, y, z)
    class(plot_arrow), intent(inout) :: this
    real(real32), intent(in) :: x, y, z
    this%m_tail = [x, y, z]
end subroutine

! ------------------------------------------------------------------------------
pure module function par_get_head(this) result(rst)
    class(plot_arrow), intent(in) :: this
    real(real32), dimension(3) :: rst
    rst = this%m_head
end function

! --------------------
module subroutine par_set_head_1(this, x)
    class(plot_arrow), intent(inout) :: this
    real(real32), intent(in) :: x(3)
    this%m_head = x
end subroutine

! --------------------
module subroutine par_set_head_2(this, x, y)
    class(plot_arrow), intent(inout) :: this
    real(real32), intent(in) :: x, y
    this%m_head = [x, y, 0.0]
end subroutine

! --------------------
module subroutine par_set_head_3(this, x, y, z)
    class(plot_arrow), intent(inout) :: this
    real(real32), intent(in) :: x, y, z
    this%m_head = [x, y, z]
end subroutine

! ------------------------------------------------------------------------------
pure module function par_get_color(this) result(rst)
    class(plot_arrow), intent(in) :: this
    type(color) :: rst
    rst = this%m_color
end function

! --------------------
module subroutine par_set_color(this, x)
    class(plot_arrow), intent(inout) :: this
    type(color), intent(in) :: x
    this%m_color = x
end subroutine

! ------------------------------------------------------------------------------
pure module function par_get_line_style(this) result(rst)
    class(plot_arrow), intent(in) :: this
    integer(int32) :: rst
    rst = this%m_linestyle
end function

! --------------------
module subroutine par_set_line_style(this, x)
    class(plot_arrow), intent(inout) :: this
    integer(int32), intent(in) :: x
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
pure module function par_get_line_width(this) result(rst)
    class(plot_arrow), intent(in) :: this
    real(real32) :: rst
    rst = this%m_linewidth
end function

! --------------------
module subroutine par_set_line_width(this, x)
    class(plot_arrow), intent(inout) :: this
    real(real32), intent(in) :: x
    this%m_linewidth = x
end subroutine

! ------------------------------------------------------------------------------
pure module function par_get_head_type(this) result(rst)
    class(plot_arrow), intent(in) :: this
    integer(int32) :: rst
    rst = this%m_head_type
end function

! --------------------
module subroutine par_set_head_type(this, x)
    class(plot_arrow), intent(inout) :: this
    integer(int32), intent(in) :: x
    if (x == ARROW_BACKHEAD .or. &
        x == ARROW_HEAD .or. &
        x == ARROW_HEADS .or. &
        x == ARROW_NO_HEAD &
    ) then
        this%m_head_type = x
    end if
end subroutine

! ------------------------------------------------------------------------------
pure module function par_get_fill(this) result(rst)
    class(plot_arrow), intent(in) :: this
    integer(int32) :: rst
    rst = this%m_filling
end function

! --------------------
module subroutine par_set_fill(this, x)
    class(plot_arrow), intent(inout) :: this
    integer(int32), intent(in) :: x
    if (x == ARROW_FILLED .or. &
        x == ARROW_EMPTY .or. &
        x == ARROW_NO_BORDER .or. &
        x == ARROW_NO_FILL &
    ) then
        this%m_filling = x
    end if
end subroutine

! ------------------------------------------------------------------------------
pure module function par_get_move_to_front(this) result(rst)
    class(plot_arrow), intent(in) :: this
    logical :: rst
    rst = this%m_front
end function

! --------------------
module subroutine par_set_move_to_front(this, x)
    class(plot_arrow), intent(inout) :: this
    logical, intent(in) :: x
    this%m_front = x
end subroutine

! ------------------------------------------------------------------------------
pure module function par_get_head_size(this) result(rst)
    class(plot_arrow), intent(in) :: this
    real(real32) :: rst
    rst = this%m_size
end function

! --------------------
module subroutine par_set_head_size(this, x)
    class(plot_arrow), intent(inout) :: this
    real(real32), intent(in) :: x
    this%m_size = x
end subroutine

! ------------------------------------------------------------------------------
pure module function par_get_head_angle(this) result(rst)
    class(plot_arrow), intent(in) :: this
    real(real32) :: rst
    rst = this%m_angle
end function

! --------------------
module subroutine par_set_head_angle(this, x)
    class(plot_arrow), intent(inout) :: this
    real(real32), intent(in) :: x
    this%m_angle = x
end subroutine

! ------------------------------------------------------------------------------
pure module function par_get_head_back_angle(this) result(rst)
    class(plot_arrow), intent(in) :: this
    real(real32) :: rst
    rst = this%m_backangle
end function

! --------------------
module subroutine par_set_head_back_angle(this, x)
    class(plot_arrow), intent(inout) :: this
    real(real32), intent(in) :: x
    this%m_backangle = x
end subroutine

! ------------------------------------------------------------------------------
pure module function par_get_use_default_size(this) result(rst)
    class(plot_arrow), intent(in) :: this
    logical :: rst
    rst = this%m_use_default_size
end function

! --------------------
module subroutine par_set_use_default_size(this, x)
    class(plot_arrow), intent(inout) :: this
    logical, intent(in) :: x
    this%m_use_default_size = x
end subroutine

! ------------------------------------------------------------------------------
module function par_get_cmd(this) result(rst)
    ! Arguments
    class(plot_arrow), intent(in) :: this
    character(len = :), allocatable :: rst

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
pure module subroutine par_assign(x, y)
    type(plot_arrow), intent(out) :: x
    class(plot_arrow), intent(in) :: y
    x%m_visible = y%m_visible
    x%m_tail = y%m_tail
    x%m_head = y%m_head
    x%m_color = y%m_color
    x%m_linestyle = y%m_linestyle
    x%m_linewidth = y%m_linewidth
    x%m_head_type = y%m_head_type
    x%m_filling = y%m_filling
    x%m_front = y%m_front
    x%m_size = y%m_size
    x%m_angle = y%m_angle
    x%m_backangle = y%m_backangle
    x%m_use_default_size = y%m_use_default_size
end subroutine

! ------------------------------------------------------------------------------
end submodule