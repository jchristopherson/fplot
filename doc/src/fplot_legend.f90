! fplot_legend.f90

module fplot_legend
    use iso_fortran_env
    use fplot_plot_object
    use fplot_constants
    use strings
    implicit none
    private
    public :: legend

    type, extends(plot_object) :: legend
        !! Defines a legend object.
        logical, private :: m_inside = .true.
            !! Inside or outside the axes?
        logical, private :: m_box = .true.
            !! Box around?
        character(len = 20), private :: m_horzPosition = LEGEND_RIGHT
            !! Horizontal position.
        character(len = 20), private :: m_vertPosition = LEGEND_TOP
            !! Verical position.
        logical, private :: m_show = .false.
            !! Is visible?
        character(len = 20), private :: m_layout = LEGEND_ARRANGE_VERTICALLY
            !! Determines the legend layout.
        logical, private :: m_opaque = .true.
            !! Opaque background?
    contains
        procedure, public :: get_draw_inside_axes => leg_get_inside
        procedure, public :: set_draw_inside_axes => leg_set_inside
        procedure, public :: get_draw_border => leg_get_box
        procedure, public :: set_draw_border => leg_set_box
        procedure, public :: get_horizontal_position => leg_get_horz_pos
        procedure, public :: set_horizontal_position => leg_set_horz_pos
        procedure, public :: get_vertical_position => leg_get_vert_pos
        procedure, public :: set_vertical_position => leg_set_vert_pos
        procedure, public :: get_is_visible => leg_get_visible
        procedure, public :: set_is_visible => leg_set_visible
        procedure, public :: get_command_string => leg_get_command_txt
        procedure, public :: get_layout => leg_get_layout
        procedure, public :: set_layout => leg_set_layout
        procedure, public :: get_is_opaque => leg_get_opaque
        procedure, public :: set_is_opaque => leg_set_opaque
    end type

contains
! ------------------------------------------------------------------------------
    pure function leg_get_inside(this) result(x)
        !! Gets a value determining if the legend should be drawn inside
        !! or outside the axes border.
        class(legend), intent(in) :: this
            !! The legend object.
        logical :: x
            !! True to draw inside the axes border; else, false for outside.
        x = this%m_inside
    end function

! ---------------------
    subroutine leg_set_inside(this, x)
        !! Sets a value determining if the legend should be drawn inside
        !! or outside the axes border.
        class(legend), intent(inout) :: this
            !! The legend object.
        logical, intent(in) :: x
            !! True to draw inside the axes border; else, false for outside.
        this%m_inside = x
    end subroutine

! ------------------------------------------------------------------------------
    pure function leg_get_box(this) result(x)
        !! Gets a value determining if the legend should have a border.
        class(legend), intent(in) :: this
            !! The legend object.
        logical :: x
            !! True if the legend should have a border; else, false.
        x = this%m_box
    end function

! ---------------------
    subroutine leg_set_box(this, x)
        !! Sets a value determining if the legend should have a border.
        class(legend), intent(inout) :: this
            !! The legend object.
        logical, intent(in) :: x
            !! True if the legend should have a border; else, false.
        this%m_box = x
    end subroutine

! ------------------------------------------------------------------------------
    pure function leg_get_horz_pos(this) result(x)
        !! Gets the horizontal position of the legend.
        class(legend), intent(in) :: this
            !! The legend object.
        character(len = :), allocatable :: x
            !! The horizontal position of the legend (LEGEND_LEFT,
            !! LEGEND_CENTER, or LEGEND_RIGHT).
        integer(int32) :: n
        n = len_trim(this%m_horzPosition)
        allocate(character(len = n) :: x)
        x = trim(this%m_horzPosition)
    end function

! ---------------------
    subroutine leg_set_horz_pos(this, x)
        !! Sets the horizontal position of the legend.
        class(legend), intent(inout) :: this
            !! The legend object.
        character(len = *), intent(in) :: x
            !! The horizontal position of the legend.  The parameter must be
            !! set to one of the following: LEGEND_LEFT, LEGEND_CENTER, or
            !! LEGEND_RIGHT.  If not, the default LEGEND_RIGHT will be used.
        this%m_horzPosition = x
        if (x /= LEGEND_LEFT .and. x /= LEGEND_RIGHT .and. x /= LEGEND_CENTER) &
            this%m_horzPosition = LEGEND_RIGHT
    end subroutine

! ------------------------------------------------------------------------------
    pure function leg_get_vert_pos(this) result(x)
        !! Gets the vertical position of the legend.
        class(legend), intent(in) :: this
            !! The legend object.
        character(len = :), allocatable :: x
            !! The vertical position of the legend (LEGEND_TOP,
            !! LEGEND_CENTER, or LEGEND_BOTTOM).
        integer(int32) :: n
        n = len_trim(this%m_vertPosition)
        allocate(character(len = n) :: x)
        x = trim(this%m_vertPosition)
    end function

! ---------------------
    subroutine leg_set_vert_pos(this, x)
        !! Sets the vertical position of the legend.
        class(legend), intent(inout) :: this
            !! The legend object.
        character(len = *), intent(in) :: x
            !! The vertical position of the legend.  The parameter must be
            !! set to one of the following: LEGEND_TOP, LEGEND_CENTER, or
            !! LEGEND_BOTTOM.  If not, the default LEGEND_TOP will be used.
        this%m_vertPosition = x
        if (x /= LEGEND_TOP .and. x /= LEGEND_CENTER .and. x /= LEGEND_BOTTOM) &
            this%m_vertPosition = LEGEND_TOP
    end subroutine

! ------------------------------------------------------------------------------
    pure function leg_get_visible(this) result(x)
        !! Gets a value determining if the legend is visible.
        class(legend), intent(in) :: this
            !! The legend object.
        logical :: x
            !! True if the legend is visible; else, false.
        x = this%m_show
    end function

! ---------------------
    subroutine leg_set_visible(this, x)
        !! Sets a value determining if the legend is visible.
        class(legend), intent(inout) :: this
            !! The legend object.
        logical, intent(in) :: x
            !! True if the legend is visible; else, false.
        this%m_show = x
    end subroutine

! ------------------------------------------------------------------------------
    function leg_get_command_txt(this) result(txt)
        !! Gets the command string defining the legend properties.
        class(legend), intent(in) :: this
            !! The legend object.
        character(len = :), allocatable :: txt
            !! The GNUPLOT command string.

        ! Local Variables
        type(string_builder) :: str

        ! Process
        call str%initialize()

        ! Visible?
        if (.not.this%get_is_visible()) then
            txt = "set key off"
            return
        end if

        ! Inside vs Outside & Position
        if (this%get_draw_inside_axes()) then
            call str%append("set key inside")
        else
            call str%append("set key outside")
        end if
        call str%append(" ")
        call str%append(this%get_vertical_position())
        call str%append(" ")
        call str%append(this%get_horizontal_position())

        ! Border
        call str%append(new_line('a'))
        if (this%get_draw_border()) then
            ! call str%append("set key box opaque")
            call str%append("set key box")
        else
            call str%append("set key nobox")
        end if

        ! Layout
        call str%append(new_line('a'))
        call str%append("set key ")
        call str%append(this%get_layout())

        ! Opaque
        call str%append(new_line('a'))
        call str%append("set key ")
        if (this%get_is_opaque()) then
            call str%append("opaque")
        else
            call str%append("noopaque")
        end if

        ! End
        txt = char(str%to_string())
    end function

! ------------------------------------------------------------------------------
    pure function leg_get_layout(this) result(rst)
        !! Gets the layout of the legend.
        class(legend), intent(in) :: this
            !! The legend object.
        character(len = :), allocatable :: rst
            !! The layout type, either LEGEND_ARRANGE_VERTICALLY or 
            !! LEGEND_ARRANGE_HORIZONTALLY.
        rst = trim(this%m_layout)
    end function

! ---------------------
    subroutine leg_set_layout(this, x)
        !! Sets the layout of the legend.
        class(legend), intent(inout) :: this
            !! The legend object.
        character(len = *), intent(in) :: x
            !! The layout type, either LEGEND_ARRANGE_VERTICALLY or 
            !! LEGEND_ARRANGE_HORIZONTALLY.
        if (x == LEGEND_ARRANGE_HORIZONTALLY .or. &
            x == LEGEND_ARRANGE_VERTICALLY) &
        then
            this%m_layout = x
        end if
    end subroutine

! ------------------------------------------------------------------------------
    pure function leg_get_opaque(this) result(rst)
        !! Gets a value determining if the legend is to be opaque.
        class(legend), intent(in) :: this
            !! The legend object.
        logical :: rst
            !! True if the legend is to be opaque; else, false.
        rst = this%m_opaque
    end function

! ---------------------
    subroutine leg_set_opaque(this, x)
        !! Sets a value determining if the legend is to be opaque.
        class(legend), intent(inout) :: this
            !! The legend object.
        logical :: x
            !! True if the legend is to be opaque; else, false.
        this%m_opaque = x
    end subroutine

! ------------------------------------------------------------------------------
end module
