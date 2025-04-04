! fplot_colormap.f90

module fplot_colormap
    use iso_fortran_env
    use fplot_plot_object
    use strings
    use ferror
    use fplot_errors
    use fplot_colors
    use forcolormap, cmap => Colormap ! avoid conflict with the internally defined colormap type
    implicit none
    private
    public :: cmap
    public :: colormap
    public :: cm_get_string_result
    public :: rainbow_colormap
    public :: hot_colormap
    public :: cool_colormap
    public :: parula_colormap
    public :: grey_colormap
    public :: earth_colormap
    public :: custom_colormap

    type, abstract, extends(plot_object) :: colormap
        !! A colormap object for a surface plot.
        character(len = :), private, allocatable :: m_label
            !! The label to associate with the colormap.
        logical, private :: m_horizontal = .false.
            !! The colormap should be drawn horizontally.
        logical, private :: m_drawBorder = .true.
            !! Draw the colormap border.
        logical, private :: m_showTics = .true.
            !! Show the tic marks.
    contains
        procedure, public :: get_command_string => cm_get_cmd
        procedure(cm_get_string_result), deferred, public :: get_color_string
        procedure, public :: get_label => cm_get_label
        procedure, public :: set_label => cm_set_label
        procedure, public :: get_horizontal => cm_get_horizontal
        procedure, public :: set_horizontal => cm_set_horizontal
        procedure, public :: get_draw_border => cm_get_draw_border
        procedure, public :: set_draw_border => cm_set_draw_border
        procedure, public :: get_show_tics => cm_get_show_tics
        procedure, public :: set_show_tics => cm_set_show_tics
    end type

    interface
        function cm_get_string_result(this) result(x)
            !! Retrieves a string result from a colormap object.
            import colormap
            class(colormap), intent(in) :: this
                !! The colormap object.
            character(len = :), allocatable :: x
                !! The string.
        end function
    end interface
! ------------------------------------------------------------------------------
    type, extends(colormap) :: rainbow_colormap
        !! Defines a rainbow colormap.
    contains
        procedure, public :: get_color_string => rcm_get_clr
    end type

! ------------------------------------------------------------------------------
    type, extends(colormap) :: hot_colormap
        !! Defines a colormap consisting of "hot" colors.
    contains
        procedure, public :: get_color_string => hcm_get_clr
    end type

! ------------------------------------------------------------------------------
    type, extends(colormap) :: cool_colormap
        !! Defines a colormap consisting of "cool" colors.
    contains
        procedure, public :: get_color_string => ccm_get_clr
    end type

! ------------------------------------------------------------------------------
    type, extends(colormap) :: parula_colormap
        !! Defines a colormap equivalent to the MATLAB parula colormap.
    contains
        procedure, public :: get_color_string => pcm_get_clr
    end type

! ------------------------------------------------------------------------------
    type, extends(colormap) :: grey_colormap
        !! Defines a grey-scaled colormap.
    contains
        procedure, public :: get_color_string => gcm_get_clr
    end type

! ------------------------------------------------------------------------------
    type, extends(colormap) :: earth_colormap
        !! Defines an earthy-colored colormap.
    contains
        procedure, public :: get_color_string => ecm_get_clr
    end type

! ------------------------------------------------------------------------------
    type, extends(colormap) :: custom_colormap
        !! Defines a custom colormap that utilizes the FORCOLORMAP library
        !! to provide the map.
        class(cmap), private, pointer :: m_map => null()
            !! The FORCOLORMAP object.
    contains
        final :: custom_final
        procedure, public :: get_color_string => custom_get_clr
        procedure, public :: set_colormap => custom_set
        procedure, public :: get_colormap => custom_get
    end type

! ------------------------------------------------------------------------------
contains
! ******************************************************************************
! COLORMAP MEMBERS
! ------------------------------------------------------------------------------
    function cm_get_cmd(this) result(x)
        !! Gets the GNUPLOT command string to represent this colormap object.
        class(colormap), intent(in) :: this
            !! The colormap object.
        character(len = :), allocatable :: x
            !! The command string.

        ! Local Variables
        type(string_builder) :: str

        ! Initialization
        call str%initialize()

        ! Palette Definition
        call str%append("set palette defined (")
        call str%append(this%get_color_string())
        call str%append(")")

        if (len(this%get_label()) > 0) then
            call str%append(new_line('a'))
            call str%append('set cblabel "')
            call str%append(this%get_label())
            call str%append('"')
        end if

        ! Orientation
        if (this%get_horizontal()) then
            call str%append(new_line('a'))
            call str%append("set colorbox horizontal")
            call str%append(new_line('a'))
            call str%append("set size 0.8,0.8; set origin 0.1,0.2")
            call str%append(new_line('a'))
            call str%append("set colorbox user origin 0.1,0.175 size 0.8,0.055")

            if (len(this%get_label()) > 0) then
                call str%append(new_line('a'))
                call str%append("set cblabel offset 0,0.8")
            end if
        end if

        ! Border & Tic Marks
        if (.not.this%get_draw_border()) then
            ! Eliminate the border
            call str%append(new_line('a'))
            call str%append("set colorbox noborder")

            ! Hide the tic marks
            call str%append(new_line('a'))
            call str%append("set cbtic scale 0")
        else
            ! Respect the tic mark visibility setting if the border is shown
            if (.not.this%get_show_tics()) then
                call str%append(new_line('a'))
                call str%append("set cbtic scale 0")
            end if
        end if

        ! End
        x = char(str%to_string())
    end function

! ------------------------------------------------------------------------------
    pure function cm_get_label(this) result(rst)
        !! Gets the label to associate with the colorbar.
        class(colormap), intent(in) :: this
            !! The colormap object.
        character(len = :), allocatable :: rst
            !! The label.
        if (allocated(this%m_label)) then
            rst = this%m_label
        else
            rst = ""
        end if
    end function

! --------------------
    subroutine cm_set_label(this, x)
        !! Sets the label to associate with the colorbar.
        class(colormap), intent(inout) :: this
            !! The colormap object.
        character(len = *), intent(in) :: x
            !! The label.
        this%m_label = x
    end subroutine

! ------------------------------------------------------------------------------
    pure function cm_get_horizontal(this) result(rst)
        !! Gets a logical value determining if the colormap should be
        !! drawn horizontally and below the plot.
        class(colormap), intent(in) :: this
            !! The colormap object.
        logical :: rst
            !! Returns true if the colormap should be drawn horizontally;
            !! else, false.
        rst = this%m_horizontal
    end function

! --------------------
    subroutine cm_set_horizontal(this, x)
        !! Sets a logical value determining if the colormap should be
        !! drawn horizontally and below the plot.
        class(colormap), intent(inout) :: this
            !! The colormap object.
        logical, intent(in) :: x
            !! Set to true if the colormap should be drawn horizontally;
            !! else, false.
        this%m_horizontal = x
    end subroutine

! ------------------------------------------------------------------------------
    pure function cm_get_draw_border(this) result(rst)
        !! Gets a logical value determining if the border should be drawn.
        class(colormap), intent(in) :: this
            !! The colormap object.
        logical :: rst
            !! Returns true if the border should be drawn; else, false.
        rst = this%m_drawBorder
    end function

! --------------------
    subroutine cm_set_draw_border(this, x)
        !! Sets a logical value determining if the border should be drawn.
        class(colormap), intent(inout) :: this
            !! The colormap object.
        logical, intent(in) :: x
            !! Set to true if the border should be drawn; else, false.
        this%m_drawBorder = x
    end subroutine

! ------------------------------------------------------------------------------
    pure function cm_get_show_tics(this) result(rst)
        !! Gets a logical value determining if the tic marks should be drawn.
        class(colormap), intent(in) :: this
            !! The colormap object.
        logical :: rst
            !! Returns true if the tic marks should be drawn; else, false.
        rst = this%m_showTics
    end function

! --------------------
    subroutine cm_set_show_tics(this, x)
        !! Sets a logical value determining if the tic marks should be drawn.
        class(colormap), intent(inout) :: this
            !! The colormap object.
        logical, intent(in) :: x
            !! Set to true if the tic marks should be drawn; else, false.
        this%m_showTics = x
    end subroutine

! ------------------------------------------------------------------------------
! TO DO:
!   - Set user-defined tic labels & limits (ref: http://gnuplot.sourceforge.net/demo_5.4/cerf.html)

! ******************************************************************************
! RAINBOW_COLORMAP MEMBERS
! ------------------------------------------------------------------------------
    function rcm_get_clr(this) result(x)
        !! Gets the GNUPLOT string defining the color distribution.
        class(rainbow_colormap), intent(in) :: this
            !! The rainbow_colormap object.
        character(len = :), allocatable :: x
            !! The command string.
        x = '0 "dark-blue", 1 "blue", 2 "cyan", 3 "green", 4 "yellow", ' // &
            '5 "orange", 6 "red", 7 "dark-red"'
    end function

! ******************************************************************************
! HOT_COLORMAP MEMBERS
! ------------------------------------------------------------------------------
    function hcm_get_clr(this) result(x)
        !! Gets the GNUPLOT string defining the color distribution.
        class(hot_colormap), intent(in) :: this
            !! The hot_colormap object.
        character(len = :), allocatable :: x
            !! The command string.
        x = '0 "black", 1 "red", 2 "orange", 3 "yellow", 4 "white"'
    end function

! ******************************************************************************
! COOL_COLORMAP MEMBERS
! ------------------------------------------------------------------------------
    function ccm_get_clr(this) result(x)
        !! Gets the GNUPLOT string defining the color distribution.
        class(cool_colormap), intent(in) :: this
            !! The cool_colormap object.
        character(len = :), allocatable :: x
            !! The command string.

        type(string_builder) :: str

        call str%append("0 '#08589E',")
        call str%append("1 '#2B8CBE',")
        call str%append("2 '#4EB3D3',")
        call str%append("3 '#7BCCC4',")
        call str%append("4 '#A8DDB5',")
        call str%append("5 '#CCEBC5',")
        call str%append("6 '#E0F3DB',")
        call str%append("7 '#F7FCF0'")

        x = char(str%to_string())
        ! x = '0 "blue", 1 "turquoise", 2 "light-green"'
    end function

! ******************************************************************************
! PARULA_COLORMAP MEMBERS
! ------------------------------------------------------------------------------
    function pcm_get_clr(this) result(x)
        !! Gets the GNUPLOT string defining the color distribution.
        class(parula_colormap), intent(in) :: this
            !! The parula_colormap object.
        character(len = :), allocatable :: x
            !! The command string.
        
        type(string_builder) :: str

        call str%append("0 '#352a87',")
        call str%append("1 '#0363e1',")
        call str%append("2 '#1485d4',")
        call str%append("3 '#06a7c6',")
        call str%append("4 '#38b99e',")
        call str%append("5 '#92bf73',")
        call str%append("6 '#d9ba56',")
        call str%append("7 '#fcce2e',")
        call str%append("8 '#f9fb0e'")

        x = char(str%to_string())
    end function

! ******************************************************************************
! GREY_COLORMAP MEMBERS
! ------------------------------------------------------------------------------
    function gcm_get_clr(this) result(x)
        !! Gets the GNUPLOT string defining the color distribution.
        class(grey_colormap), intent(in) :: this
            !! The grey_colormap object.
        character(len = :), allocatable :: x
            !! The command string.

        type(string_builder) :: str

        call str%append("0 '#FFFFFF',")
        call str%append("1 '#F0F0F0',")
        call str%append("2 '#D9D9D9',")
        call str%append("3 '#BDBDBD',")
        call str%append("4 '#969696',")
        call str%append("5 '#737373',")
        call str%append("6 '#525252',")
        call str%append("7 '#252525'")

        x = char(str%to_string())
    end function

! ******************************************************************************
! EARTH_COLORMAP
! ------------------------------------------------------------------------------
    function ecm_get_clr(this) result(x)
        !! Gets the GNUPLOT string defining the color distribution.
        class(earth_colormap), intent(in) :: this
            !! The earth_colormap object.
        character(len = :), allocatable :: x
            !! The command string.

        type(string_builder) :: str

        call str%append("0 '#8C510A',")
        call str%append("1 '#BF812D',")
        call str%append("2 '#DFC27D',")
        call str%append("3 '#F6E8C3',")
        call str%append("4 '#D9F0D3',")
        call str%append("5 '#A6DBA0',")
        call str%append("6 '#5AAE61',")
        call str%append("7 '#1B7837'")

        x = char(str%to_string())
    end function

! ------------------------------------------------------------------------------
! Additional Color Maps:
! https://github.com/Gnuplotting/gnuplot-palettes

! ******************************************************************************
! ADDED: Jan. 08, 2024 - JAC
! CUSTOM_COLORMAP
! ------------------------------------------------------------------------------
    function custom_get_clr(this) result(x)
        !! Gets the GNUPLOT string defining the color distribution.
        class(custom_colormap), intent(in) :: this
            !! The custom_colormap object.
        character(len = :), allocatable :: x
            !! The command string.

        type(string_builder) :: str
        integer(int32) :: i, n, r, g, b, c
        character(len = 6) :: ctxt

        if (.not.associated(this%m_map)) then
            allocate(character(len = 0) :: x)
            return
        end if

        n = this%m_map%get_levels()
        do i = 0, n - 1
            ! Get the RGB triple
            call this%m_map%get_RGB(i, r, g, b)
            c = ishft(r, 16) + ishft(g, 8) + b
            write(ctxt, '(Z6.6)') c

            ! Append the color information
            call str%append(to_string(i))
            call str%append(" '#")
            call str%append(ctxt)
            call str%append("'")
            if (i /= n - 1) then
                call str%append(",")
            end if
        end do

        x = char(str%to_string())
    end function

! ------------------------------------------------------------------------------
    subroutine custom_set(this, map, err)
        !! Sets the FORCOLORMAP colormap object.
        class(custom_colormap), intent(inout) :: this
            !! The custom_colormap object.
        class(cmap), intent(in) :: map
            !! The FORCOLORMAP colormap object.  The custom_colormap object 
            !! stores a copy of this object; therefore, any changes made to 
            !! x after calls to this routine will not impact the behavior of 
            !! the custom_colormap object.
        class(errors), intent(inout), optional, target :: err
            !! An error handling object.

        ! Local Variables
        integer(int32) :: flag
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        
        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Process
        if (associated(this%m_map)) deallocate(this%m_map)
        allocate(this%m_map, source = map, stat = flag)
        if (flag /= 0) then
            call report_memory_error(errmgr, "custom_set", flag)
            return
        end if
    end subroutine

! ------------------------------------------------------------------------------
    function custom_get(this) result(rst)
        !! Gets a pointer to the FORCOLORMAP colormap object.
        class(custom_colormap), intent(in) :: this
            !! The custom_colormap object.
        class(cmap), pointer :: rst
            !! A pointer to the FORCOLORMAP colormap object.
        rst => this%m_map
    end function

! ------------------------------------------------------------------------------
    subroutine custom_final(this)
        type(custom_colormap), intent(inout) :: this
            !! The custom_colormap object.
        if (associated(this%m_map)) then
            deallocate(this%m_map)
            nullify(this%m_map)
        end if
    end subroutine

! ------------------------------------------------------------------------------
end module
