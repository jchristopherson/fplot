! fplot_colormap.f90

submodule (fplot_core) fplot_colormap
contains
! ******************************************************************************
! COLORMAP MEMBERS
! ------------------------------------------------------------------------------
    module function cm_get_cmd(this) result(x)
        ! Arguments
        class(colormap), intent(in) :: this
        character(len = :), allocatable :: x

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
        x = str%to_string()
    end function

! ------------------------------------------------------------------------------
    pure module function cm_get_label(this) result(rst)
        class(colormap), intent(in) :: this
        character(len = :), allocatable :: rst
        if (allocated(this%m_label)) then
            rst = this%m_label
        else
            rst = ""
        end if
    end function

! --------------------
    module subroutine cm_set_label(this, x)
        class(colormap), intent(inout) :: this
        character(len = *), intent(in) :: x
        this%m_label = x
    end subroutine

! ------------------------------------------------------------------------------
    pure module function cm_get_horizontal(this) result(rst)
        class(colormap), intent(in) :: this
        logical :: rst
        rst = this%m_horizontal
    end function

! --------------------
    module subroutine cm_set_horizontal(this, x)
        class(colormap), intent(inout) :: this
        logical, intent(in) :: x
        this%m_horizontal = x
    end subroutine

! ------------------------------------------------------------------------------
    pure module function cm_get_draw_border(this) result(rst)
        class(colormap), intent(in) :: this
        logical :: rst
        rst = this%m_drawBorder
    end function

! --------------------
    module subroutine cm_set_draw_border(this, x)
        class(colormap), intent(inout) :: this
        logical, intent(in) :: x
        this%m_drawBorder = x
    end subroutine

! ------------------------------------------------------------------------------
    pure module function cm_get_show_tics(this) result(rst)
        class(colormap), intent(in) :: this
        logical :: rst
        rst = this%m_showTics
    end function

! --------------------
    module subroutine cm_set_show_tics(this, x)
        class(colormap), intent(inout) :: this
        logical, intent(in) :: x
        this%m_showTics = x
    end subroutine

! ------------------------------------------------------------------------------
! TO DO:
!   - Set user-defined tic labels & limits (ref: http://gnuplot.sourceforge.net/demo_5.4/cerf.html)

! ******************************************************************************
! RAINBOW_COLORMAP MEMBERS
! ------------------------------------------------------------------------------
    module function rcm_get_clr(this) result(x)
        class(rainbow_colormap), intent(in) :: this
        character(len = :), allocatable :: x
        x = '0 "dark-blue", 1 "blue", 2 "cyan", 3 "green", 4 "yellow", ' // &
            '5 "orange", 6 "red", 7 "dark-red"'
    end function

! ******************************************************************************
! HOT_COLORMAP MEMBERS
! ------------------------------------------------------------------------------
    module function hcm_get_clr(this) result(x)
        class(hot_colormap), intent(in) :: this
        character(len = :), allocatable :: x
        x = '0 "black", 1 "red", 2 "orange", 3 "yellow", 4 "white"'
    end function

! ******************************************************************************
! COOL_COLORMAP MEMBERS
! ------------------------------------------------------------------------------
    module function ccm_get_clr(this) result(x)
        class(cool_colormap), intent(in) :: this
        character(len = :), allocatable :: x

        type(string_builder) :: str

        call str%append("0 '#08589E',")
        call str%append("1 '#2B8CBE',")
        call str%append("2 '#4EB3D3',")
        call str%append("3 '#7BCCC4',")
        call str%append("4 '#A8DDB5',")
        call str%append("5 '#CCEBC5',")
        call str%append("6 '#E0F3DB',")
        call str%append("7 '#F7FCF0'")

        x = str%to_string()
        ! x = '0 "blue", 1 "turquoise", 2 "light-green"'
    end function

! ******************************************************************************
! PARULA_COLORMAP MEMBERS
! ------------------------------------------------------------------------------
    module function pcm_get_clr(this) result(x)
        class(parula_colormap), intent(in) :: this
        character(len = :), allocatable :: x
        
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

        x = str%to_string()
    end function

! ******************************************************************************
! GREY_COLORMAP MEMBERS
! ------------------------------------------------------------------------------
    module function gcm_get_clr(this) result(x)
        class(grey_colormap), intent(in) :: this
        character(len = :), allocatable :: x

        type(string_builder) :: str

        call str%append("0 '#FFFFFF',")
        call str%append("1 '#F0F0F0',")
        call str%append("2 '#D9D9D9',")
        call str%append("3 '#BDBDBD',")
        call str%append("4 '#969696',")
        call str%append("5 '#737373',")
        call str%append("6 '#525252',")
        call str%append("7 '#252525'")

        x = str%to_string()
    end function

! ******************************************************************************
! EARTH_COLORMAP
! ------------------------------------------------------------------------------
    module function ecm_get_clr(this) result(x)
        class(earth_colormap), intent(in) :: this
        character(len = :), allocatable :: x

        type(string_builder) :: str

        call str%append("0 '#8C510A',")
        call str%append("1 '#BF812D',")
        call str%append("2 '#DFC27D',")
        call str%append("3 '#F6E8C3',")
        call str%append("4 '#D9F0D3',")
        call str%append("5 '#A6DBA0',")
        call str%append("6 '#5AAE61',")
        call str%append("7 '#1B7837'")

        x = str%to_string()
    end function

! ------------------------------------------------------------------------------
! Additional Color Maps:
! https://github.com/Gnuplotting/gnuplot-palettes

! ******************************************************************************
! ADDED: Jan. 08, 2024 - JAC
! ------------------------------------------------------------------------------
    module function custom_get_clr(this) result(x)
        class(custom_colormap), intent(in) :: this
        character(len = :), allocatable :: x

        type(string_builder) :: str
        integer(int32) :: i, n, r, g, b
        type(color) :: clr

        if (.not.associated(this%m_map)) then
            allocate(character(len = 0) :: x)
            return
        end if

        n = this%m_map%get_levels()
        do i = 0, n - 1
            ! Get the RGB triple
            call this%m_map%get_RGB(i, clr%red, clr%green, clr%blue)

            ! Append the color information
            call str%append(to_string(i))
            call str%append(" '#")
            call str%append(clr%to_hex_string())
            call str%append("'")
            if (i /= n - 1) then
                call str%append(",")
            end if
        end do

        x = str%to_string()
    end function

! ------------------------------------------------------------------------------
    module subroutine custom_set(this, map, err)
        ! Arguments
        class(custom_colormap), intent(inout) :: this
        class(cmap), intent(in) :: map
        class(errors), intent(inout), optional, target :: err

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
            call errmgr%report_error("custom_init", &
                "Memory allocation error code " // char(to_string(flag)) // ".", &
                PLOT_OUT_OF_MEMORY_ERROR)
            return
        end if
    end subroutine

! ------------------------------------------------------------------------------
    module function custom_get(this) result(rst)
        class(custom_colormap), intent(in) :: this
        class(cmap), pointer :: rst
        rst => this%m_map
    end function

! ------------------------------------------------------------------------------
    module subroutine custom_final(this)
        type(custom_colormap), intent(inout) :: this
        if (associated(this%m_map)) then
            deallocate(this%m_map)
            nullify(this%m_map)
        end if
    end subroutine

! ------------------------------------------------------------------------------
end submodule
