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

        ! Process
        call str%append("set palette defined (")
        call str%append(this%get_color_string())
        call str%append(")")

        if (len(this%get_label()) > 0) then
            call str%append(new_line('a'))
            call str%append('set cblabel "')
            call str%append(this%get_label())
            call str%append('"')
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
end submodule
