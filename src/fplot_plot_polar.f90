! fplot_polar.f90

submodule (fplot_core) fplot_plot_polar
contains
! ------------------------------------------------------------------------------
    module subroutine plr_clean_up(this)
        type(plot_polar), intent(inout) :: this
        call this%free_resources()
    end subroutine

! ------------------------------------------------------------------------------
    module subroutine plr_init(this, term, fname, err)
        ! Arguments
        class(plot_polar), intent(inout) :: this
        integer(int32), intent(in), optional :: term
        character(len = *), intent(in), optional :: fname
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Initialize the base class
        call plt_init(this, term, fname, errmgr)
        if (errmgr%has_error_occurred()) return

        ! Initialize the rest of the object
        this%m_thetaStart = POLAR_THETA_RIGHT
        this%m_thetaDirection = POLAR_THETA_CCW
    end subroutine

! ------------------------------------------------------------------------------
    module function plr_get_cmd(this) result(x)
        ! Arguments
        class(plot_polar), intent(in) :: this
        character(len = :), allocatable :: x

        ! Local Variables
        integer(int32) :: i, n
        type(string_builder) :: str
        type(legend), pointer :: leg
        real(real64) :: lim(2)
        ! class(plot_label), pointer :: lbl
        class(plot_data), pointer :: ptr

        ! Initialization
        call str%initialize()

        ! Call the base routine
        call str%append(this%plot%get_command_string())

        ! Polar-Specific Settings
        call str%append(new_line('a'))
        call str%append("unset border")

        call str%append(new_line('a'))
        call str%append("set polar")

        call str%append(new_line('a'))
        call str%append("set size square")

        call str%append(new_line('a'))
        call str%append("unset xtics")
        call str%append(new_line('a'))
        call str%append("unset ytics")

        call str%append(new_line('a'))
        call str%append('set ttics 0, 30 format "%g".GPVAL_DEGREE_SIGN')

        call str%append(new_line('a'))
        call str%append("set mttics 3")

        call str%append(new_line('a'))
        call str%append("set theta ")
        call str%append(this%get_theta_start_position())
        call str%append(" ")
        call str%append(this%get_theta_direction())

        ! Radial Limits
        if (.not.this%get_autoscale()) then
            lim = this%get_radial_limits()
            call str%append(new_line('a'))
            call str%append("set rrange [")
            call str%append(to_string(lim(1)))
            call str%append(":")
            call str%append(to_string(lim(2)))
            call str%append("]")
        end if

        ! Grid
        if (this%get_show_gridlines()) then
            call str%append(new_line('a'))
            call str%append("set grid r polar")
        end if

        ! Title
        n = len_trim(this%get_title())
        if (n > 0) then
            call str%append(new_line('a'))
            call str%append('set title "')
            call str%append(this%get_title())
            call str%append('"')
        end if

        ! Border
        call str%append(new_line('a'))
        if (this%get_draw_border()) then
            call str%append("set border polar")
        else
            call str%append("set border 0")
        end if

        ! Legend
        call str%append(new_line('a'))
        leg => this%get_legend()
        if (associated(leg)) call str%append(leg%get_command_string())

        ! ! Labels
        ! do i = 1, this%get_label_count()
        !     lbl => this%get_label(i)
        !     if (.not.associated(lbl)) cycle
        !     call str%append(new_line('a'))
        !     call str%append(lbl%get_command_string())
        ! end do

        ! Define the plot function and data formatting commands
        n = this%get_count()
        call str%append(new_line('a'))
        call str%append("plot ")
        do i = 1, n
            ptr => this%get(i)
            if (.not.associated(ptr)) cycle
            call str%append(ptr%get_command_string())
            if (i /= n) call str%append(", ")
        end do

        ! Define the data to plot
        do i = 1, n
            ptr => this%get(i)
            if (.not.associated(ptr)) cycle
            call str%append(new_line('a'))
            call str%append(ptr%get_data_string())
            call str%append("e")
        end do

        ! End
        x = char(str%to_string())
    end function

! ------------------------------------------------------------------------------
    pure module function plr_get_autoscale(this) result(rst)
        class(plot_polar), intent(in) :: this
        logical :: rst
        rst = this%m_autoscale
    end function

! --------------------
    module subroutine plr_set_autoscale(this, x)
        class(plot_polar), intent(inout) :: this
        logical, intent(in) :: x
        this%m_autoscale = x
    end subroutine

! ------------------------------------------------------------------------------
    pure module function plr_get_limits(this) result(rst)
        class(plot_polar), intent(in) :: this
        real(real64) :: rst(2)
        rst = [this%m_minrad, this%m_maxrad]
    end function

! --------------------
    module subroutine plr_set_limits(this, x)
        class(plot_polar), intent(inout) :: this
        real(real64), intent(in) :: x(2)
        this%m_minrad = minval(x)
        this%m_maxrad = maxval(x)
    end subroutine

! ------------------------------------------------------------------------------
    pure module function plr_get_theta_start(this) result(rst)
        class(plot_polar), intent(in) :: this
        character(len = :), allocatable :: rst
        rst = this%m_thetaStart
    end function

! --------------------
    module subroutine plr_set_theta_start(this, x)
        class(plot_polar), intent(inout) :: this
        character(len = *), intent(in) :: x
        if (x /= POLAR_THETA_BOTTOM .and. &
            x /= POLAR_THETA_TOP .and. &
            x /= POLAR_THETA_LEFT .and. &
            x /= POLAR_THETA_RIGHT) &
        then
            ! Reset to default
            this%m_thetaStart = POLAR_THETA_RIGHT
        else
            this%m_thetaStart = x
        end if
    end subroutine

! ------------------------------------------------------------------------------
    pure module function plr_get_theta_direction(this) result(rst)
        class(plot_polar), intent(in) :: this
        character(len = :), allocatable :: rst
        rst = this%m_thetaDirection
    end function

! --------------------
    module subroutine plr_set_theta_direction(this, x)
        class(plot_polar), intent(inout) :: this
        character(len = *), intent(in) :: x
        if (x /= POLAR_THETA_CCW .and. x /= POLAR_THETA_CW) then
            ! Reset to default
            this%m_thetaDirection = POLAR_THETA_CCW
        else
            this%m_thetaDirection = x
        end if
    end subroutine

! ------------------------------------------------------------------------------

! --------------------

! ------------------------------------------------------------------------------
end submodule
