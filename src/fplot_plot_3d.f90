! fplot_plot_3d.f90

submodule (fplot_core) fplot_plot_3d
contains
! ------------------------------------------------------------------------------
    module subroutine p3d_clean_up(this)
        type(plot_3d), intent(inout) :: this
        call this%free_resources()
        if (associated(this%m_xAxis)) then
            deallocate(this%m_xAxis)
            nullify(this%m_xAxis)
        end if
        if (associated(this%m_yAxis)) then
            deallocate(this%m_yAxis)
            nullify(this%m_yAxis)
        end if
        if (associated(this%m_zAxis)) then
            deallocate(this%m_zAxis)
            nullify(this%m_zAxis)
        end if
    end subroutine

! ------------------------------------------------------------------------------
    module subroutine p3d_init(this, term, fname, err)
        ! Arguments
        class(plot_3d), intent(inout) :: this
        integer(int32), intent(in), optional :: term
        character(len = *), intent(in), optional :: fname
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

        ! Initialize the base class
        call plt_init(this, term, fname, errmgr)
        if (errmgr%has_error_occurred()) return

        ! Process
        flag = 0
        if (.not.associated(this%m_xAxis)) then
            allocate(this%m_xAxis, stat = flag)
        end if
        if (flag == 0 .and. .not.associated(this%m_yAxis)) then
            allocate(this%m_yAxis, stat = flag)
        end if
        if (flag == 0 .and. .not.associated(this%m_zAxis)) then
            allocate(this%m_zAxis, stat = flag)
        end if

        ! Error Checking
        if (flag /= 0) then
            call errmgr%report_error("p3d_init", &
                "Insufficient memory available.", PLOT_OUT_OF_MEMORY_ERROR)
            return
        end if
    end subroutine

! ------------------------------------------------------------------------------
    module function p3d_get_cmd(this) result(x)
        ! Arguments
        class(plot_3d), intent(in) :: this
        character(len = :), allocatable :: x

        ! Local Variables
        type(string_builder) :: str
        integer(int32) :: i, n
        class(plot_data), pointer :: ptr
        class(plot_axis), pointer :: xAxis, yAxis, zAxis
        type(legend), pointer :: leg
        ! class(plot_label), pointer :: lbl

        ! Initialization
        call str%initialize()

        ! Call the base routine
        call str%append(this%plot%get_command_string())

        ! Grid
        if (this%get_show_gridlines()) then
            call str%append(new_line('a'))
            call str%append("set grid")
        end if

        ! Title
        n = len_trim(this%get_title())
        if (n > 0) then
            call str%append(new_line('a'))
            call str%append('set title "')
            call str%append(this%get_title())
            call str%append('"')
        end if

        ! Axes
        call str%append(new_line('a'))
        xAxis => this%get_x_axis()
        if (associated(xAxis)) call str%append(xAxis%get_command_string())

        call str%append(new_line('a'))
        yAxis => this%get_y_axis()
        if (associated(yAxis)) call str%append(yAxis%get_command_string())

        call str%append(new_line('a'))
        zAxis => this%get_z_axis()
        if (associated(zAxis)) call str%append(zAxis%get_command_string())

        ! Tic Marks
        if (.not.this%get_tics_inward()) then
            call str%append(new_line('a'))
            call str%append("set tics out")
        end if
        if (xAxis%get_zero_axis() .or. yAxis%get_zero_axis() .or. &
                zAxis%get_zero_axis()) then
            call str%append(new_line('a'))
            call str%append("set tics axis")
        end if

        ! Border
        if (this%get_draw_border()) then
            n = 31
        else
            n = 0
            if (.not.xAxis%get_zero_axis()) n = n + 1
            if (.not.yAxis%get_zero_axis()) n = n + 4
            if (.not.zAxis%get_zero_axis()) n = n + 16

            call str%append(new_line('a'))
            call str%append("set xtics nomirror")
            call str%append(new_line('a'))
            call str%append("set ytics nomirror")
            call str%append(new_line('a'))
            call str%append("set ztics nomirror")
        end if
        call str%append(new_line('a'))
        if (n > 0) then
            call str%append("set border ")
            call str%append(to_string(n))
        else
            call str%append("unset border")
        end if

        ! Force the z-axis to move to the x-y plane
        if (this%get_z_intersect_xy()) then
            call str%append(new_line('a'))
            call str%append("set ticslevel 0")
        end if

        ! Scaling
        if (this%get_axis_equal()) then
            call str%append(new_line('a'))
            call str%append("set view equal xyz")
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

        ! Orientation
        call str%append(new_line('a'))
        call str%append("set view ")
        if (this%get_use_map_view()) then
            call str%append("map")
        else
            call str%append(to_string(this%get_elevation()))
            call str%append(",")
            call str%append(to_string(this%get_azimuth()))
        end if

        ! Coordinate system
        if (this%get_coordinate_system() == COORDINATES_CYLINDRICAL) then
            call str%append(new_line('a'))
            call str%append("set mapping cylindrical")
        else if (this%get_coordinate_system() == COORDINATES_SPHERICAL) then
            call str%append(new_line('a'))
            call str%append("set mapping spherical")
        end if

        ! Define the plot function and data formatting commands
        n = this%get_count()
        call str%append(new_line('a'))
        call str%append("splot ")
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
            ! if (i /= n) then
            !     call str%append("e")
            ! end if
        end do

        ! End
        x = char(str%to_string())
    end function

! ------------------------------------------------------------------------------
    module function p3d_get_x_axis(this) result(ptr)
        class(plot_3d), intent(in) :: this
        class(plot_axis), pointer :: ptr
        ptr => this%m_xAxis
    end function

! ------------------------------------------------------------------------------
    module function p3d_get_y_axis(this) result(ptr)
        class(plot_3d), intent(in) :: this
        class(plot_axis), pointer :: ptr
        ptr => this%m_yAxis
    end function

! ------------------------------------------------------------------------------
    module function p3d_get_z_axis(this) result(ptr)
        class(plot_3d), intent(in) :: this
        class(plot_axis), pointer :: ptr
        ptr => this%m_zAxis
    end function

! ------------------------------------------------------------------------------
    pure module function p3d_get_elevation(this) result(x)
        class(plot_3d), intent(in) :: this
        real(real64) :: x
        x = this%m_elevation
    end function

! --------------------
    module subroutine p3d_set_elevation(this, x)
        class(plot_3d), intent(inout) :: this
        real(real64), intent(in) :: x
        this%m_elevation = x
    end subroutine

! ------------------------------------------------------------------------------
    pure module function p3d_get_azimuth(this) result(x)
        class(plot_3d), intent(in) :: this
        real(real64) :: x
        x = this%m_azimuth
    end function

! --------------------
    module subroutine p3d_set_azimuth(this, x)
        class(plot_3d), intent(inout) :: this
        real(real64), intent(in) :: x
        this%m_azimuth = x
    end subroutine

! ------------------------------------------------------------------------------
    pure module function p3d_get_z_axis_intersect(this) result(x)
        class(plot_3d), intent(in) :: this
        logical :: x
        x = this%m_zIntersect
    end function

! --------------------
    module subroutine p3d_set_z_axis_intersect(this, x)
        class(plot_3d), intent(inout) :: this
        logical, intent(in) :: x
        this%m_zIntersect = x
    end subroutine

! ADDED March 29, 2023 - JAC
! ------------------------------------------------------------------------------
    pure module function p3d_get_use_map_view(this) result(rst)
        class(plot_3d), intent(in) :: this
        logical :: rst
        rst = this%m_setMap
    end function

! --------------------
    module subroutine p3d_set_use_map_view(this, x)
        class(plot_3d), intent(inout) :: this
        logical, intent(in) :: x
        this%m_setMap = x
    end subroutine

! ADDED Sept. 15, 2023 - JAC
! ------------------------------------------------------------------------------
    pure module function p3d_get_csys(this) result(rst)
        class(plot_3d), intent(in) :: this
        integer(int32) :: rst
        rst = this%m_csys
    end function

! --------------------
    module subroutine p3d_set_csys(this, x)
        class(plot_3d), intent(inout) :: this
        integer(int32), intent(in) :: x
        if (x /= COORDINATES_CARTESIAN .and. &
            x /= COORDINATES_CYLINDRICAL .and. &
            x /= COORDINATES_SPHERICAL) &
        then
            ! Set to default as the input is nonsensical
            this%m_csys = COORDINATES_CARTESIAN
        else
            this%m_csys = x
        end if
    end subroutine

! ------------------------------------------------------------------------------
end submodule
