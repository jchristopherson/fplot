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

        ! Initialize axis objects
        flag = 0

        ! Error Checking
        if (flag /= 0) then
            call errmgr%report_error("p2d_init", &
                "Insufficient memory available.", PLOT_OUT_OF_MEMORY_ERROR)
            return
        end if
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
        class(plot_label), pointer :: lbl
        class(plot_data), pointer :: ptr

        ! Initialization
        call str%initialize()

        ! Call the base routine
        call str%append(this%plot%get_command_string())

        ! Polar-Specific Settings
        call str%append(new_line('a'))
        call str%append("set polar")
        call str%append(new_line('a'))
        call str%append("set size square")
        call str%append(new_line('a'))
        call str%append("unset xtics")
        call str%append(new_line('a'))
        call str%append("unset ytics")
        call str%append(new_line('a'))
        call str%append("set ttics 0, 30")
        call str%append(new_line('a'))
        call str%append("set mttics 3")

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

        ! Labels
        do i = 1, this%get_label_count()
            lbl => this%get_label(i)
            if (.not.associated(lbl)) cycle
            call str%append(new_line('a'))
            call str%append(lbl%get_command_string())
        end do

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
            ! if (i /= n) then
            !     call str%append("e")
            ! end if
        end do

        ! End
        x = str%to_string()
    end function

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------
end submodule
