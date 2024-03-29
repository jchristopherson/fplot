! fplot_plot_data_3d.f90

submodule (fplot_core) fplot_plot_data_3d
contains
! ------------------------------------------------------------------------------
    pure module function pd3d_get_data_count(this) result(x)
        class(plot_data_3d), intent(in) :: this
        integer(int32) :: x
        if (allocated(this%m_data)) then
            x = size(this%m_data, 1)
        else
            x = 0
        end if
    end function

! ------------------------------------------------------------------------------
    pure module function pd3d_get_x_data(this, index) result(x)
        class(plot_data_3d), intent(in) :: this
        integer(int32), intent(in) :: index
        real(real64) :: x
        if (allocated(this%m_data)) then
            x = this%m_data(index, 1)
        else
            x = 0.0d0
        end if
    end function

! --------------------
    module subroutine pd3d_set_x_data(this, index, x)
        class(plot_data_3d), intent(inout) :: this
        integer(int32), intent(in) :: index
        real(real64), intent(in) :: x
        if (allocated(this%m_data)) then
            this%m_data(index, 1) = x
        end if
    end subroutine

! ------------------------------------------------------------------------------
    pure module function pd3d_get_y_data(this, index) result(x)
        class(plot_data_3d), intent(in) :: this
        integer(int32), intent(in) :: index
        real(real64) :: x
        if (allocated(this%m_data)) then
            x = this%m_data(index, 2)
        else
            x = 0.0d0
        end if
    end function

! --------------------
    module subroutine pd3d_set_y_data(this, index, x)
        class(plot_data_3d), intent(inout) :: this
        integer(int32), intent(in) :: index
        real(real64), intent(in) :: x
        if (allocated(this%m_data)) then
            this%m_data(index, 2) = x
        end if
    end subroutine

! ------------------------------------------------------------------------------
    pure module function pd3d_get_z_data(this, index) result(x)
        class(plot_data_3d), intent(in) :: this
        integer(int32), intent(in) :: index
        real(real64) :: x
        if (allocated(this%m_data)) then
            x = this%m_data(index, 3)
        else
            x = 0.0d0
        end if
    end function

! --------------------
    module subroutine pd3d_set_z_data(this, index, x)
        class(plot_data_3d), intent(inout) :: this
        integer(int32), intent(in) :: index
        real(real64), intent(in) :: x
        if (allocated(this%m_data)) then
            this%m_data(index, 3) = x
        end if
    end subroutine

! ------------------------------------------------------------------------------
    module function pd3d_get_axes_cmd(this) result(x)
        ! Arguments
        class(plot_data_3d), intent(in) :: this
        character(len = :), allocatable :: x

        ! Output
        x = ""
    end function

! ------------------------------------------------------------------------------
    module function pd3d_get_data_cmd(this) result(x)
        ! Arguments
        class(plot_data_3d), intent(in) :: this
        character(len = :), allocatable :: x

        ! Local Variables
        type(string_builder) :: str
        integer(int32) :: i
        character :: delimiter, nl
        real(real64), allocatable, dimension(:) :: xv, yv, zv, cv, ps
        real(real64), allocatable, dimension(:,:) :: pts
        real(real64) :: tol, maxz, minz, eps
        logical :: usecolors, usevarpoints

        ! Initialization
        call str%initialize()
        delimiter = achar(9) ! tab delimiter
        nl = new_line(nl)
        usecolors = this%get_use_data_dependent_colors()
        usevarpoints = this%get_use_variable_size_points()

        ! Process
        xv = this%get_x_data()
        yv = this%get_y_data()
        zv = this%get_z_data()
        if (usecolors .and. usevarpoints) then
            cv = this%get_color_data()
            ps = this%get_point_size_data()
            do i = 1, size(xv)
                call str%append(to_string(xv(i)))
                call str%append(delimiter)
                call str%append(to_string(yv(i)))
                call str%append(delimiter)
                call str%append(to_string(zv(i)))
                call str%append(delimiter)
                call str%append(to_string(ps(i)))
                call str%append(delimiter)
                call str%append(to_string(cv(i)))
                call str%append(nl)
            end do
        else if (usecolors .and. .not.usevarpoints) then
            cv = this%get_color_data()
            do i = 1, size(xv)
                call str%append(to_string(xv(i)))
                call str%append(delimiter)
                call str%append(to_string(yv(i)))
                call str%append(delimiter)
                call str%append(to_string(zv(i)))
                call str%append(delimiter)
                call str%append(to_string(cv(i)))
                call str%append(nl)
            end do
        else if (.not.usecolors .and. usevarpoints) then
            ps = this%get_point_size_data()
            do i = 1, size(xv)
                call str%append(to_string(xv(i)))
                call str%append(delimiter)
                call str%append(to_string(yv(i)))
                call str%append(delimiter)
                call str%append(to_string(zv(i)))
                call str%append(delimiter)
                call str%append(to_string(ps(i)))
                call str%append(nl)
            end do
        else
            if (this%get_simplify_data()) then
                maxz = maxval(zv)
                minz = minval(zv)
                tol = abs(this%get_simplification_factor() * (maxz - minz))
                eps = 10.0d0 * epsilon(eps)
                if (tol < eps) tol = eps
                pts = simplify_polyline(xv, yv, zv, tol)
                do i = 1, size(pts, 1)
                    call str%append(to_string(pts(i,1)))
                    call str%append(delimiter)
                    call str%append(to_string(pts(i,2)))
                    call str%append(delimiter)
                    call str%append(to_string(pts(i,3)))
                    call str%append(nl)
                end do
            else
                do i = 1, size(xv)
                    call str%append(to_string(xv(i)))
                    call str%append(delimiter)
                    call str%append(to_string(yv(i)))
                    call str%append(delimiter)
                    call str%append(to_string(zv(i)))
                    call str%append(nl)
                end do
            end if
        end if

        ! End
        x = char(str%to_string())
    end function

! ------------------------------------------------------------------------------
    module subroutine pd3d_set_data_1(this, x, y, z, c, ps, err)
        ! Arguments
        class(plot_data_3d), intent(inout) :: this
        real(real64), intent(in), dimension(:) :: x, y, z
        real(real64), intent(in), dimension(:), optional :: c, ps
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: i, n, flag, ncols
        class(errors), pointer :: errmgr
        type(errors), target :: deferr

        ! Initialization
        n = size(x)
        ncols = 3
        if (present(c)) ncols = ncols + 1
        if (present(ps)) ncols = ncols + 1
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Input Check
        if (size(y) /= n .or. size(z) /= n) then
            call errmgr%report_error("pd3d_set_data_1", &
                "The input arrays are not the same size.", &
                PLOT_ARRAY_SIZE_MISMATCH_ERROR)
            return
        end if
        if (present(c)) then
            if (size(c) /= n) then
                call errmgr%report_error("pd3d_set_data_1", &
                    "The input arrays are not the same size.", &
                    PLOT_ARRAY_SIZE_MISMATCH_ERROR)
                return
            end if
        end if

        ! Process
        if (allocated(this%m_data)) deallocate(this%m_data)
        allocate(this%m_data(n, ncols), stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("pd3d_set_data_1", &
                "Insufficient memory available.", PLOT_OUT_OF_MEMORY_ERROR)
            return
        end if
        if (present(c) .and. present(ps)) then
            call this%set_use_data_dependent_colors(.true.)
            call this%set_use_variable_size_points(.true.)
            do concurrent (i = 1:n)
                this%m_data(i, 1) = x(i)
                this%m_data(i, 2) = y(i)
                this%m_data(i, 3) = z(i)
                this%m_data(i, 4) = ps(i)
                this%m_data(i, 5) = c(i)
            end do
        else if (present(c) .and. .not.present(ps)) then
            call this%set_use_data_dependent_colors(.true.)
            call this%set_use_variable_size_points(.false.)
            do concurrent (i = 1:n)
                this%m_data(i, 1) = x(i)
                this%m_data(i, 2) = y(i)
                this%m_data(i, 3) = z(i)
                this%m_data(i, 4) = c(i)
            end do
        else if (.not.present(c) .and. present(ps)) then
            call this%set_use_data_dependent_colors(.false.)
            call this%set_use_variable_size_points(.true.)
            do concurrent (i = 1:n)
                this%m_data(i, 1) = x(i)
                this%m_data(i, 2) = y(i)
                this%m_data(i, 3) = z(i)
                this%m_data(i, 4) = ps(i)
            end do
        else
            call this%set_use_data_dependent_colors(.false.)
            call this%set_use_variable_size_points(.false.)
            do concurrent (i = 1:n)
                this%m_data(i, 1) = x(i)
                this%m_data(i, 2) = y(i)
                this%m_data(i, 3) = z(i)
            end do
        end if
    end subroutine

! ------------------------------------------------------------------------------
    module function pd3d_get_x_array(this) result(x)
        ! Arguments
        class(plot_data_3d), intent(in) :: this
        real(real64), allocatable, dimension(:) :: x

        ! Process
        if (allocated(this%m_data)) then
            x = this%m_data(:,1)
        end if
    end function

! ------------------------------------------------------------------------------
    module function pd3d_get_y_array(this) result(x)
        ! Arguments
        class(plot_data_3d), intent(in) :: this
        real(real64), allocatable, dimension(:) :: x

        ! Process
        if (allocated(this%m_data)) then
            x = this%m_data(:,2)
        end if
    end function

! ------------------------------------------------------------------------------
    module function pd3d_get_z_array(this) result(x)
        ! Arguments
        class(plot_data_3d), intent(in) :: this
        real(real64), allocatable, dimension(:) :: x

        ! Process
        if (allocated(this%m_data)) then
            x = this%m_data(:,3)
        end if
    end function

! ******************************************************************************
! ADDED: OCT. 9, 2020 - JAC
! ------------------------------------------------------------------------------
    module function pd3d_get_c_array(this) result(x)
        ! Arguments
        class(plot_data_3d), intent(in) :: this
        real(real64), allocatable, dimension(:) :: x

        ! Process
        if (allocated(this%m_data)) then
            if (size(this%m_data, 2) == 4) then
                x = this%m_data(:,4)
            else if (size(this%m_data, 2) == 5) then
                x = this%m_data(:,5)
            end if
        end if
    end function

! ******************************************************************************
! ADDED: JAN. 12, 2020 - JAC
! ------------------------------------------------------------------------------
    module function pd3d_get_ps_array(this) result(x)
        ! Arguments
        class(plot_data_3d), intent(in) :: this
        real(real64), allocatable, dimension(:) :: x

        ! Process
        if (allocated(this%m_data)) then
            if (size(this%m_data, 2) > 3) then
                x = this%m_data(:,4)
            end if
        end if
    end function

! ------------------------------------------------------------------------------
end submodule