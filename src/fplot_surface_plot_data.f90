! fplot_surface_plot_data.f90

submodule (fplot_core) fplot_surface_plot_data
contains
! ------------------------------------------------------------------------------
    !> @brief Gets the size of the stored data set.
    !!
    !! @param[in] this The suface_plot_data object.
    !! @param[in] dim The dimension of interest.  Notice, data is stored as a
    !!  2D matrix (i.e. only 1 and 2 are valid inputs).
    !! @return The size of the requested dimension.
    pure module function surfd_get_size(this, dim) result(x)
        class(surface_plot_data), intent(in) :: this
        integer(int32), intent(in) :: dim
        integer(int32) :: x
        if (allocated(this%m_x)) then
            x = size(this%m_x, dim)
        else
            x = 0
        end if
    end function

! ------------------------------------------------------------------------------
    !> @brief Gets the requested X data point.
    !!
    !! @param[in] this The surface_plot_data object.
    !! @param[in] i The row index.
    !! @param[in] j The column index.
    !! @return The value.
    pure module function surfd_get_x(this, i, j) result(x)
        class(surface_plot_data), intent(in) :: this
        integer(int32), intent(in) :: i, j
        real(real64) :: x
        if (allocated(this%m_x)) then
            x = this%m_x(i,j)
        else
            x = 0.0d0
        end if
    end function

! --------------------
    !> @brief Sets the requested X data point.
    !!
    !! @param[in,out] this The surface_plot_data object.
    !! @param[in] i The row index.
    !! @param[in] j The column index.
    !! @param[in] x The value.
    module subroutine surfd_set_x(this, i, j, x)
        class(surface_plot_data), intent(inout) :: this
        integer(int32), intent(in) :: i, j
        real(real64), intent(in) :: x
        if (allocated(this%m_x)) then
            this%m_x(i,j) = x
        end if
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets the requested Y data point.
    !!
    !! @param[in] this The surface_plot_data object.
    !! @param[in] i The row index.
    !! @param[in] j The column index.
    !! @return The value.
    pure module function surfd_get_y(this, i, j) result(x)
        class(surface_plot_data), intent(in) :: this
        integer(int32), intent(in) :: i, j
        real(real64) :: x
        if (allocated(this%m_y)) then
            x = this%m_y(i,j)
        else
            x = 0.0d0
        end if
    end function

! --------------------
    !> @brief Sets the requested Y data point.
    !!
    !! @param[in,out] this The surface_plot_data object.
    !! @param[in] i The row index.
    !! @param[in] j The column index.
    !! @param[in] x The value.
    module subroutine surfd_set_y(this, i, j, x)
        class(surface_plot_data), intent(inout) :: this
        integer(int32), intent(in) :: i, j
        real(real64), intent(in) :: x
        if (allocated(this%m_y)) then
            this%m_y(i,j) = x
        end if
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets the requested Z data point.
    !!
    !! @param[in] this The surface_plot_data object.
    !! @param[in] i The row index.
    !! @param[in] j The column index.
    !! @return The value.
    pure module function surfd_get_z(this, i, j) result(x)
        class(surface_plot_data), intent(in) :: this
        integer(int32), intent(in) :: i, j
        real(real64) :: x
        if (allocated(this%m_z)) then
            x = this%m_z(i,j)
        else
            x = 0.0d0
        end if
    end function

! --------------------
    !> @brief Sets the requested Z data point.
    !!
    !! @param[in,out] this The surface_plot_data object.
    !! @param[in] i The row index.
    !! @param[in] j The column index.
    !! @param[in] x The value.
    module subroutine surfd_set_z(this, i, j, x)
        class(surface_plot_data), intent(inout) :: this
        integer(int32), intent(in) :: i, j
        real(real64), intent(in) :: x
        if (allocated(this%m_z)) then
            this%m_z(i,j) = x
        end if
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets a value determining if a wireframe mesh should be displayed.
    !!
    !! @param[in] this The surface_plot_data object.
    !! @return Returns true if a wireframe mesh should be displayed; else, false
    !!  to display a solid surface.
    pure module function surfd_get_wireframe(this) result(x)
        class(surface_plot_data), intent(in) :: this
        logical :: x
        x = this%m_wireframe
    end function

! --------------------
    !> @brief Sets a value determining if a wireframe mesh should be displayed.
    !!
    !! @param[in,out] this The surface_plot_data object.
    !! @param[in] x Set to true if a wireframe mesh should be displayed; else, 
    !!  false to display a solid surface.
    module subroutine surfd_set_wireframe(this, x)
        class(surface_plot_data), intent(inout) :: this
        logical, intent(in) :: x
        this%m_wireframe = x
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets the GNUPLOT command string to represent this
    !! surface_plot_data object.
    !!
    !! @param[in] this The surface_plot_data object.
    !! @return The command string.
    module function surfd_get_cmd(this) result(x)
        ! Arguments
        class(surface_plot_data), intent(in) :: this
        character(len = :), allocatable :: x

        ! Local Variables
        type(string_builder) :: str
        integer(int32) :: n
        
        ! Initialization
        call str%initialize()

        ! Title
        n = len_trim(this%get_name())
        if (n > 0) then
            call str%append(' "-" title "')
            call str%append(this%get_name())
            call str%append('"')
        else
            call str%append(' "-" notitle')
        end if

        ! PM3D or wireframe?
        if (this%get_use_wireframe()) then
            call str%append(" with lines")
        else
            call str%append(" with pm3d")
        end if

        ! End
        x = str%to_string()
    end function

! ------------------------------------------------------------------------------
    !> @brief Gets the GNUPLOT command string containing the actual data
    !! to plot.
    !!
    !! @param[in] this The surface_plot_data object.
    !! @return The GNUPLOT command string.
    module function surfd_get_data_cmd(this) result(x)
        ! Arguments
        class(surface_plot_data), intent(in) :: this
        character(len = :), allocatable :: x

        ! Local Variables
        type(string_builder) :: str
        integer(int32) :: i, j, m, n
        character :: delimiter, nl

        ! Initialization
        call str%initialize()
        m = this%get_size(1)
        n = this%get_size(2)
        delimiter = achar(9) ! tab delimiter
        nl = new_line(nl)

        ! Process
        do j = 1, n
            do i = 1, m
                call str%append(to_string(this%get_x(i,j)))
                call str%append(delimiter)
                call str%append(to_string(this%get_y(i,j)))
                call str%append(delimiter)
                call str%append(to_string(this%get_z(i,j)))
                call str%append(nl)
            end do
            if (j /= n) call str%append(nl)
        end do

        ! End
        x = str%to_string()
    end function

! ------------------------------------------------------------------------------
    !> @brief Defines the data set.
    !!
    !! @param[in,out] this The plot_data_2d object.
    !! @param[in] x An M-by-N matrix containing the x-coordinate data.
    !! @param[in] y An M-by-N matrix containing the y-coordinate data.
    !! @param[in] z An M-by-N matrix containing the z-coordinate data.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - PLOT_OUT_OF_MEMORY_ERROR: Occurs if insufficient memory is available.
    !!  - PLOT_ARRAY_SIZE_MISMATCH_ERROR: Occurs if @p x, @p y, and @p z are 
    !!      not the same size.
    module subroutine surfd_set_data_1(this, x, y, z, err)
        ! Arguments
        class(surface_plot_data), intent(inout) :: this
        real(real64), intent(in), dimension(:,:) :: x, y, z
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: i, j, m, n, flag
        class(errors), pointer :: errmgr
        type(errors), target :: deferr

        ! Initialization
        m = size(x, 1)
        n = size(x, 2)
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Input Check
        if (size(y, 1) /= m .or. size(y, 2) /= n .or. size(z, 1) /= m .or. size(z, 2) /= n) then
            call errmgr%report_error("surfd_set_data_1", &
                "The input arrays are not the same size.", &
                PLOT_ARRAY_SIZE_MISMATCH_ERROR)
            return
        end if

        ! Process
        if (allocated(this%m_x)) deallocate(this%m_x)
        if (allocated(this%m_y)) deallocate(this%m_y)
        if (allocated(this%m_z)) deallocate(this%m_z)
        allocate(this%m_x(m, n), stat = flag)
        if (flag == 0) allocate(this%m_y(m, n), stat = flag)
        if (flag == 0) allocate(this%m_z(m, n), stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("surfd_set_data_1", &
                "Insufficient memory available.", PLOT_OUT_OF_MEMORY_ERROR)
            return
        end if
        do concurrent (j = 1:n)
            do i = 1, m
                this%m_x(i, j) = x(i, j)
                this%m_y(i, j) = y(i, j)
                this%m_z(i, j) = z(i, j)
            end do
        end do
    end subroutine

end submodule
