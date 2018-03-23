! fplot_plot_data_3d.f90

submodule (fplot_core) fplot_plot_data_3d
contains
! ------------------------------------------------------------------------------
    !> @brief Gets the number of data points.
    !!
    !! @param[in] this The plot_data_3d object.
    !! @return The number of data points.
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
    !> @brief Gets the requested X data point.
    !!
    !! @param[in] this The plot_data_3d object.
    !! @param[in] index The index of the data point to retrieve.
    !! @return The requested data point.
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
    !> @brief Sets the requested X data point.
    !!
    !! @param[in,out] this The plot_data_3d object.
    !! @param[in] index The index of the data point to replace.
    !! @param[in] x The data point.
    module subroutine pd3d_set_x_data(this, index, x)
        class(plot_data_3d), intent(inout) :: this
        integer(int32), intent(in) :: index
        real(real64), intent(in) :: x
        if (allocated(this%m_data)) then
            this%m_data(index, 1) = x
        end if
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets the requested Y data point.
    !!
    !! @param[in] this The plot_data_3d object.
    !! @param[in] index The index of the data point to retrieve.
    !! @return The requested data point.
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
    !> @brief Sets the requested Y data point.
    !!
    !! @param[in,out] this The plot_data_3d object.
    !! @param[in] index The index of the data point to replace.
    !! @param[in] x The data point.
    module subroutine pd3d_set_y_data(this, index, x)
        class(plot_data_3d), intent(inout) :: this
        integer(int32), intent(in) :: index
        real(real64), intent(in) :: x
        if (allocated(this%m_data)) then
            this%m_data(index, 2) = x
        end if
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets the requested Z data point.
    !!
    !! @param[in] this The plot_data_3d object.
    !! @param[in] index The index of the data point to retrieve.
    !! @return The requested data point.
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
    !> @brief Sets the requested Z data point.
    !!
    !! @param[in,out] this The plot_data_3d object.
    !! @param[in] index The index of the data point to replace.
    !! @param[in] x The data point.
    module subroutine pd3d_set_z_data(this, index, x)
        class(plot_data_3d), intent(inout) :: this
        integer(int32), intent(in) :: index
        real(real64), intent(in) :: x
        if (allocated(this%m_data)) then
            this%m_data(index, 3) = x
        end if
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets the GNUPLOT command string defining which axes the data is
    !! to be plotted against.
    !!
    !! @param[in] this The plot_data_3d object.
    !! @return The command string.
    module function pd3d_get_axes_cmd(this) result(x)
        ! Arguments
        class(plot_data_3d), intent(in) :: this
        character(len = :), allocatable :: x

        ! Output
        x = ""
    end function

! ------------------------------------------------------------------------------
    !> @brief Gets the GNUPLOT command string containing the actual data
    !! to plot.
    !!
    !! @param[in] this The plot_data_3d object.
    !! @return The command string.
    module function pd3d_get_data_cmd(this) result(x)
        ! Arguments
        class(plot_data_3d), intent(in) :: this
        character(len = :), allocatable :: x

        ! Local Variables
        type(string_builder) :: str
        integer(int32) :: i, n
        character :: delimiter, nl

        ! Initialization
        call str%initialize()
        delimiter = achar(9) ! tab delimiter
        nl = new_line(nl)
        n = this%get_count()

        ! Process
        do i = 1, n
            call str%append(to_string(this%get_x(i)))
            call str%append(delimiter)
            call str%append(to_string(this%get_y(i)))
            call str%append(delimiter)
            call str%append(to_string(this%get_z(i)))
            call str%append(nl)
        end do

        ! End
        x = str%to_string()
    end function

! ------------------------------------------------------------------------------
    !> @brief Defines the data set.
    !!
    !! @param[in,out] this The plot_data_2d object.
    !! @param[in] x An N-element array containing the x coordinate data.
    !! @param[in] y An N-element array containing the y coordinate data.
    !! @param[in] z An N-element array containing the z coordinate data.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - PLOT_OUT_OF_MEMORY_ERROR: Occurs if insufficient memory is available.
    !!  - PLOT_ARRAY_SIZE_MISMATCH_ERROR: Occurs if @p x, @p y, and @p z are 
    !!      not the same size.
    module subroutine pd3d_set_data_1(this, x, y, z, err)
        ! Arguments
        class(plot_data_3d), intent(inout) :: this
        real(real64), intent(in), dimension(:) :: x, y, z
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: i, n, flag
        class(errors), pointer :: errmgr
        type(errors), target :: deferr

        ! Initialization
        n = size(x)
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

        ! Process
        if (allocated(this%m_data)) deallocate(this%m_data)
        allocate(this%m_data(n, 3), stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("pd3d_set_data_1", &
                "Insufficient memory available.", PLOT_OUT_OF_MEMORY_ERROR)
            return
        end if
        do concurrent (i = 1:n)
            this%m_data(i, 1) = x(i)
            this%m_data(i, 2) = y(i)
            this%m_data(i, 3) = z(i)
        end do
    end subroutine

end submodule