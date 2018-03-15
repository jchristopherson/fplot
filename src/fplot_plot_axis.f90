! fplot_plot_axis.f90

submodule (fplot_core) fplot_plot_axis
contains
! ------------------------------------------------------------------------------
    !> @brief Gets the axis' title.
    !!
    !! @param[in] this The plot_axis object.
    !! @return The title.
    pure module function pa_get_title(this) result(txt)
        class(plot_axis), intent(in) :: this
        character(len = :), allocatable :: txt
        txt = trim(this%m_title)
    end function

! --------------------
    !> @brief Sets the axis' title.
    !!
    !! @param[in,out] this The plot_axis object.
    !! @param[in] txt The axis title.  The number of characters must be less
    !!  than or equal to PLOTDATA_MAX_NAME_LENGTH; else, the text string is
    !!  truncated.
    module subroutine pa_set_title(this, txt)
        ! Arguments
        class(plot_axis), intent(inout) :: this
        character(len = *), intent(in) :: txt

        ! Local Variables
        integer(int32) :: n

        ! Process
        n = min(len(txt), PLOTDATA_MAX_NAME_LENGTH)
        this%m_title = ""
        if (n /= 0) then
            this%m_title(1:n) = txt(1:n)
            this%m_hasTitle = .true.
        else
            this%m_hasTitle = .false.
        end if
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets a value determining if a title has been defined for the
    !!  plot_axis object.
    !!
    !! @param[in] this The plot_axis object.
    !! @return Returns true if a title has been defined for this axis; else,
    !!  returns false.
    pure module function pa_has_title(this) result(x)
        class(plot_axis), intent(in) :: this
        logical :: x
        x = this%m_hasTitle
    end function

! ------------------------------------------------------------------------------
    !> @brief Gets a logical value determining if the axis should be
    !! automatically scaled to fit the data.
    !!
    !! @param[in] this The plot_axis object.
    !! @return Returns true if the axis should be automatically scaled; else,
    !! false.
    pure module function pa_get_autoscale(this) result(x)
        class(plot_axis), intent(in) :: this
        logical :: x
        x = this%m_autoscale
    end function

! --------------------
    !> @brief Sets a logical value determining if the axis should be
    !! automatically scaled to fit the data.
    !!
    !! @param[in,out] this The plot_axis object.
    !! @param[in] x Set to true if the axis should be automatically scaled; else,
    !! false.
    module subroutine pa_set_autoscale(this, x)
        class(plot_axis), intent(inout) :: this
        logical, intent(in) :: x
        this%m_autoscale = x
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets the axis display limits, assuming autoscaling is not active
    !! for this axis.
    !!
    !! @param[in] this The plot_axis object.
    !! @return A two-element array containing the limits as follows:
    !!  [lower, upper].
    pure module function pa_get_axis_limits(this) result(x)
        class(plot_axis), intent(in) :: this
        real(real64), dimension(2) :: x
        x(1) = minval(this%m_limits)
        x(2) = maxval(this%m_limits)
    end function

! --------------------
    !> @brief Sets the axis display limits, assuming autoscaling is not active
    !! for this axis.
    !!
    !! @param[in,out] this The plot_axis object.
    !! @param[in] lower The lower display limit.
    !! @param[in] upper The upper display limit.
    module subroutine pa_set_axis_limits(this, lower, upper)
        class(plot_axis), intent(inout) :: this
        real(real64), intent(in) :: lower, upper
        this%m_limits(1) = lower
        this%m_limits(2) = upper
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets a logical value defining if the axis should be log scaled.
    !!
    !! @param[in,out] this The plot_axis object.
    !! @return Returns true if log scaling is applied to the axis; else, false.
    pure module function pa_get_log_scale(this) result(x)
        class(plot_axis), intent(in) :: this
        logical :: x
        x = this%m_logScale
    end function

! --------------------
    !> @brief Sets a logical value defining if the axis should be log scaled.
    !!
    !! @param[in,out] this The plot_axis object.
    !! @param[in] x Set to true if log scaling is applied to the axis; else,
    !! false.
    module subroutine pa_set_log_scale(this, x)
        class(plot_axis), intent(inout) :: this
        logical, intent(in) :: x
        this%m_logScale = x
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Returns the appropriate GNUPLOT command string to define the
    !! plot_axis properties.
    !!
    !! @param[in] this The plot_axis object.
    !! @return The GNUPLOT command string.
    module function pa_get_cmd_string(this) result(txt)
        ! Arguments
        class(plot_axis), intent(in) :: this
        character(len = :), allocatable :: txt

        ! Local Variables
        type(string_builder) :: str
        character(len = :), allocatable :: axis
        real(real64) :: lim(2)

        ! Process
        axis = this%get_id_string()
        lim = this%get_limits()
        call str%initialize()

        ! Axis Limits
        if (this%get_autoscale()) then
            call str%append("set ")
            call str%append(axis)
            call str%append("range [*:*]")
        else
            call str%append("set ")
            call str%append(axis)
            call str%append("range [")
            call str%append(to_string(lim(1)))
            call str%append(":")
            call str%append(to_string(lim(2)))
            call str%append("]")
        end if

        ! Titles
        call str%append(new_line('a'))
        if (this%is_title_defined()) then
            call str%append("set ")
            call str%append(axis)
            call str%append("label ")
            call str%append('"')
            call str%append(this%get_title())
            call str%append('"')
        else
            call str%append("set ")
            call str%append(axis)
            call str%append("label ")
            call str%append('""')
        end if

        ! Scaling
        call str%append(new_line('a'))
        if (this%get_is_log_scaled()) then
            call str%append("set log ")
            call str%append(axis)
        else
            call str%append("unset log ")
            call str%append(axis)
        end if

        ! Zero Axis
        if (this%get_zero_axis()) then
            call str%append(new_line('a'))
            call str%append("set ")
            call str%append(this%get_id_string())
            call str%append("zeroaxis linestyle -1 linewidth ")
            call str%append(to_string(this%get_zero_axis_line_width()))
        end if

        ! Output
        txt = str%to_string()
    end function

! ------------------------------------------------------------------------------
    !> @brief Gets a value determining if the axis should be drawn through zero
    !! of opposing axes.
    !!
    !! @param[in] this The plot_axis object.
    !! @return Returns true to draw as a zero axis; else, set to false.
    pure module function pa_get_zero_axis(this) result(x)
        class(plot_axis), intent(in) :: this
        logical :: x
        x = this%m_zeroAxis
    end function

! --------------------
    !> @brief Sets a value determining if the axis should be drawn through zero
    !! of opposing axes.
    !!
    !! @param[in,out] this The plot_axis object.
    !! @param[in] x Set to true to draw as a zero axis; else, set to false.
    module subroutine pa_set_zero_axis(this, x)
        class(plot_axis), intent(inout) :: this
        logical, intent(in) :: x
        this%m_zeroAxis = x
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets the width of the line used to represent the zero axis
    !!  line, if active.
    !!
    !! @param[in] this The plot_axis object.
    !! @return The width of the line, in pixels.
    pure module function pa_get_zero_axis_width(this) result(x)
        class(plot_axis), intent(in) :: this
        real(real32) :: x
        x = this%m_axisWidth
    end function

! --------------------
    !> @brief Gets the width of the line used to represent the zero axis
    !!  line, if active.
    !!
    !! @param[in,out] this The plot_axis object.
    !! @param[in] x The width of the line, in pixels.
    module subroutine pa_set_zero_axis_width(this, x)
        class(plot_axis), intent(inout) :: this
        real(real32), intent(in) :: x
        this%m_axisWidth = x
    end subroutine

end submodule