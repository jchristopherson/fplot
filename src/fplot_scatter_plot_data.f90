! fplot_scatter_plot_data.f90

submodule (fplot_core) fplot_scatter_plot_data
contains
! ------------------------------------------------------------------------------
    !> @brief Gets the GNUPLOT command string to represent this
    !! scatter_plot_data object.
    !!
    !! @param[in] this The scatter_plot_data object.
    !! @return The command string.
    module function spd_get_cmd(this) result(x)
        ! Arguments
        class(scatter_plot_data), intent(in) :: this
        character(len = :), allocatable :: x

        ! Local Variables
        type(string_builder) :: str
        integer(int32) :: n
        type(color) :: clr

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

        ! Lines or points?
        if (this%get_draw_line() .and. this%get_draw_markers()) then
            call str%append(" with linespoints")
        else if (.not.this%get_draw_line() .and. this%get_draw_markers()) then
            call str%append(" with points")
        else
            call str%append(" with lines")
        end if

        ! Line Width
        call str%append(" lw ")
        call str%append(to_string(this%get_line_width()))

        ! Line Color
        if (.not.this%get_use_auto_color()) then
            clr = this%get_line_color()
            call str%append(' lc rgb "#')
            call str%append(clr%to_hex_string())
            call str%append('"')
        end if

        ! Define other properties specific to the lines and points
        if (this%get_draw_line()) then
            call str%append(" lt ")
            call str%append(to_string(this%get_line_style()))
            if (this%get_line_style() /= LINE_SOLID) then
                call str%append(" dashtype ")
                call str%append(to_string(this%get_line_style()))
            end if
        end if
        if (this%get_draw_markers()) then
            call str%append(" pi ")
            call str%append(to_string(this%get_marker_frequency()))
            call str%append(" pt ")
            call str%append(to_string(this%get_marker_style()))
            call str%append(" ps ")
            call str%append(to_string(this%get_marker_scaling()))
        end if

        ! Define the axes structure
        call str%append(" ")
        call str%append(this%get_axes_string())

        ! End
        x = str%to_string()
    end function

! ------------------------------------------------------------------------------
    !> @brief Gets the width of the line, in pixels.
    !!
    !! @param[in] this The scatter_plot_data object.
    !! @return The line width.
    pure module function spd_get_line_width(this) result(x)
        class(scatter_plot_data), intent(in) :: this
        real(real32) :: x
        x = this%m_lineWidth
    end function

! --------------------
    !> @brief Sets the width of the line, in pixels.
    !!
    !! @param[in,out] this The scatter_plot_data object.
    !! @param[in] x The line width.
    module subroutine spd_set_line_width(this, x)
        class(scatter_plot_data), intent(inout) :: this
        real(real32), intent(in) :: x
        this%m_lineWidth = x
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets the line style.
    !!
    !! @param[in] this The scatter_plot_data object.
    !! @return The line style.  The line style must be one of the following:
    !!  - LINE_DASHED
    !!  - LINE_DASH_DOTTED
    !!  - LINE_DASH_DOT_DOT
    !!  - LINE_DOTTED
    !!  - LINE_SOLID
    pure module function spd_get_line_style(this) result(x)
        class(scatter_plot_data), intent(in) :: this
        integer(int32) :: x
        x = this%m_lineStyle
    end function

! --------------------
    !> @brief Sets the line style.
    !!
    !! @param[in,out] this The scatter_plot_data object.
    !! @param[in] x The line style.  The line style must be one of the
    !!      following:
    !!  - LINE_DASHED
    !!  - LINE_DASH_DOTTED
    !!  - LINE_DASH_DOT_DOT
    !!  - LINE_DOTTED
    !!  - LINE_SOLID
    module subroutine spd_set_line_style(this, x)
        class(scatter_plot_data), intent(inout) :: this
        integer(int32), intent(in) :: x
        if (x == LINE_DASHED .or. &
            x == LINE_DASH_DOTTED .or. &
            x == LINE_DASH_DOT_DOT .or. &
            x == LINE_DOTTED .or. &
            x == LINE_SOLID) then
            ! Only reset the line style if it is a valid type.
            this%m_lineStyle = x
        end if
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets the line color.
    !!
    !! @param[in] this The scatter_plot_data object.
    !! @return The color.
    pure module function spd_get_line_color(this) result(x)
        class(scatter_plot_data), intent(in) :: this
        type(color) :: x
        x = this%m_lineColor
    end function

! --------------------
    !> @brief Sets the line color.
    !!
    !! @param[in,out] this The scatter_plot_data object.
    !! @param[in] x The color.
    module subroutine spd_set_line_color(this, x)
        class(scatter_plot_data), intent(inout) :: this
        type(color), intent(in) :: x
        this%m_lineColor = x
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets a value determining if a line should be drawn.
    !!
    !! @param[in] this The scatter_plot_data object.
    !! @return Returns true if the line should be drawn; else, false.
    pure module function spd_get_draw_line(this) result(x)
        class(scatter_plot_data), intent(in) :: this
        logical :: x
        x = this%m_drawLine
    end function

! --------------------
    !> @brief Sets a value determining if a line should be drawn.
    !!
    !! @param[in,out] this The scatter_plot_data object.
    !! @param[in] x Set to true if the line should be drawn; else, false.
    module subroutine spd_set_draw_line(this, x)
        class(scatter_plot_data), intent(inout) :: this
        logical, intent(in) :: x
        this%m_drawLine = x
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets a value determining if data point markers should be drawn.
    !!
    !! @param[in] this The scatter_plot_data object.
    !! @return Returns true if the markers should be drawn; else, false.
    pure module function spd_get_draw_markers(this) result(x)
        class(scatter_plot_data), intent(in) :: this
        logical :: x
        x = this%m_drawMarkers
    end function

! --------------------
    !> @brief Sets a value determining if data point markers should be drawn.
    !!
    !! @param[in,out] this The scatter_plot_data object.
    !! @param[in] x Set to true if the markers should be drawn; else, false.
    module subroutine spd_set_draw_markers(this, x)
        class(scatter_plot_data), intent(inout) :: this
        logical, intent(in) :: x
        this%m_drawMarkers = x
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets the marker style.
    !!
    !! @param[in] this The scatter_plot_data object.
    !! @return The marker type.  The marker type must be one of the following:
    !!  - MARKER_ASTERISK
    !!  - MARKER_EMPTY_CIRCLE
    !!  - MARKER_EMPTY_NABLA
    !!  - MARKER_EMPTY_RHOMBUS
    !!  - MARKER_EMPTY_SQUARE
    !!  - MARKER_EMPTY_TRIANGLE
    !!  - MARKER_FILLED_CIRCLE
    !!  - MARKER_FILLED_NABLA
    !!  - MARKER_FILLED_RHOMBUS
    !!  - MARKER_FILLED_SQUARE
    !!  - MARKER_FILLED_TRIANGLE
    !!  - MARKER_PLUS
    !!  - MARKER_X
    pure module function spd_get_marker_style(this) result(x)
        class(scatter_plot_data), intent(in) :: this
        integer(int32) :: x
        x = this%m_markerType
    end function

! --------------------
    !> @brief Sets the marker style.
    !!
    !! @param[in,out] this The scatter_plot_data object.
    !! @param[in] x The marker type.  The marker type must be one of the
    !!  following:
    !!  - MARKER_ASTERISK
    !!  - MARKER_EMPTY_CIRCLE
    !!  - MARKER_EMPTY_NABLA
    !!  - MARKER_EMPTY_RHOMBUS
    !!  - MARKER_EMPTY_SQUARE
    !!  - MARKER_EMPTY_TRIANGLE
    !!  - MARKER_FILLED_CIRCLE
    !!  - MARKER_FILLED_NABLA
    !!  - MARKER_FILLED_RHOMBUS
    !!  - MARKER_FILLED_SQUARE
    !!  - MARKER_FILLED_TRIANGLE
    !!  - MARKER_PLUS
    !!  - MARKER_X
    module subroutine spd_set_marker_style(this, x)
        class(scatter_plot_data), intent(inout) :: this
        integer(int32), intent(in) :: x
        if (x == MARKER_ASTERISK .or. &
            x == MARKER_EMPTY_CIRCLE .or. &
            x == MARKER_EMPTY_NABLA .or. &
            x == MARKER_EMPTY_RHOMBUS .or. &
            x == MARKER_EMPTY_SQUARE .or. &
            x == MARKER_EMPTY_TRIANGLE .or. &
            x == MARKER_FILLED_CIRCLE .or. &
            x == MARKER_FILLED_NABLA .or. &
            x == MARKER_FILLED_RHOMBUS .or. &
            x == MARKER_FILLED_SQUARE .or. &
            x == MARKER_FILLED_TRIANGLE .or. &
            x == MARKER_PLUS .or. &
            x == MARKER_X) then

            ! Only alter the value if the marker is a known type
            this%m_markerType = x
        end if
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets the marker scaling.
    !!
    !! @param[in] this The scatter_plot_data object.
    !! @return The scaling factor.
    pure module function spd_get_marker_scaling(this) result(x)
        class(scatter_plot_data), intent(in) :: this
        real(real32) :: x
        x = this%m_markerSize
    end function

! --------------------
    !> @brief Sets the marker scaling.
    !!
    !! @param[in,out] this The scatter_plot_data object.
    !! @param[in] x The scaling factor.
    module subroutine spd_set_marker_scaling(this, x)
        class(scatter_plot_data), intent(inout) :: this
        real(real32), intent(in) :: x
        this%m_markerSize = x
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets the marker frequency.
    !!
    !! @param[in] this The scatter_plot_data object.
    !! @return The marker frequency.
    pure module function spd_get_marker_frequency(this) result(x)
        class(scatter_plot_data), intent(in) :: this
        integer(int32) :: x
        x = this%m_markerFrequency
    end function

! --------------------
    !> @brief Sets the marker frequency.
    !!
    !! @param[in,out] this The scatter_plot_data object.
    !! @param[in] x The marker frequency.
    module subroutine spd_set_marker_frequency(this, x)
        class(scatter_plot_data), intent(inout) :: this
        integer(int32), intent(in) :: x
        this%m_markerFrequency = x
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets a value determining if GNUPLOT should automatically choose
    !! line colors.
    !!
    !! @param[in] this The scatter_plot_data object.
    !! @return Returns true if GNUPLOT should choose colors; else, false.
    pure module function spd_get_use_auto_colors(this) result(x)
        class(scatter_plot_data), intent(in) :: this
        logical :: x
        x = this%m_useAutoColor
    end function

! --------------------
    !> @brief Sets a value determining if GNUPLOT should automatically choose
    !! line colors.
    !!
    !! @param[in,out] this The scatter_plot_data object.
    !! @param[in] x Set to true if GNUPLOT should choose colors; else, false.
    module subroutine spd_set_use_auto_colors(this, x)
        class(scatter_plot_data), intent(inout) :: this
        logical, intent(in) :: x
        this%m_useAutoColor = x
    end subroutine

end submodule
