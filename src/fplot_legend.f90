! fplot_legend.f90

submodule (fplot_core) fplot_legend
contains
! ------------------------------------------------------------------------------
    !> @brief Gets a value determining if the legend should be drawn inside the
    !! axes border (true), or outside the axes border (false).
    !!
    !! @param[in] this The legend object.
    !! @return The logical value.
    pure module function leg_get_inside(this) result(x)
        class(legend), intent(in) :: this
        logical :: x
        x = this%m_inside
    end function

! ---------------------
    !> @brief Sets a value determining if the legend should be drawn inside the
    !! axes border (true), or outside the axes border (false).
    !!
    !! @param[in,out] this The legend object.
    !! @param[in] x The logical value.
    module subroutine leg_set_inside(this, x)
        class(legend), intent(inout) :: this
        logical, intent(in) :: x
        this%m_inside = x
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets a value determining if the legend should have a border.
    !!
    !! @param[in] this The legend object.
    !! @return The logical value.
    pure module function leg_get_box(this) result(x)
        class(legend), intent(in) :: this
        logical :: x
        x = this%m_box
    end function

! ---------------------
    !> @brief Sets a value determining if the legend should have a border.
    !!
    !! @param[in,out] this The legend object.
    !! @param[in] x The logical value.
    module subroutine leg_set_box(this, x)
        class(legend), intent(inout) :: this
        logical, intent(in) :: x
        this%m_box = x
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets the horizontal position of the legend.
    !!
    !! @param[in] this The legend object.
    !! @return The horizontal position of the legend (LEGEND_LEFT,
    !!  LEGEND_CENTER, or LEGEND_RIGHT).
    pure module function leg_get_horz_pos(this) result(x)
        class(legend), intent(in) :: this
        character(len = :), allocatable :: x
        x = trim(this%m_horzPosition)
    end function

! ---------------------
    !> @brief Sets the horizontal position of the legend.
    !!
    !! @param[in,out] this The legend object.
    !! @param x The horizontal position of the legend.  The parameter must be
    !!  set to one of the following: LEGEND_LEFT, LEGEND_CENTER, or
    !!  LEGEND_RIGHT.  If not, the default LEGEND_RIGHT will be used.
    module subroutine leg_set_horz_pos(this, x)
        class(legend), intent(inout) :: this
        character(len = *), intent(in) :: x
        this%m_horzPosition = x
        if (x /= LEGEND_LEFT .and. x /= LEGEND_RIGHT .and. x /= LEGEND_CENTER) &
            this%m_horzPosition = LEGEND_RIGHT
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets the vertical position of the legend.
    !!
    !! @param[in] this The legend object.
    !! @return The vertical position of the legend (LEGEND_TOP,
    !!  LEGEND_CENTER, or LEGEND_BOTTOM).
    pure module function leg_get_vert_pos(this) result(x)
        class(legend), intent(in) :: this
        character(len = :), allocatable :: x
        x = trim(this%m_vertPosition)
    end function

! ---------------------
    !> @brief Sets the vertical position of the legend.
    !!
    !! @param[in,out] this The legend object.
    !! @param x The vertical position of the legend.  The parameter must be
    !!  set to one of the following: LEGEND_TOP, LEGEND_CENTER, or
    !!  LEGEND_BOTTOM.  If not, the default LEGEND_TOP will be used.
    module subroutine leg_set_vert_pos(this, x)
        class(legend), intent(inout) :: this
        character(len = *), intent(in) :: x
        this%m_vertPosition = x
        if (x /= LEGEND_TOP .and. x /= LEGEND_CENTER .and. x /= LEGEND_BOTTOM) &
            this%m_vertPosition = LEGEND_TOP
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets a value determining if the legend is visible.
    !!
    !! @param[in] this The legend object.
    !! @return The logical value.
    pure module function leg_get_visible(this) result(x)
        class(legend), intent(in) :: this
        logical :: x
        x = this%m_show
    end function

! ---------------------
    !> @brief Sets a value determining if the legend is visible.
    !!
    !! @param[in,out] this The legend object.
    !! @param[in] x The logical value.
    module subroutine leg_set_visible(this, x)
        class(legend), intent(inout) :: this
        logical, intent(in) :: x
        this%m_show = x
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets the command string defining the legend properties.
    !!
    !! @param[in] this The legend object.
    !! @return The GNUPLOT command string.
    module function leg_get_command_txt(this) result(txt)
        ! Arguments
        class(legend), intent(in) :: this
        character(len = :), allocatable :: txt

        ! Local Variables
        type(string_builder) :: str

        ! Process
        call str%initialize()

        ! Visible?
        if (.not.this%get_is_visible()) then
            txt = "set key off"
            return
        end if

        ! Inside vs Outside & Position
        if (this%get_draw_inside_axes()) then
            call str%append("set key inside")
        else
            call str%append("set key outside")
        end if
        call str%append(" ")
        call str%append(this%get_vertical_position())
        call str%append(" ")
        call str%append(this%get_horizontal_position())

        ! Border
        call str%append(new_line('a'))
        if (this%get_draw_border()) then
            call str%append("set key box opaque")
        else
            call str%append("set key nobox")
        end if

        ! End
        txt = str%to_string()
    end function

end submodule
