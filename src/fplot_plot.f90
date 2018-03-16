! fplot_plot.f90

submodule (fplot_core) fplot_plot
contains
! ------------------------------------------------------------------------------
    !> @brief Cleans up resources held by the plot object.
    !!
    !! @param[in,out] this The plot object.
    module subroutine plt_clean_up(this)
        class(plot), intent(inout) :: this
        if (associated(this%m_terminal)) then
            deallocate(this%m_terminal)
            nullify(this%m_terminal)
        end if
        if (associated(this%m_legend)) then
            deallocate(this%m_legend)
            nullify(this%m_legend)
        end if
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Initializes the plot object.
    !!
    !! @param[in,out] this The plot object.
    !! @param[in] term An optional input that is used to define the terminal.
    !!  The default terminal is a WXT terminal.  The acceptable inputs are:
    !!  - GNUPLOT_TERMINAL_PNG
    !!  - GNUPLOT_TERMINAL_QT
    !!  - GNUPLOT_TERMINAL_WIN32
    !!  - GNUPLOT_TERMINAL_WXT
    !!  - GNUPLOT_TERMINAL_LATEX
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !! - PLOT_OUT_OF_MEMORY_ERROR: Occurs if insufficient memory is available.
    module subroutine plt_init(this, term, err)
        ! Arguments
        class(plot), intent(inout) :: this
        integer(int32), intent(in), optional :: term
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: flag, t
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        type(wxt_terminal), pointer :: wxt
        type(windows_terminal), pointer :: win
        type(qt_terminal), pointer :: qt
        type(png_terminal), pointer :: png
        type(latex_terminal), pointer :: latex

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        if (present(term)) then
            t = term
        else
            t = GNUPLOT_TERMINAL_WXT
        end if

        ! Process
        flag = 0
        if (associated(this%m_terminal)) deallocate(this%m_terminal)
        select case (t)
        case (GNUPLOT_TERMINAL_PNG)
            allocate(png, stat = flag)
            this%m_terminal => png
        case (GNUPLOT_TERMINAL_QT)
            allocate(qt, stat = flag)
            this%m_terminal => qt
        case (GNUPLOT_TERMINAL_WIN32)
            allocate(win, stat = flag)
            this%m_terminal => win
        case (GNUPLOT_TERMINAL_LATEX)
            allocate(latex, stat = flag)
            this%m_terminal => latex
        case default ! WXT is the default
            allocate(wxt, stat = flag)
            this%m_terminal => wxt
        end select

        if (flag == 0 .and. .not.associated(this%m_legend)) then
            allocate(this%m_legend, stat = flag)
        end if

        ! Error Checking
        if (flag /= 0) then
            call errmgr%report_error("plt_init", &
                "Insufficient memory available.", PLOT_OUT_OF_MEMORY_ERROR)
            return
        end if
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets the plot's title.
    !!
    !! @param[in] this The plot object.
    !! @return The plot's title.
    pure module function plt_get_title(this) result(txt)
        class(plot), intent(in) :: this
        character(len = :), allocatable :: txt
        txt = trim(this%m_title)
    end function

! --------------------
    !> @brief Sets the plot's title.
    !!
    !! @param[in,out] this The plot object.
    !! @param[in] txt The plot's title.  The number of characters must be less
    !! than or equal to PLOTDATA_MAX_NAME_LENGTH; else, the text string is
    !! truncated.
    module subroutine plt_set_title(this, txt)
        class(plot), intent(inout) :: this
        character(len = *), intent(in) :: txt
        integer :: n
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
    !!  plot object.
    !!
    !! @param[in] this The plot object.
    !! @return Returns true if a title has been defined for this plot; else,
    !!  returns false.
    pure module function plt_has_title(this) result(x)
        class(plot), intent(in) :: this
        logical :: x
        x = this%m_hasTitle
    end function

! ------------------------------------------------------------------------------
    !> @brief Gets the plot's legend object.
    !!
    !! @param[in] this The plot object.
    !! @return A pointer to the legend object.
    module function plt_get_legend(this) result(x)
        class(plot), intent(in) :: this
        type(legend), pointer :: x
        x => this%m_legend
    end function

! ------------------------------------------------------------------------------
    !> @brief Gets the number of stored plot_data objects.
    !!
    !! @param[in] this The plot object.
    !! @return The number of plot_data objects.
    pure module function plt_get_count(this) result(x)
        class(plot), intent(in) :: this
        integer(int32) :: x
        x = this%m_data%get_count()
    end function

! ------------------------------------------------------------------------------
    !> @brief Pushes a plot_data object onto the stack.
    !!
    !! @param[in,out] this The plot object.
    !! @param[in] x The plot_data object.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !! - PLOT_OUT_OF_MEMORY_ERROR: Occurs if insufficient memory is available.
    module subroutine plt_push_data(this, x, err)
        ! Arguments
        class(plot), intent(inout) :: this
        class(plot_data), intent(in) :: x
        class(errors), intent(inout), optional, target :: err

        ! Process
        call this%m_data%push(x, err)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Pops the last plot_data object from the stack.
    !!
    !! @param[in,out] this The plot object.
    module subroutine plt_pop_data(this)
        class(plot), intent(inout) :: this
        call this%m_data%pop()
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Removes all plot_data objects from the plot.
    !!
    !! @param[in,out] this The plot object.
    module subroutine plt_clear_all(this)
        class(plot), intent(inout) :: this
        call this%m_data%clear()
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets a pointer to the requested plot_data object.
    !!
    !! @param[in] this The plot object.
    !! @param[in] i The index of the plot_data object.
    !! @return A pointer to the requested plot_data object.
    module function plt_get(this, i) result(x)
        ! Arguments
        class(plot), intent(in) :: this
        integer(int32), intent(in) :: i
        class(plot_data), pointer :: x

        ! Local Variables
        class(*), pointer :: item

        ! Process
        item => this%m_data%get(i)
        select type (item)
        class is (plot_data)
            x => item
        class default
            nullify(x)
        end select
    end function


! --------------------
    !> @brief Sets the requested plot_data object into the plot.
    !!
    !! @param[in,out] this The plot object.
    !! @param[in] i The index of the plot_data object.
    !! @param[in] x The plot_data object.
    module subroutine plt_set(this, i, x)
        class(plot), intent(inout) :: this
        integer(int32), intent(in) :: i
        class(plot_data), intent(in) :: x
        call this%m_data%set(i, x)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets the GNUPLOT terminal object.
    !!
    !! @param[in] this The plot object.
    !! @return A pointer to the GNUPLOT terminal object.
    module function plt_get_term(this) result(x)
        class(plot), intent(in) :: this
        class(terminal), pointer :: x
        x => this%m_terminal
    end function

! ------------------------------------------------------------------------------
    !> @brief Gets a flag determining if the grid lines should be shown.
    !!
    !! @param[in] this The plot object.
    !! @return Returns true if the grid lines should be shown; else, false.
    pure module function plt_get_show_grid(this) result(x)
        class(plot), intent(in) :: this
        logical :: x
        x = this%m_showGrid
    end function

! --------------------
    !> @brief Sets a flag determining if the grid lines should be shown.
    !!
    !! @param[in,out] this The plot object.
    !! @param[in] x Set to true if the grid lines should be shown; else, false.
    module subroutine plt_set_show_grid(this, x)
        class(plot), intent(inout) :: this
        logical, intent(in) :: x
        this%m_showGrid = x
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Launches GNUPLOT and draws the plot per the current state of the
    !! command list.
    !!
    !! @param[in] this The plot object.
    !! @param[in] persist An optional parameter that can be used to keep GNUPLOT
    !!  open.  Set to true to force GNUPLOT to remain open; else, set to false
    !!  to allow GNUPLOT to close after drawing.  The default is true.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - PLOT_GNUPLOT_FILE_ERROR: Occurs if the command file cannot be written.
    module subroutine plt_draw(this, persist, err)
        ! Arguments
        class(plot), intent(in) :: this
        logical, intent(in), optional :: persist
        class(errors), intent(inout), optional, target :: err

        ! Parameters
        character(len = *), parameter :: fname = "temp_gnuplot_file.plt"

        ! Local Variables
        logical :: p
        integer(int32) :: fid, flag
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg

        ! Initialization
        if (present(persist)) then
            p = persist
        else
            p = .true.
        end if
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Open the file for writing, and write the contents to file
        open(newunit = fid, file = fname, iostat = flag)
        if (flag > 0) then
            write(errmsg, "(AI0A)") &
                "The file could not be opened/created.  Error code ", flag, &
                " was encountered."
            call errmgr%report_error("plt_draw", trim(errmsg), &
                PLOT_GNUPLOT_FILE_ERROR)
            return
        end if
        write(fid, '(A)') this%get_command_string()
        close(fid)

        ! Launch GNUPLOT
        if (p) then
            call execute_command_line("gnuplot --persist " // fname)
        else
            call execute_command_line("gnuplot " // fname)
        end if

        ! Clean up by deleting the file
        open(newunit = fid, file = fname)
        close(fid, status = "delete")
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Saves a GNUPLOT command file.
    !!
    !! @param[in] this The plot object.
    !! @param[in] fname The filename.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - PLOT_GNUPLOT_FILE_ERROR: Occurs if the command file cannot be written.
    module subroutine plt_save(this, fname, err)
        ! Arguments
        class(plot), intent(in) :: this
        character(len = *), intent(in) :: fname
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: fid, flag
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Open the file for writing, and write the contents to file
        open(newunit = fid, file = fname, iostat = flag)
        if (flag > 0) then
            write(errmsg, "(AI0A)") &
                "The file could not be opened/created.  Error code ", flag, &
                " was encountered."
            call errmgr%report_error("plt_save", trim(errmsg), &
                PLOT_GNUPLOT_FILE_ERROR)
            return
        end if
        write(fid, '(A)') this%get_command_string()
        close(fid)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets the name of the font used for plot text.
    !!
    !! @param[in] this The plot object.
    !! @return The font name.
    module function plt_get_font(this) result(x)
        class(plot), intent(in) :: this
        character(len = :), allocatable :: x
        class(terminal), pointer :: term
        term => this%get_terminal()
        x = term%get_font_name()
    end function

! --------------------
    !> @brief Sets the name of the font used for plot text.
    !!
    !! @param[in,out] this The plot object.
    !! @param[in] x The font name.
    module subroutine plt_set_font(this, x)
        class(plot), intent(inout) :: this
        character(len = *), intent(in) :: x
        class(terminal), pointer :: term
        term => this%get_terminal()
        call term%set_font_name(x)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets the size of the font used by the plot.
    !!
    !! @param[in] this The plot object.
    !! @return The size of the font, in points.
    module function plt_get_font_size(this) result(x)
        class(plot), intent(in) :: this
        integer(int32) :: x
        class(terminal), pointer :: term
        term => this%get_terminal()
        x = term%get_font_size()
    end function

! --------------------
    !> @brief Sets the size of the font used by the plot.
    !!
    !! @param[in,out] this The plot object.
    !! @param[in] x The font size, in points.  If a value of zero is provided,
    !! the font size is reset to its default value; or, if a negative value
    !! is provided, the absolute value of the supplied value is utilized.
    module subroutine plt_set_font_size(this, x)
        class(plot), intent(inout) :: this
        integer(int32), intent(in) :: x
        class(terminal), pointer :: term
        term => this%get_terminal()
        call term%set_font_size(x)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets a value determining if the axis tic marks should point
    !! inwards.
    !!
    !! @param[in] this The plot object.
    !! @return Returns true if the tic marks should point inwards; else, false
    !!  if the tic marks should point outwards.
    pure module function plt_get_tics_in(this) result(x)
        class(plot), intent(in) :: this
        logical :: x
        x = this%m_ticsIn
    end function

! --------------------
    !> @brief Sets a value determining if the axis tic marks should point
    !! inwards.
    !!
    !! @param[in,out] this The plot object.
    !! @param[in] x Set to true if the tic marks should point inwards; else,
    !!  false if the tic marks should point outwards.
    module subroutine plt_set_tics_in(this, x)
        class(plot), intent(inout) :: this
        logical, intent(in) :: x
        this%m_ticsIn = x
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets a value determining if the border should be drawn.
    !!
    !! @param[in] this The plot object.
    !! @return Returns true if the border should be drawn; else, false.
    pure module function plt_get_draw_border(this) result(x)
        class(plot), intent(in) :: this
        logical :: x
        x = this%m_drawBorder
    end function

! --------------------
    !> @brief Sets a value determining if the border should be drawn.
    !!
    !! @param[in,out] this The plot object.
    !! @param[in] x Set to true if the border should be drawn; else, false.
    module subroutine plt_set_draw_border(this, x)
        class(plot), intent(inout) :: this
        logical, intent(in) :: x
        this%m_drawBorder = x
    end subroutine
end submodule