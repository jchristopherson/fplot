! fplot_plot.f90

submodule (fplot_core) fplot_plot
contains
! ------------------------------------------------------------------------------
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
        if (associated(this%m_colormap)) then
            deallocate(this%m_colormap)
            nullify(this%m_colormap)
        end if
    end subroutine

! ------------------------------------------------------------------------------
    module subroutine plt_init(this, term, fname, err)
        ! Arguments
        class(plot), intent(inout) :: this
        integer(int32), intent(in), optional :: term
        character(len = *), intent(in), optional :: fname
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
            if (present(fname)) call png%set_filename(fname)
            this%m_terminal => png
        case (GNUPLOT_TERMINAL_QT)
            allocate(qt, stat = flag)
            this%m_terminal => qt
        case (GNUPLOT_TERMINAL_WIN32)
            allocate(win, stat = flag)
            this%m_terminal => win
        case (GNUPLOT_TERMINAL_LATEX)
            allocate(latex, stat = flag)
            if (present(fname)) call latex%set_filename(fname)
            this%m_terminal => latex
        case default ! WXT is the default
            allocate(wxt, stat = flag)
            this%m_terminal => wxt
        end select

        ! Establish the colormap
        nullify(this%m_colormap)

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
    module function plt_get_title(this) result(txt)
        class(plot), intent(in) :: this
        character(len = :), allocatable :: txt
        integer(int32) :: n
        n = len_trim(this%m_title)
        allocate(character(len = n) :: txt)
        txt = trim(this%m_title)
    end function

! --------------------
    module subroutine plt_set_title(this, txt)
        class(plot), intent(inout) :: this
        character(len = *), intent(in) :: txt
        integer :: n
        n = min(len_trim(txt), PLOTDATA_MAX_NAME_LENGTH)
        this%m_title = ""
        if (n /= 0) then
            this%m_title(1:n) = txt(1:n)
            this%m_hasTitle = .true.
        else
            this%m_hasTitle = .false.
        end if
    end subroutine

! ------------------------------------------------------------------------------
    pure module function plt_has_title(this) result(x)
        class(plot), intent(in) :: this
        logical :: x
        x = this%m_hasTitle
    end function

! ------------------------------------------------------------------------------
    module function plt_get_legend(this) result(x)
        class(plot), intent(in) :: this
        type(legend), pointer :: x
        x => this%m_legend
    end function

! ------------------------------------------------------------------------------
    pure module function plt_get_count(this) result(x)
        class(plot), intent(in) :: this
        integer(int32) :: x
        x = this%m_data%count()
    end function

! ------------------------------------------------------------------------------
    module subroutine plt_push_data(this, x, err)
        ! Arguments
        class(plot), intent(inout) :: this
        class(plot_data), intent(inout) :: x
        class(errors), intent(inout), optional, target :: err
        class(legend), pointer :: lgnd

        ! Index the color tracking index if the type is of plot_data_colored
        select type (x)
        class is (plot_data_colored)
            call x%set_color_index(this%m_colorIndex)
            if (this%m_colorIndex == size(color_list)) then
                this%m_colorIndex = 1
            else
                this%m_colorIndex = this%m_colorIndex + 1
            end if
        end select

        ! Store the object
        call this%m_data%push(x, err = err)
    end subroutine

! ------------------------------------------------------------------------------
    module subroutine plt_pop_data(this)
        ! Arguments
        class(plot), intent(inout) :: this
        class(legend), pointer :: lgnd

        ! Process
        call this%m_data%pop()
        if (this%m_data%count() < 2) then
            lgnd => this%get_legend()
            call lgnd%set_is_visible(.false.)
        end if
    end subroutine

! ------------------------------------------------------------------------------
    module subroutine plt_clear_all(this)
        ! Arguments
        class(plot), intent(inout) :: this

        ! Process
        this%m_colorIndex = 1
        call this%m_data%clear()
    end subroutine

! ------------------------------------------------------------------------------
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
    module subroutine plt_set(this, i, x)
        class(plot), intent(inout) :: this
        integer(int32), intent(in) :: i
        class(plot_data), intent(in) :: x
        call this%m_data%set(i, x)
    end subroutine

! ------------------------------------------------------------------------------
    module function plt_get_term(this) result(x)
        class(plot), intent(in) :: this
        class(terminal), pointer :: x
        x => this%m_terminal
    end function

! ------------------------------------------------------------------------------
    pure module function plt_get_show_grid(this) result(x)
        class(plot), intent(in) :: this
        logical :: x
        x = this%m_showGrid
    end function

! --------------------
    module subroutine plt_set_show_grid(this, x)
        class(plot), intent(inout) :: this
        logical, intent(in) :: x
        this%m_showGrid = x
    end subroutine

! ------------------------------------------------------------------------------
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
        class(terminal), pointer :: term

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
        term => this%get_terminal()

        ! Open the file for writing, and write the contents to file
        open(newunit = fid, file = fname, iostat = flag)
        if (flag > 0) then
            write(errmsg, 100) &
                "The file could not be opened/created.  Error code ", flag, &
                " was encountered."
            call errmgr%report_error("plt_draw", trim(errmsg), &
                PLOT_GNUPLOT_FILE_ERROR)
            return
        end if
        write(fid, '(A)') term%get_command_string()
        write(fid, '(A)') new_line('a')
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
        
100     format(A, I0, A)
    end subroutine

! ------------------------------------------------------------------------------
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
        class(terminal), pointer :: term

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        term => this%get_terminal()

        ! Open the file for writing, and write the contents to file
        open(newunit = fid, file = fname, iostat = flag)
        if (flag > 0) then
            write(errmsg, 100) &
                "The file could not be opened/created.  Error code ", flag, &
                " was encountered."
            call errmgr%report_error("plt_save", trim(errmsg), &
                PLOT_GNUPLOT_FILE_ERROR)
            return
        end if
        write(fid, '(A)') term%get_command_string()
        write(fid, '(A)') new_line('a')
        write(fid, '(A)') this%get_command_string()
        close(fid)
        
100     format(A, I0, A)
    end subroutine

! ------------------------------------------------------------------------------
    module function plt_get_font(this) result(x)
        class(plot), intent(in) :: this
        character(len = :), allocatable :: x
        class(terminal), pointer :: term
        term => this%get_terminal()
        x = term%get_font_name()
    end function

! --------------------
    module subroutine plt_set_font(this, x)
        class(plot), intent(inout) :: this
        character(len = *), intent(in) :: x
        class(terminal), pointer :: term
        term => this%get_terminal()
        call term%set_font_name(x)
    end subroutine

! ------------------------------------------------------------------------------
    module function plt_get_font_size(this) result(x)
        class(plot), intent(in) :: this
        integer(int32) :: x
        class(terminal), pointer :: term
        term => this%get_terminal()
        x = term%get_font_size()
    end function

! --------------------
    module subroutine plt_set_font_size(this, x)
        class(plot), intent(inout) :: this
        integer(int32), intent(in) :: x
        class(terminal), pointer :: term
        term => this%get_terminal()
        call term%set_font_size(x)
    end subroutine

! ------------------------------------------------------------------------------
    pure module function plt_get_tics_in(this) result(x)
        class(plot), intent(in) :: this
        logical :: x
        x = this%m_ticsIn
    end function

! --------------------
    module subroutine plt_set_tics_in(this, x)
        class(plot), intent(inout) :: this
        logical, intent(in) :: x
        this%m_ticsIn = x
    end subroutine

! ------------------------------------------------------------------------------
    pure module function plt_get_draw_border(this) result(x)
        class(plot), intent(in) :: this
        logical :: x
        x = this%m_drawBorder
    end function

! --------------------
    module subroutine plt_set_draw_border(this, x)
        class(plot), intent(inout) :: this
        logical, intent(in) :: x
        this%m_drawBorder = x
    end subroutine

! ******************************************************************************
! ADDED: JUNE 22, 2018 - JAC
! ------------------------------------------------------------------------------
    module subroutine plt_push_label(this, lbl, err)
        ! Arguments
        class(plot), intent(inout) :: this
        class(plot_label), intent(in) :: lbl
        class(errors), intent(inout), optional, target :: err

        ! Process
        call this%m_labels%push(lbl, err = err)
    end subroutine

! ------------------------------------------------------------------------------
    module subroutine plt_pop_label(this)
        class(plot), intent(inout) :: this
        call this%m_labels%pop()
    end subroutine

! ------------------------------------------------------------------------------
    module function plt_get_label(this, i) result(x)
        ! Arguments
        class(plot), intent(in) :: this
        integer(int32), intent(in) :: i
        class(plot_label), pointer :: x
        
        ! Local Variables
        class(*), pointer :: item

        ! Process
        item => this%m_labels%get(i)
        select type (item)
        class is (plot_label)
            x => item
        class default
            nullify(x)
        end select
    end function

! --------------------
    module subroutine plt_set_label(this, i, x)
        class(plot), intent(inout) :: this
        integer(int32), intent(in) :: i
        class(plot_label), intent(in) :: x
        call this%m_labels%set(i, x)
    end subroutine

! ------------------------------------------------------------------------------
    pure module function plt_get_label_count(this) result(x)
        class(plot), intent(in) :: this
        integer(int32) :: x
        x = this%m_labels%count()
    end function

! ------------------------------------------------------------------------------
    module subroutine plt_clear_labels(this)
        class(plot), intent(inout) :: this
        call this%m_labels%clear()
    end subroutine

! ******************************************************************************
! ADDED: SEPT. 25, 2020 - JAC
! ------------------------------------------------------------------------------
    pure module function plt_get_axis_equal(this) result(rst)
        class(plot), intent(in) :: this
        logical :: rst
        rst = this%m_axisEqual
    end function

! --------------------
    module subroutine plt_set_axis_equal(this, x)
        class(plot), intent(inout) :: this
        logical, intent(in) :: x
        this%m_axisEqual = x
    end subroutine

! ******************************************************************************
! ADDED: OCT. 8, 2020 - JAC
! ------------------------------------------------------------------------------
    module function plt_get_colormap(this) result(x)
        class(plot), intent(in) :: this
        class(colormap), pointer :: x
        x => this%m_colormap
    end function

! --------------------
    module subroutine plt_set_colormap(this, x, err)
        ! Arguments
        class(plot), intent(inout) :: this
        class(colormap), intent(in) :: x
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

        ! Process
        if (associated(this%m_colormap)) deallocate(this%m_colormap)
        allocate(this%m_colormap, stat = flag, source = x)
        if (flag /= 0) then
            call errmgr%report_error("surf_set_colormap", &
                "Insufficient memory available.", PLOT_OUT_OF_MEMORY_ERROR)
            return
        end if
    end subroutine

! ------------------------------------------------------------------------------
    pure module function plt_get_show_colorbar(this) result(x)
        class(plot), intent(in) :: this
        logical :: x
        x = this%m_showColorbar
    end function

! --------------------
    module subroutine plt_set_show_colorbar(this, x)
        class(plot), intent(inout) :: this
        logical, intent(in) :: x
        this%m_showColorbar = x
    end subroutine

! ------------------------------------------------------------------------------
    module function plt_get_cmd(this) result(x)
        ! Arguments
        class(plot), intent(in) :: this
        character(len = :), allocatable :: x

        ! Local Variables
        integer(int32) :: i
        type(string_builder) :: str
        class(colormap), pointer :: clr
        class(plot_arrow), pointer :: arrow
        class(plot_label), pointer :: lbl

        ! Initialization
        call str%initialize()

        ! Define the colormap
        clr => this%get_colormap()
        if (associated(clr)) then
            call str%append(new_line('a'))
            call str%append(clr%get_command_string())
        end if

        ! Show the colorbar
        if (.not.this%get_show_colorbar()) then
            call str%append(new_line('a'))
            call str%append("unset colorbox")
        end if

        ! Arrows
        do i = 1, this%get_arrow_count()
            arrow => this%get_arrow(i)
            if (.not.associated(arrow)) cycle
            call str%append(new_line('a'))
            call str%append(arrow%get_command_string())
        end do

        ! Labels
        do i = 1, this%get_label_count()
            lbl => this%get_label(i)
            if (.not.associated(lbl)) cycle
            call str%append(new_line('a'))
            call str%append(lbl%get_command_string())
        end do

        ! End
        x = char(str%to_string())
    end function

! ******************************************************************************
! ADDED: 1/3/2024 - JAC
! ------------------------------------------------------------------------------
    module subroutine plt_push_arrow(this, x, err)
        class(plot), intent(inout) :: this
        class(plot_arrow), intent(in) :: x
        class(errors), intent(inout), optional, target :: err
        call this%m_arrows%push(x, manage = .true., err = err)
    end subroutine

! ------------------------------------------------------------------------------
    module subroutine plt_pop_arrow(this)
        class(plot), intent(inout) :: this
        call this%m_arrows%pop()
    end subroutine

! ------------------------------------------------------------------------------
    module function plt_get_arrow(this, i) result(rst)
        class(plot), intent(in) :: this
        integer(int32), intent(in) :: i
        class(plot_arrow), pointer :: rst
        
        class(*), pointer :: ptr
        ptr => this%m_arrows%get(i)
        select type (ptr)
        class is (plot_arrow)
            rst => ptr
        class default
            nullify(rst)
        end select
    end function

! ------------------------------------------------------------------------------
    module subroutine plt_set_arrow(this, i, x)
        class(plot), intent(inout) :: this
        integer(int32), intent(in) :: i
        class(plot_arrow), intent(in) :: x
        call this%m_arrows%set(i, x)
    end subroutine

! ------------------------------------------------------------------------------
    pure module function plt_get_arrow_count(this) result(rst)
        class(plot), intent(in) :: this
        integer(int32) :: rst
        rst = this%m_arrows%count()
    end function

! ------------------------------------------------------------------------------
    module subroutine plt_clear_arrows(this)
        class(plot), intent(inout) :: this
        call this%m_arrows%clear()
    end subroutine

! ------------------------------------------------------------------------------
end submodule