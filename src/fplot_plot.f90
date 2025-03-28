! fplot_plot.f90

module fplot_plot
    use iso_fortran_env
    use fplot_plot_object
    use fplot_plot_data
    use fplot_terminal
    use fplot_windows_terminal
    use fplot_qt_terminal
    use fplot_wxt_terminal
    use fplot_png_terminal
    use fplot_latex_terminal
    use fplot_colormap
    use fplot_colors
    use fplot_errors
    use fplot_constants
    use fplot_legend
    use fplot_label
    use fplot_arrow
    use ferror
    use strings
    use collections
    implicit none
    private
    public :: plot

    type, extends(plot_object) :: plot
        !! Defines the basic GNUPLOT plot.
        character(len = PLOTDATA_MAX_NAME_LENGTH), private :: m_title = ""
            !! The plot title.
        logical, private :: m_hasTitle = .false.
            !! Has a title?
        class(terminal), private, pointer :: m_terminal => null()
            !! The GNUPLOT terminal object to target.
        type(list), private :: m_data
            !! A collection of plot_data items to plot.
        type(legend), private, pointer :: m_legend => null()
            !! The legend.
        logical, private :: m_showGrid = .true.
            !! Show grid lines?
        logical, private :: m_ticsIn = .true.
            !! Point tic marks in?
        logical, private :: m_drawBorder = .true.
            !! Draw the border?
        type(list), private :: m_labels ! Added 6/22/2018, JAC
            !! A collection of plot_label items to draw.
        integer(int32), private :: m_colorIndex = 1
            !! The color index to use for automatic line coloring for scatter plots.
        logical, private :: m_axisEqual = .false.
            !! Determines if the axes should be scaled proportionally.
        class(colormap), private, pointer :: m_colormap
            !! The colormap.
        logical, private :: m_showColorbar = .true.
            !! Show the colorbar?
        type(list), private :: m_arrows ! Added 1/3/2024, JAC
            !! A collection of plot_arrow items to draw.
    contains
        procedure, public :: free_resources => plt_clean_up
        procedure, public :: initialize => plt_init
        procedure, public :: get_title => plt_get_title
        procedure, public :: set_title => plt_set_title
        procedure, public :: is_title_defined => plt_has_title
        procedure, public :: get_legend => plt_get_legend
        procedure, public :: get_count => plt_get_count
        procedure, public :: push => plt_push_data
        procedure, public :: pop => plt_pop_data
        procedure, public :: clear_all => plt_clear_all
        procedure, public :: get => plt_get
        procedure, public :: set => plt_set
        procedure, public :: get_terminal => plt_get_term
        procedure, public :: get_show_gridlines => plt_get_show_grid
        procedure, public :: set_show_gridlines => plt_set_show_grid
        procedure, public :: draw => plt_draw
        procedure, public :: save_file => plt_save
        procedure, public :: get_font_name => plt_get_font
        procedure, public :: set_font_name => plt_set_font
        procedure, public :: get_font_size => plt_get_font_size
        procedure, public :: set_font_size => plt_set_font_size
        procedure, public :: get_tics_inward => plt_get_tics_in
        procedure, public :: set_tics_inward => plt_set_tics_in
        procedure, public :: get_draw_border => plt_get_draw_border
        procedure, public :: set_draw_border => plt_set_draw_border
        procedure, public :: push_label => plt_push_label
        procedure, public :: pop_label => plt_pop_label
        procedure, public :: get_label => plt_get_label
        procedure, public :: set_label => plt_set_label
        procedure, public :: get_label_count => plt_get_label_count
        procedure, public :: clear_all_labels => plt_clear_labels
        procedure, public :: get_axis_equal => plt_get_axis_equal
        procedure, public :: set_axis_equal => plt_set_axis_equal
        procedure, public :: get_colormap => plt_get_colormap
        procedure, public :: set_colormap => plt_set_colormap
        procedure, public :: get_show_colorbar => plt_get_show_colorbar
        procedure, public :: set_show_colorbar => plt_set_show_colorbar
        procedure, public :: get_command_string => plt_get_cmd
        procedure, public :: push_arrow => plt_push_arrow
        procedure, public :: pop_arrow => plt_pop_arrow
        procedure, public :: get_arrow => plt_get_arrow
        procedure, public :: set_arrow => plt_set_arrow
        procedure, public :: get_arrow_count => plt_get_arrow_count
        procedure, public :: clear_arrows => plt_clear_arrows
    end type

contains
! ------------------------------------------------------------------------------
    subroutine plt_clean_up(this)
        !! Cleans up resources held by the plot object.  Inheriting
        !! classes are expected to call this routine to free internally held
        !! resources.
        class(plot), intent(inout) :: this
            !! The plot object.
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
    subroutine plt_init(this, term, fname, err)
        !! Initializes the plot object.
        class(plot), intent(inout) :: this
            !! The plot object.
        integer(int32), intent(in), optional :: term
            !! An optional input that is used to define the terminal.
            !! The default terminal is a WXT terminal.  The acceptable inputs 
            !! are:
            !!
            !!  - GNUPLOT_TERMINAL_PNG
            !!
            !!  - GNUPLOT_TERMINAL_QT
            !!
            !!  - GNUPLOT_TERMINAL_WIN32
            !!
            !!  - GNUPLOT_TERMINAL_WXT
            !!
            !!  - GNUPLOT_TERMINAL_LATEX
        character(len = *), intent(in), optional :: fname
            !! A filename to pass to the terminal in the event the
            !! terminal is a file type (e.g. GNUPLOT_TERMINAL_PNG).
        class(errors), intent(inout), optional, target :: err
            !! An error handling object.

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
            call report_memory_error(errmgr, "plt_init", flag)
            return
        end if
    end subroutine

! ------------------------------------------------------------------------------
    function plt_get_title(this) result(txt)
        !! Gets the plot's title.
        class(plot), intent(in) :: this
            !! The plot object.
        character(len = :), allocatable :: txt
            !! The title.
        integer(int32) :: n
        n = len_trim(this%m_title)
        allocate(character(len = n) :: txt)
        txt = trim(this%m_title)
    end function

! --------------------
    subroutine plt_set_title(this, txt)
        !! Sets the plot's title.
        class(plot), intent(inout) :: this
            !! The plot object.
        character(len = *), intent(in) :: txt
            !! The title.
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
    pure function plt_has_title(this) result(x)
        !! Gets a value determining if a title has been defined for the plot 
        !! object.
        class(plot), intent(in) :: this
            !! The plot object.
        logical :: x
            !! Returns true if a title has been defined for this plot; else,
            !! returns false.
        x = this%m_hasTitle
    end function

! ------------------------------------------------------------------------------
    function plt_get_legend(this) result(x)
        !! Gets the plot's legend object.
        class(plot), intent(in) :: this
            !! The plot object.
        type(legend), pointer :: x
            !! A pointer to the legend object.
        x => this%m_legend
    end function

! ------------------------------------------------------------------------------
    pure function plt_get_count(this) result(x)
        !! Gets the number of stored plot_data objects.
        class(plot), intent(in) :: this
            !! The plot object.
        integer(int32) :: x
            !! The number of plot_data objects.
        x = this%m_data%count()
    end function

! ------------------------------------------------------------------------------
    subroutine plt_push_data(this, x, err)
        !! Pushes a plot_data object onto the stack.
        class(plot), intent(inout) :: this
            !! The plot object.
        class(plot_data), intent(inout) :: x
            !! The plot_data object.
        class(errors), intent(inout), optional, target :: err
            !! An error handling object.

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
    subroutine plt_pop_data(this)
        !! Pops the last plot_data object from the stack.
        class(plot), intent(inout) :: this
            !! The plot object.

        ! Process
        call this%m_data%pop()
    end subroutine

! ------------------------------------------------------------------------------
    subroutine plt_clear_all(this)
        !! Removes all plot_data objects from the plot.
        class(plot), intent(inout) :: this
            !! The plot object.

        ! Process
        this%m_colorIndex = 1
        call this%m_data%clear()
    end subroutine

! ------------------------------------------------------------------------------
    function plt_get(this, i) result(x)
        !! Gets a pointer to the requested plot_data object.
        class(plot), intent(in) :: this
            !! The plot object.
        integer(int32), intent(in) :: i
            !! The index of the plot_data object.
        class(plot_data), pointer :: x
            !! A pointer to the requested plot_data object.

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
    subroutine plt_set(this, i, x)
        !! Sets the requested plot_data object into the plot.
        class(plot), intent(inout) :: this
            !! The plot object.
        integer(int32), intent(in) :: i
            !! The index of the plot_data object.
        class(plot_data), intent(in) :: x
            !! The plot_data object.
        call this%m_data%set(i, x)
    end subroutine

! ------------------------------------------------------------------------------
    function plt_get_term(this) result(x)
        !! Gets the GNUPLOT terminal object.
        class(plot), intent(in) :: this
            !! The plot object.
        class(terminal), pointer :: x
            !! A pointer to the GNUPLOT terminal object.
        x => this%m_terminal
    end function

! ------------------------------------------------------------------------------
    pure function plt_get_show_grid(this) result(x)
        !! Gets a flag determining if the grid lines should be shown.
        class(plot), intent(in) :: this
            !! The plot object.
        logical :: x
            !! Returns true if the grid lines should be shown; else, false.
        x = this%m_showGrid
    end function

! --------------------
    subroutine plt_set_show_grid(this, x)
        !! Sets a flag determining if the grid lines should be shown.
        class(plot), intent(inout) :: this
            !! The plot object.
        logical, intent(in) :: x
            !! Set to true if the grid lines should be shown; else, false.
        this%m_showGrid = x
    end subroutine

! ------------------------------------------------------------------------------
    subroutine plt_draw(this, persist, err)
        !! Launches GNUPLOT and draws the plot per the current state of
        !! the command list.
        class(plot), intent(in) :: this
            !! The plot object.
        logical, intent(in), optional :: persist
            !! An optional parameter that can be used to keep GNUPLOT open.  
            !! Set to true to force GNUPLOT to remain open; else, set to false 
            !! to allow GNUPLOT to close after drawing.  The default is true.
        class(errors), intent(inout), optional, target :: err
            !! An error handling object.

        ! Parameters
        character(len = *), parameter :: fname = "temp_gnuplot_file.plt"

        ! Local Variables
        logical :: p
        integer(int32) :: fid, flag
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
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
            call report_file_create_error(errmgr, "plt_draw", fname, flag)
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
    end subroutine

! ------------------------------------------------------------------------------
    subroutine plt_save(this, fname, err)
        !! Saves a GNUPLOT command file.
        class(plot), intent(in) :: this
            !! The plot object.
        character(len = *), intent(in) :: fname
            !! The filename.
        class(errors), intent(inout), optional, target :: err
            !! An error handling object.

        ! Local Variables
        integer(int32) :: fid, flag
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
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
            call report_file_create_error(errmgr, "plt_save", fname, flag)
            return
        end if
        write(fid, '(A)') term%get_command_string()
        write(fid, '(A)') new_line('a')
        write(fid, '(A)') this%get_command_string()
        close(fid)
    end subroutine

! ------------------------------------------------------------------------------
    function plt_get_font(this) result(x)
        !! Gets the name of the font used for plot text.
        class(plot), intent(in) :: this
            !! The plot object.
        character(len = :), allocatable :: x
            !! The font name.
        class(terminal), pointer :: term
        term => this%get_terminal()
        x = term%get_font_name()
    end function

! --------------------
    subroutine plt_set_font(this, x)
        !! Sets the name of the font used for plot text.
        class(plot), intent(inout) :: this
            !! The plot object.
        character(len = *), intent(in) :: x
            !! The font name.
        class(terminal), pointer :: term
        term => this%get_terminal()
        call term%set_font_name(x)
    end subroutine

! ------------------------------------------------------------------------------
    function plt_get_font_size(this) result(x)
        !! Gets the size of the font used by the plot.
        class(plot), intent(in) :: this
            !! The plot object.
        integer(int32) :: x
            !! The size of the font, in points.
        class(terminal), pointer :: term
        term => this%get_terminal()
        x = term%get_font_size()
    end function

! --------------------
    subroutine plt_set_font_size(this, x)
        !! Sets the size of the font used by the plot.
        class(plot), intent(inout) :: this
            !! The plot object.
        integer(int32), intent(in) :: x
            !! The font size, in points.  If a value of zero is provided,
            !! the font size is reset to its default value; or, if a negative 
            !! value is provided, the absolute value of the supplied value is
            !! utilized.
        class(terminal), pointer :: term
        term => this%get_terminal()
        call term%set_font_size(x)
    end subroutine

! ------------------------------------------------------------------------------
    pure function plt_get_tics_in(this) result(x)
        !! Gets a value determining if the axis tic marks should point inwards.
        class(plot), intent(in) :: this
            !! The plot object.
        logical :: x
            !! Returns true if the tic marks should point inwards; else, false
            !! if the tic marks should point outwards.
        x = this%m_ticsIn
    end function

! --------------------
    subroutine plt_set_tics_in(this, x)
        !! Sets a value determining if the axis tic marks should point inwards.
        class(plot), intent(inout) :: this
            !! The plot object.
        logical, intent(in) :: x
            !! Set to true if the tic marks should point inwards; else, false
            !! if the tic marks should point outwards.
        this%m_ticsIn = x
    end subroutine

! ------------------------------------------------------------------------------
    pure function plt_get_draw_border(this) result(x)
        !! Gets a value determining if the border should be drawn.
        class(plot), intent(in) :: this
            !! The plot object.
        logical :: x
            !! Returns true if the border should be drawn; else, false.
        x = this%m_drawBorder
    end function

! --------------------
    subroutine plt_set_draw_border(this, x)
        !! Sets a value determining if the border should be drawn.
        class(plot), intent(inout) :: this
            !! The plot object.
        logical, intent(in) :: x
            !! Set to true if the border should be drawn; else, false.
        this%m_drawBorder = x
    end subroutine

! ******************************************************************************
! ADDED: JUNE 22, 2018 - JAC
! ------------------------------------------------------------------------------
    subroutine plt_push_label(this, lbl, err)
        !! Adds a label to the plot.
        class(plot), intent(inout) :: this
            !! The plot object.
        class(plot_label), intent(in) :: lbl
            !! The plot label.
        class(errors), intent(inout), optional, target :: err
            !! An error handling object.

        ! Process
        call this%m_labels%push(lbl, err = err)
    end subroutine

! ------------------------------------------------------------------------------
    subroutine plt_pop_label(this)
        !! Removes the last label from the plot.
        class(plot), intent(inout) :: this
            !! The plot object.
        call this%m_labels%pop()
    end subroutine

! ------------------------------------------------------------------------------
    function plt_get_label(this, i) result(x)
        !! Gets the requested plot_label from the plot.
        class(plot), intent(in) :: this
            !! The plot object.
        integer(int32), intent(in) :: i
            !! The index of the plot_label object to retrieve.
        class(plot_label), pointer :: x
            !! A pointer to the requested plot_label object.
        
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
    subroutine plt_set_label(this, i, x)
        !! Sets the specified plot_label object.
        class(plot), intent(inout) :: this
            !! The plot object.
        integer(int32), intent(in) :: i
            !! The index of the plot_label to replace.
        class(plot_label), intent(in) :: x
            !! The new plot_label object.
        call this%m_labels%set(i, x)
    end subroutine

! ------------------------------------------------------------------------------
    pure function plt_get_label_count(this) result(x)
        !! Gets the number of plot_label objects belonging to the plot.
        class(plot), intent(in) :: this
            !! The plot object.
        integer(int32) :: x
            !! The number of plot_label objects.
        x = this%m_labels%count()
    end function

! ------------------------------------------------------------------------------
    subroutine plt_clear_labels(this)
        !! Clears all plot_label objects from the plot.
        class(plot), intent(inout) :: this
            !! The plot object.
        call this%m_labels%clear()
    end subroutine

! ******************************************************************************
! ADDED: SEPT. 25, 2020 - JAC
! ------------------------------------------------------------------------------
    pure function plt_get_axis_equal(this) result(rst)
        !! Gets a flag determining if the axes should be equally scaled.
        class(plot), intent(in) :: this
            !! The plot object.
        logical :: rst
            !! Returns true if the axes should be scaled equally; else, false.
        rst = this%m_axisEqual
    end function

! --------------------
    subroutine plt_set_axis_equal(this, x)
        !! Sets a flag determining if the axes should be equally scaled.
        class(plot), intent(inout) :: this
            !! The plot object.
        logical, intent(in) :: x
            !! Set to true if the axes should be scaled equally; else, false.
        this%m_axisEqual = x
    end subroutine

! ******************************************************************************
! ADDED: OCT. 8, 2020 - JAC
! ------------------------------------------------------------------------------
    function plt_get_colormap(this) result(x)
        !! Gets a pointer to the colormap object.
        class(plot), intent(in) :: this
            !! The plot object.
        class(colormap), pointer :: x
            !! A pointer to the colormap object.  If no colormap is defined, a
            !! null pointer is returned.
        x => this%m_colormap
    end function

! --------------------
    subroutine plt_set_colormap(this, x, err)
        !! Sets the colormap object.
        class(plot), intent(inout) :: this
            !! The plot object.
        class(colormap), intent(in) :: x
            !! The colormap object.  Notice, a copy of this object is
            !! stored, and the plot object then manages the lifetime of the
            !! copy.
        class(errors), intent(inout), optional, target :: err
            !! An error handler object.

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
    pure function plt_get_show_colorbar(this) result(x)
        !! Gets a value determining if the colorbar should be shown.
        class(plot), intent(in) :: this
            !! The plot object.
        logical :: x
            !! Returns true if the colorbar should be drawn; else, false.
        x = this%m_showColorbar
    end function

! --------------------
    subroutine plt_set_show_colorbar(this, x)
        !! Sets a value determining if the colorbar should be shown.
        class(plot), intent(inout) :: this
            !! The plot object.
        logical, intent(in) :: x
            !! Set to true if the colorbar should be drawn; else, false.
        this%m_showColorbar = x
    end subroutine

! ------------------------------------------------------------------------------
    function plt_get_cmd(this) result(x)
        !! Gets the GNUPLOT command string to represent this plot object.
        class(plot), intent(in) :: this
            !! The plot object.
        character(len = :), allocatable :: x
            !! The command string.

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
    subroutine plt_push_arrow(this, x, err)
        !! Pushes a new @ref plot_arrow object onto the plot.
        class(plot), intent(inout) :: this
            !! The plot object.
        class(plot_arrow), intent(in) :: x
            !! The plot_arrow object.
        class(errors), intent(inout), optional, target :: err
            !! An error handling object.
        call this%m_arrows%push(x, manage = .true., err = err)
    end subroutine

! ------------------------------------------------------------------------------
    subroutine plt_pop_arrow(this)
        !! Pops the last plot_arrow object from the plot.
        class(plot), intent(inout) :: this
            !! The plot object.
        call this%m_arrows%pop()
    end subroutine

! ------------------------------------------------------------------------------
    function plt_get_arrow(this, i) result(rst)
        !! Gets a pointer to the requested plot_arrow object.
        class(plot), intent(in) :: this
            !! The plot object.
        integer(int32), intent(in) :: i
            !! The index of the plot_arrow to retrieve.
        class(plot_arrow), pointer :: rst
            !! The plot_arrow object to retrieve.
        
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
    subroutine plt_set_arrow(this, i, x)
        !! Sets a plot_arrow into the plot.
        class(plot), intent(inout) :: this
            !! The plot object.
        integer(int32), intent(in) :: i
            !! The index of the plot_arrow object to replace.
        class(plot_arrow), intent(in) :: x
            !! The new plot_arrow object.
        call this%m_arrows%set(i, x)
    end subroutine

! ------------------------------------------------------------------------------
    pure function plt_get_arrow_count(this) result(rst)
        !! Gets the number of plot_arrow objects held by the plot object.
        class(plot), intent(in) :: this
            !! The plot object.
        integer(int32) :: rst
            !! The plot_arrow objects count.
        rst = this%m_arrows%count()
    end function

! ------------------------------------------------------------------------------
    subroutine plt_clear_arrows(this)
        !! Clears all plot_arrow objects from the plot.
        class(plot), intent(inout) :: this
            !! The plot object.
        call this%m_arrows%clear()
    end subroutine

! ------------------------------------------------------------------------------
end module