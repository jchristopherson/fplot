! fplot_multiplot.f90

module fplot_multiplot
    use iso_fortran_env
    use fplot_plot_object
    use fplot_plot
    use fplot_terminal
    use fplot_windows_terminal
    use fplot_qt_terminal
    use fplot_wxt_terminal
    use fplot_png_terminal
    use fplot_latex_terminal
    use fplot_constants
    use fplot_errors
    use collections
    use ferror
    use strings
    implicit none
    private
    public :: multiplot

    type, extends(plot_object) :: multiplot
        !! Defines a multi-plot layout.
        type(list), private :: m_plots
            !! The collection of plot objects.
        integer(int32), private :: m_rows = 0
            !! The number of rows of plots.
        integer(int32), private :: m_cols = 0
            !! The number of columns of plots.
        character(len = PLOTDATA_MAX_NAME_LENGTH), private :: m_title
            !! The page title.
        logical, private :: m_hasTitle = .false.
            !! Has a title?
        class(terminal), pointer :: m_terminal => null()
            !! The GNUPLOT terminal object to target.
    contains
        final :: mp_clean
        procedure, public :: get_command_string => mp_get_command
        procedure, public :: initialize => mp_init
        procedure, public :: get_row_count => mp_get_rows
        procedure, public :: get_column_count => mp_get_cols
        procedure, public :: get_plot_count => mp_get_count
        procedure, public :: get_title => mp_get_title
        procedure, public :: set_title => mp_set_title
        procedure, public :: draw => mp_draw
        procedure, public :: get => mp_get
        procedure, public :: set => mp_set
        procedure, public :: is_title_defined => mp_has_title
        procedure, public :: get_terminal => mp_get_term
        procedure, public :: save_file => mp_save
        procedure, public :: get_font_name => mp_get_font
        procedure, public :: set_font_name => mp_set_font
        procedure, public :: get_font_size => mp_get_font_size
        procedure, public :: set_font_size => mp_set_font_size
    end type

contains
! ------------------------------------------------------------------------------
    function mp_get_command(this) result(x)
        !! Gets the GNUPLOT commands for this object.
        class(multiplot), intent(in) :: this
            !! The multiplot object.
        character(len = :), allocatable :: x
            !! The command string.

        ! Local Variables
        type(string_builder) :: str
        integer(int32) :: i, j, m, n
        class(plot), pointer :: ptr

        ! Initialization
        call str%initialize()
        m = this%get_row_count()
        n = this%get_column_count()

        ! Set up the multiplot
        call str%append("set multiplot layout ")
        call str%append(to_string(m))
        call str%append(",")
        call str%append(to_string(n))
        call str%append(" columnsfirst")
        if (this%is_title_defined()) then
            call str%append(" title ")
            call str%append('"')
            call str%append(this%get_title())
            call str%append('"')
        end if
        call str%append(new_line('a'))

        ! Write commands for each plot object
        do j = 1, n
            do i = 1, m
                ptr => this%get(i, j)
                call str%append(new_line('a'))
                call str%append(ptr%get_command_string())
            end do
        end do

        ! Close out the multiplot
        call str%append(new_line('a'))
        call str%append("unset multiplot")

        ! Get the string
        x = char(str%to_string())
    end function

! ------------------------------------------------------------------------------
    subroutine mp_init(this, m, n, term, err)
        !! Initializes the multiplot object.
        class(multiplot), intent(inout) :: this
            !! The multiplot object.
        integer(int32), intent(in) :: m
            !! The number of rows of plots.
        integer(int32), intent(in) :: n
            !! The number of columns of plots.
        integer(int32), intent(in), optional :: term
            !! An optional input that is used to define the terminal.  The 
            !! default terminal is a WXT terminal.  The acceptable inputs are:
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
        class(errors), intent(inout), optional, target :: err
            !! An error handling object.

        ! Local Variables
        integer(int32) :: flag, t, i
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
        call this%m_plots%clear()
        this%m_rows = m
        this%m_cols = n
        flag = 0

        ! Populate the list with a dummy variable at the outset.  This allows
        ! the list to be appropriately sized so the user may use the "set"
        ! subroutine appropriately
        do i = 1, m * n
            call this%m_plots%push(i)
        end do

        ! Define the terminal
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

        ! Error Checking
        if (flag /= 0) then
            call report_memory_error(errmgr, "mp_init", flag)
            return
        end if
    end subroutine
    
! ------------------------------------------------------------------------------
    subroutine mp_clean(this)
        !! Cleans up resources held by the multiplot object.
        type(multiplot), intent(inout) :: this
            !! The multiplot object.
        if (associated(this%m_terminal)) deallocate(this%m_terminal)
        nullify(this%m_terminal)
    end subroutine

! ------------------------------------------------------------------------------
    pure function mp_get_rows(this) result(x)
        !! Gets the number of rows of plots.
        class(multiplot), intent(in) :: this
            !! The multiplot object.
        integer(int32) :: x
            !! The row count.
        x = this%m_rows
    end function

! --------------------
    pure function mp_get_cols(this) result(x)
        !! Gets the number of columns of plots.
        class(multiplot), intent(in) :: this
            !! The multiplot object.
        integer(int32) :: x
            !! The column count.
        x = this%m_cols
    end function

! --------------------
    pure function mp_get_count(this) result(x)
        !! Gets the total number of plots.
        class(multiplot), intent(in) :: this
            !! The multiplot object.
        integer(int32) :: x
            !! The plot count.
        x = this%m_plots%count()
    end function
    
! ------------------------------------------------------------------------------
    function mp_get_title(this) result(x)
        !! Gets the multiplot's title.
        class(multiplot), intent(in) :: this
            !! The multiplot object.
        character(len = :), allocatable :: x
            !! The title.
        x = this%m_title
    end function

! --------------------
    subroutine mp_set_title(this, x)
        !! Sets the multiplot's title.
        class(multiplot), intent(inout) :: this
            !! The multiplot object.
        character(len = *), intent(in) :: x
            !! The title.

        ! Local Variables
        integer(int32) :: n

        ! Process
        n = min(len(x), PLOTDATA_MAX_NAME_LENGTH)
        this%m_title = ""
        if (n /= 0) then
            this%m_title(1:n) = x(1:n)
            this%m_hasTitle = .true.
        else
            this%m_hasTitle = .false.
        end if
    end subroutine
    
! ------------------------------------------------------------------------------
    subroutine mp_draw(this, persist, err)
        !! Launches GNUPLOT and draws the multiplot per the current state of
        !! the command list.
        class(multiplot), intent(in) :: this
            !! The multiplot object.
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
            call report_file_create_error(errmgr, "mp_draw", fname, flag)
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
    function mp_get(this, i, j) result(x)
        !! Gets the requested plot object.
        class(multiplot), intent(in) :: this
            !! The multiplot object.
        integer(int32), intent(in) :: i
            !! The row index of the plot to retrieve.
        integer(int32), intent(in) :: j
            !! The column index of the plot to retrieve.
        class(plot), pointer :: x
            !! A pointer to the plot object.

        ! Local Variables
        class(*), pointer :: item
        integer(int32) :: ind

        ! Process
        ind = this%m_rows * (j - 1) + i
        item => this%m_plots%get(ind)
        select type (item)
        class is (plot)
            x => item
        class default
            nullify(x)
        end select
    end function

! --------------------
    subroutine mp_set(this, i, j, x)
        !! Replaces the specified plot.
        class(multiplot), intent(inout) :: this
            !! The multiplot object.
        integer(int32), intent(in) :: i
            !! The row index of the plot to replace.
        integer(int32), intent(in) :: j
            !! The column index of the plot to replace.
        class(plot), intent(in) :: x
            !! The new plot.

        ! Local Variables
        integer(int32) :: ind

        ! Process
        ind = this%m_rows * (j - 1) + i
        call this%m_plots%set(ind, x)
    end subroutine
    
! ------------------------------------------------------------------------------
    pure function mp_has_title(this) result(x)
        !! Gets a value determining if a title has been defined for the
        !! multiplot object.
        class(multiplot), intent(in) :: this
            !! The multiplot object.
        logical :: x
            !! Returns true if a title has been defined for this multiplot; 
            !! else, returns false.
        x = this%m_hasTitle
    end function

! ------------------------------------------------------------------------------
    function mp_get_term(this) result(x)
        !! Gets the GNUPLOT terminal object.
        class(multiplot), intent(in) :: this
            !! The multiplot object.
        class(terminal), pointer :: x
            !! A pointer to the terminal object.
        x => this%m_terminal
    end function

! ------------------------------------------------------------------------------
    subroutine mp_save(this, fname, err)
        !! Saves a GNUPLOT command file.
        class(multiplot), intent(in) :: this
            !! The multiplot object.
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
            call report_file_create_error(errmgr, "mp_save", fname, flag)
            return
        end if
        write(fid, '(A)') term%get_command_string()
        write(fid, '(A)') new_line('a')
        write(fid, '(A)') this%get_command_string()
        close(fid)
    end subroutine

! ------------------------------------------------------------------------------
    function mp_get_font(this) result(x)
        !! Gets the name of the font used for plot text.
        class(multiplot), intent(in) :: this
            !! The multiplot object.
        character(len = :), allocatable :: x
            !! The font name.
        class(terminal), pointer :: term
        term => this%get_terminal()
        x = term%get_font_name()
    end function

! --------------------
    subroutine mp_set_font(this, x)
        !! Sets the name of the font used for plot text.
        class(multiplot), intent(inout) :: this
            !! The multiplot object.
        character(len = *), intent(in) :: x
            !! The font name.
        class(terminal), pointer :: term
        term => this%get_terminal()
        call term%set_font_name(x)
    end subroutine

! ------------------------------------------------------------------------------
    function mp_get_font_size(this) result(x)
        !! Gets the size of the font used by the plot.
        class(multiplot), intent(in) :: this
            !! The multiplot object.
        integer(int32) :: x
            !! The font size.
        class(terminal), pointer :: term
        term => this%get_terminal()
        x = term%get_font_size()
    end function

! --------------------
    subroutine mp_set_font_size(this, x)
        !! Sets the size of the font used by the plot.
        class(multiplot), intent(inout) :: this
            !! The multiplot object.
        integer(int32), intent(in) :: x
            !! The font size.
        class(terminal), pointer :: term
        term => this%get_terminal()
        call term%set_font_size(x)
    end subroutine

! ------------------------------------------------------------------------------
end module
