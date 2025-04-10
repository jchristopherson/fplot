module fplot_stats_plots
    use iso_fortran_env
    use fplot_plot_object
    use fplot_plot
    use fplot_plot_data_2d
    use fplot_plot_data_histogram
    use fplot_plot_2d
    use fplot_multiplot
    use fplot_terminal
    use fplot_constants
    use fplot_errors
    use fplot_colors
    use fplot_plot_axis
    use collections
    use strings
    use ferror
    implicit none
    private
    public :: correlation_plot

    type, extends(plot_object) :: correlation_plot
        !! Defines a multiplot arrangement designed to illustrate correlation
        !! between data sets.
        type(multiplot), private :: m_plt
            !! The multiplot object.
    contains
        procedure, public :: get_command_string => cp_get_command
        procedure, public :: initialize => cp_init
        procedure, public :: get_row_count => cp_get_rows
        procedure, public :: get_column_count => cp_get_cols
        procedure, public :: get_plot_count => cp_get_count
        procedure, public :: draw => cp_draw
        procedure, public :: save_file => cp_save
        procedure, public :: get => cp_get
        procedure, public :: get_terminal => cp_get_term
        procedure, public :: get_font_name => cp_get_font
        procedure, public :: set_font_name => cp_set_font
        procedure, public :: get_font_size => cp_get_font_size
        procedure, public :: set_font_size => cp_set_font_size
    end type

contains
! ------------------------------------------------------------------------------
    function cp_get_command(this) result(x)
        !! Gets the GNUPLOT commands for this object.
        class(correlation_plot), intent(in) :: this
            !! The correlation_plot object.
        character(len = :), allocatable :: x
            !! The command string.
    end function

! ------------------------------------------------------------------------------
    subroutine cp_init(this, x, labels, term, width, height, err)
        !! Initializes the correlation_plot object.
        class(correlation_plot), intent(inout) :: this
            !! The correlation_plot object.
        real(real64), intent(in), dimension(:,:) :: x
            !! The data to plot with each column representing a data set.
        type(string), intent(in), optional, dimension(:) :: labels
            !! An optional array containing a label to associate with each
            !! data set in x.  If supplied, this array must have the same length
            !! as x has columns.
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
        integer(int32), intent(in), optional :: width
            !! Optionally, the width of the plot window.
        integer(int32), intent(in), optional :: height
            !! Optionally, the height of the plot window.
        class(errors), intent(inout), optional, target :: err
            !! An error handling object.

        ! Local Variables
        integer(int32) :: i, j, k, t, n, flag
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        type(plot_2d), allocatable, dimension(:) :: plts
        type(plot_data_2d) :: pdata
        type(plot_data_histogram) :: hdata
        class(plot_axis), pointer :: xAxis, yAxis
        
        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        n = size(x, 2)
        call this%m_plt%initialize(n, n, term = term, width = width, &
            height = height, err = errmgr)
        if (errmgr%has_error_occurred()) return
        allocate(plts(n * n), stat = flag)
        if (flag /= 0) then
            call report_memory_error(errmgr, "cp_init", flag)
            return
        end if
        call this%m_plt%set_font_size(11)    ! use a small font size

        ! Input Checking
        if (present(labels)) then
            if (size(labels) /= n) then
                call report_array_size_mismatch_error(errmgr, "cp_init", &
                    "labels", n, size(labels))
                return
            end if
        end if

        ! Create plots
        k = 0
        call pdata%set_draw_line(.false.)
        call pdata%set_draw_markers(.true.)
        call pdata%set_marker_style(MARKER_FILLED_CIRCLE)
        call pdata%set_marker_scaling(0.5)
        if (errmgr%has_error_occurred()) return
        do j = 1, n
            do i = 1, n
                k = k + 1
                call plts(k)%initialize(err = errmgr)
                if (errmgr%has_error_occurred()) return
                if (i == j) then
                    ! Plot a histogram of the data
                    call hdata%define_data(x(:,i), err = errmgr)
                    if (errmgr%has_error_occurred()) return
                    call plts(k)%push(hdata)
                else
                    ! Plot a scatter plot
                    call pdata%define_data(x(:,j), x(:,i), err = errmgr)
                    if (errmgr%has_error_occurred()) return
                    call plts(k)%push(pdata)
                end if

                ! Deal with axis labels
                if (j == 1) then
                    ! Display y axis labels for these plots
                    yAxis => plts(k)%get_y_axis()
                    if (present(labels)) then
                        call yAxis%set_title(char(labels(i)))
                    else
                        call yAxis%set_title(char("x_{" // to_string(i) // "}"))
                    end if
                end if

                ! Get an x-axis object for the plot
                xAxis => plts(k)%get_x_axis()

                ! Define axis labels
                if (i == n) then
                    ! Display x axis labels for these plots
                    if (present(labels)) then
                        call xAxis%set_title(char(labels(j)))
                    else
                        call xAxis%set_title(char("x_{" // to_string(j) // "}"))
                    end if
                end if

                ! Rotate histogram tic labels
                call xAxis%set_tic_label_angle(45.0)
                call xAxis%set_tic_label_rotation_origin(GNUPLOT_ROTATION_ORIGIN_RIGHT)

                ! Store the plot - the collection makes a copy of the plot and
                ! manages it's lifetime
                call this%m_plt%set(i, j, plts(k))
            end do
        end do
    end subroutine

! ------------------------------------------------------------------------------
    pure function cp_get_rows(this) result(x)
        !! Gets the number of rows of plots.
        class(correlation_plot), intent(in) :: this
            !! The correlation_plot object.
        integer(int32) :: x
            !! The row count.

        x = this%m_plt%get_row_count()
    end function

! --------------------
    pure function cp_get_cols(this) result(x)
        !! Gets the number of columns of plots.
        class(correlation_plot), intent(in) :: this
            !! The correlation_plot object.
        integer(int32) :: x
            !! The column count.

        x = this%m_plt%get_column_count()
    end function

! --------------------
    pure function cp_get_count(this) result(x)
        !! Gets the total number of plots.
        class(correlation_plot), intent(in) :: this
            !! The correlation_plot object.
        integer(int32) :: x
            !! The plot count.

        x = this%m_plt%get_plot_count()
    end function
    
! ------------------------------------------------------------------------------
    subroutine cp_draw(this, persist, err)
        !! Launches GNUPLOT and draws the correlation_plot per the current 
        !! state of the command list.
        class(correlation_plot), intent(in) :: this
            !! The correlation_plot object.
        logical, intent(in), optional :: persist
            !! An optional parameter that can be used to keep GNUPLOT open.  
            !! Set to true to force GNUPLOT to remain open; else, set to false
            !! to allow GNUPLOT to close after drawing.  The default is true.
        class(errors), intent(inout), optional, target :: err
                !! An error handling object.
        
        call this%m_plt%draw(persist, err)
    end subroutine

! ------------------------------------------------------------------------------
    subroutine cp_save(this, fname, err)
        !! Saves a GNUPLOT command file.
        class(correlation_plot), intent(in) :: this
            !! The correlation_plot object.
        character(len = *), intent(in) :: fname
            !! The filename.
        class(errors), intent(inout), optional, target :: err
            !! An error handling object.

        call this%m_plt%save_file(fname, err)
    end subroutine

! ------------------------------------------------------------------------------
    function cp_get(this, i, j) result(x)
        !! Gets the requested plot object.
        class(correlation_plot), intent(in) :: this
            !! The correlation_plot object.
        integer(int32), intent(in) :: i
            !! The row index of the plot to retrieve.
        integer(int32), intent(in) :: j
            !! The column index of the plot to retrieve.
        class(plot), pointer :: x
            !! A pointer to the plot object.

        x => this%m_plt%get(i, j)
    end function

! ------------------------------------------------------------------------------
    function cp_get_term(this) result(x)
        !! Gets the GNUPLOT terminal object.
        class(correlation_plot), intent(in) :: this
            !! The correlation_plot object.
        class(terminal), pointer :: x
            !! A pointer to the terminal object.
        x => this%m_plt%get_terminal()
    end function

! ------------------------------------------------------------------------------
    function cp_get_font(this) result(x)
        !! Gets the name of the font used for plot text.
        class(correlation_plot), intent(in) :: this
            !! The correlation_plot object.
        character(len = :), allocatable :: x
            !! The font name.
        x = this%m_plt%get_font_name()
    end function

! --------------------
    subroutine cp_set_font(this, x)
        !! Sets the name of the font used for plot text.
        class(correlation_plot), intent(inout) :: this
            !! The correlation_plot object.
        character(len = *), intent(in) :: x
            !! The font name.
        call this%m_plt%set_font_name(x)
    end subroutine

! ------------------------------------------------------------------------------
    function cp_get_font_size(this) result(x)
        !! Gets the size of the font used by the plot.
        class(correlation_plot), intent(in) :: this
            !! The correlation_plot object.
        integer(int32) :: x
            !! The font size.
        x = this%m_plt%get_font_size()
    end function

! --------------------
    subroutine cp_set_font_size(this, x)
        !! Sets the size of the font used by the plot.
        class(correlation_plot), intent(inout) :: this
            !! The correlation_plot object.
        integer(int32), intent(in) :: x
            !! The font size.
        call this%m_plt%set_font_size(x)
    end subroutine

! ------------------------------------------------------------------------------
end module