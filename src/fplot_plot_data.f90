! fplot_plot_data.f90

module fplot_plot_data
    use iso_fortran_env
    use fplot_plot_object
    use fplot_constants
    use fplot_colors
    use strings
    use ferror
    use fplot_errors
    implicit none
    private
    public :: plot_data
    public :: pd_get_string_result
    public :: plot_data_colored
    public :: scatter_plot_data
    public :: spd_get_int_value
    public :: spd_get_string_result
    public :: spd_get_value
    public :: spd_set_value

    type, abstract, extends(plot_object) :: plot_data
        !! A container for plot data.
    private
        character(len = PLOTDATA_MAX_NAME_LENGTH) :: m_name = ""
            !! The name to associate with the data set.
    contains
        procedure, public :: get_name => pd_get_name
        procedure, public :: set_name => pd_set_name
        procedure(pd_get_string_result), deferred, public :: get_data_string
    end type

    interface
        function pd_get_string_result(this) result(x)
            !! Retrieves a string from a plot_data object.
            import plot_data
            class(plot_data), intent(in) :: this
                !! The plot_data object.
            character(len = :), allocatable :: x
                !! The string.
        end function
    end interface

    type, abstract, extends(plot_data) :: plot_data_colored
        !! Defines a colored plot data set.
    private
        type(color) :: m_color = CLR_BLUE
            !! The line color.
        logical :: m_useAutoColor = .true.
            !! Let the object choose colors automatically?
        integer(int32) :: m_colorIndex = 1
            !! The color index to use, assuming we're using auto color
    contains
        procedure, public :: get_line_color => pdc_get_line_color
        procedure, public :: set_line_color => pdc_set_line_color
        procedure, public :: get_color_index => pdc_get_color_index
        procedure, public :: set_color_index => pdc_set_color_index
    end type

    type, abstract, extends(plot_data_colored) :: scatter_plot_data
        !! A plot_data object for describing scatter plot data sets.
    private
        logical :: m_drawLine = .true.
            !! Draw a line connecting the dots?
        logical :: m_drawMarkers = .false.
            !! Draw the markers?
        integer(int32) :: m_markerFrequency = 1
            !! Marker frequency.
        real(real32) :: m_lineWidth = 1.0
            !! Line width.
        integer(int32) :: m_lineStyle = LINE_SOLID
            !! Line style.
        integer(int32) :: m_markerType = MARKER_FILLED_CIRCLE
            !! Marker type.
        real(real32) :: m_markerSize = 0.5
            !! Marker size multiplier.
        logical :: m_simplifyData = .true.
            !! True if large data sets should be simplified before sending to
            !! GNUPLOT.
        real(real64) :: m_simplifyFactor = 1.0d-3
            !! A scaling factor used to establish the simplification tolerance.
            !! The simplification tolerance is established by multiplying this
            !! factor by the range in the dependent variable data.
        logical :: m_dataDependentColors = .false.
            !! Determines if the data should utilize data-dependent colors.
        logical :: m_filledCurve = .false.
            !! Fill the curve?
        logical :: m_useVariableSizePoints = .false.
            !! Use variable size data points?
    contains
        procedure, public :: get_command_string => spd_get_cmd
        procedure, public :: get_line_width => spd_get_line_width
        procedure, public :: set_line_width => spd_set_line_width
        procedure, public :: get_line_style => spd_get_line_style
        procedure, public :: set_line_style => spd_set_line_style
        procedure, public :: get_draw_line => spd_get_draw_line
        procedure, public :: set_draw_line => spd_set_draw_line
        procedure, public :: get_draw_markers => spd_get_draw_markers
        procedure, public :: set_draw_markers => spd_set_draw_markers
        procedure, public :: get_marker_style => spd_get_marker_style
        procedure, public :: set_marker_style => spd_set_marker_style
        procedure, public :: get_marker_scaling => spd_get_marker_scaling
        procedure, public :: set_marker_scaling => spd_set_marker_scaling
        procedure, public :: get_marker_frequency => spd_get_marker_frequency
        procedure, public :: set_marker_frequency => spd_set_marker_frequency
        procedure(spd_get_int_value), deferred, public :: get_count
        procedure(spd_get_value), deferred, public :: get_x
        procedure(spd_set_value), deferred, public :: set_x
        procedure(spd_get_value), deferred, public :: get_y
        procedure(spd_set_value), deferred, public :: set_y
        procedure(spd_get_string_result), deferred, public :: get_axes_string
        procedure, public :: get_simplify_data => spd_get_simplify_data
        procedure, public :: set_simplify_data => spd_set_simplify_data
        procedure, public :: get_simplification_factor => spd_get_simplify_factor
        procedure, public :: set_simplification_factor => spd_set_simplify_factor
        procedure, public :: get_use_data_dependent_colors => &
            spd_get_data_dependent_colors
        procedure, public :: set_use_data_dependent_colors => &
            spd_set_data_dependent_colors
        procedure, public :: get_fill_curve => spd_get_filled
        procedure, public :: set_fill_curve => spd_set_filled
        procedure, public :: get_use_variable_size_points => spd_get_use_var_point_size
        procedure, public :: set_use_variable_size_points => spd_set_use_var_point_size
    end type

    interface
        pure function spd_get_value(this, index) result(x)
            !! Gets an indexed value from the scatter_plot_data object.
            use, intrinsic :: iso_fortran_env, only : int32, real64
            import scatter_plot_data
            class(scatter_plot_data), intent(in) :: this
                !! The scatter_plot_data object.
            integer(int32), intent(in) :: index
                !! The index.
            real(real64) :: x
                !! The value.
        end function

        subroutine spd_set_value(this, index, x)
            !! Sets an indexed value from the scatter_plot_data object.
            use, intrinsic :: iso_fortran_env, only : int32, real64
            import scatter_plot_data
            class(scatter_plot_data), intent(inout) :: this
                !! The scatter_plot_data object.
            integer(int32), intent(in) :: index
                !! The index.
            real(real64), intent(in) :: x
                !! The value.
        end subroutine

        pure function spd_get_int_value(this) result(x)
            !! Gets an integer value from the scatter_plot_data object.
            use, intrinsic :: iso_fortran_env, only : int32
            import scatter_plot_data
            class(scatter_plot_data), intent(in) :: this
                !! The scatter_plot_data object.
            integer(int32) :: x
                !! The value.
        end function

        function spd_get_string_result(this) result(x)
            !! Gets a string value from the scatter_plot_data object.
            import scatter_plot_data
            class(scatter_plot_data), intent(in) :: this
                !! The scatter_plot_data object.
            character(len = :), allocatable :: x
                !! The string.
        end function
    end interface

contains
! ------------------------------------------------------------------------------
    pure function pd_get_name(this) result(txt)
        !! Gets the name to associate with this data set.
        class(plot_data), intent(in) :: this
            !! The plot_data object.
        character(len = :), allocatable :: txt
            !! The name.
        txt = trim(this%m_name)
    end function

! --------------------
    subroutine pd_set_name(this, txt)
        !! Sets the name to associate with this data set.
        class(plot_data), intent(inout) :: this
            !! The plot_data object.
        character(len = *), intent(in) :: txt
            !! The name.
        integer(int32) :: n
        n = min(len(txt), PLOTDATA_MAX_NAME_LENGTH)
        this%m_name = ""
        if (n /= 0) then
            this%m_name(1:n) = txt(1:n)
        end if
    end subroutine

! ******************************************************************************
! PLOT_DATA_COLORED
! ------------------------------------------------------------------------------
    pure function pdc_get_line_color(this) result(x)
        !! Gets the object color.
        class(plot_data_colored), intent(in) :: this
            !! The plot_data_colored object.
        type(color) :: x
            !! The color.
        if (this%m_useAutoColor) then
            x = color_list(this%get_color_index())
        else
            x = this%m_color
        end if
    end function

! --------------------
    subroutine pdc_set_line_color(this, x)
        !! Sets the object color.
        class(plot_data_colored), intent(inout) :: this
            !! The plot_data_colored object.
        type(color), intent(in) :: x
            !! The color.
        this%m_color = x
        this%m_useAutoColor = .false.
    end subroutine

! ------------------------------------------------------------------------------
    pure function pdc_get_color_index(this) result(x)
        !! Gets the color index.
        class(plot_data_colored), intent(in) :: this
            !! The plot_data_colored object.
        integer(int32) :: x
            !! The index value.
        x = this%m_colorIndex
    end function

! --------------------
    subroutine pdc_set_color_index(this, x)
        !! Sets the color index.
        class(plot_data_colored), intent(inout) :: this
            !! The plot_data_colored object.
        integer(int32), intent(in) :: x
            !! The index value.
        this%m_colorIndex = x
    end subroutine

! ******************************************************************************
! SCATTER_PLOT_DATA
! ------------------------------------------------------------------------------
    function spd_get_cmd(this) result(x)
        !! Gets the GNUPLOT command string to represent this
        !! scatter_plot_data object.
        class(scatter_plot_data), intent(in) :: this
            !! The scatter_plot_data object.
        character(len = :), allocatable :: x
            !! The command string.

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

        ! Lines, points, or filled
        if (this%get_fill_curve()) then
            call str%append(" with filledcurves")
        else
            if (this%get_draw_line() .and. this%get_draw_markers()) then
                call str%append(" with linespoints")
            else if (.not.this%get_draw_line() .and. this%get_draw_markers()) then
                call str%append(" with points")
            else
                call str%append(" with lines")
            end if
        end if

        ! Line Width
        call str%append(" lw ")
        call str%append(to_string(this%get_line_width()))

        ! Line Color
        if (this%get_use_data_dependent_colors()) then
            ! http://www.gnuplotting.org/using-a-palette-as-line-color/
            call str%append(" lc palette")
        else
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
            if (this%get_use_variable_size_points()) then
                call str%append("variable")
            else
                call str%append(to_string(this%get_marker_scaling()))
            end if
        end if

        ! Define the axes structure
        call str%append(" ")
        call str%append(this%get_axes_string())

        ! End
        x = char(str%to_string())
    end function

! ------------------------------------------------------------------------------
    pure function spd_get_line_width(this) result(x)
        !! Gets the width of the line, in pixels.
        class(scatter_plot_data), intent(in) :: this
            !! The scatter_plot_data object.
        real(real32) :: x
            !! The line width.
        x = this%m_lineWidth
    end function

! --------------------
    subroutine spd_set_line_width(this, x)
        !! Sets the width of the line, in pixels.
        class(scatter_plot_data), intent(inout) :: this
            !! The scatter_plot_data object.
        real(real32), intent(in) :: x
            !! The line width.
        this%m_lineWidth = x
    end subroutine

! ------------------------------------------------------------------------------
    pure function spd_get_line_style(this) result(x)
        !! Gets the line style.
        class(scatter_plot_data), intent(in) :: this
            !! The scatter_plot_data object.
        integer(int32) :: x
            !! The line style.  The line style must be one of the following.
            !!
            !!  - LINE_DASHED
            !!
            !!  - LINE_DASH_DOTTED
            !!
            !!  - LINE_DASH_DOT_DOT
            !!
            !!  - LINE_DOTTED
            !!
            !!  - LINE_SOLID
        x = this%m_lineStyle
    end function

! --------------------
    subroutine spd_set_line_style(this, x)
        !! Sets the line style.
        class(scatter_plot_data), intent(inout) :: this
            !! The scatter_plot_data object.
        integer(int32), intent(in) :: x
            !! The line style.  The line style must be one of the following.
            !!
            !!  - LINE_DASHED
            !!
            !!  - LINE_DASH_DOTTED
            !!
            !!  - LINE_DASH_DOT_DOT
            !!
            !!  - LINE_DOTTED
            !!
            !!  - LINE_SOLID
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
    pure function spd_get_draw_line(this) result(x)
        !! Gets a value determining if a line should be drawn.
        class(scatter_plot_data), intent(in) :: this
            !! The scatter_plot_data object.
        logical :: x
            !! Returns true if the line should be drawn; else, false.
        x = this%m_drawLine
    end function

! --------------------
    subroutine spd_set_draw_line(this, x)
        !! Sets a value determining if a line should be drawn.
        class(scatter_plot_data), intent(inout) :: this
            !! The scatter_plot_data object.
        logical, intent(in) :: x
            !! Set to true if the line should be drawn; else, false.
        this%m_drawLine = x
    end subroutine

! ------------------------------------------------------------------------------
    pure function spd_get_draw_markers(this) result(x)
        !! Gets a value determining if data point markers should be drawn.
        class(scatter_plot_data), intent(in) :: this
            !! The scatter_plot_data object.
        logical :: x
            !! Returns true if the markers should be drawn; else, false.
        x = this%m_drawMarkers
    end function

! --------------------
    subroutine spd_set_draw_markers(this, x)
        !! Sets a value determining if data point markers should be drawn.
        class(scatter_plot_data), intent(inout) :: this
            !! The scatter_plot_data object.
        logical, intent(in) :: x
            !! Set to true if the markers should be drawn; else, false.
        this%m_drawMarkers = x
    end subroutine

! ------------------------------------------------------------------------------
    pure function spd_get_marker_style(this) result(x)
        !! Gets the marker style.
        class(scatter_plot_data), intent(in) :: this
            !! The scatter_plot_data object.
        integer(int32) :: x
            !! The marker type.  The marker type must be one of the following:
            !!
            !!  - MARKER_ASTERISK
            !!
            !!  - MARKER_EMPTY_CIRCLE
            !!
            !!  - MARKER_EMPTY_NABLA
            !!
            !!  - MARKER_EMPTY_RHOMBUS
            !!
            !!  - MARKER_EMPTY_SQUARE
            !!
            !!  - MARKER_EMPTY_TRIANGLE
            !!
            !!  - MARKER_FILLED_CIRCLE
            !!
            !!  - MARKER_FILLED_NABLA
            !!
            !!  - MARKER_FILLED_RHOMBUS
            !!
            !!  - MARKER_FILLED_SQUARE
            !!
            !!  - MARKER_FILLED_TRIANGLE
            !!
            !!  - MARKER_PLUS
            !!
            !!  - MARKER_X
        x = this%m_markerType
    end function

! --------------------
    subroutine spd_set_marker_style(this, x)
        !! Sets the marker style.
        class(scatter_plot_data), intent(inout) :: this
            !! The scatter_plot_data object.
        integer(int32), intent(in) :: x
            !! The marker type.  The marker type must be one of the following:
            !!
            !!  - MARKER_ASTERISK
            !!
            !!  - MARKER_EMPTY_CIRCLE
            !!
            !!  - MARKER_EMPTY_NABLA
            !!
            !!  - MARKER_EMPTY_RHOMBUS
            !!
            !!  - MARKER_EMPTY_SQUARE
            !!
            !!  - MARKER_EMPTY_TRIANGLE
            !!
            !!  - MARKER_FILLED_CIRCLE
            !!
            !!  - MARKER_FILLED_NABLA
            !!
            !!  - MARKER_FILLED_RHOMBUS
            !!
            !!  - MARKER_FILLED_SQUARE
            !!
            !!  - MARKER_FILLED_TRIANGLE
            !!
            !!  - MARKER_PLUS
            !!
            !!  - MARKER_X
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
    pure function spd_get_marker_scaling(this) result(x)
        !! Gets the marker scaling.
        class(scatter_plot_data), intent(in) :: this
            !! The scatter_plot_data object.
        real(real32) :: x
            !! The scaling factor.
        x = this%m_markerSize
    end function

! --------------------
    subroutine spd_set_marker_scaling(this, x)
        !! Sets the marker scaling.
        class(scatter_plot_data), intent(inout) :: this
            !! The scatter_plot_data object.
        real(real32), intent(in) :: x
            !! The scaling factor.
        this%m_markerSize = x
    end subroutine

! ------------------------------------------------------------------------------
    pure function spd_get_marker_frequency(this) result(x)
        !! Gets the marker frequency.
        class(scatter_plot_data), intent(in) :: this
            !! The scatter_plot_data object.
        integer(int32) :: x
            !! The marker frequency.
        x = this%m_markerFrequency
    end function

! --------------------
    subroutine spd_set_marker_frequency(this, x)
        !! Sets the marker frequency.
        class(scatter_plot_data), intent(inout) :: this
            !! The scatter_plot_data object.
        integer(int32), intent(in) :: x
            !! The marker frequency.
        this%m_markerFrequency = x
    end subroutine

! ------------------------------------------------------------------------------
    pure function spd_get_simplify_data(this) result(x)
        !! Gets a value determining if the stored data should be
        !! simplified (reduced) before passing to GNUPLOT.
        class(scatter_plot_data), intent(in) :: this
            !! The scatter_plot_data object.
        logical :: x
            !! True if the data should be simplified prior to sending
            !! to GNUPLOT; else, false to leave the data alone.
        x = this%m_simplifyData
    end function

! --------------------
    subroutine spd_set_simplify_data(this, x)
        !! Sets a value determining if the stored data should be
        !! simplified (reduced) before passing to GNUPLOT.
        class(scatter_plot_data), intent(inout) :: this
            !! The scatter_plot_data object.
        logical, intent(in) :: x
            !! True if the data should be simplified prior to sending
            !! to GNUPLOT; else, false to leave the data alone.
        this%m_simplifyData = x
    end subroutine

! ------------------------------------------------------------------------------
    pure function spd_get_simplify_factor(this) result(x)
        !! Gets a factor used to establish the simplification tolerance.
        class(scatter_plot_data), intent(in) :: this
            !! The scatter_plot_data object.
        real(real64) :: x
            !! The scaling factor.
        x = this%m_simplifyFactor
    end function

! --------------------
    subroutine spd_set_simplify_factor(this, x)
        !! Sets a factor used to establish the simplification tolerance.
        class(scatter_plot_data), intent(inout) :: this
            !! The scatter_plot_data object.
        real(real64), intent(in) :: x
            !! The scaling factor.
        this%m_simplifyFactor = x
    end subroutine

! ------------------------------------------------------------------------------
    pure function spd_get_data_dependent_colors(this) result(rst)
        !! Gets a value determing if data-dependent colors should be used.
        class(scatter_plot_data), intent(in) :: this
            !! The scatter_plot_data object.
        logical :: rst
            !! True if data-dependent colors should be used; else, false.
        rst = this%m_dataDependentColors
    end function

! --------------------
    subroutine spd_set_data_dependent_colors(this, x)
        !! Sets a value determing if data-dependent colors should be used.
        class(scatter_plot_data), intent(inout) :: this
            !! The scatter_plot_data object.
        logical, intent(in) :: x
            !! True if data-dependent colors should be used; else, false.
        this%m_dataDependentColors = x
    end subroutine

! ******************************************************************************
! ADDED: JUNE 28, 2021 - JAC
! ------------------------------------------------------------------------------
    pure function spd_get_filled(this) result(rst)
        !! Gets a logical value determining if a filled curve should be drawn.
        class(scatter_plot_data), intent(in) :: this
            !! The scatter_plot_data object.
        logical :: rst
            !! True if the curve should be filled; else, false.
        rst = this%m_filledCurve
    end function

! --------------------
    subroutine spd_set_filled(this, x)
        !! Sets a logical value determining if a filled curve should be drawn.
        class(scatter_plot_data), intent(inout) :: this
            !! The scatter_plot_data object.
        logical, intent(in) :: x
            !! True if the curve should be filled; else, false.
        this%m_filledCurve = x
    end subroutine

! ******************************************************************************
! ADDED: JAN 12, 2024 - JAC
! ------------------------------------------------------------------------------
    pure function spd_get_use_var_point_size(this) result(rst)
        !! Gets a logical value determining if variable sized data points
        !! should be used.  The default is false, such that points will be of
        !! a constant size.
        class(scatter_plot_data), intent(in) :: this
            !! The scatter_plot_data object.
        logical :: rst
            !! True if variable size points should be used; else, false.
        rst = this%m_useVariableSizePoints
    end function

! --------------------
    subroutine spd_set_use_var_point_size(this, x)
        !! Sets a logical value determining if variable sized data points
        !! should be used.  The default is false, such that points will be of
        !! a constant size.
        class(scatter_plot_data), intent(inout) :: this
            !! The scatter_plot_data object.
        logical, intent(in) :: x
            !! True if variable size points should be used; else, false.
        this%m_useVariableSizePoints = x
    end subroutine

! ------------------------------------------------------------------------------
end module