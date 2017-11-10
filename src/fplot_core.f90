! fplot_core.f90

!> @brief \b fplot_core
!!
!! @par Purpose
!! Provides types and routines specific necessary to support plotting
!! operations.
module fplot_core
    use, intrinsic :: iso_fortran_env, only : real64, real32, int32
    use strings
    use fplot_list
    use fplot_errors
    use ferror, only : errors
    implicit none
    private
    public :: GNUPLOT_TERMINAL_WIN32
    public :: GNUPLOT_TERMINAL_WXT
    public :: GNUPLOT_TERMINAL_QT
    public :: GNUPLOT_TERMINAL_PNG
    public :: CLR_BLACK
    public :: CLR_WHITE
    public :: CLR_RED
    public :: CLR_LIME
    public :: CLR_BLUE
    public :: CLR_YELLOW
    public :: CLR_CYAN
    public :: CLR_MAGENTA
    public :: CLR_SILVER
    public :: CLR_GRAY
    public :: CLR_MAROON
    public :: CLR_OLIVE
    public :: CLR_GREEN
    public :: CLR_PURPLE
    public :: CLR_TEAL
    public :: CLR_NAVY
    public :: MARKER_PLUS
    public :: MARKER_X
    public :: MARKER_ASTERISK
    public :: MARKER_EMPTY_SQUARE
    public :: MARKER_FILLED_SQUARE
    public :: MARKER_EMPTY_CIRCLE
    public :: MARKER_FILLED_CIRCLE
    public :: MARKER_EMPTY_TRIANGLE
    public :: MARKER_FILLED_TRIANGLE
    public :: MARKER_EMPTY_NABLA
    public :: MARKER_FILLED_NABLA
    public :: MARKER_EMPTY_RHOMBUS
    public :: MARKER_FILLED_RHOMBUS
    public :: LINE_SOLID
    public :: LINE_DASHED
    public :: LINE_DOTTED
    public :: LINE_DASH_DOTTED
    public :: LINE_DASH_DOT_DOT
    public :: LEGEND_CENTER
    public :: LEGEND_LEFT
    public :: LEGEND_RIGHT
    public :: LEGEND_TOP
    public :: LEGEND_BOTTOM
    public :: PALETTE_RAINBOW
    public :: PALETTE_HOT
    public :: PALETTE_COOL
    public :: PALETTE_DARK
    public :: PALETTE_GREENS
    public :: PALETTE_BLUES
    public :: PALETTE_REDS
    public :: PLOTDATA_MAX_NAME_LENGTH
    public :: color
    public :: plot_data
    public :: plot_axis
    public :: terminal
    public :: windows_terminal
    public :: qt_terminal
    public :: wxt_terminal
    public :: png_terminal
    public :: legend
    public :: plot
    public :: scatter_plot_data
    public :: scatter_plot

! ******************************************************************************
! GNUPLOT TERMINAL CONSTANTS
! ------------------------------------------------------------------------------
    !> @brief Defines a Win32 terminal.
    integer(int32), parameter :: GNUPLOT_TERMINAL_WIN32 = 1
    !> @brief Defines a WXT terminal.
    integer(int32), parameter :: GNUPLOT_TERMINAL_WXT = 2
    !> @brief Defines a QT terminal.
    integer(int32), parameter :: GNUPLOT_TERMINAL_QT = 3
    !> @brief Defines a PNG terminal.
    integer(int32), parameter :: GNUPLOT_TERMINAL_PNG = 4

! ******************************************************************************
! MARKER CONSTANTS
! ------------------------------------------------------------------------------
    !> @brief Defines a + data point marker.
    integer(int32), parameter :: MARKER_PLUS = 1
    !> @brief Defines an x data point marker.
    integer(int32), parameter :: MARKER_X = 2
    !> @brief Defines an * data point marker.
    integer(int32), parameter :: MARKER_ASTERISK = 3
    !> @brief Defines an empty square-shaped data point marker.
    integer(int32), parameter :: MARKER_EMPTY_SQUARE = 4
    !> @brief Defines an filled square-shaped data point marker.
    integer(int32), parameter :: MARKER_FILLED_SQUARE = 5
    !> @brief Defines an empty circle-shaped data point marker.
    integer(int32), parameter :: MARKER_EMPTY_CIRCLE = 6
    !> @brief Defines an filled circle-shaped data point marker.
    integer(int32), parameter :: MARKER_FILLED_CIRCLE = 7
    !> @brief Defines an empty triangle-shaped data point marker.
    integer(int32), parameter :: MARKER_EMPTY_TRIANGLE = 8
    !> @brief Defines an filled triangle-shaped data point marker.
    integer(int32), parameter :: MARKER_FILLED_TRIANGLE = 9
    !> @brief Defines an empty nabla-shaped data point marker.
    integer(int32), parameter :: MARKER_EMPTY_NABLA = 10
    !> @brief Defines an filled nabla-shaped data point marker.
    integer(int32), parameter :: MARKER_FILLED_NABLA = 11
    !> @brief Defines an empty rhombus-shaped data point marker.
    integer(int32), parameter :: MARKER_EMPTY_RHOMBUS = 12
    !> @brief Defines an filled rhombus-shaped data point marker.
    integer(int32), parameter :: MARKER_FILLED_RHOMBUS = 13

! ******************************************************************************
! LINE CONSTANTS
! ------------------------------------------------------------------------------
    !> @brief Defines a solid line.
    integer(int32), parameter :: LINE_SOLID = 1
    !> @brief Defines a dashed line.
    integer(int32), parameter :: LINE_DASHED = 2
    !> @brief Defines a dotted line.
    integer(int32), parameter :: LINE_DOTTED = 3
    !> @brief Defines a dash-dotted line.
    integer(int32), parameter :: LINE_DASH_DOTTED = 4
    !> @brief Defines a dash-dot-dotted line.
    integer(int32), parameter :: LINE_DASH_DOT_DOT = 5

! ******************************************************************************
! PALETTE CONSTANTS
! ------------------------------------------------------------------------------
    !> @brief Defines a rainbow palette for 3D surface plots.
    integer(int32), parameter :: PALETTE_RAINBOW = 1
    !> @brief Defines a hot-colored palette for 3D surface plots.
    integer(int32), parameter :: PALETTE_HOT = 2
    !> @brief Defines a cool-colored palette for 3D surface plots.
    integer(int32), parameter :: PALETTE_COOL = 3
    !> @brief Defines a dark themed palette for 3D surface plots
    integer(int32), parameter :: PALETTE_DARK = 4
    !> @brief Defines a green-colored palette for 3D surface plots.
    integer(int32), parameter :: PALETTE_GREENS = 5
    !> @brief Defines a blue-colored palette for 3D surface plots.
    integer(int32), parameter :: PALETTE_BLUES = 6
    !> @brief Defines a red-colored palette for 3D surface plots.
    integer(int32), parameter :: PALETTE_REDS = 7

! ******************************************************************************
! LEGEND CONSTANTS
! ------------------------------------------------------------------------------
    !> @brief Defines the legend should be placed at the top of the plot.
    character(len = *), parameter :: LEGEND_TOP = "top"
    !> @brief Defines the legend should be centered on the plot.
    character(len = *), parameter :: LEGEND_CENTER = "center"
    !> @brief Defines the legend should be placed at the left of the plot.
    character(len = *), parameter :: LEGEND_LEFT = "left"
    !> @brief Defines the legend should be placed at the right of the plot.
    character(len = *), parameter :: LEGEND_RIGHT = "right"
    !> @brief Defines the legend should be placed at the bottom of the plot.
    character(len = *), parameter :: LEGEND_BOTTOM = "bottom"

! ******************************************************************************
! PLOT DATA CONSTANTS
! ------------------------------------------------------------------------------
    !> @brief Defines the maximum number of characters allowed in a graph label.
    integer(int32), parameter :: PLOTDATA_MAX_NAME_LENGTH = 128

! ******************************************************************************
! PRIVATE/DEFAULT CONSTANTS
! ------------------------------------------------------------------------------
    !> @brief The default GNUPLOT window width, in pixels.
    integer(int32), parameter :: GNUPLOT_DEFAULT_WINDOW_WIDTH = 640
    !> @brief The default GNUPLOT window height, in pixels.
    integer(int32), parameter :: GNUPLOT_DEFAULT_WINDOW_HEIGHT = 420
    !> @brief Defines the maximum number of characters allowed in a graph label.
    integer(int32), parameter :: GNUPLOT_MAX_LABEL_LENGTH = 128
    !> @brief Defines the default font used by text on the graph.
    character(len = *), parameter :: GNUPLOT_DEFAULT_FONTNAME = "Calibri"
    !> @brief Defines the default font size used by text on the graph.
    integer(int32), parameter :: GNUPLOT_DEFAULT_FONT_SIZE = 10
    !> @brief Defines the maximum number of characters allowed in a file path.
    integer(int32), parameter :: GNUPLOT_MAX_PATH_LENGTH = 256

! ******************************************************************************
! COLORS
! ------------------------------------------------------------------------------
    !> @brief Describes an RGB color.
    type color
        !> @brief The red component of the color (must be between 0 and 255).
        integer(int32) :: red = 0
        !> @brief The green component of the color (must be between 0 and 255).
        integer(int32) :: green = 0
        !> @brief The blue component of the color (must be between 0 and 255).
        integer(int32) :: blue = 255
    contains
        !> @brief Returns the color in hexadecimal format.
        procedure, pass :: to_hex_string => clr_to_hex_string
        !> @brief Copies another color to this color.
        procedure, pass :: copy_from => clr_copy_from
    end type

! ------------------------------------------------------------------------------
    !> @brief Defines a black color.
    type(color), parameter :: CLR_BLACK = color(0, 0, 0)
    !> @brief Defines a white color.
    type(color), parameter :: CLR_WHITE = color(255, 255, 255)
    !> @brief Defines a red color.
    type(color), parameter :: CLR_RED = color(255, 0, 0)
    !> @brief Defines a lime color.
    type(color), parameter :: CLR_LIME = color(0, 255, 0)
    !> @brief Defines a blue color.
    type(color), parameter :: CLR_BLUE = color(0, 0, 255)
    !> @brief Defines a yellow color.
    type(color), parameter :: CLR_YELLOW = color(255, 255, 0)
    !> @brief Defines a cyan color.
    type(color), parameter :: CLR_CYAN = color(0, 255, 255)
    !> @brief Defines a magenta color.
    type(color), parameter :: CLR_MAGENTA = color(255, 0, 255)
    !> @brief Defines a silver color.
    type(color), parameter :: CLR_SILVER = color(192, 192, 192)
    !> @brief Defines a gray color.
    type(color), parameter :: CLR_GRAY = color(128, 128, 128)
    !> @brief Defines a maroon color.
    type(color), parameter :: CLR_MAROON = color(128, 0, 0)
    !> @brief Defines a olive color.
    type(color), parameter :: CLR_OLIVE = color(128, 128, 0)
    !> @brief Defines a green color.
    type(color), parameter :: CLR_GREEN = color(0, 128, 0)
    !> @brief Defines a purple color.
    type(color), parameter :: CLR_PURPLE = color(128, 0, 128)
    !> @brief Defines a teal color.
    type(color), parameter :: CLR_TEAL = color(0, 128, 128)
    !> @brief Defines a navy color.
    type(color), parameter :: CLR_NAVY = color(0, 0, 128)

! ******************************************************************************
! TYPES
! ------------------------------------------------------------------------------
    !> @brief The base type for a GNUPLOT object.
    type, abstract :: plot_object
    contains
        !> @brief Returns the appropriate GNUPLOT command string to define the
        !! plot object properties.
        procedure(get_string_result), deferred, public :: get_command_string
    end type

! ------------------------------------------------------------------------------
    !> @brief Defines a GNUPLOT terminal object.
    type, abstract, extends(plot_object) :: terminal
    private
        !> The window height, in pixels
        integer(int32) :: m_windowHeight = GNUPLOT_DEFAULT_WINDOW_HEIGHT
        !> The window width, in pixels
        integer(int32) :: m_windowWidth = GNUPLOT_DEFAULT_WINDOW_WIDTH
        !> The plot window number.
        integer(int32) :: m_termID = 0
        !> The plot window title.
        character(len = GNUPLOT_MAX_LABEL_LENGTH) :: m_title = ""
        !> Determines if a plot title is defined
        logical :: m_hasTitle = .false.
        !> The font used by the graph.
        character(len = GNUPLOT_MAX_LABEL_LENGTH) :: m_fontName = &
            GNUPLOT_DEFAULT_FONTNAME
        !> The size of the font used by the graph.
        integer(int32) :: m_fontSize = GNUPLOT_DEFAULT_FONT_SIZE
    contains
        !> @brief Gets the width of the plot window.
        procedure, public :: get_window_width => term_get_window_width
        !> @brief Sets the width of the plot window.
        procedure, public :: set_window_width => term_set_window_width
        !> @brief Gets the height of the plot window.
        procedure, public :: get_window_height => term_get_window_height
        !> @brief Sets the height of the plot window.
        procedure, public :: set_window_height => term_set_window_height
        !> @brief Returns the appropriate GNUPLOT command string to establish
        !! appropriate parameters.
        procedure, public :: get_command_string => term_get_command_string
        !> @brief Gets the targeted plot window number.
        procedure, public :: get_plot_window_number => &
            term_get_plot_window_number
        !> @brief Sets the targeted plot window number.
        procedure, public :: set_plot_window_number => &
            term_set_plot_window_number
        !> @brief Gets the plot window's title.
        procedure, public :: get_title => term_get_title
        !> @brief Sets the plot window's title.
        procedure, public :: set_title => term_set_title
        !> @brief Gets the name of the font used for text displayed by the
        !! graph.
        procedure, public :: get_font_name => term_get_font_name
        !> @brief Sets the name of the font used for text displayed by the
        !! graph.
        procedure, public :: set_font_name => term_set_font_name
        !> @brief Gets the size of the font used by the graph.
        procedure, public :: get_font_size => term_get_font_size
        !> @brief Sets the size of the font used by the graph.
        procedure, public :: set_font_size => term_set_font_size
        !> @brief Gets the GNUPLOT terminal identification string.
        procedure(term_get_string_result), deferred, public :: get_id_string
    end type

! ------------------------------------------------------------------------------
    !> @brief Defines a GNUPLOT Win32 terminal object.
    type, extends(terminal) :: windows_terminal
    contains
        !> @brief Retrieves a GNUPLOT terminal identifier string.
        procedure, public :: get_id_string => wt_get_term_string
    end type

! ------------------------------------------------------------------------------
    !> @brief Defines a GNUPLOT QT terminal object.
    type, extends(terminal) :: qt_terminal
    contains
        !> @brief Retrieves a GNUPLOT terminal identifier string.
        procedure, public :: get_id_string => qt_get_term_string
    end type

! ------------------------------------------------------------------------------
    !> @brief Defines a GNUPLOT WXT terminal object.
    type, extends(terminal) :: wxt_terminal
    contains
        !> @brief Retrieves a GNUPLOT terminal identifier string.
        procedure, public :: get_id_string => wxt_get_term_string
    end type

! ------------------------------------------------------------------------------
    !> @brief Defines a GNUPLOT PNG terminal object.
    type, extends(terminal) :: png_terminal
    private
        !> The filename of the PNG file to write.
        character(len = GNUPLOT_MAX_PATH_LENGTH) :: m_fname = "default.png"
    contains
        !> @brief Gets the filename for the output PNG file.
        procedure, public :: get_filename => png_get_filename
        !> @brief Sets the filename for the output PNG file.
        procedure, public :: set_filename => png_set_filename
        !> @brief Retrieves a GNUPLOT terminal identifier string.
        procedure, public :: get_id_string => png_get_term_string
        !> @brief Returns the appropriate GNUPLOT command string to establish
        !! appropriate parameters.
        procedure, public :: get_command_string => png_get_command_string
    end type

! ------------------------------------------------------------------------------
    !> @brief Provides a container for plot data.
    type, abstract, extends(plot_object) :: plot_data
    private
        !> The name of the data set.
        character(len = PLOTDATA_MAX_NAME_LENGTH) :: m_name = ""
    contains
        !> @brief Gets the name to associate with this data set.
        procedure, public :: get_name => pd_get_name
        !> @brief Sets the name to associate with this data set.
        procedure, public :: set_name => pd_set_name
        !> @brief Gets the GNUPLOT command string containing the actual data
        !! to plot.
        procedure(pd_get_string_result), deferred, public :: get_data_string
    end type

! ------------------------------------------------------------------------------
    !> @brief Describes a single plot axis.
    type, abstract, extends(plot_object) :: plot_axis
    private
        !> @brief Has a title.
        logical :: m_hasTitle = .false.
        !> @brief The axis title.
        character(len = PLOTDATA_MAX_NAME_LENGTH) :: m_title = ""
        !> @brief Autoscale?
        logical :: m_autoscale = .true.
        !> @brief Display limits.
        real(real64), dimension(2) :: m_limits = [0.0d0, 1.0d0]
        !> @brief Log scaled?
        logical :: m_logScale = .false.
        !> @brief Zero axis?
        logical :: m_zeroAxis = .false.
        !> @brief The width, in pixels, of the zero axis line.
        real(real32) :: m_axisWidth = 2.5
    contains
        !> @brief Gets the axis' title.
        procedure, public :: get_title => pa_get_title
        !> @brief Sets the axis' title.
        procedure, public :: set_title => pa_set_title
        !> @brief Gets a value determining if a title has been defined for the
        !!  plot_axis object.
        procedure, public :: is_title_defined => pa_has_title
        !> @brief Gets a logical value determining if the axis should be
        !! automatically scaled to fit the data.
        procedure, public :: get_autoscale => pa_get_autoscale
        !> @brief Sets a logical value determining if the axis should be
        !! automatically scaled to fit the data.
        procedure, public :: set_autoscale => pa_set_autoscale
        !> @brief Gets the axis display limits, assuming autoscaling is not
        !! active for this axis.
        procedure, public :: get_limits => pa_get_axis_limits
        !> @brief Sets the axis display limits, assuming autoscaling is not
        !! active for this axis.
        procedure, public :: set_limits => pa_set_axis_limits
        !> @brief Gets a logical value defining if the axis should be log
        !! scaled.
        procedure, public :: get_is_log_scaled => pa_get_log_scale
        !> @brief Sets a logical value defining if the axis should be log
        !! scaled.
        procedure, public :: set_is_log_scaled => pa_set_log_scale
        !> @brief Returns the appropriate GNUPLOT command string to define the
        !! plot_axis properties.
        procedure, public :: get_command_string => pa_get_cmd_string
        !> @brief Gets a value determining if the axis should be drawn through
        !! zero of opposing axes.
        procedure, public :: get_zero_axis => pa_get_zero_axis
        !> @brief Sets a value determining if the axis should be drawn through
        !! zero of opposing axes.
        procedure, public :: set_zero_axis => pa_set_zero_axis
        !> @brief Gets the width of the line used to represent the zero axis
        !!  line, if active.
        procedure, public :: get_zero_axis_line_width => pa_get_zero_axis_width
        !> @brief Sets the width of the line used to represent the zero axis
        !!  line, if active.
        procedure, public :: set_zero_axis_line_width => pa_set_zero_axis_width
        !> @brief Gets a string identifying the axis as: x, y, z, y2, etc.
        procedure(pa_get_string_result), deferred, public :: get_id_string
    end type

! ------------------------------------------------------------------------------
    !> @brief Defines a legend object.
    type, extends(plot_object) :: legend
    private
        !> Legend on inside or outside of axes
        logical :: m_inside = .true.
        !> Draw a box around the legend
        logical :: m_box = .true.
        !> Defines the horizontal position
        character(len = 20) :: m_horzPosition = LEGEND_RIGHT
        !> Defines the vertical position
        character(len = 20) :: m_vertPosition = LEGEND_TOP
        !> Determines if the legend is visible.
        logical :: m_show = .true.
    contains
        !> @brief Gets a value determining if the legend should be drawn inside
        !! the axes border (true), or outside the axes border (false).
        procedure, public :: get_draw_inside_axes => leg_get_inside
        !> @brief Sets a value determining if the legend should be drawn inside
        !! the axes border (true), or outside the axes border (false).
        procedure, public :: set_draw_inside_axes => leg_set_inside
        !> @brief Gets a value determining if the legend should have a border.
        procedure, public :: get_draw_border => leg_get_box
        !> @brief Sets a value determining if the legend should have a border.
        procedure, public :: set_draw_border => leg_set_box
        !> @brief Gets the horizontal position of the legend.
        procedure, public :: get_horizontal_position => leg_get_horz_pos
        !> @brief Sets the horizontal position of the legend.
        procedure, public :: set_horizontal_position => leg_set_horz_pos
        !> @brief Gets the vertical position of the legend.
        procedure, public :: get_vertical_position => leg_get_vert_pos
        !> @brief Gets the vertical position of the legend.
        procedure, public :: set_vertical_position => leg_set_vert_pos
        !> @brief Gets a value determining if the legend is visible.
        procedure, public :: get_is_visible => leg_get_visible
        !> @brief Sets a value determining if the legend is visible.
        procedure, public :: set_is_visible => leg_set_visible
        !> @brief Gets the command string defining the legend properties.
        procedure, public :: get_command_string => leg_get_command_txt
    end type

! ------------------------------------------------------------------------------
    !> @brief Defines the basic GNUPLOT plot.
    type, abstract, extends(plot_object) :: plot
    private
        !> The plot title
        character(len = PLOTDATA_MAX_NAME_LENGTH) :: m_title = ""
        !> Has a title?
        logical :: m_hasTitle = .false.
        !> The GNUPLOT terminal object to target.
        class(terminal), pointer :: m_terminal => null()
        !> A collection of plot_data items to plot.
        type(list) :: m_data
        !> The legend.
        type(legend), pointer :: m_legend => null()
        !> Show grid lines?
        logical :: m_showGrid = .true.
    contains
        !> @brief Cleans up resources held by the plot object.
        procedure, public :: free_resources => plt_clean_up
        !> @brief Initializes the plot object.
        procedure, public :: initialize => plt_init
        !> @brief Gets the plot's title.
        procedure, public :: get_title => plt_get_title
        !> @brief Sets the plot's title.
        procedure, public :: set_title => plt_set_title
        !> @brief Gets a value determining if a title has been defined for the
        !!  plot object.
        procedure, public :: is_title_defined => plt_has_title
        !> @brief Gets the plot's legend object.
        procedure, public :: get_legend => plt_get_legend
        !> @brief Gets the number of stored plot_data objects.
        procedure, public :: get_count => plt_get_count
        !> @brief Pushes a plot_data object onto the stack.
        procedure, public :: push => plt_push_data
        !> @brief Pops the last plot_data object from the stack.
        procedure, public :: pop => plt_pop_data
        !> @brief Removes all plot_data objects from the plot.
        procedure, public :: clear_all => plt_clear_all
        !> @brief Gets a pointer to the requested plot_data object.
        procedure, public :: get => plt_get
        !> @brief Sets the requested plot_data object into the plot.
        procedure, public :: set => plt_set
        !> @brief Gets the GNUPLOT terminal object.
        procedure, public :: get_terminal => plt_get_term
        !> @brief Gets a flag determining if the grid lines should be shown.
        procedure, public :: get_show_gridlines => plt_get_show_grid
        !> @brief Sets a flag determining if the grid lines should be shown.
        procedure, public :: set_show_gridlines => plt_set_show_grid
        !> @brief Launches GNUPLOT and draws the plot per the current state of
        !! the command list.
        procedure, public :: draw => plt_draw
    end type

! ******************************************************************************
! CONCRETE PLOT_DATA TYPES
! ------------------------------------------------------------------------------
    !> @brief A plot_data object for describing 2D scatter plot data sets.
    type, extends(plot_data) :: scatter_plot_data
    private
        !> Draw the line?
        logical :: m_drawLine = .true.
        !> Draw the markers?
        logical :: m_drawMarkers = .false.
        !> Marker frequency.
        integer(int32) :: m_markerFrequency = 1
        !> Line color.
        type(color) :: m_lineColor = CLR_BLUE
        !> Line width.
        real(real32) :: m_lineWidth = 1.0
        !> Line style.
        integer(int32) :: m_lineStyle = LINE_SOLID
        !> Marker type.
        integer(int32) :: m_markerType = MARKER_X
        !> Marker size multiplier.
        real(real32) :: m_markerSize = 1.0
        !> Let GNUPLOT choose colors automatically
        logical :: m_useAutoColor = .true.
        !> An N-by-2 matrix containing the x and y data points.
        real(real64), allocatable, dimension(:,:) :: m_data
        !> Draw against the secondary y axis?
        logical :: m_useY2 = .false.
    contains
        !> @brief Gets the GNUPLOT command string to represent this
        !! scatter_plot_data object.
        procedure, public :: get_command_string => spd_get_cmd
        !> @brief Gets the GNUPLOT command string containing the actual data
        !! to plot.
        procedure, public :: get_data_string => spd_get_data_cmd
        !> @brief Gets the width of the line, in pixels.
        procedure, public :: get_line_width => spd_get_line_width
        !> @brief Sets the width of the line, in pixels.
        procedure, public :: set_line_width => spd_set_line_width
        !> @brief Gets the line style.
        procedure, public :: get_line_style => spd_get_line_style
        !> @brief Sets the line style.
        procedure, public :: set_line_style => spd_set_line_style
        !> @brief Gets the line color.
        procedure, public :: get_line_color => spd_get_line_color
        !> @brief Sets the line color.
        procedure, public :: set_line_color => spd_set_line_color
        !> @brief Gets a value determining if a line should be drawn.
        procedure, public :: get_draw_line => spd_get_draw_line
        !> @brief Sets a value determining if a line should be drawn.
        procedure, public :: set_draw_line => spd_set_draw_line
        !> @brief Gets a value determining if data point markers should be
        !! drawn.
        procedure, public :: get_draw_markers => spd_get_draw_markers
        !> @brief Sets a value determining if data point markers should be
        !! drawn.
        procedure, public :: set_draw_markers => spd_set_draw_markers
        !> @brief Gets the marker style.
        procedure, public :: get_marker_style => spd_get_marker_style
        !> @brief Sets the marker style.
        procedure, public :: set_marker_style => spd_set_marker_style
        !> @brief Gets the marker scaling.
        procedure, public :: get_marker_scaling => spd_get_marker_scaling
        !> @brief Sets the marker scaling.
        procedure, public :: set_marker_scaling => spd_set_marker_scaling
        !> @brief Gets the marker frequency.
        procedure, public :: get_marker_frequency => spd_get_marker_frequency
        !> @brief Sets the marker frequency.
        procedure, public :: set_marker_frequency => spd_set_marker_frequency
        !> @brief Gets a value determining if GNUPLOT should automatically
        !! choose line colors.
        procedure, public :: get_use_auto_color => spd_get_use_auto_colors
        !> @brief Sets a value determining if GNUPLOT should automatically
        !! choose line colors.
        procedure, public :: set_use_auto_color => spd_set_use_auto_colors
        !> @brief Gets the number of data points.
        procedure, public :: get_count => spd_get_data_count
        !> @brief Gets the requested X data point.
        procedure, public :: get_x => spd_get_x_data
        !> @brief Sets the requested X data point.
        procedure, public :: set_x => spd_set_x_data
        !> @brief Gets the requested Y data point.
        procedure, public :: get_y => spd_get_y_data
        !> @brief Sets the requested X data point.
        procedure, public :: set_y => spd_set_y_data
        !> @brief Defines the data set.
        generic, public :: define_data => spd_set_data_1, spd_set_data_2
        !> @brief Gets a value determining if the data should be plotted against
        !! the secondary y-axis.
        procedure, public :: get_draw_against_y2 => spd_get_draw_against_y2
        !> @brief Sets a value determining if the data should be plotted against
        !! the secondary y-axis.
        procedure, public :: set_draw_against_y2 => spd_set_draw_against_y2

        procedure :: spd_set_data_1
        procedure :: spd_set_data_2
    end type

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ******************************************************************************
! CONCRETE PLOT TYPES
! ------------------------------------------------------------------------------
    !> @brief A plot object defining a 2D scatter plot.
    type, extends(plot) :: scatter_plot
    private
        !> The x-axis.
        type(x_axis), pointer :: m_xAxis => null()
        !> The y-axis.
        type(y_axis), pointer :: m_yAxis => null()
        !> The secondary y-axis.
        type(y2_axis), pointer :: m_y2Axis => null()
        !> Display the secondary y axis?
        logical :: m_useY2 = .false.
        !> Point tic marks in?
        logical :: m_ticsIn = .true.
        !> Draw the border?
        logical :: m_drawBorder = .true.
    contains
        !> @brief Cleans up resources held by the scatter_plot object.
        final :: sp_clean_up
        !> @brief Initializes the scatter_plot object.
        procedure, public :: initialize => sp_init
        !> @brief Gets the GNUPLOT command string to represent this scatter_plot
        !! object.
        procedure, public :: get_command_string => sp_get_cmd
        !> @brief Gets the x-axis object.
        procedure, public :: get_x_axis => sp_get_x_axis
        !> @brief Gets the y-axis object.
        procedure, public :: get_y_axis => sp_get_y_axis
        !> @brief Gets the secondary y-axis object.
        procedure, public :: get_y2_axis => sp_get_y2_axis
        !> @brief Gets a flag determining if the secondary y-axis should be
        !! displayed.
        procedure, public :: get_use_y2_axis => sp_get_use_y2
        !> @brief Sets a flag determining if the secondary y-axis should be
        !! displayed.
        procedure, public :: set_use_y2_axis => sp_set_use_y2
        !> @brief Gets a value determining if the axis tic marks should point
        !! inwards.
        procedure, public :: get_tics_inward => sp_get_tics_in
        !> @brief Sets a value determining if the axis tic marks should point
        !! inwards.
        procedure, public :: set_tics_inward => sp_set_tics_in
        !> @brief Gets a value determining if the border should be drawn.
        procedure, public :: get_draw_border => sp_get_draw_border
        !> @brief Sets a value determining if the border should be drawn.
        procedure, public :: set_draw_border => sp_set_draw_border
    end type

! ******************************************************************************
! CONCRETE PLOT_AXIS TYPES
! ------------------------------------------------------------------------------
    !> @brief An x-axis object.
    type, extends(plot_axis) :: x_axis
    contains
        !> @brief Gets the axis identification string.
        procedure, public :: get_id_string => xa_get_id
    end type

! ------------------------------------------------------------------------------
    !> @brief A y-axis object.
    type, extends(plot_axis) :: y_axis
    contains
        !> @brief Gets the axis identification string.
        procedure, public :: get_id_string => ya_get_id
    end type

! ------------------------------------------------------------------------------
    !> @brief A secondary y-axis object.
    type, extends(plot_axis) :: y2_axis
    contains
        !> @brief Gets the axis identification string.
        procedure, public :: get_id_string => y2a_get_id
    end type

! ******************************************************************************
! ABSTRACT METHOD INTERFACES
! ------------------------------------------------------------------------------
    interface
        !> @brief Retrieves a string from a plot_object.
        !!
        !! @param[in] this The plot_object object.
        !! @return The string.
        function get_string_result(this) result(x)
            import plot_object
            class(plot_object), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        !> @brief Retrieves a string from a terminal.
        !!
        !! @param[in] this The terminal object.
        !! @return The string.
        function term_get_string_result(this) result(x)
            import terminal
            class(terminal), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        !> @brief Retrieves a string from a plot_data object.
        !!
        !! @param[in] this The plot_data object.
        !! @return The string.
        function pd_get_string_result(this) result(x)
            import plot_data
            class(plot_data), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        !> @brief Retrieves a string from a plot_axis.
        !!
        !! @param[in] this The plot_axis object.
        !! @return The string.
        function pa_get_string_result(this) result(x)
            import plot_axis
            class(plot_axis), intent(in) :: this
            character(len = :), allocatable :: x
        end function
    end interface


contains
! ******************************************************************************
! COLOR MEMBERS
! ------------------------------------------------------------------------------
    !> @brief Returns the color in hexadecimal format.
    !!
    !! @param[in] this The color object.
    !! @return A string containing the hexadecimal equivalent.
    pure function clr_to_hex_string(this) result(txt)
        ! Arguments
        class(color), intent(in) :: this
        character(6) :: txt

        ! Local Variables
        integer(int32) :: r, g, b, clr

        ! Clip each color if necessary
        if (this%red < 0) then
            r = 0
        else if (this%red > 255) then
            r = 255
        else
            r = this%red
        end if

        if (this%green < 0) then
            g = 0
        else if (this%green > 255) then
            g = 255
        else
            g = this%green
        end if

        if (this%blue < 0) then
            b = 0
        else if (this%blue > 255) then
            b = 255
        else
            b = this%blue
        end if

        ! Build the color information
        clr = ishft(r, 16) + ishft(g, 8) + b

        ! Convert the integer to a hexadecimal string
        write(txt, '(Z6.6)') clr
    end function

! ------------------------------------------------------------------------------
    !> @brief Copies another color to this color.
    !!
    !! @param[in,out] this The color object.
    !! @param[in] clr The color to copy.
    subroutine clr_copy_from(this, clr)
        class(color), intent(inout) :: this
        class(color), intent(in) :: clr
        this%red = clr%red
        this%green = clr%green
        this%blue = clr%blue
    end subroutine

! ******************************************************************************
! TERMINAL MEMBERS
! ------------------------------------------------------------------------------
    !> @brief Gets the width of the plot window.
    !!
    !! @param[in] this The terminal object.
    !! @return The width of the plot window.
    pure function term_get_window_width(this) result(x)
        class(terminal), intent(in) :: this
        integer :: x
        x = this%m_windowWidth
    end function

! --------------------
    !> @brief Sets the width of the plot window.
    !!
    !! @param[in,out] this The terminal object.
    !! @param[in] x The width of the plot window.  If a value of zero is
    !! provided, the window width is reset to its default value; or, if a
    !! negative value is provided, the absolute value of the supplied value is
    !! utilized.
    subroutine term_set_window_width(this, x)
        class(terminal), intent(inout) :: this
        integer, intent(in) :: x
        if (x == 0) then
            this%m_windowWidth = GNUPLOT_DEFAULT_WINDOW_WIDTH
        else
            this%m_windowWidth = abs(x)
        end if
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets the height of the plot window.
    !!
    !! @param[in] this The terminal object.
    !! @return The height of the plot window.
    pure function term_get_window_height(this) result(x)
        class(terminal), intent(in) :: this
        integer :: x
        x = this%m_windowHeight
    end function

! --------------------
    !> @brief Sets the height of the plot window.
    !!
    !! @param[in,out] this The terminal object.
    !! @param[in] x The height of the plot window.  If a value of zero is
    !! provided, the window height is reset to its default value; or, if a
    !! negative value is provided, the absolute value of the supplied value is
    !! utilized.
    subroutine term_set_window_height(this, x)
        class(terminal), intent(inout) :: this
        integer, intent(in) :: x
        if (x == 0) then
            this%m_windowHeight = GNUPLOT_DEFAULT_WINDOW_HEIGHT
        else
            this%m_windowHeight = abs(x)
        end if
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets the targeted plot window number.
    !!
    !! @param[in] this The terminal object.
    !! @return The plot window number.
    pure function term_get_plot_window_number(this) result(x)
        class(terminal), intent(in) :: this
        integer(int32) :: x
        x = this%m_termID
    end function

! --------------------
    !> @brief Sets the targeted plot window number.
    !!
    !! @param[in,out] this The terminal object.
    !! @param[in] x The plot window number.
    subroutine term_set_plot_window_number(this, x)
        class(terminal), intent(inout) :: this
        integer(int32), intent(in) :: x
        this%m_termID = x
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets the plot window's title.
    !!
    !! @param[in] this The terminal object.
    !! @return The title.
    pure function term_get_title(this) result(str)
        class(terminal), intent(in) :: this
        character(len = :), allocatable :: str
        str = trim(this%m_title)
    end function

! --------------------
    !> @brief Sets the plot window's title.
    !!
    !! @param[in,out] this The terminal object.
    !! @param[in] txt The title.
    subroutine term_set_title(this, txt)
        class(terminal), intent(inout) :: this
        character(len = *), intent(in) :: txt
        integer(int32) :: n
        n = min(len(txt), GNUPLOT_MAX_LABEL_LENGTH)
        this%m_title = ""
        if (n /= 0) then
            this%m_title(1:n) = txt(1:n)
            this%m_hasTitle = .true.
        else
            this%m_hasTitle = .false.
        end if
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets the name of the font used for text displayed by the graph.
    !!
    !! @param[in] this The terminal object.
    !! @return The font name.
    pure function term_get_font_name(this) result(name)
        class(terminal), intent(in) :: this
        character(len = :), allocatable :: name
        name = trim(this%m_fontName)
    end function

! --------------------
    !> @brief Sets the name of the font used for text displayed by the graph.
    !!
    !! @param[in,out] this The terminal object.
    !! @param[in] name The name of the font.  If no name is supplied, the name
    !! is reset back to its default setting.
    subroutine term_set_font_name(this, name)
        class(terminal), intent(inout) :: this
        character(len = *), intent(in) :: name
        integer(int32) :: n
        n = min(len(name), GNUPLOT_MAX_LABEL_LENGTH)
        this%m_fontName = ""
        if (n == 0) then
            this%m_fontName = GNUPLOT_DEFAULT_FONTNAME
        else
            this%m_fontName(1:n) = name(1:n)
        end if
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets the size of the font used by the graph.
    !!
    !! @param[in] this The terminal object.
    !! @return The font size, in points.
    pure function term_get_font_size(this) result(sz)
        class(terminal), intent(in) :: this
        integer :: sz
        sz = this%m_fontSize
    end function

! --------------------
    !> @brief Sets the size of the font used by the graph.
    !!
    !! @param[in,out] this The terminal object.
    !! @param[in] sz The font size, in points.  If a value of zero is provided,
    !! the font size is reset to its default value; or, if a negative value
    !! is provided, the absolute value of the supplied value is utilized.
    subroutine term_set_font_size(this, sz)
        class(terminal), intent(inout) :: this
        integer(int32), intent(in) :: sz
        if (sz == 0) then
            this%m_fontSize = GNUPLOT_DEFAULT_FONT_SIZE
        else
            this%m_fontSize = abs(sz)
        end if
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Returns the appropriate GNUPLOT command string to establish
    !! appropriate parameters.
    !!
    !! @param[in] this The terminal object.
    !! @return The GNUPLOT command string.
    function term_get_command_string(this) result(x)
        ! Arguments
        class(terminal), intent(in) :: this
        character(len = :), allocatable :: x

        ! Local Variables
        type(string_builder) :: str

        ! Process
        call str%initialize()
        call str%append("set term ")
        call str%append(this%get_id_string())
        call str%append(" enhanced ")
        call str%append(to_string(this%get_plot_window_number()))
        call str%append(" font ")
        call str%append('"')
        call str%append(this%get_font_name())
        call str%append(',')
        call str%append(to_string(this%get_font_size()))
        call str%append('"')
        call str%append(" size ")
        call str%append(to_string(this%get_window_width()))
        call str%append(",")
        call str%append(to_string(this%get_window_height()))
        if (this%m_hasTitle) then
            call str%append(' title "')
            call str%append(this%get_title())
            call str%append('"')
        end if
        x = str%to_string()
    end function

! ******************************************************************************
! WINDOWS_TERMINAL MEMBERS
! ------------------------------------------------------------------------------
    !> @brief Retrieves a GNUPLOT terminal identifier string.
    !!
    !! @param[in] this The windows_terminal object.
    !! @return The string.
    pure function wt_get_term_string(this) result(x)
        class(windows_terminal), intent(in) :: this
        character(len = :), allocatable :: x
        x = "win"
    end function

! ******************************************************************************
! QT_TERMINAL MEMBERS
! ------------------------------------------------------------------------------
    !> @brief Retrieves a GNUPLOT terminal identifier string.
    !!
    !! @param[in] this The qt_terminal object.
    !! @return The string.
    pure function qt_get_term_string(this) result(x)
        class(qt_terminal), intent(in) :: this
        character(len = :), allocatable :: x
        x = "qt"
    end function


! ******************************************************************************
! WXT_TERMINAL_MEMBERS
! ------------------------------------------------------------------------------
    !> @brief Retrieves a GNUPLOT terminal identifier string.
    !!
    !! @param[in] this The wxt_terminal object.
    !! @return The string.
    pure function wxt_get_term_string(this) result(x)
        class(wxt_terminal), intent(in) :: this
        character(len = :), allocatable :: x
        x = "wxt"
    end function

! ******************************************************************************
! PNG_TERMINAL MEMBERS
! ------------------------------------------------------------------------------
    !> @brief Retrieves a GNUPLOT terminal identifier string.
    !!
    !! @param[in] this The png_terminal object.
    !! @return The string.
    pure function png_get_term_string(this) result(x)
        class(png_terminal), intent(in) :: this
        character(len = :), allocatable :: x
        x = "png"
    end function

! ------------------------------------------------------------------------------
    !> @brief Gets the filename for the output PNG file.
    !!
    !! @param[in] this The png_terminal object.
    !! @return The filename, including the file extension (.png).
    pure function png_get_filename(this) result(txt)
        class(png_terminal), intent(in) :: this
        character(len = :), allocatable :: txt
        txt = trim(this%m_fname)
    end function

! --------------------
    !> @brief Sets the filename for the output PNG file.
    !!
    !! @param[in,out] this The png_terminal object.
    !! @param[in] The filename, including the file extension (.png).
    subroutine png_set_filename(this, txt)
        class(png_terminal), intent(inout) :: this
        character(len = *), intent(in) :: txt
        integer(int32) :: n
        n = min(len(txt), GNUPLOT_MAX_PATH_LENGTH)
        this%m_fname = ""
        if (n /= 0) then
            this%m_fname(1:n) = txt(1:n)
        else
            this%m_fname = "default.png"
        end if
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Returns the appropriate GNUPLOT command string to establish
    !! appropriate parameters.
    !!
    !! @param[in] this The terminal object.
    !! @return The GNUPLOT command string.
    function png_get_command_string(this) result(x)
        ! Arguments
        class(png_terminal), intent(in) :: this
        character(len = :), allocatable :: x

        ! Local Variables
        type(string_builder) :: str

        ! Process
        call str%initialize()
        call str%append("set term png ")
        call str%append(" font ")
        call str%append('"')
        call str%append(this%get_font_name())
        call str%append(',')
        call str%append(to_string(this%get_font_size()))
        call str%append('"')
        call str%append(" size ")
        call str%append(to_string(this%get_window_width()))
        call str%append(",")
        call str%append(to_string(this%get_window_height()))
        call str%append(new_line('a'))
        call str%append("set output ")
        call str%append('"')
        call str%append(this%get_filename())
        call str%append('"')
        x = str%to_string()
    end function

! ******************************************************************************
! PLOT_DATA MEMBERS
! ------------------------------------------------------------------------------
    !> @brief Gets the name to associate with this data set.
    !!
    !! @param[in] this The plot_data object.
    !! @return The name.
    pure function pd_get_name(this) result(txt)
        class(plot_data), intent(in) :: this
        character(len = :), allocatable :: txt
        txt = trim(this%m_name)
    end function

! --------------------
    !> @brief Sets the name to associate with this data set.
    !!
    !! @param[in,out] this The plot_data object.
    !! @param[in] txt The name.
    subroutine pd_set_name(this, txt)
        class(plot_data), intent(inout) :: this
        character(len = *), intent(in) :: txt
        integer(int32) :: n
        n = min(len(txt), PLOTDATA_MAX_NAME_LENGTH)
        this%m_name = ""
        if (n /= 0) then
            this%m_name(1:n) = txt(1:n)
        end if
    end subroutine

! ******************************************************************************
! PLOT_AXIS MEMBERS
! ------------------------------------------------------------------------------
    !> @brief Gets the axis' title.
    !!
    !! @param[in] this The plot_axis object.
    !! @return The title.
    pure function pa_get_title(this) result(txt)
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
    subroutine pa_set_title(this, txt)
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
    pure function pa_has_title(this) result(x)
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
    pure function pa_get_autoscale(this) result(x)
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
    subroutine pa_set_autoscale(this, x)
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
    pure function pa_get_axis_limits(this) result(x)
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
    subroutine pa_set_axis_limits(this, lower, upper)
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
    pure function pa_get_log_scale(this) result(x)
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
    subroutine pa_set_log_scale(this, x)
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
    function pa_get_cmd_string(this) result(txt)
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
        call str%append(new_line('a'))
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
    pure function pa_get_zero_axis(this) result(x)
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
    subroutine pa_set_zero_axis(this, x)
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
    pure function pa_get_zero_axis_width(this) result(x)
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
    subroutine pa_set_zero_axis_width(this, x)
        class(plot_axis), intent(inout) :: this
        real(real32), intent(in) :: x
        this%m_axisWidth = x
    end subroutine

! ******************************************************************************
! LEGEND MEMBERS
! ------------------------------------------------------------------------------
    !> @brief Gets a value determining if the legend should be drawn inside the
    !! axes border (true), or outside the axes border (false).
    !!
    !! @param[in] this The legend object.
    !! @return The logical value.
    pure function leg_get_inside(this) result(x)
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
    subroutine leg_set_inside(this, x)
        class(legend), intent(inout) :: this
        logical, intent(in) :: x
        this%m_inside = x
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets a value determining if the legend should have a border.
    !!
    !! @param[in] this The legend object.
    !! @return The logical value.
    pure function leg_get_box(this) result(x)
        class(legend), intent(in) :: this
        logical :: x
        x = this%m_box
    end function

! ---------------------
    !> @brief Sets a value determining if the legend should have a border.
    !!
    !! @param[in,out] this The legend object.
    !! @param[in] x The logical value.
    subroutine leg_set_box(this, x)
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
    pure function leg_get_horz_pos(this) result(x)
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
    subroutine leg_set_horz_pos(this, x)
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
    pure function leg_get_vert_pos(this) result(x)
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
    subroutine leg_set_vert_pos(this, x)
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
    pure function leg_get_visible(this) result(x)
        class(legend), intent(in) :: this
        logical :: x
        x = this%m_show
    end function

! ---------------------
    !> @brief Sets a value determining if the legend is visible.
    !!
    !! @param[in,out] this The legend object.
    !! @param[in] x The logical value.
    subroutine leg_set_visible(this, x)
        class(legend), intent(inout) :: this
        logical, intent(in) :: x
        this%m_show = x
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets the command string defining the legend properties.
    !!
    !! @param[in] this The legend object.
    !! @return The GNUPLOT command string.
    function leg_get_command_txt(this) result(txt)
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

! ******************************************************************************
! PLOT MEMBERS
! ------------------------------------------------------------------------------
    !> @brief Cleans up resources held by the plot object.
    !!
    !! @param[in,out] this The plot object.
    subroutine plt_clean_up(this)
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
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !! - PLOT_OUT_OF_MEMORY_ERROR: Occurs if insufficient memory is available.
    subroutine plt_init(this, term, err)
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
    pure function plt_get_title(this) result(txt)
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
    subroutine plt_set_title(this, txt)
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
    pure function plt_has_title(this) result(x)
        class(plot), intent(in) :: this
        logical :: x
        x = this%m_hasTitle
    end function

! ------------------------------------------------------------------------------
    !> @brief Gets the plot's legend object.
    !!
    !! @param[in] this The plot object.
    !! @return A pointer to the legend object.
    function plt_get_legend(this) result(x)
        class(plot), intent(in) :: this
        type(legend), pointer :: x
        x => this%m_legend
    end function

! ------------------------------------------------------------------------------
    !> @brief Gets the number of stored plot_data objects.
    !!
    !! @param[in] this The plot object.
    !! @return The number of plot_data objects.
    pure function plt_get_count(this) result(x)
        class(plot), intent(in) :: this
        integer(int32) :: x
        x = this%m_data%get_count()
    end function

! ------------------------------------------------------------------------------
    !> @brief Pushes a plot_data object onto the stack.
    !!
    !! @param[in,out] this The plot object.
    !! @param[in] x The plot_data object.
    !! @param[out] err
    subroutine plt_push_data(this, x, err)
        ! Arguments
        class(plot), intent(inout) :: this
        class(plot_data), intent(in) :: x
        class(errors), intent(inout), optional, target :: err

        ! Process
        call this%m_data%push(x) ! Include err
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Pops the last plot_data object from the stack.
    !!
    !! @param[in,out] this The plot object.
    subroutine plt_pop_data(this)
        class(plot), intent(inout) :: this
        call this%m_data%pop()
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Removes all plot_data objects from the plot.
    !!
    !! @param[in,out] this The plot object.
    subroutine plt_clear_all(this)
        class(plot), intent(inout) :: this
        call this%m_data%clear()
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets a pointer to the requested plot_data object.
    !!
    !! @param[in] this The plot object.
    !! @param[in] i The index of the plot_data object.
    !! @return A pointer to the requested plot_data object.
    function plt_get(this, i) result(x)
        ! Arguments
        class(plot), intent(in) :: this
        integer(int32), intent(in) :: i
        class(plot_data), pointer :: x

        ! Local Variables
        type(container) :: cntr
        class(*), pointer :: item

        ! Process
        cntr = this%m_data%get(i)
        item => cntr%get()
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
    subroutine plt_set(this, i, x)
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
    function plt_get_term(this) result(x)
        class(plot), intent(in) :: this
        class(terminal), pointer :: x
        x => this%m_terminal
    end function

! ------------------------------------------------------------------------------
    !> @brief Gets a flag determining if the grid lines should be shown.
    !!
    !! @param[in] this The plot object.
    !! @return Returns true if the grid lines should be shown; else, false.
    pure function plt_get_show_grid(this) result(x)
        class(plot), intent(in) :: this
        logical :: x
        x = this%m_showGrid
    end function

! --------------------
    !> @brief Sets a flag determining if the grid lines should be shown.
    !!
    !! @param[in,out] this The plot object.
    !! @param[in] x Set to true if the grid lines should be shown; else, false.
    subroutine plt_set_show_grid(this, x)
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
    subroutine plt_draw(this, persist, err)
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
        end if
        write(fid, '(A)') this%get_command_string()
        close(fid)

        ! Launch GNUPLOT
        if (p) then
            call execute_command_line("gnuplot -persist " // fname)
        else
            call execute_command_line("gnuplot " // fname)
        end if

        ! Clean up by deleting the file
        open(newunit = fid, file = fname)
        close(fid, status = "delete")
    end subroutine

! ******************************************************************************
! SCATTER_PLOT_DATA MEMBERS
! ------------------------------------------------------------------------------
    !> @brief Gets the GNUPLOT command string to represent this
    !! scatter_plot_data object.
    !!
    !! @param[in] this The scatter_plot_data object.
    !! @return The command string.
    function spd_get_cmd(this) result(x)
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
        n = len(this%get_name())
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

        ! Define which axes the data is to be plotted against
        if (this%get_draw_against_y2()) then
            call str%append(" axes x1y2")
        else
            call str%append(" axes x1y1")
        end if

        ! End
        x = str%to_string()
    end function

! ------------------------------------------------------------------------------
    !> @brief Gets the GNUPLOT command string containing the actual data
    !! to plot.
    !!
    !! @param[in] this The scatter_plot_data object.
    !! @return The command string.
    function spd_get_data_cmd(this) result(x)
        ! Arguments
        class(scatter_plot_data), intent(in) :: this
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
            call str%append(nl)
        end do

        ! End
        x = str%to_string()
    end function

! ------------------------------------------------------------------------------
    !> @brief Gets the width of the line, in pixels.
    !!
    !! @param[in] this The scatter_plot_data object.
    !! @return The line width.
    pure function spd_get_line_width(this) result(x)
        class(scatter_plot_data), intent(in) :: this
        real(real32) :: x
        x = this%m_lineWidth
    end function

! --------------------
    !> @brief Sets the width of the line, in pixels.
    !!
    !! @param[in,out] this The scatter_plot_data object.
    !! @param[in] x The line width.
    subroutine spd_set_line_width(this, x)
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
    pure function spd_get_line_style(this) result(x)
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
    subroutine spd_set_line_style(this, x)
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
    pure function spd_get_line_color(this) result(x)
        class(scatter_plot_data), intent(in) :: this
        type(color) :: x
        x = this%m_lineColor
    end function

! --------------------
    !> @brief Sets the line color.
    !!
    !! @param[in,out] this The scatter_plot_data object.
    !! @param[in] x The color.
    subroutine spd_set_line_color(this, x)
        class(scatter_plot_data), intent(inout) :: this
        type(color), intent(in) :: x
        this%m_lineColor = x
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets a value determining if a line should be drawn.
    !!
    !! @param[in] this The scatter_plot_data object.
    !! @return Returns true if the line should be drawn; else, false.
    pure function spd_get_draw_line(this) result(x)
        class(scatter_plot_data), intent(in) :: this
        logical :: x
        x = this%m_drawLine
    end function

! --------------------
    !> @brief Sets a value determining if a line should be drawn.
    !!
    !! @param[in,out] this The scatter_plot_data object.
    !! @param[in] x Set to true if the line should be drawn; else, false.
    subroutine spd_set_draw_line(this, x)
        class(scatter_plot_data), intent(inout) :: this
        logical, intent(in) :: x
        this%m_drawLine = x
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets a value determining if data point markers should be drawn.
    !!
    !! @param[in] this The scatter_plot_data object.
    !! @return Returns true if the markers should be drawn; else, false.
    pure function spd_get_draw_markers(this) result(x)
        class(scatter_plot_data), intent(in) :: this
        logical :: x
        x = this%m_drawMarkers
    end function

! --------------------
    !> @brief Sets a value determining if data point markers should be drawn.
    !!
    !! @param[in,out] this The scatter_plot_data object.
    !! @param[in] x Set to true if the markers should be drawn; else, false.
    subroutine spd_set_draw_markers(this, x)
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
    pure function spd_get_marker_style(this) result(x)
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
    subroutine spd_set_marker_style(this, x)
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
    pure function spd_get_marker_scaling(this) result(x)
        class(scatter_plot_data), intent(in) :: this
        real(real32) :: x
        x = this%m_markerSize
    end function

! --------------------
    !> @brief Sets the marker scaling.
    !!
    !! @param[in,out] this The scatter_plot_data object.
    !! @param[in] x The scaling factor.
    subroutine spd_set_marker_scaling(this, x)
        class(scatter_plot_data), intent(inout) :: this
        real(real32), intent(in) :: x
        this%m_markerSize = x
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets the marker frequency.
    !!
    !! @param[in] this The scatter_plot_data object.
    !! @return The marker frequency.
    pure function spd_get_marker_frequency(this) result(x)
        class(scatter_plot_data), intent(in) :: this
        integer(int32) :: x
        x = this%m_markerFrequency
    end function

! --------------------
    !> @brief Sets the marker frequency.
    !!
    !! @param[in,out] this The scatter_plot_data object.
    !! @param[in] x The marker frequency.
    subroutine spd_set_marker_frequency(this, x)
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
    pure function spd_get_use_auto_colors(this) result(x)
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
    subroutine spd_set_use_auto_colors(this, x)
        class(scatter_plot_data), intent(inout) :: this
        logical, intent(in) :: x
        this%m_useAutoColor = x
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets the number of data points.
    !!
    !! @param[in] this The scatter_plot_data object.
    !! @return The number of data points.
    pure function spd_get_data_count(this) result(x)
        class(scatter_plot_data), intent(in) :: this
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
    !! @param[in] this The scatter_plot_data object.
    !! @param[in] i The index of the data point to retrieve.
    !! @return The requested data point.
    pure function spd_get_x_data(this, i) result(x)
        class(scatter_plot_data), intent(in) :: this
        integer(int32), intent(in) :: i
        real(real64) :: x
        if (allocated(this%m_data)) then
            x = this%m_data(i, 1)
        else
            x = 0.0d0
        end if
    end function

! --------------------
    !> @brief Sets the requested X data point.
    !!
    !! @param[in,out] this The scatter_plot_data object.
    !! @param[in] i The index of the data point to replace.
    !! @param[in] x The data point.
    subroutine spd_set_x_data(this, i, x)
        class(scatter_plot_data), intent(inout) :: this
        integer(int32), intent(in) :: i
        real(real64), intent(in) :: x
        if (allocated(this%m_data)) then
            this%m_data(i, 1) = x
        end if
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets the requested Y data point.
    !!
    !! @param[in] this The scatter_plot_data object.
    !! @param[in] i The index of the data point to retrieve.
    !! @return The requested data point.
    pure function spd_get_y_data(this, i) result(x)
        class(scatter_plot_data), intent(in) :: this
        integer(int32), intent(in) :: i
        real(real64) :: x
        if (allocated(this%m_data)) then
            x = this%m_data(i, 2)
        else
            x = 0.0d0
        end if
    end function

! --------------------
    !> @brief Sets the requested Y data point.
    !!
    !! @param[in,out] this The scatter_plot_data object.
    !! @param[in] i The index of the data point to replace.
    !! @param[in] x The data point.
    subroutine spd_set_y_data(this, i, x)
        class(scatter_plot_data), intent(inout) :: this
        integer(int32), intent(in) :: i
        real(real64), intent(in) :: x
        if (allocated(this%m_data)) then
            this%m_data(i, 2) = x
        end if
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Defines the data set.
    !!
    !! @param[in,out] this The scatter_plot_data object.
    !! @param[in] x An N-element array containing the x coordinate data.
    !! @param[in] y An N-element array containing the y coordinate data.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - PLOT_OUT_OF_MEMORY_ERROR: Occurs if insufficient memory is available.
    !!  - PLOT_ARRAY_SIZE_MISMATCH_ERROR: Occurs if @p x and @p y are not the
    !!      same size.
    subroutine spd_set_data_1(this, x, y, err)
        ! Arguments
        class(scatter_plot_data), intent(inout) :: this
        real(real64), intent(in), dimension(:) :: x, y
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
        if (size(y) /= n) then
            call errmgr%report_error("spd_set_data_1", &
                "The input arrays are not the same size.", &
                PLOT_ARRAY_SIZE_MISMATCH_ERROR)
            return
        end if

        ! Process
        if (allocated(this%m_data)) deallocate(this%m_data)
        allocate(this%m_data(n, 2), stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("spd_set_data_1", &
                "Insufficient memory available.", PLOT_OUT_OF_MEMORY_ERROR)
            return
        end if
        do concurrent (i = 1:n)
            this%m_data(i, 1) = x(i)
            this%m_data(i, 2) = y(i)
        end do
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Defines the data set.
    !!
    !! @param[in,out] this The scatter_plot_data object.
    !! @param[in] y An N-element array containing the y-coordinate data.  This
    !!  data will be plotted against its own index.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - PLOT_OUT_OF_MEMORY_ERROR: Occurs if insufficient memory is available.
    subroutine spd_set_data_2(this, y, err)
        ! Arguments
        class(scatter_plot_data), intent(inout) :: this
        real(real64), intent(in), dimension(:) :: y
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: i, n, flag
        class(errors), pointer :: errmgr
        type(errors), target :: deferr

        ! Initialization
        n = size(y)
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Process
        if (allocated(this%m_data)) deallocate(this%m_data)
        allocate(this%m_data(n, 2), stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("spd_set_data_2", &
                "Insufficient memory available.", PLOT_OUT_OF_MEMORY_ERROR)
            return
        end if
        do concurrent (i = 1:n)
            this%m_data(i, 1) = real(i, real64)
            this%m_data(i, 2) = y(i)
        end do
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets a value determining if the data should be plotted against the
    !! secondary y-axis.
    !!
    !! @param[in] this The scatter_plot_data object.
    !! @return Returns true if the data should be plotted against the secondary
    !!  y-axis; else, false to plot against the primary y-axis.
    pure function spd_get_draw_against_y2(this) result(x)
        class(scatter_plot_data), intent(in) :: this
        logical :: x
        x = this%m_useY2
    end function

! --------------------
    !> @brief Sets a value determining if the data should be plotted against the
    !! secondary y-axis.
    !!
    !! @param[in,out] this The scatter_plot_data object.
    !! @param[in] x Set to true if the data should be plotted against the
    !!  secondary y-axis; else, false to plot against the primary y-axis.
    subroutine spd_set_draw_against_y2(this, x)
        class(scatter_plot_data), intent(inout) :: this
        logical, intent(in) :: x
        this%m_useY2 = x
    end subroutine

! ******************************************************************************
! SCATTER_PLOT MEMBERS
! ------------------------------------------------------------------------------
    !> @brief Cleans up resources held by the scatter_plot object.
    !!
    !! @param[in,out] this The scatter_plot object.
    subroutine sp_clean_up(this)
        type(scatter_plot), intent(inout) :: this
        call this%free_resources()
        if (associated(this%m_xAxis)) then
            deallocate(this%m_xAxis)
            nullify(this%m_xAxis)
        end if
        if (associated(this%m_yAxis)) then
            deallocate(this%m_yAxis)
            nullify(this%m_yAxis)
        end if
        if (associated(this%m_y2Axis)) then
            deallocate(this%m_y2Axis)
            nullify(this%m_y2Axis)
        end if
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Initializes the scatter_plot object.
    !!
    !! @param[in] this The scatter_plot object.
    !! @param[in] term An optional input that is used to define the terminal.
    !!  The default terminal is a WXT terminal.  The acceptable inputs are:
    !!  - GNUPLOT_TERMINAL_PNG
    !!  - GNUPLOT_TERMINAL_QT
    !!  - GNUPLOT_TERMINAL_WIN32
    !!  - GNUPLOT_TERMINAL_WXT
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !! - PLOT_OUT_OF_MEMORY_ERROR: Occurs if insufficient memory is available.
    subroutine sp_init(this, term, err)
        ! Arguments
        class(scatter_plot), intent(inout) :: this
        integer(int32), intent(in), optional :: term
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

        ! Initialize the base class
        call plt_init(this, term, errmgr)
        if (errmgr%has_error_occurred()) return

        ! Process
        flag = 0
        if (.not.associated(this%m_xAxis)) then
            allocate(this%m_xAxis, stat = flag)
        end if
        if (flag == 0 .and. .not.associated(this%m_yAxis)) then
            allocate(this%m_yAxis, stat = flag)
        end if
        if (flag == 0 .and. .not.associated(this%m_y2Axis)) then
            allocate(this%m_y2Axis, stat = flag)
        end if

        ! Error Checking
        if (flag /= 0) then
            call errmgr%report_error("sp_init", &
                "Insufficient memory available.", PLOT_OUT_OF_MEMORY_ERROR)
            return
        end if
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets the GNUPLOT command string to represent this scatter_plot
    !! object.
    !!
    !! @param[in] this The scatter_plot object.
    !! @return The command string.
    function sp_get_cmd(this) result(x)
        ! Arguments
        class(scatter_plot), intent(in) :: this
        character(len = :), allocatable :: x

        ! Local Variables
        type(string_builder) :: str
        integer(int32) :: i, n
        class(plot_data), pointer :: ptr
        class(plot_axis), pointer :: axis, xAxis, yAxis
        class(terminal), pointer :: term
        type(legend), pointer :: leg

        ! Initialization
        call str%initialize()

        ! Write the terminal commands
        term => this%get_terminal()
        call str%append(term%get_command_string())

        ! Grid
        if (this%get_show_gridlines()) then
            call str%append(new_line('a'))
            call str%append("set grid")
        end if

        ! Title
        n = len(this%get_title())
        if (n > 0) then
            call str%append(new_line('a'))
            call str%append('set title "')
            call str%append(this%get_title())
            call str%append('"')
        end if

        ! Axes
        call str%append(new_line('a'))
        xAxis => this%get_x_axis()
        if (associated(xAxis)) call str%append(xAxis%get_command_string())

        call str%append(new_line('a'))
        yAxis => this%get_y_axis()
        if (associated(yAxis)) call str%append(yAxis%get_command_string())

        ! Secondary Axes
        if (this%get_use_y2_axis()) then
            call str%append(new_line('a'))
            axis => this%get_y2_axis()
            if (associated(axis)) then
                call str%append(axis%get_command_string())
                call str%append(new_line('a'))
                call str%append("set y2tics")
                call str%append(new_line('a'))
                call str%append("set ytics nomirror")
            end if
        end if

        ! Tic Marks
        if (.not.this%get_tics_inward()) then
            call str%append(new_line('a'))
            call str%append("set tics out")
        end if
        if ((xAxis%get_zero_axis() .or. yAxis%get_zero_axis()) .and. &
                .not.this%get_use_y2_axis()) then
            ! Set tics to the axis only if there is a zero axis, and no
            ! secondary y axis
            call str%append(new_line('a'))
            call str%append("set tics axis")
        end if


        ! Border
        call str%append(new_line('a'))
        call str%append("set border back")

        if (this%get_draw_border()) then
            n = 31
        else
            n = 0
            if (.not.xAxis%get_zero_axis()) n = n + 1
            if (.not.yAxis%get_zero_axis()) n = n + 2

            call str%append(new_line('a'))
            call str%append("set xtics nomirror")
            call str%append(new_line('a'))
            call str%append("set ytics nomirror")

            if (this%get_use_y2_axis()) then
                n = n + 8
            end if
        end if

        call str%append(new_line('a'))
        if (n > 0) then
            call str%append("set border ")
            call str%append(to_string(n))
        else
            call str%append("unset border")
        end if

        ! Legend
        call str%append(new_line('a'))
        leg => this%get_legend()
        if (associated(leg)) call str%append(leg%get_command_string())

        ! Define the plot function and data formatting commands
        n = this%get_count()
        call str%append(new_line('a'))
        call str%append("plot ")
        do i = 1, n
            ptr => this%get(i)
            if (.not.associated(ptr)) cycle
            call str%append(ptr%get_command_string())
            if (i /= n) call str%append(", ")
        end do

        ! Define the data to plot
        do i = 1, n
            ptr => this%get(i)
            if (.not.associated(ptr)) cycle
            call str%append(new_line('a'))
            call str%append(ptr%get_data_string())
            if (i /= n) then
                call str%append("e")
                call str%append(new_line('a'))
            end if
        end do

        ! End
        x = str%to_string()
    end function

! ------------------------------------------------------------------------------
    !> @brief Gets the x-axis object.
    !!
    !! @param[in] this The scatter_plot object.
    !! @return A pointer to the x-axis object.
    function sp_get_x_axis(this) result(ptr)
        class(scatter_plot), intent(in) :: this
        class(plot_axis), pointer :: ptr
        ptr => this%m_xAxis
    end function

! ------------------------------------------------------------------------------
    !> @brief Gets the y-axis object.
    !!
    !! @param[in] this The scatter_plot object.
    !! @return A pointer to the y-axis object.
    function sp_get_y_axis(this) result(ptr)
        class(scatter_plot), intent(in) :: this
        class(plot_axis), pointer :: ptr
        ptr => this%m_yAxis
    end function

! ------------------------------------------------------------------------------
    !> @brief Gets the secondary y-axis object.
    !!
    !! @param[in] this The scatter_plot object.
    !! @return A pointer to the secondary y-axis object.
    function sp_get_y2_axis(this) result(ptr)
        class(scatter_plot), intent(in) :: this
        class(plot_axis), pointer :: ptr
        ptr => this%m_y2Axis
    end function

! ------------------------------------------------------------------------------
    !> @brief Gets a flag determining if the secondary y-axis should be
    !! displayed.
    !!
    !! @param[in] this The scatter_plot object.
    !! @return Returns true if the axis should be displayed; else, false.
    pure function sp_get_use_y2(this) result(x)
        class(scatter_plot), intent(in) :: this
        logical :: x
        x = this%m_useY2
    end function

! --------------------
    !> @brief Sets a flag determining if the secondary y-axis should be
    !! displayed.
    !!
    !! @param[in,out] this The scatter_plot object.
    !! @param[in] x Set to true if the axis should be displayed; else, false.
    subroutine sp_set_use_y2(this, x)
        class(scatter_plot), intent(inout) :: this
        logical, intent(in) :: x
        this%m_useY2 = x
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets a value determining if the axis tic marks should point
    !! inwards.
    !!
    !! @param[in] this The scatter_plot object.
    !! @return Returns true if the tic marks should point inwards; else, false
    !!  if the tic marks should point outwards.
    pure function sp_get_tics_in(this) result(x)
        class(scatter_plot), intent(in) :: this
        logical :: x
        x = this%m_ticsIn
    end function

! --------------------
    !> @brief Sets a value determining if the axis tic marks should point
    !! inwards.
    !!
    !! @param[in,out] this The scatter_plot object.
    !! @param[in] x Set to true if the tic marks should point inwards; else,
    !!  false if the tic marks should point outwards.
    subroutine sp_set_tics_in(this, x)
        class(scatter_plot), intent(inout) :: this
        logical, intent(in) :: x
        this%m_ticsIn = x
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets a value determining if the border should be drawn.
    !!
    !! @param[in] this The scatter_plot object.
    !! @return Returns true if the border should be drawn; else, false.
    pure function sp_get_draw_border(this) result(x)
        class(scatter_plot), intent(in) :: this
        logical :: x
        x = this%m_drawBorder
    end function

! --------------------
    !> @brief Sets a value determining if the border should be drawn.
    !!
    !! @param[in,out] this The scatter_plot object.
    !! @param[in] x Set to true if the border should be drawn; else, false.
    subroutine sp_set_draw_border(this, x)
        class(scatter_plot), intent(inout) :: this
        logical, intent(in) :: x
        this%m_drawBorder = x
    end subroutine

! ******************************************************************************
! X_AXIS MEMBERS
! ------------------------------------------------------------------------------
    !> @brief Gets the axis identification string.
    !!
    !! @param[in] this The x_axis object.
    !! @return The string.
    function xa_get_id(this) result(x)
        class(x_axis), intent(in) :: this
        character(len = :), allocatable :: x
        x = "x"
    end function

! ******************************************************************************
! Y_AXIS MEMBERS
! ------------------------------------------------------------------------------
    !> @brief Gets the axis identification string.
    !!
    !! @param[in] this The y_axis object.
    !! @return The string.
    function ya_get_id(this) result(x)
        class(y_axis), intent(in) :: this
        character(len = :), allocatable :: x
        x = "y"
    end function

! ******************************************************************************
! Y2_AXIS MEMBERS
! ------------------------------------------------------------------------------
    !> @brief Gets the axis identification string.
    !!
    !! @param[in] this The y2_axis object.
    !! @return The string.
    function y2a_get_id(this) result(x)
        class(y2_axis), intent(in) :: this
        character(len = :), allocatable :: x
        x = "y2"
    end function

! ------------------------------------------------------------------------------
end module
