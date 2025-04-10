module fplot_constants
    use iso_fortran_env
    implicit none


! ******************************************************************************
! GNUPLOT TERMINAL CONSTANTS
! ------------------------------------------------------------------------------
    integer(int32), parameter :: GNUPLOT_TERMINAL_WIN32 = 1
        !! Defines a Win32 terminal.
    integer(int32), parameter :: GNUPLOT_TERMINAL_WXT = 2
        !! Defines a WXT terminal.
    integer(int32), parameter :: GNUPLOT_TERMINAL_QT = 3
        !! Defines a QT terminal.
    integer(int32), parameter :: GNUPLOT_TERMINAL_PNG = 4
        !! Defines a PNG terminal.
    integer(int32), parameter :: GNUPLOT_TERMINAL_LATEX = 5
        !! Defines a LATEX terminal.

! ******************************************************************************
! MARKER CONSTANTS
! ------------------------------------------------------------------------------
    integer(int32), parameter :: MARKER_PLUS = 1
        !! Defines a + data point marker.
    integer(int32), parameter :: MARKER_X = 2
        !! Defines an x data point marker.
    integer(int32), parameter :: MARKER_ASTERISK = 3
        !! Defines an * data point marker.
    integer(int32), parameter :: MARKER_EMPTY_SQUARE = 4
        !! Defines an empty square-shaped data point marker.
    integer(int32), parameter :: MARKER_FILLED_SQUARE = 5
        !! Defines an filled square-shaped data point marker.
    integer(int32), parameter :: MARKER_EMPTY_CIRCLE = 6
        !! Defines an empty circle-shaped data point marker.
    integer(int32), parameter :: MARKER_FILLED_CIRCLE = 7
        !! Defines an filled circle-shaped data point marker.
    integer(int32), parameter :: MARKER_EMPTY_TRIANGLE = 8
        !! Defines an empty triangle-shaped data point marker.
    integer(int32), parameter :: MARKER_FILLED_TRIANGLE = 9
        !! Defines an filled triangle-shaped data point marker.
    integer(int32), parameter :: MARKER_EMPTY_NABLA = 10
        !! Defines an empty nabla-shaped data point marker.
    integer(int32), parameter :: MARKER_FILLED_NABLA = 11
        !! Defines an filled nabla-shaped data point marker.
    integer(int32), parameter :: MARKER_EMPTY_RHOMBUS = 12
        !! Defines an empty rhombus-shaped data point marker.
    integer(int32), parameter :: MARKER_FILLED_RHOMBUS = 13
        !! Defines an filled rhombus-shaped data point marker.

! ******************************************************************************
! LINE CONSTANTS
! ------------------------------------------------------------------------------
    integer(int32), parameter :: LINE_SOLID = 1
        !! Defines a solid line.
    integer(int32), parameter :: LINE_DASHED = 2
        !! Defines a dashed line.
    integer(int32), parameter :: LINE_DOTTED = 3
        !! Defines a dotted line.
    integer(int32), parameter :: LINE_DASH_DOTTED = 4
        !! Defines a dash-dotted line.
    integer(int32), parameter :: LINE_DASH_DOT_DOT = 5
        !! Defines a dash-dot-dotted line.

! ******************************************************************************
! LEGEND CONSTANTS
! ------------------------------------------------------------------------------
    character(len = *), parameter :: LEGEND_TOP = "top"
        !! Defines the legend should be placed at the top of the plot.
    character(len = *), parameter :: LEGEND_CENTER = "center"
        !! Defines the legend should be centered on the plot.
    character(len = *), parameter :: LEGEND_LEFT = "left"
        !! Defines the legend should be placed at the left of the plot.
    character(len = *), parameter :: LEGEND_RIGHT = "right"
        !! Defines the legend should be placed at the right of the plot.
    character(len = *), parameter :: LEGEND_BOTTOM = "bottom"
        !! Defines the legend should be placed at the bottom of the plot.
    character(len = *), parameter :: LEGEND_ARRANGE_VERTICALLY = "vertical"
        !! Defines the legend should be arranged such that the column count
        !! is minimized.
    character(len = *), parameter :: LEGEND_ARRANGE_HORIZONTALLY = "horizontal"
        !! Defines the legend should be arranged such that the row count
        !! is minimized.

! ******************************************************************************
! POLAR PLOT CONSTANTS
! ------------------------------------------------------------------------------
    character(len = *), parameter :: POLAR_THETA_TOP = "top"
        !! States that theta should start at the top of the plot.
    character(len = *), parameter :: POLAR_THETA_RIGHT = "right"
        !! States that theta should start at the right of the plot.
    character(len = *), parameter :: POLAR_THETA_BOTTOM = "bottom"
        !! States that theta should start at the bottom of the plot.
    character(len = *), parameter :: POLAR_THETA_LEFT = "left"
        !! States that theta should start at the left of the plot.
    character(len = *), parameter :: POLAR_THETA_CCW = "ccw"
        !! States that theta should proceed in a counter-clockwise direction.
    character(len = *), parameter :: POLAR_THETA_CW = "cw"
        !! States that theta should proceed in a clockwise direction.

! ******************************************************************************
! COORDINATE SYSTEM CONSTANTS
! ------------------------------------------------------------------------------
    integer(int32), parameter :: COORDINATES_CARTESIAN = 100
        !! Defines a Cartesian coordinate system.
    integer(int32), parameter :: COORDINATES_SPHERICAL = 101
        !! Defines a spherical coordinate system.
    integer(int32), parameter :: COORDINATES_CYLINDRICAL = 102
        !! Defines a cylindrical coordinate system.

! ******************************************************************************
! ARROW CONSTANTS
! ------------------------------------------------------------------------------
    integer(int32), parameter :: ARROW_NO_HEAD = 0
        !! Defines an arrow with no head.
    integer(int32), parameter :: ARROW_HEAD = 1
        !! Defines an arrow with a traditional head.
    integer(int32), parameter :: ARROW_BACKHEAD = 2
        !! Defines an arrow with it's head at it's back end (tail).
    integer(int32), parameter :: ARROW_HEADS = 3
        !! Defines an arrow with a head on both ends.
    integer(int32), parameter :: ARROW_FILLED = 100
        !! Defines a filled arrow head.
    integer(int32), parameter :: ARROW_EMPTY = 101
        !! Defines an empty arrow head.
    integer(int32), parameter :: ARROW_NO_FILL = 102
        !! Defines an arrow head without fill.
    integer(int32), parameter :: ARROW_NO_BORDER = 103
        !! Defines an arrow head with no border.

! ******************************************************************************
! PLOT DATA CONSTANTS
! ------------------------------------------------------------------------------
    integer(int32), parameter :: PLOTDATA_MAX_NAME_LENGTH = 128
        !! Defines the maximum number of characters allowed in a graph label.

! ******************************************************************************
! PRIVATE/DEFAULT CONSTANTS
! ------------------------------------------------------------------------------
    integer(int32), parameter :: GNUPLOT_DEFAULT_WINDOW_WIDTH = 640
        !! The default GNUPLOT window width, in pixels.
    integer(int32), parameter :: GNUPLOT_DEFAULT_WINDOW_HEIGHT = 420
        !! The default GNUPLOT window height, in pixels.
    integer(int32), parameter :: GNUPLOT_MAX_LABEL_LENGTH = 128
        !! Defines the maximum number of characters allowed in a graph label.
    character(len = *), parameter :: GNUPLOT_DEFAULT_FONTNAME = "Calibri"
        !! Defines the default font used by text on the graph.
    integer(int32), parameter :: GNUPLOT_DEFAULT_FONT_SIZE = 14
        !! Defines the default font size used by text on the graph.
    integer(int32), parameter :: GNUPLOT_MAX_PATH_LENGTH = 256
        !! Defines the maximum number of characters allowed in a file path.

! ******************************************************************************
! HORIZONTAL ALIGNMENT CONSTANTS
! ------------------------------------------------------------------------------
    character(len = *), parameter :: GNUPLOT_HORIZONTAL_ALIGN_LEFT = "left"
        !! Defines the text should be aligned to the left.
    character(len = *), parameter :: GNUPLOT_HORIZONTAL_ALIGN_CENTER = "center"
        !! Defines the text should be centered.
    character(len = *), parameter :: GNUPLOT_HORIZONTAL_ALIGN_RIGHT = "right"
        !! Defines the text should be aligned to the right.

! ******************************************************************************
! ROTATION ORIGIN CONSTANTS
! ------------------------------------------------------------------------------
    character(len = *), parameter :: GNUPLOT_ROTATION_ORIGIN_RIGHT = "right"
        !! Defines the text should be rotated around the right side of the text.
    character(len = *), parameter :: GNUPLOT_ROTATION_ORIGIN_CENTER = "center"
        !! Defines the text should be rotated around the center of the text.
    character(len = *), parameter :: GNUPLOT_ROTATION_ORIGIN_LEFT = "left"
        !! Defines the text should be rotated around the left side of the text.
end module