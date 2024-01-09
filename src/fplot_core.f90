! fplot_core.f90

!> @brief \b fplot_core
!!
!! @par Purpose
!! FPLOT is a Fortran library providing a means of interacting with
!! [Gnuplot](http://www.gnuplot.info/) from a Fortran program.  The library is
!! designed in an object-oriented manner, and as such utilizes language features
!! that require a compiler that supports the 2003 and 2008 standards.  Additionally,
!! it is expected that Gnuplot is installed on the system path.  For full
!! functionallity, a minimum of Gnuplot v5.2 is expected.
!!
!! @image html example_surface_plot_lighting_2.png
module fplot_core
    use iso_fortran_env, only : real64, real32, int32
    use iso_c_binding
    use fplot_string_builder
    use collections
    use iso_varying_string
    use ferror
    use forcolormap, cmap => Colormap ! avoid conflict with the internally defined colormap type
    implicit none
    private
    public :: PLOT_OUT_OF_MEMORY_ERROR
    public :: PLOT_INVALID_INPUT_ERROR
    public :: PLOT_INVALID_OPERATION_ERROR
    public :: PLOT_ARRAY_SIZE_MISMATCH_ERROR
    public :: PLOT_GNUPLOT_FILE_ERROR
    public :: GNUPLOT_TERMINAL_WIN32
    public :: GNUPLOT_TERMINAL_WXT
    public :: GNUPLOT_TERMINAL_QT
    public :: GNUPLOT_TERMINAL_PNG
    public :: GNUPLOT_TERMINAL_LATEX
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
    public :: CLR_ORANGE
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
    public :: LEGEND_ARRANGE_VERTICALLY
    public :: LEGEND_ARRANGE_HORIZONTALLY
    public :: POLAR_THETA_BOTTOM
    public :: POLAR_THETA_LEFT
    public :: POLAR_THETA_RIGHT
    public :: POLAR_THETA_TOP
    public :: POLAR_THETA_CCW
    public :: POLAR_THETA_CW
    public :: PLOTDATA_MAX_NAME_LENGTH
    public :: COORDINATES_CARTESIAN
    public :: COORDINATES_SPHERICAL
    public :: COORDINATES_CYLINDRICAL
    public :: ARROW_NO_HEAD
    public :: ARROW_HEAD
    public :: ARROW_BACKHEAD
    public :: ARROW_HEADS
    public :: ARROW_FILLED
    public :: ARROW_EMPTY
    public :: ARROW_NO_FILL
    public :: ARROW_NO_BORDER
    public :: linspace
    public :: logspace
    public :: meshgrid
    public :: color
    public :: plot_data
    public :: plot_axis
    public :: terminal
    public :: windows_terminal
    public :: qt_terminal
    public :: wxt_terminal
    public :: png_terminal
    public :: latex_terminal
    public :: legend
    public :: plot
    public :: scatter_plot_data
    public :: plot_data_2d
    public :: plot_2d
    public :: plot_data_3d
    public :: plot_3d
    public :: surface_plot_data
    public :: surface_plot
    public :: colormap
    public :: rainbow_colormap
    public :: hot_colormap
    public :: cool_colormap
    public :: plot_label
    public :: multiplot
    public :: plot_data_error_bars
    public :: plot_data_colored
    public :: plot_data_bar
    public :: plot_data_histogram
    public :: plot_bar
    public :: plot_data_tri_2d
    public :: delaunay_tri_surface
    public :: delaunay_tri_2d
    public :: tri_surface_plot_data
    public :: vector_field_plot_data
    public :: plot_polar
    public :: filled_plot_data
    public :: parula_colormap
    public :: grey_colormap
    public :: earth_colormap
    public :: simplify_polyline
    public :: plot_arrow
    public :: custom_colormap
    public :: cmap
    public :: assignment(=)
    public :: operator(==)
    public :: operator(/=)

! ******************************************************************************
! ERROR CODES
! ------------------------------------------------------------------------------
    !> @brief Occurs if there is insufficient memory available for the
    !! requested operation.
    integer(int32), parameter :: PLOT_OUT_OF_MEMORY_ERROR = 1000
    !> @brief Occurs if an invalid input is provided.
    integer(int32), parameter :: PLOT_INVALID_INPUT_ERROR = 1001
    !> @brief Occurs if an attempt is made to perform an invalid operation.
    integer(int32), parameter :: PLOT_INVALID_OPERATION_ERROR = 1002
    !> @brief Occurs if there is an array size mismatch error.
    integer(int32), parameter :: PLOT_ARRAY_SIZE_MISMATCH_ERROR = 1003
    !> @brief Occurs if there is a GNUPLOT file error.
    integer(int32), parameter :: PLOT_GNUPLOT_FILE_ERROR = 1004

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
    !> @brief Defines a LATEX terminal.
    integer(int32), parameter :: GNUPLOT_TERMINAL_LATEX = 5

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
    !> @brief Defines the legend should be arranged such that the column count
    !! is minimized.
    character(len = *), parameter :: LEGEND_ARRANGE_VERTICALLY = "vertical"
    !> @brief Defines the legend should be arranged such that the row count
    !! is minimized.
    character(len = *), parameter :: LEGEND_ARRANGE_HORIZONTALLY = "horizontal"

! ******************************************************************************
! POLAR PLOT CONSTANTS
! ------------------------------------------------------------------------------
    !> @brief States that theta should start at the top of the plot.
    character(len = *), parameter :: POLAR_THETA_TOP = "top"
    !> @brief States that theta should start at the right of the plot.
    character(len = *), parameter :: POLAR_THETA_RIGHT = "right"
    !> @brief States that theta should start at the bottom of the plot.
    character(len = *), parameter :: POLAR_THETA_BOTTOM = "bottom"
    !> @brief States that theta should start at the left of the plot.
    character(len = *), parameter :: POLAR_THETA_LEFT = "left"
    !> @brief States that theta should proceed in a counter-clockwise direction.
    character(len = *), parameter :: POLAR_THETA_CCW = "ccw"
    !> @brief States that theta should proceed in a clockwise direction.
    character(len = *), parameter :: POLAR_THETA_CW = "cw"

! ******************************************************************************
! COORDINATE SYSTEM CONSTANTS
! ------------------------------------------------------------------------------
    !> @brief Defines a Cartesian coordinate system.
    integer(int32), parameter :: COORDINATES_CARTESIAN = 100
    !> @brief Defines a spherical coordinate system.
    integer(int32), parameter :: COORDINATES_SPHERICAL = 101
    !> @brief Defines a cylindrical coordinate system.
    integer(int32), parameter :: COORDINATES_CYLINDRICAL = 102

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
    integer(int32), parameter :: GNUPLOT_DEFAULT_FONT_SIZE = 14
    !> @brief Defines the maximum number of characters allowed in a file path.
    integer(int32), parameter :: GNUPLOT_MAX_PATH_LENGTH = 256

! ******************************************************************************
! OPERATORS
! ------------------------------------------------------------------------------
    interface assignment(=)
        module procedure :: clr_assign
        module procedure :: lbl_assign
        module procedure :: par_assign
    end interface

    interface operator(==)
        module procedure :: clr_equals
    end interface

    interface operator(/=)
        module procedure :: clr_not_equals
    end interface

! ******************************************************************************
! BASE TYPES
! ------------------------------------------------------------------------------
    !> @brief The base type for a GNUPLOT object.
    type, abstract :: plot_object
    contains
        !> @brief Returns the appropriate GNUPLOT command string to define the
        !! plot object properties.
        procedure(get_string_result), deferred, public :: get_command_string
    end type

! ******************************************************************************
! FPLOT_CORE_ROUTINES.F90
! ------------------------------------------------------------------------------
    interface
        !> @brief Constructs a linearly spaced array.
        !!
        !! @param[in] start The first value in the array.
        !! @param[in] finish The last value in the array.
        !! @param[in] npts The number of values in the array.
        !!
        !! @return The resulting array.
        pure module function linspace(start, finish, npts) result(x)
            real(real64), intent(in) :: start, finish
            integer(int32), intent(in) :: npts
            real(real64), allocatable, dimension(:) :: x
        end function

        !> @brief Construcst a logarithmically spaced array.
        !!
        !! @param[in] start The exponent of the first value in the array.
        !! @param[in] finish The exponent of the final value in the array.
        !! @param[in] npts The number of values in the array.
        !!
        !! @return The resulting array.
        pure module function logspace(start, finish, npts) result(x)
            real(real64), intent(in) :: start, finish
            integer(int32), intent(in) :: npts
            real(real64), allocatable, dimension(:) :: x
        end function

        !> @brief Constructs two matrices (X and Y) from x and y data arrays.
        !!
        !! @param[in] x An M-element array of x data points.
        !! @param[in] y An N-element array of y data points.
        !! @return An N-by-M-by-2 array containing the x data matrix on the
        !!  first page of the array, and the y data matrix on the second page.
        pure module function meshgrid(x, y) result(xy)
            real(real64), intent(in), dimension(:) :: x, y
            real(real64), allocatable, dimension(:,:,:) :: xy
        end function
    end interface

! ******************************************************************************
! FPLOT_COLORS.F90
! ------------------------------------------------------------------------------
    !> @brief Describes an RGB color.
    type color
        !> @brief The red component of the color (must be between 0 and 255).
        integer(int32), public :: red = 0
        !> @brief The green component of the color (must be between 0 and 255).
        integer(int32), public :: green = 0
        !> @brief The blue component of the color (must be between 0 and 255).
        integer(int32), public :: blue = 255
    contains
        !> @brief Returns the color in hexadecimal format.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure character(6) function clr_to_hex_string(class(color) this)
        !! @endcode
        !!
        !! @param[in] this The color object.
        !! @return A string containing the hexadecimal equivalent.
        procedure, public, pass :: to_hex_string => clr_to_hex_string
        !> @brief Copies another color to this color.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine copy_from(class(color) this, class(color) clr)
        !! @endcode
        !!
        !! @param[in,out] this The color object.
        !! @param[in] clr The color to copy.
        procedure, public, pass :: copy_from => clr_copy_from
    end type

! ------------------------------------------------------------------------------
    interface
        pure module function clr_to_hex_string(this) result(txt)
            class(color), intent(in) :: this
            character(6) :: txt
        end function

        pure module subroutine clr_copy_from(this, clr)
            class(color), intent(inout) :: this
            class(color), intent(in) :: clr
        end subroutine

        pure module subroutine clr_assign(x, y)
            type(color), intent(out) :: x
            class(color), intent(in) :: y
        end subroutine

        pure module function clr_equals(x, y) result(rst)
            type(color), intent(in) :: x, y
            logical :: rst
        end function

        pure module function clr_not_equals(x, y) result(rst)
            type(color), intent(in) :: x, y
            logical :: rst
        end function
    end interface

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
    !> @brief Defines an orange color.
    type(color), parameter :: CLR_ORANGE = color(255, 165, 0)

    ! A list of colors that can be cycled through by plotting code
    type(color), parameter, dimension(7) :: color_list = [ &
        color(0, int(0.447 * 255), int(0.741 * 255)), &
        color(int(0.85 * 255), int(0.325 * 255), int(0.098 * 255)), &
        color(int(0.929 * 255), int(0.694 * 255), int(0.125 * 255)), &
        color(int(0.494 * 255), int(0.184 * 255), int(0.556 * 255)), &
        color(int(0.466 * 255), int(0.674 * 255), int(0.188 * 255)), &
        color(int(0.301 * 255), int(0.745 * 255), int(0.933 * 255)), &
        color(int(0.635 * 255), int(0.078 * 255), int(0.184 * 255))]
    ! type(color), parameter, dimension(8) :: color_list = [ &
    !     CLR_BLUE, CLR_GREEN, CLR_RED, CLR_CYAN, CLR_LIME, CLR_PURPLE, &
    !     CLR_ORANGE, CLR_BLACK]

! ******************************************************************************
! FPLOT_LABEL.F90
! ------------------------------------------------------------------------------
    !> @brief Defines a label object for a plot.
    type, extends(plot_object) :: plot_label
    private
        !> Determines if the label is visible
        logical :: m_visible = .true.
        !> The x, y, and z coordinates of the label
        real(real32), dimension(3) :: m_position
        !> The rotation angle of the label
        real(real32) :: m_angle = 0.0
        !> The label text
        character(len = PLOTDATA_MAX_NAME_LENGTH) :: m_text
    contains
        !> @brief Gets the GNUPLOT command string for the label.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(:) function allocatable get_command_string(class(plot_label) this)
        !! @endcode
        !!
        !! @param[in] this The plot_label object.
        !! @return The command string.
        procedure, public :: get_command_string => lbl_get_cmd
        !> @brief Gets a value determining if the label is to be drawn.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function get_is_visible(class(plot_label) this)
        !! @endcode
        !!
        !! @param[in] this The plot_label object.
        !! @return Returns true if the label is to be drawn; else, false.
        procedure, public :: get_is_visible => lbl_get_is_visible
        !> @brief Sets a value determining if the label is to be drawn.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_is_visible(class(plot_label) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The plot_label object.
        !! @param[in] x Set to true to draw the label; else, false.
        procedure, public :: set_is_visible => lbl_set_is_visible
        !> @brief Gets the position of the label in terms of plot coordinates.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure real(real32) dimension(3) function get_position(class(plot_label) this)
        !! @endcode
        !!
        !! @param[in] this The plot_label object.
        !! @return A 3-element array containing the X, Y, and Z position of the label.
        procedure, public :: get_position => lbl_get_position
        !> @brief Sets the position of the label in terms of plot coordinates.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_position(class(plot_label) this, real(real32) x(3))
        !! @endcode
        !!
        !! @param[in,out] this The plot_label object.
        !! @param[in] x A 3-element array containing the X, Y, and Z position of the
        !!  label.
        procedure, public :: set_position => lbl_set_position
        !> @brief Gets the angle of the label text, in degrees.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure real(real32) function get_angle(class(plot_label) this)
        !! @endcode
        !!
        !! @param[in] this The plot_label object.
        !! @return The angle, in degrees.
        procedure, public :: get_angle => lbl_get_angle
        !> @brief Sets the angle of the label text, in degrees.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_angle(class(plot_label) this, real(real32) x)
        !! @endcode
        !!
        !! @param[in,out] this The plot_label object.
        !! @param[in] x The angle, in degrees.
        procedure, public :: set_angle => lbl_set_angle
        !> @brief Gets the text displayed by the label.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function allocatable get_text(class(plot_label) this)
        !! @endcode
        !!
        !! @param[in] this The plot_label object.
        !! @return The string of text to display.
        procedure, public :: get_text => lbl_get_txt
        !> @brief Sets the text displayed by the label.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_text(class(plot_label) this, character(len = *) x)
        !! @endcode
        !!
        !! @param[in,out] this The plot_label object.
        !! @param[in] x The text string to display.
        procedure, public :: set_text => lbl_set_txt
    end type

! ------------------------------------------------------------------------------
    interface
        module function lbl_get_cmd(this) result(x)
            class(plot_label), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        pure module function lbl_get_is_visible(this) result(x)
            class(plot_label), intent(in) :: this
            logical :: x
        end function

        module subroutine lbl_set_is_visible(this, x)
            class(plot_label), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        pure module function lbl_get_position(this) result(x)
            class(plot_label), intent(in) :: this
            real(real32), dimension(3) :: x
        end function

        module subroutine lbl_set_position(this, x)
            class(plot_label), intent(inout) :: this
            real(real32), intent(in), dimension(3) :: x
        end subroutine

        pure module function lbl_get_angle(this) result(x)
            class(plot_label), intent(in) :: this
            real(real32) :: x
        end function

        module subroutine lbl_set_angle(this, x)
            class(plot_label), intent(inout) :: this
            real(real32), intent(in) :: x
        end subroutine

        module function lbl_get_txt(this) result(x)
            class(plot_label), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        module subroutine lbl_set_txt(this, x)
            class(plot_label), intent(inout) :: this
            character(len = *), intent(in) :: x
        end subroutine

        pure module subroutine lbl_assign(x, y)
            type(plot_label), intent(out) :: x
            class(plot_label), intent(in) :: y
        end subroutine
    end interface

! ******************************************************************************
! FPLOT_ARROW.F90
! ------------------------------------------------------------------------------
    !> @brief Defines an arrow with no head.
    integer(int32), parameter :: ARROW_NO_HEAD = 0
    !> @brief Defines an arrow with a traditional head.
    integer(int32), parameter :: ARROW_HEAD = 1
    !> @brief Defines an arrow with it's head at it's back end (tail).
    integer(int32), parameter :: ARROW_BACKHEAD = 2
    !> @brief Defines an arrow with a head on both ends.
    integer(int32), parameter :: ARROW_HEADS = 3
    !> @brief Defines a filled arrow head.
    integer(int32), parameter :: ARROW_FILLED = 100
    !> @brief Defines an empty arrow head.
    integer(int32), parameter :: ARROW_EMPTY = 101
    !> @brief Defines an arrow head without fill.
    integer(int32), parameter :: ARROW_NO_FILL = 102
    !> @brief Defines an arrow head with no border.
    integer(int32), parameter :: ARROW_NO_BORDER = 103

! ------------------------------------------------------------------------------
    !> @brief Defines an arrow to be used on by a @ref plot object.
    type, extends(plot_object) :: plot_arrow
        ! Determines if the arrow is visible.
        logical, private :: m_visible = .true.
        ! The x, y, z coordinates of the tail
        real(real32), dimension(3) :: m_tail = [0.0, 0.0, 0.0]
        ! The x, y, z coordinates of the head
        real(real32), dimension(3) :: m_head = [0.0, 0.0, 0.0]
        ! The arrow color.
        type(color) :: m_color = CLR_BLACK
        ! The line style
        integer(int32) :: m_linestyle = LINE_SOLID
        ! The line width
        real(real32) :: m_linewidth = 1.0
        ! The head configuration
        integer(int32) :: m_head_type = ARROW_HEAD
        ! Arrow filling
        integer(int32) :: m_filling = ARROW_FILLED
        ! Move to front?
        logical :: m_front = .true.
        ! Arrow head size
        real(real32) :: m_size = 0.375
        ! Arrow head angle
        real(real32) :: m_angle = 10.0
        ! Arrow head back angle
        real(real32) :: m_backangle = 90.0
        ! Use default head size
        logical :: m_use_default_size = .true.
    contains
        !> @brief Gets a value determining if the arrow is visible.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function get_is_visible(class(plot_arrow) this)
        !! @endcode
        !!
        !! @param[in] this The @ref plot_arrow object.
        !! @return True if the arrow is visible; else, false.
        procedure, public :: get_is_visible => par_get_is_visible
        !> @brief Sets a value determining if the arrow is visible.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_is_visible(class(plot_arrow) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The @ref plot_arrow object.
        !! @param[in] x True if the arrow is visible; else, false.
        procedure, public :: set_is_visible => par_set_is_visible
        !> @brief Gets the coordinates of the arrow's tail.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure real(real32)(3) function get_tail_location(class(plot_arrow) this)
        !! @endcode
        !!
        !! @param[in] this The @ref plot_arrow object.
        !! @return A 3-element array containing the x, y, and z coordinates of
        !!  the arrow's tail.
        procedure, public :: get_tail_location => par_get_tail
        !> @brief Sets the location of the arrow's tail.
        !!
        !! @par Syntax 1
        !! @code{.f90}
        !! subroutine set_tail_location(class(plot_arrow) this, real(real32) x(3))
        !! @endcode
        !!
        !! @param[in,out] this The @ref plot_arrow object.
        !! @param[in] x A 3-element array containing the x, y, and z coordiantes
        !!  of the arrow's tail.
        !!
        !! @par Syntax 2
        !! @code{.f90}
        !! subroutine set_tail_location(class(plot_arrow) this, real(real32) x, real(real32) y)
        !! @endcode
        !!
        !! @param[in,out] this The @ref plot_arrow object.
        !! @param[in] x The x-coordinate of the arrow's tail.
        !! @param[in] y The y-coordinate of the arrow's tail.
        !!
        !! @par Syntax 3
        !! @code{.f90}
        !! subroutine set_tail_location(class(plot_arrow) this, real(real32) x, real(real32) y, real(real32) z)
        !! @endcode
        !!
        !! @param[in,out] this The @ref plot_arrow object.
        !! @param[in] x The x-coordinate of the arrow's tail.
        !! @param[in] y The y-coordinate of the arrow's tail.
        !! @param[in] z The z-coordinate of the arrow's tail.
        generic, public :: set_tail_location => par_set_tail_1, &
            par_set_tail_2, par_set_tail_3
        procedure, private :: par_set_tail_1
        procedure, private :: par_set_tail_2
        procedure, private :: par_set_tail_3
        !> @brief Gets the coordinates of the arrow's head.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure real(real32)(3) function get_head_location(class(plot_arrow) this)
        !! @endcode
        !!
        !! @param[in] this The @ref plot_arrow object.
        !! @return A 3-element array containing the x, y, and z coordinates of
        !!  the arrow's head.
        procedure, public :: get_head_location => par_get_head
        !> @brief Sets the location of the arrow's head.
        !!
        !! @par Syntax 1
        !! @code{.f90}
        !! subroutine set_head_location(class(plot_arrow) this, real(real32) x(3))
        !! @endcode
        !!
        !! @param[in,out] this The @ref plot_arrow object.
        !! @param[in] x A 3-element array containing the x, y, and z coordiantes
        !!  of the arrow's head.
        !!
        !! @par Syntax 2
        !! @code{.f90}
        !! subroutine set_head_location(class(plot_arrow) this, real(real32) x, real(real32) y)
        !! @endcode
        !!
        !! @param[in,out] this The @ref plot_arrow object.
        !! @param[in] x The x-coordinate of the arrow's head.
        !! @param[in] y The y-coordinate of the arrow's head.
        !!
        !! @par Syntax 3
        !! @code{.f90}
        !! subroutine set_head_location(class(plot_arrow) this, real(real32) x, real(real32) y, real(real32) z)
        !! @endcode
        !!
        !! @param[in,out] this The @ref plot_arrow object.
        !! @param[in] x The x-coordinate of the arrow's head.
        !! @param[in] y The y-coordinate of the arrow's head.
        !! @param[in] z The z-coordinate of the arrow's head.
        generic, public :: set_head_location => par_set_head_1, &
            par_set_head_2, par_set_head_3
        procedure, private :: par_set_head_1
        procedure, private :: par_set_head_2
        procedure, private :: par_set_head_3
        !> @brief Gets the color of the arrow.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure type(color) function get_color(class(arrow) this)
        !! @endcode
        !!
        !! @param[in] this The @ref plot_arrow object.
        !! @return The color.
        procedure, public :: get_color => par_get_color
        !> @brief Sets the color of the arrow.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_color(class(arrow) this, type(color) x)
        !! @endcode
        !!
        !! @param[in,out] this The @ref plot_arrow object.
        !! @param[in] x The color.
        procedure, public :: set_color => par_set_color
        !> @brief Gets the line style used to draw the arrow.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure integer(int32) function get_line_style(class(plot_arrow) this)
        !! @endcode
        !!
        !! @param[in] this The @ref plot_arrow object.
        !! @return The line style.
        procedure, public :: get_line_style => par_get_line_style
        !> @brief Sets the line style used to draw the arrow.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_line_style(class(plot_arrow) this, integer(int32) x)
        !! @endcode
        !!
        !! @param[in,out] this The @ref plot_arrow object.
        !! @param The line style.  The value must be one of the following.
        !!  - LINE_SOLID
        !!  - LINE_DASHED
        !!  - LINE_DASH_DOTTED
        !!  - LINE_DASH_DOT_DOT
        !!  - LINE_DOTTED
        !! If the value is not one of the above, the command is ignored.
        procedure, public :: set_line_style => par_set_line_style
        !> @brief Gets the width of the lines used to draw the arrow.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure real(real32) function get_line_width(class(plot_arrow) this)
        !! @endcode
        !!
        !! @param[in] this The @ref plot_arrow object.
        !! @return The width of the line.
        procedure, public :: get_line_width => par_get_line_width
        !> @brief Sets the width of the lines used to draw the arrow.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_line_width(class(plot_arrow) this, real(real32) x)
        !! @endcode
        !!
        !! @param[in,out] this The @ref plot_arrow object.
        !! @param[in] x The width of the line.
        procedure, public :: set_line_width => par_set_line_width
        !> @brief Gets the type of arrow head.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure integer(int32) function get_head_type(class(plot_arrow) this)
        !! @endcode
        !!
        !! @param[in] this The @ref plot_arrow object.
        !! @return The arrow head type.  It is one of the following constants.
        !! - ARROW_HEAD
        !! - ARROW_BACKHEAD
        !! - ARROW_HEADS
        !! - ARROW_NO_HEAD
        procedure, public :: get_head_type => par_get_head_type
        !> @brief Sets the type of arrow head.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_head_type(class(plot_arrow) this, integer(int32) x)
        !! @endcode
        !!
        !! @param[in,out] this The @ref plot_arrow object.
        !! @param[in] x The arrow head type.  It must be one of the following 
        !!  constants.
        !! - ARROW_HEAD
        !! - ARROW_BACKHEAD
        !! - ARROW_HEADS
        !! - ARROW_NO_HEAD
        procedure, public :: set_head_type => par_set_head_type
        !> @brief Gets a flag denoting the head fill type.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure integer(int32) function get_head_fill(class(plot_arrow) this)
        !! @endcode
        !!
        !! @param[in] this The @ref plot_arrow object.
        !! @return The flag denoting head fill.  It will be one of the 
        !!  following constants.
        !! - ARROW_FILLED
        !! - ARROW_EMPTY
        !! - ARROW_NO_BORDER
        !! - ARROW_NO_FILL
        procedure, public :: get_head_fill => par_get_fill
        !> @brief Sets a flag denoting the head fill type.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_head_fill(class(plot_arrow) this, integer(int32) x)
        !! @endcode
        !!
        !! @param[in,out] this The @ref plot_arrow object.
        !! @param[in] x The flag denoting head fill.  It must be one of the 
        !!  following constants.
        !! - ARROW_FILLED
        !! - ARROW_EMPTY
        !! - ARROW_NO_BORDER
        !! - ARROW_NO_FILL
        procedure, public :: set_head_fill => par_set_fill
        !> @brief Gets a value determining if the arrow should be moved to the
        !! front.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function get_move_to_front(class(plot_arrow) this)
        !! @endcode
        !!
        !! @param[in] this The @ref plot_arrow object.
        !! @return True if the arrow should be moved to the front; else, false.
        procedure, public :: get_move_to_front => par_get_move_to_front
        !> @brief Sets a value determining if the arrow should be moved to the
        !! front.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_move_to_front(class(plot_arrow) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The @ref plot_arrow object.
        !! @param[in] x True if the arrow should be moved to the front; else, 
        !!  false.
        procedure, public :: set_move_to_front => par_set_move_to_front
        !> @brief Gets the size of the arrow head.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure real(real32) function get_head_size(class(plot_arrow) this)
        !! @endcode
        !!
        !! @param[in] this The @ref plot_arrow object.
        !! @return The head size.
        procedure, public :: get_head_size => par_get_head_size
        !> @brief Sets the size of the arrow head.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_head_size(class(plot_arrow) this, real(real32) x)
        !! @endcode
        !!
        !! @param[in,out] this The @ref plot_arrow object.
        !! @param[in] x The head size.
        procedure, public :: set_head_size => par_set_head_size
        !> @brief Gets the angle of the arrow head.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure real(real32) function get_head_angle(class(plot_arrow) this)
        !! @endcode
        !!
        !! @param[in] this The @ref plot_arrow object.
        !! @return The angle, in degrees.
        procedure, public :: get_head_angle => par_get_head_angle
        !> @brief Sets the angle of the arrow head.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_head_angle(class(plot_arrow) this, real(real32) x)
        !! @endcode
        !!
        !! @param[in,out] this The @ref plot_arrow object.
        !! @param[in] x The angle, in degrees.
        procedure, public :: set_head_angle => par_set_head_angle
        !> @brief Gets the angle of the back of the arrow head.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure real(real32) function get_head_back_angle(class(plot_arrow) this)
        !! @endcode
        !!
        !! @param[in] this The @ref plot_arrow object.
        !! @return The angle, in degrees.
        procedure, public :: get_head_back_angle => par_get_head_back_angle
        !> @brief Sets the angle of the back of the arrow head.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_head_back_angle(class(plot_arrow) this, real(real32) x)
        !! @endcode
        !!
        !! @param[in,out] this The @ref plot_arrow object.
        !! @param[in] x The angle, in degrees.
        procedure, public :: set_head_back_angle => par_set_head_back_angle
        !> @brief Gets a value determining if arrow head sizing defaults 
        !! should be used.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function get_use_default_size(class(plot_arrow) this)
        !! @endcode
        !!
        !! @param[in] this The @ref plot_arrow object.
        !! @return True if the defaults should be used; else, false.
        procedure, public :: get_use_default_size => par_get_use_default_size
        !> @brief Sets a value determining if arrow head sizing defaults 
        !! should be used.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_use_default_size(class(plot_arrow) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The @ref plot_arrow object.
        !! @param[in] x True if the defaults should be used; else, false.
        procedure, public :: set_use_default_size => par_set_use_default_size
        !> @brief Returns the appropriate GNUPLOT command string to establish
        !! appropriate parameters.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_command_string(class(plot_arrow) this)
        !! @endcode
        !!
        !! @param[in] this The plot_arrow object.
        !! @return The GNUPLOT command string.
        procedure, public :: get_command_string => par_get_cmd
    end type

! ------------------------------------------------------------------------------
    ! fplot_arrow.f90
    interface
        pure module function par_get_is_visible(this) result(rst)
            class(plot_arrow), intent(in) :: this
            logical :: rst
        end function

        module subroutine par_set_is_visible(this, x)
            class(plot_arrow), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        pure module function par_get_tail(this) result(rst)
            class(plot_arrow), intent(in) :: this
            real(real32), dimension(3) :: rst
        end function

        module subroutine par_set_tail_1(this, x)
            class(plot_arrow), intent(inout) :: this
            real(real32), intent(in) :: x(3)
        end subroutine

        module subroutine par_set_tail_2(this, x, y)
            class(plot_arrow), intent(inout) :: this
            real(real32), intent(in) :: x, y
        end subroutine

        module subroutine par_set_tail_3(this, x, y, z)
            class(plot_arrow), intent(inout) :: this
            real(real32), intent(in) :: x, y, z
        end subroutine

        pure module function par_get_head(this) result(rst)
            class(plot_arrow), intent(in) :: this
            real(real32), dimension(3) :: rst
        end function

        module subroutine par_set_head_1(this, x)
            class(plot_arrow), intent(inout) :: this
            real(real32), intent(in) :: x(3)
        end subroutine

        module subroutine par_set_head_2(this, x, y)
            class(plot_arrow), intent(inout) :: this
            real(real32), intent(in) :: x, y
        end subroutine

        module subroutine par_set_head_3(this, x, y, z)
            class(plot_arrow), intent(inout) :: this
            real(real32), intent(in) :: x, y, z
        end subroutine

        pure module function par_get_color(this) result(rst)
            class(plot_arrow), intent(in) :: this
            type(color) :: rst
        end function

        module subroutine par_set_color(this, x)
            class(plot_arrow), intent(inout) :: this
            type(color), intent(in) :: x
        end subroutine

        pure module function par_get_line_style(this) result(rst)
            class(plot_arrow), intent(in) :: this
            integer(int32) :: rst
        end function

        module subroutine par_set_line_style(this, x)
            class(plot_arrow), intent(inout) :: this
            integer(int32), intent(in) :: x
        end subroutine

        pure module function par_get_line_width(this) result(rst)
            class(plot_arrow), intent(in) :: this
            real(real32) :: rst
        end function

        module subroutine par_set_line_width(this, x)
            class(plot_arrow), intent(inout) :: this
            real(real32), intent(in) :: x
        end subroutine

        pure module function par_get_head_type(this) result(rst)
            class(plot_arrow), intent(in) :: this
            integer(int32) :: rst
        end function

        module subroutine par_set_head_type(this, x)
            class(plot_arrow), intent(inout) :: this
            integer(int32), intent(in) :: x
        end subroutine

        module function par_get_cmd(this) result(rst)
            class(plot_arrow), intent(in) :: this
            character(len = :), allocatable :: rst
        end function

        pure module function par_get_fill(this) result(rst)
            class(plot_arrow), intent(in) :: this
            integer(int32) :: rst
        end function

        module subroutine par_set_fill(this, x)
            class(plot_arrow), intent(inout) :: this
            integer(int32), intent(in) :: x
        end subroutine

        pure module function par_get_move_to_front(this) result(rst)
            class(plot_arrow), intent(in) :: this
            logical :: rst
        end function

        module subroutine par_set_move_to_front(this, x)
            class(plot_arrow), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        pure module function par_get_head_size(this) result(rst)
            class(plot_arrow), intent(in) :: this
            real(real32) :: rst
        end function

        module subroutine par_set_head_size(this, x)
            class(plot_arrow), intent(inout) :: this
            real(real32), intent(in) :: x
        end subroutine

        pure module function par_get_head_angle(this) result(rst)
            class(plot_arrow), intent(in) :: this
            real(real32) :: rst
        end function

        module subroutine par_set_head_angle(this, x)
            class(plot_arrow), intent(inout) :: this
            real(real32), intent(in) :: x
        end subroutine

        pure module function par_get_head_back_angle(this) result(rst)
            class(plot_arrow), intent(in) :: this
            real(real32) :: rst
        end function

        pure module function par_get_use_default_size(this) result(rst)
            class(plot_arrow), intent(in) :: this
            logical :: rst
        end function

        module subroutine par_set_use_default_size(this, x)
            class(plot_arrow), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        module subroutine par_set_head_back_angle(this, x)
            class(plot_arrow), intent(inout) :: this
            real(real32), intent(in) :: x
        end subroutine

        pure module subroutine par_assign(x, y)
            type(plot_arrow), intent(out) :: x
            class(plot_arrow), intent(in) :: y
        end subroutine
    end interface

! ******************************************************************************
! FPLOT_TERMINAL.F90
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
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure integer(int32) function get_window_width(class(terminal) this)
        !! @endcode
        !!
        !! @param[in] this The terminal object.
        !! @return The width of the plot window.
        procedure, public :: get_window_width => term_get_window_width
        !> @brief Sets the width of the plot window.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_window_width(class(terminal) this, integer(int32) x)
        !! @endcode
        !!
        !! @param[in,out] this The terminal object.
        !! @param[in] x The width of the plot window.  If a value of zero is
        !! provided, the window width is reset to its default value; or, if a
        !! negative value is provided, the absolute value of the supplied value
        !! is utilized.
        procedure, public :: set_window_width => term_set_window_width
        !> @brief Gets the height of the plot window.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure integer(int32) function get_window_height(class(terminal) this)
        !! @endcode
        !!
        !! @param[in] this The terminal object.
        !! @return The height of the plot window.
        procedure, public :: get_window_height => term_get_window_height
        !> @brief Sets the height of the plot window.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_window_height(class(terminal) this, integer(int32) x)
        !! @endcode
        !!
        !! @param[in,out] this The terminal object.
        !! @param[in] x The height of the plot window.  If a value of zero is
        !! provided, the window height is reset to its default value; or, if a
        !! negative value is provided, the absolute value of the supplied value is
        !! utilized.
        procedure, public :: set_window_height => term_set_window_height
        !> @brief Returns the appropriate GNUPLOT command string to establish
        !! appropriate parameters.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_command_string(class(terminal) this)
        !! @endcode
        !!
        !! @param[in] this The terminal object.
        !! @return The GNUPLOT command string.
        procedure, public :: get_command_string => term_get_command_string
        !> @brief Gets the targeted plot window number.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure integer(int32) function get_plot_window_number(class(terminal) this)
        !! @endcode
        !!
        !! @param[in] this The terminal object.
        !! @return The plot window number.
        procedure, public :: get_plot_window_number => &
            term_get_plot_window_number
        !> @brief Sets the targeted plot window number.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_plot_window_number(class(terminal) this, integer(int32) x)
        !! @endcode
        !!
        !! @param[in,out] this The terminal object.
        !! @param[in] x The plot window number.
        procedure, public :: set_plot_window_number => &
            term_set_plot_window_number
        !> @brief Gets the plot window's title.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_title(class(terminal) this)
        !! @endcode
        !!
        !! @param[in] this The terminal object.
        !! @return The title.
        procedure, public :: get_title => term_get_title
        !> @brief Sets the plot window's title.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_title(class(terminal) this, character(len = *) txt)
        !! @endcode
        !!
        !! @param[in,out] this The terminal object.
        !! @param[in] txt The title.
        procedure, public :: set_title => term_set_title
        !> @brief Gets the name of the font used for text displayed by the
        !! graph.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_font_name(class(terminal) this)
        !! @endcode
        !!
        !! @param[in] this The terminal object.
        !! @return The font name.
        procedure, public :: get_font_name => term_get_font_name
        !> @brief Sets the name of the font used for text displayed by the
        !! graph.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_font_name(class(terminal) this, character(len = *) name)
        !! @endcode
        !!
        !! @param[in,out] this The terminal object.
        !! @param[in] name The name of the font.  If no name is supplied, the
        !!  name is reset back to its default setting.
        procedure, public :: set_font_name => term_set_font_name
        !> @brief Gets the size of the font used by the graph.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure integer(int32) function get_font_size(class(terminal) this)
        !! @endcode
        !!
        !! @param[in] this The terminal object.
        !! @return The font size, in points.
        procedure, public :: get_font_size => term_get_font_size
        !> @brief Sets the size of the font used by the graph.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_font_size(class(terminal) this, integer(int32) sz)
        !! @endcode
        !!
        !! @param[in,out] this The terminal object.
        !! @param[in] sz The font size, in points.  If a value of zero is provided,
        !! the font size is reset to its default value; or, if a negative value
        !! is provided, the absolute value of the supplied value is utilized.
        procedure, public :: set_font_size => term_set_font_size
        !> @brief Gets the GNUPLOT terminal identification string.
        procedure(term_get_string_result), deferred, public :: get_id_string
    end type

! ------------------------------------------------------------------------------
    interface
        pure module function term_get_window_width(this) result(x)
            class(terminal), intent(in) :: this
            integer :: x
        end function

        module subroutine term_set_window_width(this, x)
            class(terminal), intent(inout) :: this
            integer, intent(in) :: x
        end subroutine

        pure module function term_get_window_height(this) result(x)
            class(terminal), intent(in) :: this
            integer :: x
        end function

        module subroutine term_set_window_height(this, x)
            class(terminal), intent(inout) :: this
            integer, intent(in) :: x
        end subroutine

        pure module function term_get_plot_window_number(this) result(x)
            class(terminal), intent(in) :: this
            integer(int32) :: x
        end function

        module subroutine term_set_plot_window_number(this, x)
            class(terminal), intent(inout) :: this
            integer(int32), intent(in) :: x
        end subroutine

        module function term_get_title(this) result(str)
            class(terminal), intent(in) :: this
            character(len = :), allocatable :: str
        end function

        module subroutine term_set_title(this, txt)
            class(terminal), intent(inout) :: this
            character(len = *), intent(in) :: txt
        end subroutine

        module function term_get_font_name(this) result(name)
            class(terminal), intent(in) :: this
            character(len = :), allocatable :: name
        end function

        module subroutine term_set_font_name(this, name)
            class(terminal), intent(inout) :: this
            character(len = *), intent(in) :: name
        end subroutine

        pure module function term_get_font_size(this) result(sz)
            class(terminal), intent(in) :: this
            integer(int32) :: sz
        end function

        module subroutine term_set_font_size(this, sz)
            class(terminal), intent(inout) :: this
            integer(int32), intent(in) :: sz
        end subroutine

        module function term_get_command_string(this) result(x)
            class(terminal), intent(in) :: this
            character(len = :), allocatable :: x
        end function
    end interface

! ******************************************************************************
! FPLOT_WINDOWS_TERMINAL.F90
! ------------------------------------------------------------------------------
    !> @brief Defines a GNUPLOT Win32 terminal object.
    type, extends(terminal) :: windows_terminal
    private
        !> The terminal ID string
        character(len = 3) :: m_id = "win"
    contains
        !> @brief Retrieves a GNUPLOT terminal identifier string.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_id_string(class(windows_terminal) this)
        !! @endcode
        !!
        !! @param[in] this The windows_terminal object.
        !! @return The string.
        procedure, public :: get_id_string => wt_get_term_string
    end type

! ------------------------------------------------------------------------------
    interface
        module function wt_get_term_string(this) result(x)
            class(windows_terminal), intent(in) :: this
            character(len = :), allocatable :: x
        end function
    end interface

! ******************************************************************************
! FPLOT_QT_TERMINAL.F90
! ------------------------------------------------------------------------------
    !> @brief Defines a GNUPLOT QT terminal object.
    type, extends(terminal) :: qt_terminal
    private
        !> The terminal ID string
        character(len = 2) :: m_id = "qt"
    contains
        !> @brief Retrieves a GNUPLOT terminal identifier string.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_id_string(class(qt_terminal) this)
        !! @endcode
        !!
        !! @param[in] this The qt_terminal object.
        !! @return The string.
        procedure, public :: get_id_string => qt_get_term_string
    end type

! ------------------------------------------------------------------------------
    interface
        module function qt_get_term_string(this) result(x)
            class(qt_terminal), intent(in) :: this
            character(len = :), allocatable :: x
        end function
    end interface

! ******************************************************************************
! FPLOT_WXT_TERMINAL.F90
! ------------------------------------------------------------------------------
    !> @brief Defines a GNUPLOT WXT terminal object.
    type, extends(terminal) :: wxt_terminal
    private
        !> The terminal ID string
        character(len = 3) :: m_id = "wxt"
    contains
        !> @brief Retrieves a GNUPLOT terminal identifier string.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_id_string(class(wxt_terminal) this)
        !! @endcode
        !!
        !! @param[in] this The wxt_terminal object.
        !! @return The string.
        procedure, public :: get_id_string => wxt_get_term_string
    end type

! ------------------------------------------------------------------------------
    interface
        module function wxt_get_term_string(this) result(x)
            class(wxt_terminal), intent(in) :: this
            character(len = :), allocatable :: x
        end function
    end interface

! ******************************************************************************
! FPLOT_PNG_TERMINAL.F90
! ------------------------------------------------------------------------------
    !> @brief Defines a GNUPLOT PNG terminal object.
    type, extends(terminal) :: png_terminal
    private
        !> The terminal ID string
        character(len = 3) :: m_id = "png"
        !> The filename of the PNG file to write.
        character(len = GNUPLOT_MAX_PATH_LENGTH) :: m_fname = "default.png"
    contains
        !> @brief Gets the filename for the output PNG file.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_filename(class(png_terminal) this)
        !! @endcode
        !!
        !! @param[in] this The png_terminal object.
        !! @return The filename, including the file extension (.png).
        procedure, public :: get_filename => png_get_filename
        !> @brief Sets the filename for the output PNG file.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_filename(class(png_terminal) this, character(len = *) txt)
        !! @endcode
        !!
        !! @param[in,out] this The png_terminal object.
        !! @param[in] txt The filename, including the file extension (.png).
        procedure, public :: set_filename => png_set_filename
        !> @brief Retrieves a GNUPLOT terminal identifier string.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_id_string(class(png_terminal) this)
        !! @endcode
        !!
        !! @param[in] this The png_terminal object.
        !! @return The string.
        procedure, public :: get_id_string => png_get_term_string
        !> @brief Returns the appropriate GNUPLOT command string to establish
        !! appropriate parameters.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_command_string(class(png_terminal) this)
        !! @endcode
        !!
        !! @param[in] this The terminal object.
        !! @return The GNUPLOT command string.
        procedure, public :: get_command_string => png_get_command_string
    end type

! ------------------------------------------------------------------------------
    interface
        module function png_get_term_string(this) result(x)
            class(png_terminal), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        module function png_get_filename(this) result(txt)
            class(png_terminal), intent(in) :: this
            character(len = :), allocatable :: txt
        end function

        module subroutine png_set_filename(this, txt)
            class(png_terminal), intent(inout) :: this
            character(len = *), intent(in) :: txt
        end subroutine

        module function png_get_command_string(this) result(x)
            class(png_terminal), intent(in) :: this
            character(len = :), allocatable :: x
        end function
    end interface

! ******************************************************************************
! FPLOT_LATEX.F90
! ------------------------------------------------------------------------------
    !> @brief Defines a GNUPLOT LATEX terminal object.
    type, extends(terminal) :: latex_terminal
    private
        !> The terminal ID string
        character(len = 14) :: m_id = "epslatex color"
        !> The filename of the PNG file to write.
        character(len = GNUPLOT_MAX_PATH_LENGTH) :: m_fname = "default.tex"
    contains
        !> @brief Gets the filename for the output LATEX file.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_filename(class(latex_terminal) this)
        !! @endcode
        !!
        !! @param[in] this The latex_terminal object.
        !! @return The filename, including the file extension (.tex).
        procedure, public :: get_filename => tex_get_filename
        !> @brief Sets the filename for the output LATEX file.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_filename(class(latex_terminal) this, character(len = *) txt)
        !! @endcode
        !!
        !! @param[in,out] this The latex_terminal object.
        !! @param[in] txt The filename, including the file extension (.tex).
        procedure, public :: set_filename => tex_set_filename
        !> @brief Retrieves a GNUPLOT terminal identifier string.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_id_string(class(latex_terminal) this)
        !! @endcode
        !!
        !! @param[in] this The latex_terminal object.
        !! @return The string.
        procedure, public :: get_id_string => tex_get_term_string
        !> @brief Returns the appropriate GNUPLOT command string to establish
        !! appropriate parameters.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_command_string(class(latex_terminal) this)
        !! @endcode
        !!
        !! @param[in] this The terminal object.
        !! @return The GNUPLOT command string.
        procedure, public :: get_command_string => tex_get_command_string
    end type

! ------------------------------------------------------------------------------
    interface
        module function tex_get_term_string(this) result(x)
            class(latex_terminal), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        module function tex_get_filename(this) result(txt)
            class(latex_terminal), intent(in) :: this
            character(len = :), allocatable :: txt
        end function

        module subroutine tex_set_filename(this, txt)
            class(latex_terminal), intent(inout) :: this
            character(len = *), intent(in) :: txt
        end subroutine

        module function tex_get_command_string(this) result(x)
            class(latex_terminal), intent(in) :: this
            character(len = :), allocatable :: x
        end function
    end interface

! ******************************************************************************
! FPLOT_PLOT_DATA.F90
! ------------------------------------------------------------------------------
    !> @brief Provides a container for plot data.
    type, abstract, extends(plot_object) :: plot_data
    private
        !> The name of the data set.
        character(len = PLOTDATA_MAX_NAME_LENGTH) :: m_name = ""
    contains
        !> @brief Gets the name to associate with this data set.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_name(class(plot_data) this)
        !! @endcode
        !!
        !! @param[in] this The plot_data object.
        !! @return The name.
        procedure, public :: get_name => pd_get_name
        !> @brief Sets the name to associate with this data set.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_name(class(plot_data) this, character(len = *) txt)
        !! @endcode
        !!
        !! @param[in,out] this The plot_data object.
        !! @param[in] txt The name.
        procedure, public :: set_name => pd_set_name
        !> @brief Gets the GNUPLOT command string containing the actual data
        !! to plot.
        procedure(pd_get_string_result), deferred, public :: get_data_string
    end type

! ------------------------------------------------------------------------------
    interface
        pure module function pd_get_name(this) result(txt)
            class(plot_data), intent(in) :: this
            character(len = :), allocatable :: txt
        end function

        module subroutine pd_set_name(this, txt)
            class(plot_data), intent(inout) :: this
            character(len = *), intent(in) :: txt
        end subroutine
    end interface

! ******************************************************************************
! FPLOT_PLOT_DATA_COLORED.F90
! ------------------------------------------------------------------------------
    !> @brief Defines a plot_data based object best represented by a color.
    type, abstract, extends(plot_data) :: plot_data_colored
    private
        !> The line color.
        type(color) :: m_color = CLR_BLUE
        !> Let the object choose colors automatically
        logical :: m_useAutoColor = .true.
        !> The color index to use, assuming we're using auto color
        integer(int32) :: m_colorIndex = 1
    contains
        !> @brief Gets the line color.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure type(color) function get_line_color(class(plot_data_colored) this)
        !! @endcode
        !!
        !! @param[in] this The plot_data_colored instance.
        !! @return The color.
        procedure, public :: get_line_color => pdc_get_line_color
        !> @brief Sets the line color.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_line_color(class(plot_data_colored) this, type(color) x)
        !! @endcode
        !!
        !! @param[in,out] this The plot_data_colored instance.
        !! @param[in] x The color.
        procedure, public :: set_line_color => pdc_set_line_color
        procedure, private :: get_color_index => pdc_get_color_index
        procedure, private :: set_color_index => pdc_set_color_index
    end type

! ------------------------------------------------------------------------------
    interface
        pure module function pdc_get_line_color(this) result(x)
            class(plot_data_colored), intent(in) :: this
            type(color) :: x
        end function

        module subroutine pdc_set_line_color(this, x)
            class(plot_data_colored), intent(inout) :: this
            type(color), intent(in) :: x
        end subroutine

        pure module function pdc_get_color_index(this) result(x)
            class(plot_data_colored), intent(in) :: this
            integer(int32) :: x
        end function

        module subroutine pdc_set_color_index(this, x)
            class(plot_data_colored), intent(inout) :: this
            integer(int32), intent(in) :: x
        end subroutine
    end interface

! ******************************************************************************
! FPLOT_PLOT_AXIS.F90
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
        real(real32) :: m_axisWidth = 1.0
        ! ADDED March 29, 2023 - JAC
        !> @brief Use default tic label format?
        logical :: m_defaultTicLabels = .true.
        !> @brief The tic label format.
        character(len = PLOTDATA_MAX_NAME_LENGTH) :: m_ticLabelFmt = "%g"
    contains
        !> @brief Gets the axis' title.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_title(class(plot_axis) this)
        !! @endcode
        !!
        !! @param[in] this The plot_axis object.
        !! @return The title.
        procedure, public :: get_title => pa_get_title
        !> @brief Sets the axis' title.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_title(class(plot_axis) this, character(len = *) txt)
        !! @endcode
        !!
        !! @param[in,out] this The plot_axis object.
        !! @param[in] txt The axis title.  The number of characters must be less
        !!  than or equal to PLOTDATA_MAX_NAME_LENGTH; else, the text string is
        !!  truncated.
        procedure, public :: set_title => pa_set_title
        !> @brief Gets a value determining if a title has been defined for the
        !!  plot_axis object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function is_title_defined(class(plot_axis) this)
        !! @endcode
        !!
        !! @param[in] this The plot_axis object.
        !! @return Returns true if a title has been defined for this axis; else,
        !!  returns false.
        procedure, public :: is_title_defined => pa_has_title
        !> @brief Gets a logical value determining if the axis should be
        !! automatically scaled to fit the data.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function get_autoscale(class(plot_axis) this)
        !! @endcode
        !!
        !! @param[in] this The plot_axis object.
        !! @return Returns true if the axis should be automatically scaled; else,
        !! false.
        procedure, public :: get_autoscale => pa_get_autoscale
        !> @brief Sets a logical value determining if the axis should be
        !! automatically scaled to fit the data.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_autoscale(class(plot_axis) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The plot_axis object.
        !! @param[in] x Set to true if the axis should be automatically scaled; else,
        !! false.
        procedure, public :: set_autoscale => pa_set_autoscale
        !> @brief Gets the axis display limits, assuming autoscaling is not
        !! active for this axis.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure real(real64) function, dimension(2) get_limits(class(plot_axis) this)
        !! @endcode
        !!
        !! @param[in] this The plot_axis object.
        !! @return A two-element array containing the limits as follows:
        !!  [lower, upper].
        procedure, public :: get_limits => pa_get_axis_limits
        !> @brief Sets the axis display limits, assuming autoscaling is not
        !! active for this axis.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_limits(class(plot_axis) this, real(real64) lower, real(real64) upper)
        !! @endcode
        !!
        !! @param[in,out] this The plot_axis object.
        !! @param[in] lower The lower display limit.
        !! @param[in] upper The upper display limit.
        procedure, public :: set_limits => pa_set_axis_limits
        !> @brief Gets a logical value defining if the axis should be log
        !! scaled.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function get_is_log_scaled(class(plot_axis) this)
        !! @endcode
        !!
        !! @param[in,out] this The plot_axis object.
        !! @return Returns true if log scaling is applied to the axis; else, false.
        procedure, public :: get_is_log_scaled => pa_get_log_scale
        !> @brief Sets a logical value defining if the axis should be log
        !! scaled.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_is_log_scaled(class(plot_axis) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The plot_axis object.
        !! @param[in] x Set to true if log scaling is applied to the axis; else,
        !! false.
        procedure, public :: set_is_log_scaled => pa_set_log_scale
        !> @brief Returns the appropriate GNUPLOT command string to define the
        !! plot_axis properties.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_command_string(class(plot_axis) this)
        !! @endcode
        !!
        !! @param[in] this The plot_axis object.
        !! @return The GNUPLOT command string.
        procedure, public :: get_command_string => pa_get_cmd_string
        !> @brief Gets a value determining if the axis should be drawn through
        !! zero of opposing axes.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function get_zero_axis(class(plot_axis) this)
        !! @endcode
        !!
        !! @param[in] this The plot_axis object.
        !! @return Returns true to draw as a zero axis; else, set to false.
        procedure, public :: get_zero_axis => pa_get_zero_axis
        !> @brief Sets a value determining if the axis should be drawn through
        !! zero of opposing axes.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_zero_axis(class(plot_axis) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The plot_axis object.
        !! @param[in] x Set to true to draw as a zero axis; else, set to false.
        procedure, public :: set_zero_axis => pa_set_zero_axis
        !> @brief Gets the width of the line used to represent the zero axis
        !!  line, if active.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure real(real32) function get_zero_axis_line_width(class(plot_axis) this)
        !! @endcode
        !!
        !! @param[in] this The plot_axis object.
        !! @return The width of the line, in pixels.
        procedure, public :: get_zero_axis_line_width => pa_get_zero_axis_width
        !> @brief Sets the width of the line used to represent the zero axis
        !!  line, if active.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_zero_axis_line_width(class(plot_axis) this, real(real32) x)
        !! @endcode
        !!
        !! @param[in,out] this The plot_axis object.
        !! @param[in] x The width of the line, in pixels.
        procedure, public :: set_zero_axis_line_width => pa_set_zero_axis_width
        !> @brief Gets a string identifying the axis as: x, y, z, y2, etc.
        procedure(pa_get_string_result), deferred, public :: get_id_string

        !> @brief Gets a value determining if the default tic label format will
        !! be used.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function get_use_default_tic_label_format(class(plot_axis) this)
        !! @endcode
        !!
        !! @param[in] this The plot_axis object.
        !! @return Returns true if the default tic label format will be used; 
        !!  else, false.
        procedure, public :: get_use_default_tic_label_format => &
            pa_get_use_dft_tic_lbl_fmt
        !> @brief Sets a value determining if the default tic label format will
        !! be used.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_use_default_tic_label_format(class(plot_axis) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The plot_axis object.
        !! @param[in] x Set to true if the default tic label format will be 
        !!  used; else, false.
        procedure, public :: set_use_default_tic_label_format => &
            pa_set_use_dft_tic_lbl_fmt
        !> @brief Gets the tic label format.  The format string can be any 
        !! format string accepted by the C command 'printf.'
        !!
        !! @par Syntax
        !! @code{.f90}
        !! allocatable character(len = :) function get_tic_label_format(class(plot_axis) this)
        !! @endcode
        !!
        !! @param[in] this The plot_axis object.
        !! @return The tic label format string.
        procedure, public :: get_tic_label_format => pa_get_tic_label_fmt
        !> @brief Sets the tic label format.  The format string can be any 
        !! format string accepted by the C command 'printf.'
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_tic_label_format(class(plot_axis) this, character(len = *) x)
        !! @endcode
        !!
        !! @param[in,out] this The plot_axis object.
        !! @param[in] x The tic label format string.
        procedure, public :: set_tic_label_format => pa_set_tic_label_fmt
    end type

! ------------------------------------------------------------------------------
    interface
        module function pa_get_title(this) result(txt)
            class(plot_axis), intent(in) :: this
            character(len = :), allocatable :: txt
        end function

        module subroutine pa_set_title(this, txt)
            class(plot_axis), intent(inout) :: this
            character(len = *), intent(in) :: txt
        end subroutine

        pure module function pa_has_title(this) result(x)
            class(plot_axis), intent(in) :: this
            logical :: x
        end function

        pure module function pa_get_autoscale(this) result(x)
            class(plot_axis), intent(in) :: this
            logical :: x
        end function

        module subroutine pa_set_autoscale(this, x)
            class(plot_axis), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        pure module function pa_get_axis_limits(this) result(x)
            class(plot_axis), intent(in) :: this
            real(real64), dimension(2) :: x
        end function

        module subroutine pa_set_axis_limits(this, lower, upper)
            class(plot_axis), intent(inout) :: this
            real(real64), intent(in) :: lower, upper
        end subroutine

        pure module function pa_get_log_scale(this) result(x)
            class(plot_axis), intent(in) :: this
            logical :: x
        end function

        module subroutine pa_set_log_scale(this, x)
            class(plot_axis), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        module function pa_get_cmd_string(this) result(txt)
            class(plot_axis), intent(in) :: this
            character(len = :), allocatable :: txt
        end function

        pure module function pa_get_zero_axis(this) result(x)
            class(plot_axis), intent(in) :: this
            logical :: x
        end function

        module subroutine pa_set_zero_axis(this, x)
            class(plot_axis), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        pure module function pa_get_zero_axis_width(this) result(x)
            class(plot_axis), intent(in) :: this
            real(real32) :: x
        end function

        module subroutine pa_set_zero_axis_width(this, x)
            class(plot_axis), intent(inout) :: this
            real(real32), intent(in) :: x
        end subroutine

        pure module function pa_get_use_dft_tic_lbl_fmt(this) result(rst)
            class(plot_axis), intent(in) :: this
            logical :: rst
        end function

        module subroutine pa_set_use_dft_tic_lbl_fmt(this, x)
            class(plot_axis), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        pure module function pa_get_tic_label_fmt(this) result(rst)
            class(plot_axis), intent(in) :: this
            character(len = :), allocatable :: rst
        end function

        module subroutine pa_set_tic_label_fmt(this, x)
            class(plot_axis), intent(inout) :: this
            character(len = *), intent(in) :: x
        end subroutine
    end interface

! ******************************************************************************
! FPLOT_LEGEND.F90
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
        logical :: m_show = .false.
        !> Determines the legend layout.
        character(len = 20) :: m_layout = LEGEND_ARRANGE_VERTICALLY
        !> Opaque background?
        logical :: m_opaque = .true.
    contains
        !> @brief Gets a value determining if the legend should be drawn inside
        !! the axes border (true), or outside the axes border (false).
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function get_draw_inside_axes(class(legend) this)
        !! @endcode
        !!
        !! @param[in] this The legend object.
        !! @return The logical value.
        procedure, public :: get_draw_inside_axes => leg_get_inside
        !> @brief Sets a value determining if the legend should be drawn inside
        !! the axes border (true), or outside the axes border (false).
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_draw_inside_axes(class(legend) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The legend object.
        !! @param[in] x The logical value.
        procedure, public :: set_draw_inside_axes => leg_set_inside
        !> @brief Gets a value determining if the legend should have a border.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function get_draw_border(class(legend) this)
        !! @endcode
        !!
        !! @param[in] this The legend object.
        !! @return The logical value.
        procedure, public :: get_draw_border => leg_get_box
        !> @brief Sets a value determining if the legend should have a border.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_draw_border(class(legend) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The legend object.
        !! @param[in] x The logical value.
        procedure, public :: set_draw_border => leg_set_box
        !> @brief Gets the horizontal position of the legend.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_horizontal_position(class(legend) this)
        !! @endcode
        !!
        !! @param[in] this The legend object.
        !! @return The horizontal position of the legend (LEGEND_LEFT,
        !!  LEGEND_CENTER, or LEGEND_RIGHT).
        procedure, public :: get_horizontal_position => leg_get_horz_pos
        !> @brief Sets the horizontal position of the legend.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_horizontal_position(class(legend) this, character(len = *) x)
        !! @endcode
        !!
        !! @param[in,out] this The legend object.
        !! @param x The horizontal position of the legend.  The parameter must be
        !!  set to one of the following: LEGEND_LEFT, LEGEND_CENTER, or
        !!  LEGEND_RIGHT.  If not, the default LEGEND_RIGHT will be used.
        procedure, public :: set_horizontal_position => leg_set_horz_pos
        !> @brief Gets the vertical position of the legend.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_vertical_position(class(legend) this)
        !! @endcode
        !!
        !! @param[in] this The legend object.
        !! @return The vertical position of the legend (LEGEND_TOP,
        !!  LEGEND_CENTER, or LEGEND_BOTTOM).
        procedure, public :: get_vertical_position => leg_get_vert_pos
        !> @brief Gets the vertical position of the legend.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_vertical_position(class(legend) this, character(len = *) x)
        !! @endcode
        !!
        !! @param[in,out] this The legend object.
        !! @param x The vertical position of the legend.  The parameter must be
        !!  set to one of the following: LEGEND_TOP, LEGEND_CENTER, or
        !!  LEGEND_BOTTOM.  If not, the default LEGEND_TOP will be used.
        procedure, public :: set_vertical_position => leg_set_vert_pos
        !> @brief Gets a value determining if the legend is visible.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function get_is_visible(class(legend) this)
        !! @endcode
        !!
        !! @param[in] this The legend object.
        !! @return The logical value.
        procedure, public :: get_is_visible => leg_get_visible
        !> @brief Sets a value determining if the legend is visible.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_is_visible(class(legend) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The legend object.
        !! @param[in] x The logical value.
        procedure, public :: set_is_visible => leg_set_visible
        !> @brief Gets the command string defining the legend properties.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_command_string(class(legend) this)
        !! @endcode
        !!
        !! @param[in] this The legend object.
        !! @return The GNUPLOT command string.
        procedure, public :: get_command_string => leg_get_command_txt
        !> @brief Gets the layout of the legend.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) pure function get_layout(class(legend) this)
        !! @endcode
        !!
        !! @param[in] this The legend object.
        !! @return The layout type, either @ref LEGEND_ARRANGE_VERTICALLY 
        !!  or @ref LEGEND_ARRANGE_HORIZONTALLY.
        procedure, public :: get_layout => leg_get_layout
        !> @brief Sets the layout of the legend.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_layout(class(legend) this, character(len = *) x)
        !! @endcode
        !!
        !! @param[in,out] this The legend object.
        !! @param[in] x The layout type, either @ref LEGEND_ARRANGE_VERTICALLY 
        !!  or @ref LEGEND_ARRANGE_HORIZONTALLY.
        procedure, public :: set_layout => leg_set_layout
        !> @brief Gets a value determining if the legend is to be opaque.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! logical pure function get_is_opaque(class(legend) this)
        !! @endcode
        !!
        !! @param[in] this The legend object.
        !! @return True if the legend is to be opaque; else, false.
        procedure, public :: get_is_opaque => leg_get_opaque
        !> @brief Sets a value determining if the legend is to be opaque.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_is_opaque(class(legend) this)
        !! @endcode
        !!
        !! @param[in,out] this The legend object.
        !! @param[in] x True if the legend is to be opaque; else, false.
        procedure, public :: set_is_opaque => leg_set_opaque
    end type

! ------------------------------------------------------------------------------
    interface
        pure module function leg_get_inside(this) result(x)
            class(legend), intent(in) :: this
            logical :: x
        end function

        module subroutine leg_set_inside(this, x)
            class(legend), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        pure module function leg_get_box(this) result(x)
            class(legend), intent(in) :: this
            logical :: x
        end function

        module subroutine leg_set_box(this, x)
            class(legend), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        module function leg_get_horz_pos(this) result(x)
            class(legend), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        module subroutine leg_set_horz_pos(this, x)
            class(legend), intent(inout) :: this
            character(len = *), intent(in) :: x
        end subroutine

        module function leg_get_vert_pos(this) result(x)
            class(legend), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        module subroutine leg_set_vert_pos(this, x)
            class(legend), intent(inout) :: this
            character(len = *), intent(in) :: x
        end subroutine

        pure module function leg_get_visible(this) result(x)
            class(legend), intent(in) :: this
            logical :: x
        end function

        module subroutine leg_set_visible(this, x)
            class(legend), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        module function leg_get_command_txt(this) result(txt)
            class(legend), intent(in) :: this
            character(len = :), allocatable :: txt
        end function

        pure module function leg_get_layout(this) result(rst)
            class(legend), intent(in) :: this
            character(len = :), allocatable :: rst
        end function

        module subroutine leg_set_layout(this, x)
            class(legend), intent(inout) :: this
            character(len = *), intent(in) :: x
        end subroutine

        pure module function leg_get_opaque(this) result(rst)
            class(legend), intent(in) :: this
            logical :: rst
        end function

        module subroutine leg_set_opaque(this, x)
            class(legend), intent(inout) :: this
            logical :: x
        end subroutine
    end interface


! ******************************************************************************
! FPLOT_COLORMAP.F90
! ------------------------------------------------------------------------------
    !> @brief A colormap object for a surface plot.
    type, abstract, extends(plot_object) :: colormap
    private
        !> The label to associate with the colormap
        character(len = :), allocatable :: m_label
        !> The colormap should be drawn horizontally
        logical :: m_horizontal = .false.
        !> Draw the colormap border
        logical :: m_drawBorder = .true.
        !> Show the tic marks
        logical :: m_showTics = .true.
    contains
        !> @brief Gets the GNUPLOT command string to represent this colormap
        !! object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable :: get_command_string(class(colormap) this)
        !! @endcode
        !!
        !! @param[in] this The colormap object.
        !! @return The command string.
        procedure, public :: get_command_string => cm_get_cmd
        !> @brief Gets the GNUPLOT string defining the color distribution.  For
        !! instance, this routine could return the string: '0 "dark-blue",
        !! 1 "blue", 2 "cyan", 3 "green", 4 "yellow", 5 "orange", 6 "red",
        !! 7 "dark-red"'.  This string would result in a rainbow type map.
        procedure(cm_get_string_result), deferred, public :: get_color_string
        !> @brief Gets the label to associate with the colorbar.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) get_label(class(colormap) this)
        !! @endcode
        !!
        !! @param[in] this The colormap object.
        !! @return The label.
        procedure, public :: get_label => cm_get_label
        !> @brief Sets the label to associate with the colorbar.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_label(class(colormap) this, character(len = *) x)
        !! @endcode
        !!
        !! @param[in,out] this The colormap object.
        !! @param[in] x The label.
        procedure, public :: set_label => cm_set_label
        !> @brief Gets a logical value determining if the colormap should be
        !! drawn horizontally and below the plot.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! logical get_horizontal(class(colormap) this)
        !! @endcode
        !!
        !! @param[in] this The colormap object.
        !! @return Returns true if the colormap should be drawn horizontally;
        !!  else, false.
        procedure, public :: get_horizontal => cm_get_horizontal
        !> @brief Sets a logical value determining if the colormap should be
        !! drawn horizontally and below the plot.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_horizontal(class(colormap) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The colormap object.
        !! @param[in] x Set to true if the colormap should be drawn 
        !!  horizontally; else, false.
        procedure, public :: set_horizontal => cm_set_horizontal
        !> @brief Gets a logical value determining if the border should be
        !! drawn.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! logical get_draw_border(class(colormap) this)
        !! @endcode
        !!
        !! @param[in] this The colormap object.
        !! @return Returns true if the border should be drawn; else, false.
        procedure, public :: get_draw_border => cm_get_draw_border
        !> @brief Sets a logical value determining if the border should be
        !! drawn.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_draw_border(class(colormap) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The colormap object.
        !! @param[in] x Set to true if the border should be drawn; else, false.
        procedure, public :: set_draw_border => cm_set_draw_border
        !> @brief Gets a logical value determining if the tic marks should be
        !! drawn.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! logical get_show_tics(class(colormap) this)
        !! @endcode
        !!
        !! @param[in] this The colormap object.
        !! @return Returns true if the tic marks should be drawn; else, false.
        procedure, public :: get_show_tics => cm_get_show_tics
        !> @brief Sets a logical value determining if the tic marks should be
        !! drawn.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_show_tics(class(colormap) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The colormap object.
        !! @param[in] x Set to true if the tic marks should be drawn; else, 
        !!  false.
        !> @brief Sets a logical value determining if the border should be
        !! drawn.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_draw_border(class(colormap) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The colormap object.
        !! @param[in] x Set to true if the border should be drawn; else, false.
        procedure, public :: set_show_tics => cm_set_show_tics
    end type

! ------------------------------------------------------------------------------
    !> @brief Defines a rainbow colormap.
    type, extends(colormap) :: rainbow_colormap
    contains
        !> @brief Gets the GNUPLOT string defining the color distribution.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_color_string(class(rainbow_colormap) this)
        !! @endcode
        !!
        !! @param[in] this The rainbow_colormap object.
        !! @return The command string.
        procedure, public :: get_color_string => rcm_get_clr
    end type

! ------------------------------------------------------------------------------
    !> @brief Defines a colormap consisting of "hot" colors.
    type, extends(colormap) :: hot_colormap
    contains
        !> @brief Gets the GNUPLOT string defining the color distribution.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_color_string(class(hot_colormap) this)
        !! @endcode
        !!
        !! @param[in] this The hot_colormap object.
        !! @return The command string.
        procedure, public :: get_color_string => hcm_get_clr
    end type

! ------------------------------------------------------------------------------
    !> @brief Defines a colormap consisting of "cool" colors.
    type, extends(colormap) :: cool_colormap
    contains
        !> @brief Gets the GNUPLOT string defining the color distribution.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_color_string(class(cool_colormap) this)
        !! @endcode
        !!
        !! @param[in] this The cool_colormap object.
        !! @return The command string.
        procedure, public :: get_color_string => ccm_get_clr
    end type

! ------------------------------------------------------------------------------
    !> @brief Defines a colormap equivalent to the MATLAB parula colormap.
    type, extends(colormap) :: parula_colormap
    contains
        !> @brief Gets the GNUPLOT string defining the color distribution.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_color_string(class(parula_colormap) this)
        !! @endcode
        !!
        !! @param[in] this The parula_colormap object.
        !! @return The command string.
        procedure, public :: get_color_string => pcm_get_clr
    end type

! ------------------------------------------------------------------------------
    !> @brief Defines a grey-scaled colormap.
    type, extends(colormap) :: grey_colormap
    contains
        !> @brief Gets the GNUPLOT string defining the color distribution.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_color_string(class(grey_colormap) this)
        !! @endcode
        !!
        !! @param[in] this The grey_colormap object.
        !! @return The command string.
        procedure, public :: get_color_string => gcm_get_clr
    end type

! ------------------------------------------------------------------------------
    !> @brief Defines an earthy-colored colormap.
    type, extends(colormap) :: earth_colormap
    contains
        !> @brief Gets the GNUPLOT string defining the color distribution.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_color_string(class(earth_colormap) this)
        !! @endcode
        !!
        !! @param[in] this The earth_colormap object.
        !! @return The command string.
        procedure, public :: get_color_string => ecm_get_clr
    end type

! ------------------------------------------------------------------------------
    !> @brief Defines a custom colormap that utilizes the FORCOLORMAP library
    !! to provide the map.
    type, extends(colormap) :: custom_colormap
        class(cmap), private, pointer :: m_map => null()
    contains
        final :: custom_final
        !> @brief Gets the GNUPLOT string defining the color distribution.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_color_string(class(custom_colormap) this)
        !! @endcode
        !!
        !! @param[in] this The @ref custom_colormap object.
        !! @return The command string.
        procedure, public :: get_color_string => custom_get_clr
        !> @brief Sets the FORCOLORMAP colormap object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_colormap(class(custom_colormap) this, class(cmap) x, optional class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The @ref custom_colormap object.
        !! @param[in] x The FORCOLORMAP colormap object.  The @ref 
        !!  custom_colormap object stores a copy of this object; 
        !!  therefore, any changes made to @p x after calls to
        !!  this routine will not impact the behavior of the
        !!  @ref custom_colormap object.
        !! @param[in,out] err An optional errors-based object that if provided 
        !!  can be used to retrieve information relating to any errors 
        !!  encountered during execution.  If not provided, a default 
        !!  implementation of the errors class is used internally to provide 
        !!  error handling.  Possible errors and warning messages that may be 
        !!  encountered are as follows.
        !! - PLOT_OUT_OF_MEMORY_ERROR: Occurs if a memory allocation error
        !!      occurs.
        procedure, public :: set_colormap => custom_set
        !> @brief Gets a pointer to the FORCOLORMAP colormap object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! class(cmap), pointer function get_colormap(class(custom_colormap) this)
        !! @endcode
        !!
        !! @param[in] this The @ref custom_colormap object.
        !! @return A pointer to the FORCOLORMAP colormap object.
        procedure, public :: get_colormap => custom_get
    end type

! ------------------------------------------------------------------------------
    interface
        module function cm_get_cmd(this) result(x)
            class(colormap), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        pure module function cm_get_label(this) result(rst)
            class(colormap), intent(in) :: this
            character(len = :), allocatable :: rst
        end function

        module subroutine cm_set_label(this, x)
            class(colormap), intent(inout) :: this
            character(len = *), intent(in) :: x
        end subroutine

        pure module function cm_get_horizontal(this) result(rst)
            class(colormap), intent(in) :: this
            logical :: rst
        end function

        module subroutine cm_set_horizontal(this, x)
            class(colormap), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        pure module function cm_get_draw_border(this) result(rst)
            class(colormap), intent(in) :: this
            logical :: rst
        end function

        module subroutine cm_set_draw_border(this, x)
            class(colormap), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        pure module function cm_get_show_tics(this) result(rst)
            class(colormap), intent(in) :: this
            logical :: rst
        end function

        module subroutine cm_set_show_tics(this, x)
            class(colormap), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        ! --------------------

        module function rcm_get_clr(this) result(x)
            class(rainbow_colormap), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        ! --------------------

        module function hcm_get_clr(this) result(x)
            class(hot_colormap), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        ! --------------------

        module function ccm_get_clr(this) result(x)
            class(cool_colormap), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        ! --------------------

        module function pcm_get_clr(this) result(x)
            class(parula_colormap), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        ! --------------------

        module function gcm_get_clr(this) result(x)
            class(grey_colormap), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        ! --------------------

        module function ecm_get_clr(this) result(x)
            class(earth_colormap), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        ! --------------------
        module function custom_get_clr(this) result(x)
            class(custom_colormap), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        ! --------------------
        module subroutine custom_set(this, map, err)
            class(custom_colormap), intent(inout) :: this
            class(cmap), intent(in) :: map
            class(errors), intent(inout), optional, target :: err
        end subroutine

        ! --------------------
        module function custom_get(this) result(rst)
            class(custom_colormap), intent(in) :: this
            class(cmap), pointer :: rst
        end function

        ! --------------------
        module subroutine custom_final(this)
            type(custom_colormap), intent(inout) :: this
        end subroutine

        ! --------------------
    end interface

! ******************************************************************************
! FPLOT_PLOT.F90
! ------------------------------------------------------------------------------
    !> @brief Defines the basic GNUPLOT plot.
    type, extends(plot_object) :: plot
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
        !> Point tic marks in?
        logical :: m_ticsIn = .true.
        !> Draw the border?
        logical :: m_drawBorder = .true.
        !> A collection of plot_label items to draw.
        type(list) :: m_labels ! Added 6/22/2018, JAC
        !> The color index to use for automatic line coloring for scatter plots.
        integer(int32) :: m_colorIndex = 1
        !> Determines if the axes should be scaled proportionally.
        logical :: m_axisEqual = .false.
        !> The colormap.
        class(colormap), pointer :: m_colormap
        !> Show the colorbar?
        logical :: m_showColorbar = .true.
        !> A collection of plot_arrow items to draw.
        type(list) :: m_arrows ! Added 1/3/2024, JAC
    contains
        !> @brief Cleans up resources held by the plot object.  Inheriting
        !! classes are expected to call this routine to free internally held
        !! resources.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! module free_resources(class(plot) this)
        !! @endcode
        !!
        !! @param[in,out] this The plot object.
        procedure, public :: free_resources => plt_clean_up
        !> @brief Initializes the plot object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine initialize(class(plot) this, optional class(terminal) term, optional class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The plot object.
        !! @param[in] term An optional input that is used to define the terminal.
        !!  The default terminal is a WXT terminal.  The acceptable inputs are:
        !!  - GNUPLOT_TERMINAL_PNG
        !!  - GNUPLOT_TERMINAL_QT
        !!  - GNUPLOT_TERMINAL_WIN32
        !!  - GNUPLOT_TERMINAL_WXT
        !!  - GNUPLOT_TERMINAL_LATEX
        !! @param[in] fname A filename to pass to the terminal in the event the
        !!  terminal is a file type (e.g. GNUPLOT_TERMINAL_PNG).
        !! @param[in,out] err An optional errors-based object that if provided can be
        !!  used to retrieve information relating to any errors encountered during
        !!  execution.  If not provided, a default implementation of the errors
        !!  class is used internally to provide error handling.  Possible errors and
        !!  warning messages that may be encountered are as follows.
        !! - PLOT_OUT_OF_MEMORY_ERROR: Occurs if insufficient memory is available.
        procedure, public :: initialize => plt_init
        !> @brief Gets the plot's title.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_title(class(plot))
        !! @endcode
        !!
        !! @param[in] this The plot object.
        !! @return The plot's title.
        procedure, public :: get_title => plt_get_title
        !> @brief Sets the plot's title.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_title(class(plot) this, character(len = *) txt)
        !! @endcode
        !!
        !! @param[in,out] this The plot object.
        !! @param[in] txt The plot's title.  The number of characters must be less
        !! than or equal to PLOTDATA_MAX_NAME_LENGTH; else, the text string is
        !! truncated.
        procedure, public :: set_title => plt_set_title
        !> @brief Gets a value determining if a title has been defined for the
        !!  plot object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function is_title_defined(class(plot) this)
        !! @endcode
        !!
        !! @param[in] this The plot object.
        !! @return Returns true if a title has been defined for this plot; else,
        !!  returns false.
        procedure, public :: is_title_defined => plt_has_title
        !> @brief Gets the plot's legend object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! class(legend) function, pointer get_legend(class(this) plot)
        !! @endcode
        !!
        !! @param[in] this The plot object.
        !! @return A pointer to the legend object.
        procedure, public :: get_legend => plt_get_legend
        !> @brief Gets the number of stored plot_data objects.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure integer(int32) function get_count(class(plot) this)
        !! @endcode
        !!
        !! @param[in] this The plot object.
        !! @return The number of plot_data objects.
        procedure, public :: get_count => plt_get_count
        !> @brief Pushes a plot_data object onto the stack.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine push(class(plot) this, class(plot_data) x, optional class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The plot object.
        !! @param[in] x The plot_data object.
        !! @param[in,out] err An optional errors-based object that if provided can be
        !!  used to retrieve information relating to any errors encountered during
        !!  execution.  If not provided, a default implementation of the errors
        !!  class is used internally to provide error handling.  Possible errors and
        !!  warning messages that may be encountered are as follows.
        !! - PLOT_OUT_OF_MEMORY_ERROR: Occurs if insufficient memory is available.
        procedure, public :: push => plt_push_data
        !> @brief Pops the last plot_data object from the stack.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine pop(class(plot) this)
        !! @endcode
        !!
        !! @param[in,out] this The plot object.
        procedure, public :: pop => plt_pop_data
        !> @brief Removes all plot_data objects from the plot.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine clear(class(plot) this)
        !! @endcode
        !!
        !! @param[in,out] this The plot object.
        procedure, public :: clear_all => plt_clear_all
        !> @brief Gets a pointer to the requested plot_data object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! class(plot_data) function, pointer get(class(plot), integer(int32) i)
        !! @endcode
        !!
        !! @param[in] this The plot object.
        !! @param[in] i The index of the plot_data object.
        !! @return A pointer to the requested plot_data object.
        procedure, public :: get => plt_get
        !> @brief Sets the requested plot_data object into the plot.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set(class(plot) this, integer(int32) i, class(plot_data) x)
        !! @endcode
        !!
        !! @param[in,out] this The plot object.
        !! @param[in] i The index of the plot_data object.
        !! @param[in] x The plot_data object.
        procedure, public :: set => plt_set
        !> @brief Gets the GNUPLOT terminal object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! class(terminal) function, pointer get_terminal(class(plot) this)
        !! @endcode
        !!
        !! @param[in] this The plot object.
        !! @return A pointer to the GNUPLOT terminal object.
        procedure, public :: get_terminal => plt_get_term
        !> @brief Gets a flag determining if the grid lines should be shown.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function get_show_gridlines(class(plot) this)
        !! @endcode
        !!
        !! @param[in] this The plot object.
        !! @return Returns true if the grid lines should be shown; else, false.
        procedure, public :: get_show_gridlines => plt_get_show_grid
        !> @brief Sets a flag determining if the grid lines should be shown.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_show_gridlines(class(plot) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The plot object.
        !! @param[in] x Set to true if the grid lines should be shown; else, false.
        procedure, public :: set_show_gridlines => plt_set_show_grid
        !> @brief Launches GNUPLOT and draws the plot per the current state of
        !! the command list.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine draw(class(plot) this, optional logical persist, optional class(errors) err)
        !! @endcode
        !!
        !! @param[in] this The plot object.
        !! @param[in] persist An optional parameter that can be used to keep GNUPLOT
        !!  open.  Set to true to force GNUPLOT to remain open; else, set to false
        !!  to allow GNUPLOT to close after drawing.  The default is true.
        !! @param[in,out] err An optional errors-based object that if provided can be
        !!  used to retrieve information relating to any errors encountered during
        !!  execution.  If not provided, a default implementation of the errors
        !!  class is used internally to provide error handling.  Possible errors and
        !!  warning messages that may be encountered are as follows.
        !!  - PLOT_GNUPLOT_FILE_ERROR: Occurs if the command file cannot be written.
        procedure, public :: draw => plt_draw
        !> @brief Saves a GNUPLOT command file.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine save_file(class(plot) this, character(len = *) fname, optional class(errors) err)
        !! @endcode
        !!
        !! @param[in] this The plot object.
        !! @param[in] fname The filename.
        !! @param[in,out] err An optional errors-based object that if provided can be
        !!  used to retrieve information relating to any errors encountered during
        !!  execution.  If not provided, a default implementation of the errors
        !!  class is used internally to provide error handling.  Possible errors and
        !!  warning messages that may be encountered are as follows.
        !!  - PLOT_GNUPLOT_FILE_ERROR: Occurs if the command file cannot be written.
        procedure, public :: save_file => plt_save
        !> @brief Gets the name of the font used for plot text.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_font_name(class(plot) this)
        !! @endcode
        !!
        !! @param[in] this The plot object.
        !! @return The font name.
        procedure, public :: get_font_name => plt_get_font
        !> @brief Sets the name of the font used for plot text.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_font_name(class(plot) this, character(len = *) x)
        !! @endcode
        !!
        !! @param[in,out] this The plot object.
        !! @param[in] x The font name.
        procedure, public :: set_font_name => plt_set_font
        !> @brief Gets the size of the font used by the plot.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! integer(int32) function get_font_size(class(plot) this)
        !! @endcode
        !!
        !! @param[in] this The plot object.
        !! @return The size of the font, in points.
        procedure, public :: get_font_size => plt_get_font_size
        !> @brief Sets the size of the font used by the plot.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_font_size(class(plot) this, integer(int32) x)
        !! @endcode
        !!
        !! @param[in,out] this The plot object.
        !! @param[in] x The font size, in points.  If a value of zero is provided,
        !! the font size is reset to its default value; or, if a negative value
        !! is provided, the absolute value of the supplied value is utilized.
        procedure, public :: set_font_size => plt_set_font_size
        !> @brief Gets a value determining if the axis tic marks should point
        !! inwards.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function get_tics_inward(class(plot) this)
        !! @endcode
        !!
        !! @param[in] this The plot object.
        !! @return Returns true if the tic marks should point inwards; else, false
        !!  if the tic marks should point outwards.
        procedure, public :: get_tics_inward => plt_get_tics_in
        !> @brief Sets a value determining if the axis tic marks should point
        !! inwards.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_tics_inward(class(plot) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The plot object.
        !! @param[in] x Set to true if the tic marks should point inwards; else,
        !!  false if the tic marks should point outwards.
        procedure, public :: set_tics_inward => plt_set_tics_in
        !> @brief Gets a value determining if the border should be drawn.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function get_draw_border(class(plot) this)
        !! @endcode
        !!
        !! @param[in] this The plot object.
        !! @return Returns true if the border should be drawn; else, false.
        procedure, public :: get_draw_border => plt_get_draw_border
        !> @brief Sets a value determining if the border should be drawn.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_draw_border(class(plot) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The plot object.
        !! @param[in] x Set to true if the border should be drawn; else, false.
        procedure, public :: set_draw_border => plt_set_draw_border
        !> @brief Adds a label to the plot.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine push_label(class(plot) this, class(plot_labels) lbl, optional class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The plot object.
        !! @param[in] lbl The plot label.
        !! @param[in,out] err An optional errors-based object for managing 
        !!  errors.  The default implementation of the errors type is used if
        !!  nothing is supplied.
        procedure, public :: push_label => plt_push_label
        !> @brief Removes the last label from the plot.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine pop_label(class(plot) this)
        !! @endcode
        !!
        !! @param[in,out] this The plot object.
        procedure, public :: pop_label => plt_pop_label
        !> @brief Gets the requested plot_label from the plot.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! class(plot_label) pointer function get_label(class(plot) this, integer(int32) i)
        !! @endcode
        !!
        !! @param[in] this The plot object.
        !! @param[in] i The index of the plot_label object to retrieve.
        !! @return A pointer to the requested plot_label object.
        procedure, public :: get_label => plt_get_label
        !> @brief Sets the specified plot_label object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_label(class(plot) this, integer(int32) i, class(plot_label) x)
        !! @endcode
        !!
        !! @param[in,out] this The plot object.
        !! @param[in] i The index of the plot_label to replace.
        !! @param[in] x The new plot_label object.
        procedure, public :: set_label => plt_set_label
        !> @brief Gets the number of plot_label objects belonging to the plot.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure integer(int32) function get_label_count(class(plot) this)
        !! @endcode
        !!
        !! @param[in] this The plot object.
        !! @return The number of plot_label objects.
        procedure, public :: get_label_count => plt_get_label_count
        !> @brief Clears all plot_label objects from the plot.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine clear_all_labels(class(plot) this)
        !! @endcode
        !!
        !! @param[in,out] this The plot object.
        procedure, public :: clear_all_labels => plt_clear_labels
        !> @brief Gets a flag determining if the axes should be equally scaled.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! logical function get_axis_equal(class(plot) this)
        !! @endcode
        !!
        !! @param[in] this The plot object.
        !! @return Returns true if the axes should be scaled equally; else, 
        !!  false.
        procedure, public :: get_axis_equal => plt_get_axis_equal
        !> @brief Sets a flag determining if the axes should be equally scaled.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_axis_equal(class(plot) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The plot object.
        !! @param[in] x Set to true if the axes should be scaled equally; else, 
        !!  false.
        procedure, public :: set_axis_equal => plt_set_axis_equal
        !> @brief Gets a pointer to the colormap object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! class(colormap) function, pointer get_colormap(class(plot) this)
        !! @endcode
        !!
        !! @param[in] this The plot object.
        !! @return A pointer to the colormap object.  If no colormap is defined, a
        !!  null pointer is returned.
        procedure, public :: get_colormap => plt_get_colormap
        !> @brief Sets the colormap object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_colormap(class(plot) this, class(colormap) x, optional class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The plot object.
        !! @param[in] x The colormap object.  Notice, a copy of this object is
        !!  stored, and the plot object then manages the lifetime of the
        !!  copy.
        !! @param[out] err An optional errors-based object that if provided can be
        !!  used to retrieve information relating to any errors encountered during
        !!  execution.  If not provided, a default implementation of the errors
        !!  class is used internally to provide error handling.  Possible errors and
        !!  warning messages that may be encountered are as follows.
        !! - PLOT_OUT_OF_MEMORY_ERROR: Occurs if insufficient memory is available.
        procedure, public :: set_colormap => plt_set_colormap
        !> @brief Gets a value determining if the colorbar should be shown.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function get_show_colorbar(class(plot) this)
        !! @endcode
        !!
        !! @param[in] this The plot object.
        !! @return Returns true if the colorbar should be drawn; else, false.
        procedure, public :: get_show_colorbar => plt_get_show_colorbar
        !> @brief Sets a value determining if the colorbar should be shown.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_show_colorbar(class(plot) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The plot object.
        !! @param[in] x Set to true if the colorbar should be drawn; else, false.
        procedure, public :: set_show_colorbar => plt_set_show_colorbar
        !> @brief Gets the GNUPLOT command string to represent this plot
        !! object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_command_string(class(plot) this)
        !! @endcode
        !!
        !! @param[in] this The plot object.
        !! @return The command string.
        procedure, public :: get_command_string => plt_get_cmd
        !> @brief Pushes a new @ref plot_arrow object onto the plot.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine push_arrow(class(plot) this, class(plot_arrow) x, optional class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The @ref plot object.
        !! @param[in] x The @ref plot_arrow object.  This instance is copied, 
        !!  and the copy is stored and managed by the @ref plot object.
        !! @param[in,out] err An optional errors-based object for managing 
        !!  errors.  The default implementation of the errors type is used if
        !!  nothing is supplied.
        procedure, public :: push_arrow => plt_push_arrow
        !> @brief Pops a @ref plot_arrow object from the plot.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine pop_arrow(class(plot) this)
        !! @endcode
        !!
        !! @param[in,out] this The @ref plot object.
        procedure, public :: pop_arrow => plt_pop_arrow
        !> @brief Gets a pointer to the requested @ref plot_arrow object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! class(plot_arrow), pointer function get_arrow(class(plot) this, integer(int32) i)
        !! @endcode
        !!
        !! @param[in] this The @ref plot object.
        !! @param[in] i The index of the @ref plot_arrow to retrieve.
        procedure, public :: get_arrow => plt_get_arrow
        !> @brief Sets a @ref plot_arrow into the @ref plot.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_arrow(class(plot) this, integer(int32) i, class(plot_arrow) x)
        !! @endcode
        !!
        !! @param[in,out] this The @ref plot object.
        !! @param[in] i The index of the @ref plot_arrow to retrieve.
        !! @param[in] x The @ref plot_arrow to set.  This instance is copied, 
        !!  and the copy is stored and managed by the @ref plot object.
        procedure, public :: set_arrow => plt_set_arrow
        !> @brief Gets the number of @ref plot_arrow objects held by the
        !! @ref plot object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure integer(int32) function get_arrow_count(class(plot_arrow) this)
        !! @endcode
        !!
        !! @param[in] this The @ref plot object.
        !! @return The number of @ref plot_arrow objects held by the @ref plot
        !!  object.
        procedure, public :: get_arrow_count => plt_get_arrow_count
        !> @brief Clears all @ref plot_arrow objects from the @ref plot.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine clear_arrows(class(plot) this)
        !! @endcode
        !!
        !! @param[in,out] this The @ref plot object.
        procedure, public :: clear_arrows => plt_clear_arrows
    end type

! ------------------------------------------------------------------------------
    interface
        module subroutine plt_clean_up(this)
            class(plot), intent(inout) :: this
        end subroutine

        module subroutine plt_init(this, term, fname, err)
            class(plot), intent(inout) :: this
            integer(int32), intent(in), optional :: term
            character(len = *), intent(in), optional :: fname
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module function plt_get_title(this) result(txt)
            class(plot), intent(in) :: this
            character(len = :), allocatable :: txt
        end function

        module subroutine plt_set_title(this, txt)
            class(plot), intent(inout) :: this
            character(len = *), intent(in) :: txt
        end subroutine

        pure module function plt_has_title(this) result(x)
            class(plot), intent(in) :: this
            logical :: x
        end function

        module function plt_get_legend(this) result(x)
            class(plot), intent(in) :: this
            type(legend), pointer :: x
        end function

        pure module function plt_get_count(this) result(x)
            class(plot), intent(in) :: this
            integer(int32) :: x
        end function

        module subroutine plt_push_data(this, x, err)
            class(plot), intent(inout) :: this
            class(plot_data), intent(inout) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine plt_pop_data(this)
            class(plot), intent(inout) :: this
        end subroutine

        module subroutine plt_clear_all(this)
            class(plot), intent(inout) :: this
        end subroutine

        module function plt_get(this, i) result(x)
            class(plot), intent(in) :: this
            integer(int32), intent(in) :: i
            class(plot_data), pointer :: x
        end function

        module subroutine plt_set(this, i, x)
            class(plot), intent(inout) :: this
            integer(int32), intent(in) :: i
            class(plot_data), intent(in) :: x
        end subroutine

        module function plt_get_term(this) result(x)
            class(plot), intent(in) :: this
            class(terminal), pointer :: x
        end function

        pure module function plt_get_show_grid(this) result(x)
            class(plot), intent(in) :: this
            logical :: x
        end function

        module subroutine plt_set_show_grid(this, x)
            class(plot), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        module subroutine plt_draw(this, persist, err)
            class(plot), intent(in) :: this
            logical, intent(in), optional :: persist
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine plt_save(this, fname, err)
            class(plot), intent(in) :: this
            character(len = *), intent(in) :: fname
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module function plt_get_font(this) result(x)
            class(plot), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        module subroutine plt_set_font(this, x)
            class(plot), intent(inout) :: this
            character(len = *), intent(in) :: x
        end subroutine

        module function plt_get_font_size(this) result(x)
            class(plot), intent(in) :: this
            integer(int32) :: x
        end function

        module subroutine plt_set_font_size(this, x)
            class(plot), intent(inout) :: this
            integer(int32), intent(in) :: x
        end subroutine

        pure module function plt_get_tics_in(this) result(x)
            class(plot), intent(in) :: this
            logical :: x
        end function

        module subroutine plt_set_tics_in(this, x)
            class(plot), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        pure module function plt_get_draw_border(this) result(x)
            class(plot), intent(in) :: this
            logical :: x
        end function

        module subroutine plt_set_draw_border(this, x)
            class(plot), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        module subroutine plt_push_label(this, lbl, err)
            class(plot), intent(inout) :: this
            class(plot_label), intent(in) :: lbl
            class(errors), intent(inout), optional, target :: err
        end subroutine

         module subroutine plt_pop_label(this)
            class(plot), intent(inout) :: this
        end subroutine

        module function plt_get_label(this, i) result(x)
            class(plot), intent(in) :: this
            integer(int32), intent(in) :: i
            class(plot_label), pointer :: x
        end function

        module subroutine plt_set_label(this, i, x)
            class(plot), intent(inout) :: this
            integer(int32), intent(in) :: i
            class(plot_label), intent(in) :: x
        end subroutine

        pure module function plt_get_label_count(this) result(x)
            class(plot), intent(in) :: this
            integer(int32) :: x
        end function

        module subroutine plt_clear_labels(this)
            class(plot), intent(inout) :: this
        end subroutine

        pure module function plt_get_axis_equal(this) result(rst)
            class(plot), intent(in) :: this
            logical :: rst
        end function

        module subroutine plt_set_axis_equal(this, x)
            class(plot), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        module function plt_get_colormap(this) result(x)
            class(plot), intent(in) :: this
            class(colormap), pointer :: x
        end function

        module subroutine plt_set_colormap(this, x, err)
            class(plot), intent(inout) :: this
            class(colormap), intent(in) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        pure module function plt_get_show_colorbar(this) result(x)
            class(plot), intent(in) :: this
            logical :: x
        end function

        module subroutine plt_set_show_colorbar(this, x)
            class(plot), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        module function plt_get_cmd(this) result(x)
            class(plot), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        module subroutine plt_push_arrow(this, x, err)
            class(plot), intent(inout) :: this
            class(plot_arrow), intent(in) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine plt_pop_arrow(this)
            class(plot), intent(inout) :: this
        end subroutine

        module function plt_get_arrow(this, i) result(rst)
            class(plot), intent(in) :: this
            integer(int32), intent(in) :: i
            class(plot_arrow), pointer :: rst
        end function

        module subroutine plt_set_arrow(this, i, x)
            class(plot), intent(inout) :: this
            integer(int32), intent(in) :: i
            class(plot_arrow), intent(in) :: x
        end subroutine

        pure module function plt_get_arrow_count(this) result(rst)
            class(plot), intent(in) :: this
            integer(int32) :: rst
        end function

        module subroutine plt_clear_arrows(this)
            class(plot), intent(inout) :: this
        end subroutine
    end interface

! ******************************************************************************
! FPLOT_SCATTER_PLOT_DATA.F90
! ------------------------------------------------------------------------------
    !> @brief A plot_data object for describing scatter plot data sets.
    type, abstract, extends(plot_data_colored) :: scatter_plot_data
    private
        !> Draw the line?
        logical :: m_drawLine = .true.
        !> Draw the markers?
        logical :: m_drawMarkers = .false.
        !> Marker frequency.
        integer(int32) :: m_markerFrequency = 1
        !> Line width.
        real(real32) :: m_lineWidth = 1.0
        !> Line style.
        integer(int32) :: m_lineStyle = LINE_SOLID
        !> Marker type.
        integer(int32) :: m_markerType = MARKER_X
        !> Marker size multiplier.
        real(real32) :: m_markerSize = 1.0
        !> True if large data sets should be simplified before sending to
        !! GNUPLOT.
        logical :: m_simplifyData = .true.
        !> A scaling factor used to establish the simplification tolerance.
        !! The simplification tolerance is established by multiplying this
        !! factor by the range in the dependent variable data.
        real(real64) :: m_simplifyFactor = 1.0d-3
        !> Determines if the data should utilize data-dependent colors.
        logical :: m_dataDependentColors = .false.
        !> Fill the curve?
        logical :: m_filledCurve = .false.
    contains
        !> @brief Gets the GNUPLOT command string to represent this
        !! scatter_plot_data object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_command_string(class(scatter_plot_data) this)
        !! @endcode
        !!
        !! @param[in] this The scatter_plot_data object.
        !! @return The command string.
        procedure, public :: get_command_string => spd_get_cmd
        !> @brief Gets the width of the line, in pixels.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure real(real32) function get_line_width(class(scatter_plot_data) this)
        !! @endcode
        !!
        !! @param[in] this The scatter_plot_data object.
        !! @return The line width.
        procedure, public :: get_line_width => spd_get_line_width
        !> @brief Sets the width of the line, in pixels.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_line_width(class(scatter_plot_data) this, real(real32) x)
        !! @endcode
        !!
        !! @param[in,out] this The scatter_plot_data object.
        !! @param[in] x The line width.
        procedure, public :: set_line_width => spd_set_line_width
        !> @brief Gets the line style.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure integer(int32) function get_line_style(class(scatter_plot_data) this)
        !! @endcode
        !!
        !! @param[in] this The scatter_plot_data object.
        !! @return The line style.  The line style must be one of the following:
        !!  - LINE_DASHED
        !!  - LINE_DASH_DOTTED
        !!  - LINE_DASH_DOT_DOT
        !!  - LINE_DOTTED
        !!  - LINE_SOLID
        procedure, public :: get_line_style => spd_get_line_style
        !> @brief Sets the line style.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_line_style(class(scatter_plot_data) this, integer(int32) x)
        !! @endcode
        !!
        !! @param[in,out] this The scatter_plot_data object.
        !! @param[in] x The line style.  The line style must be one of the
        !!      following:
        !!  - LINE_DASHED
        !!  - LINE_DASH_DOTTED
        !!  - LINE_DASH_DOT_DOT
        !!  - LINE_DOTTED
        !!  - LINE_SOLID
        procedure, public :: set_line_style => spd_set_line_style
        !> @brief Gets a value determining if a line should be drawn.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function get_draw_line(class(scatter_plot_data) this)
        !! @endcode
        !!
        !! @param[in] this The scatter_plot_data object.
        !! @return Returns true if the line should be drawn; else, false.
        procedure, public :: get_draw_line => spd_get_draw_line
        !> @brief Sets a value determining if a line should be drawn.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_draw_line(class(scatter_plot_data) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The scatter_plot_data object.
        !! @param[in] x Set to true if the line should be drawn; else, false.
        procedure, public :: set_draw_line => spd_set_draw_line
        !> @brief Gets a value determining if data point markers should be
        !! drawn.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function get_draw_markers(class(scatter_plot_data) this)
        !! @endcode
        !!
        !! @param[in] this The scatter_plot_data object.
        !! @return Returns true if the markers should be drawn; else, false.
        procedure, public :: get_draw_markers => spd_get_draw_markers
        !> @brief Sets a value determining if data point markers should be
        !! drawn.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_draw_markers(class(scatter_plot_data) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The scatter_plot_data object.
        !! @param[in] x Set to true if the markers should be drawn; else, false.
        procedure, public :: set_draw_markers => spd_set_draw_markers
        !> @brief Gets the marker style.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure integer(int32) function get_marker_style(class(scatter_plot_data) this)
        !! @endcode
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
        procedure, public :: get_marker_style => spd_get_marker_style
        !> @brief Sets the marker style.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_marker_style(class(scatter_plot_data) this, integer(int32) x)
        !! @endcode
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
        procedure, public :: set_marker_style => spd_set_marker_style
        !> @brief Gets the marker scaling.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure real(real32) function get_marker_scaling(class(scatter_plot_data) this)
        !! @endcode
        !!
        !! @param[in] this The scatter_plot_data object.
        !! @return The scaling factor.
        procedure, public :: get_marker_scaling => spd_get_marker_scaling
        !> @brief Sets the marker scaling.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_marker_scaling(class(scatter_plot_data) this, real(real32) x)
        !! @endcode
        !!
        !! @param[in,out] this The scatter_plot_data object.
        !! @param[in] x The scaling factor.
        procedure, public :: set_marker_scaling => spd_set_marker_scaling
        !> @brief Gets the marker frequency.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure integer(int32) function get_marker_frequency(class(scatter_plot_data) this)
        !! @endcode
        !!
        !! @param[in] this The scatter_plot_data object.
        !! @return The marker frequency.
        procedure, public :: get_marker_frequency => spd_get_marker_frequency
        !> @brief Sets the marker frequency.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_marker_frequency(class(scatter_plot_data) this, integer(int32) x)
        !! @endcode
        !!
        !! @param[in,out] this The scatter_plot_data object.
        !! @param[in] x The marker frequency.
        procedure, public :: set_marker_frequency => spd_set_marker_frequency
        !> @brief Gets the number of data points.
        procedure(spd_get_int_value), deferred, public :: get_count
        !> @brief Gets the requested X data point.
        procedure(spd_get_value), deferred, public :: get_x
        !> @brief Sets the requested X data point.
        procedure(spd_set_value), deferred, public :: set_x
        !> @brief Gets the requested Y data point.
        procedure(spd_get_value), deferred, public :: get_y
        !> @brief Sets the requested X data point.
        procedure(spd_set_value), deferred, public :: set_y
        !> @brief Gets the GNUPLOT command string defining which axes the data
        !! is to be plotted against.
        procedure(spd_get_string_result), deferred, public :: get_axes_string
        !> @brief Gets a value determining if the stored data should be
        !! simplified (reduced) before passing to GNUPLOT.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function get_simplify_data(class(scatter_plot_data) this)
        !! @endcode
        !!
        !! @param[in] this The scatter_plot_data object.
        !! @return Returns true if the data should be simplified prior to sending
        !!  to GNUPLOT; else, false to leave the data alone.
        procedure, public :: get_simplify_data => spd_get_simplify_data
        !> @brief Sets a value determining if the stored data should be
        !! simplified (reduced) before passing to GNUPLOT.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_simplify_data(class(scatter_plot_data) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The scatter_plot_data object.
        !! @param[in] x True if the data should be simplified prior to sending
        !!  to GNUPLOT; else, false to leave the data alone.
        procedure, public :: set_simplify_data => spd_set_simplify_data
        !> @brief Gets a factor used to establish the simplification tolerance.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure real(real64) function get_simplification_factor(class(scatter_plot_data) this)
        !! @endcode
        !!
        !! @param[in] this The scatter_plot_data object.
        !! @return Returns the scaling factor.
        procedure, public :: get_simplification_factor => spd_get_simplify_factor
        !> @brief Sets a factor used to establish the simplification tolerance.  The
        !! tolerance is established by multplying this factor by the range of the
        !! dependent variable data.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_simplification_factor(class(scatter_plot_data) this, real(real64) x)
        !! @endcode
        !!
        !! @param[in,out] this The scatter_plot_data object.
        !! @param[in] x The scaling factor.
        procedure, public :: set_simplification_factor => spd_set_simplify_factor
        !> @brief Gets a value determing if data-dependent colors should be 
        !! used.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function get_use_data_dependent_colors(class(scatter_plot_data) this)
        !! @endcode
        !!
        !! @param[in] this The scatter_plot_data object.
        !! @return Returns true if data-dependent colors should be used; else, 
        !! false.
        procedure, public :: get_use_data_dependent_colors => &
            spd_get_data_dependent_colors
        !> @brief Sets a value determing if data dependent colors should be 
        !! used.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_use_data_dependent_colors(class(scatter_plot_data) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The scatter_plot_data object.
        !! @param[in] x True if data-dependent colors should be used; else, 
        !!  false.
        procedure, public :: set_use_data_dependent_colors => &
            spd_set_data_dependent_colors
        !> @brief Gets a logical value determining if a filled curve should be
        !!  drawn.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! logical function get_fill_curve(class(scatter_plot_data) this)
        !! @endcode
        !!
        !! @param[in] this The scatter_plot_data object.
        !! @return Returns true if the curve should be filled; else, false.
        procedure, public :: get_fill_curve => spd_get_filled
        !> @brief Sets a logical value determining if a filled curve should be
        !!  drawn.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_fill_curve(class(scatter_plot_data) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The scatter_plot_data object.
        !! @param[in] Set to true if the curve should be filled; else, false.
        procedure, public :: set_fill_curve => spd_set_filled
    end type

! ------------------------------------------------------------------------------
    interface
        module function spd_get_cmd(this) result(x)
            class(scatter_plot_data), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        pure module function spd_get_line_width(this) result(x)
            class(scatter_plot_data), intent(in) :: this
            real(real32) :: x
        end function

        module subroutine spd_set_line_width(this, x)
            class(scatter_plot_data), intent(inout) :: this
            real(real32), intent(in) :: x
        end subroutine

        pure module function spd_get_line_style(this) result(x)
            class(scatter_plot_data), intent(in) :: this
            integer(int32) :: x
        end function

        module subroutine spd_set_line_style(this, x)
            class(scatter_plot_data), intent(inout) :: this
            integer(int32), intent(in) :: x
        end subroutine

        pure module function spd_get_draw_line(this) result(x)
            class(scatter_plot_data), intent(in) :: this
            logical :: x
        end function

        module subroutine spd_set_draw_line(this, x)
            class(scatter_plot_data), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        pure module function spd_get_draw_markers(this) result(x)
            class(scatter_plot_data), intent(in) :: this
            logical :: x
        end function

        module subroutine spd_set_draw_markers(this, x)
            class(scatter_plot_data), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        pure module function spd_get_marker_style(this) result(x)
            class(scatter_plot_data), intent(in) :: this
            integer(int32) :: x
        end function

        module subroutine spd_set_marker_style(this, x)
            class(scatter_plot_data), intent(inout) :: this
            integer(int32), intent(in) :: x
        end subroutine

        pure module function spd_get_marker_scaling(this) result(x)
            class(scatter_plot_data), intent(in) :: this
            real(real32) :: x
        end function

        module subroutine spd_set_marker_scaling(this, x)
            class(scatter_plot_data), intent(inout) :: this
            real(real32), intent(in) :: x
        end subroutine

        pure module function spd_get_marker_frequency(this) result(x)
            class(scatter_plot_data), intent(in) :: this
            integer(int32) :: x
        end function

        module subroutine spd_set_marker_frequency(this, x)
            class(scatter_plot_data), intent(inout) :: this
            integer(int32), intent(in) :: x
        end subroutine

        pure module function spd_get_simplify_data(this) result(x)
            class(scatter_plot_data), intent(in) :: this
            logical :: x
        end function
        
        module subroutine spd_set_simplify_data(this, x)
            class(scatter_plot_data), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        pure module function spd_get_simplify_factor(this) result(x)
            class(scatter_plot_data), intent(in) :: this
            real(real64) :: x
        end function
        
        module subroutine spd_set_simplify_factor(this, x)
            class(scatter_plot_data), intent(inout) :: this
            real(real64), intent(in) :: x
        end subroutine

        pure module function spd_get_data_dependent_colors(this) result(rst)
            class(scatter_plot_data), intent(in) :: this
            logical :: rst
        end function

        module subroutine spd_set_data_dependent_colors(this, x)
            class(scatter_plot_data), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        pure module function spd_get_filled(this) result(rst)
            class(scatter_plot_data), intent(in) :: this
            logical :: rst
        end function

        module subroutine spd_set_filled(this, x)
            class(scatter_plot_data), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine
    end interface

! ******************************************************************************
! FPLOT_PLOT_DATA_2D.F90
! ------------------------------------------------------------------------------
    !> @brief Defines a two-dimensional plot data set.
    type, extends(scatter_plot_data) :: plot_data_2d
    private
        !> An N-by-2 matrix containing the x and y data points.
        real(real64), allocatable, dimension(:,:) :: m_data
        !> Draw against the secondary y axis?
        logical :: m_useY2 = .false.
    contains
        !> @brief Gets the GNUPLOT command string defining which axes the data
        !! is to be plotted against.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_axis_string(class(plot_data_2d) this)
        !! @endcode
        !!
        !! @param[in] this The plot_data_2d object.
        !! @return The command string.
        procedure, public :: get_axes_string => pd2d_get_axes_cmd
        !> @brief Gets the GNUPLOT command string containing the actual data
        !! to plot.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_data_string(class(plot_data_2d) this)
        !! @endcode
        !!
        !! @param[in] this The plot_data_2d object.
        !! @return The command string.
        procedure, public :: get_data_string => pd2d_get_data_cmd
        !> @brief Gets the number of data points.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure integer(int32) get_count(class(plot_data_2d) this)
        !! @endcode
        !!
        !! @param[in] this The plot_data_2d object.
        !! @return The number of data points.
        procedure, public :: get_count => pd2d_get_data_count
        !> @brief Gets the requested X data point.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure real(real64) get_x(class(plot_data_2d) this, integer(int32) index)
        !! @endcode
        !!
        !! @param[in] this The plot_data_2d object.
        !! @param[in] index The index of the data point to retrieve.
        !! @return The requested data point.
        procedure, public :: get_x => pd2d_get_x_data
        !> @brief Sets the requested X data point.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_x(class(plot_data_2d) this, integer(int32) index, real(real64) x)
        !! @endcode
        !!
        !! @param[in,out] this The plot_data_2d object.
        !! @param[in] index The index of the data point to replace.
        !! @param[in] x The data point.
        procedure, public :: set_x => pd2d_set_x_data
        !> @brief Gets the requested Y data point.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure real(real64) get_y(class(plot_data_2d) this, integer(int32) index)
        !! @endcode
        !!
        !! @param[in] this The plot_data_2d object.
        !! @param[in] index The index of the data point to retrieve.
        !! @return The requested data point.
        procedure, public :: get_y => pd2d_get_y_data
        !> @brief Sets the requested Y data point.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_y(class(plot_data_2d) this, integer(int32) index, real(real64) x)
        !! @endcode
        !!
        !! @param[in,out] this The plot_data_2d object.
        !! @param[in] index The index of the data point to replace.
        !! @param[in] x The data point.
        procedure, public :: set_y => pd2d_set_y_data
        !> @brief Gets a value determining if the data should be plotted against
        !! the secondary y-axis.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function get_draw_against_y2(class(plot_data_2d) this)
        !! @endcode
        !!
        !! @param[in] this The plot_data_2d object.
        !! @return Returns true if the data should be plotted against the secondary
        !!  y-axis; else, false to plot against the primary y-axis.
        procedure, public :: get_draw_against_y2 => pd2d_get_draw_against_y2
        !> @brief Sets a value determining if the data should be plotted against
        !! the secondary y-axis.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_draw_against_y2(class(plot_data_2d) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The plot_data_2d object.
        !! @param[in] x Set to true if the data should be plotted against the
        !!  secondary y-axis; else, false to plot against the primary y-axis.
        procedure, public :: set_draw_against_y2 => pd2d_set_draw_against_y2
        !> @brief Defines the data set.
        !!
        !! @par Overload 1
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine define_data(class(plot_data_2d) this, real(real64) x(:), real(real64) y(:), optional class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The plot_data_2d object.
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
        !!
        !! @par Overload 2
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine define_data(class(plot_data_2d) this, real(real64) y(:), optional class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The plot_data_2d object.
        !! @param[in] y An N-element array containing the y-coordinate data.  This
        !!  data will be plotted against its own index.
        !! @param[out] err An optional errors-based object that if provided can be
        !!  used to retrieve information relating to any errors encountered during
        !!  execution.  If not provided, a default implementation of the errors
        !!  class is used internally to provide error handling.  Possible errors and
        !!  warning messages that may be encountered are as follows.
        !!  - PLOT_OUT_OF_MEMORY_ERROR: Occurs if insufficient memory is available.
        !!
        !! @par Overload 3
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine define_data(class(plot_data_2d) this, real(real64) x(:), real(real64) y(:), real(real64) c(:), optional class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The plot_data_2d object.
        !! @param[in] x An N-element array containing the x coordinate data.
        !! @param[in] y An N-element array containing the y coordinate data.
        !! @param[in] c An N-element array defining how color should vary with
        !!  the current colormap for each value.
        !! @param[out] err An optional errors-based object that if provided can be
        !!  used to retrieve information relating to any errors encountered during
        !!  execution.  If not provided, a default implementation of the errors
        !!  class is used internally to provide error handling.  Possible errors and
        !!  warning messages that may be encountered are as follows.
        !!  - PLOT_OUT_OF_MEMORY_ERROR: Occurs if insufficient memory is available.
        !!  - PLOT_ARRAY_SIZE_MISMATCH_ERROR: Occurs if @p x and @p y are not the
        !!      same size.
        generic, public :: define_data => pd2d_set_data_1, pd2d_set_data_2
        procedure :: pd2d_set_data_1
        procedure :: pd2d_set_data_2

        !> @brief Gets the stored X data array.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! real(real64)(:) function get_x_data(class(plot_data_2d) this)
        !! @endcode
        !!
        !! @param[in] this The plot_data_2d object.
        !! @return A copy of the stored data array.
        procedure, public :: get_x_data => pd2d_get_x_array
        !> @brief Gets the stored Y data array.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! real(real64)(:) function get_y_data(class(plot_data_2d) this)
        !! @endcode
        !!
        !! @param[in] this The plot_data_2d object.
        !! @return A copy of the stored data array.
        procedure, public :: get_y_data => pd2d_get_y_array
        !> @brief Gets the stored color scaling data array.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! real(real64)(:) function get_color_data(class(plot_data_2d) this)
        !! @endcode
        !!
        !! @param[in] this The plot_data_2d object.
        !! @return A copy of the stored data array.
        procedure, public :: get_color_data => pd2d_get_c_array
    end type

! ------------------------------------------------------------------------------
    interface
        module function pd2d_get_axes_cmd(this) result(x)
            class(plot_data_2d), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        module function pd2d_get_data_cmd(this) result(x)
            class(plot_data_2d), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        pure module function pd2d_get_data_count(this) result(x)
            class(plot_data_2d), intent(in) :: this
            integer(int32) :: x
        end function

        pure module function pd2d_get_x_data(this, index) result(x)
            class(plot_data_2d), intent(in) :: this
            integer(int32), intent(in) :: index
            real(real64) :: x
        end function

        module subroutine pd2d_set_x_data(this, index, x)
            class(plot_data_2d), intent(inout) :: this
            integer(int32), intent(in) :: index
            real(real64), intent(in) :: x
        end subroutine

        pure module function pd2d_get_y_data(this, index) result(x)
            class(plot_data_2d), intent(in) :: this
            integer(int32), intent(in) :: index
            real(real64) :: x
        end function

        module subroutine pd2d_set_y_data(this, index, x)
            class(plot_data_2d), intent(inout) :: this
            integer(int32), intent(in) :: index
            real(real64), intent(in) :: x
        end subroutine

        module subroutine pd2d_set_data_1(this, x, y, c, err)
            class(plot_data_2d), intent(inout) :: this
            real(real64), intent(in), dimension(:) :: x, y
            real(real64), intent(in), dimension(:), optional :: c
            class(errors), intent(inout), optional, target :: err
        end subroutine

        pure module function pd2d_get_draw_against_y2(this) result(x)
            class(plot_data_2d), intent(in) :: this
            logical :: x
        end function

        module subroutine pd2d_set_draw_against_y2(this, x)
            class(plot_data_2d), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        module subroutine pd2d_set_data_2(this, y, err)
            class(plot_data_2d), intent(inout) :: this
            real(real64), intent(in), dimension(:) :: y
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module function pd2d_get_x_array(this) result(x)
            class(plot_data_2d), intent(in) :: this
            real(real64), allocatable, dimension(:) :: x
        end function

        module function pd2d_get_y_array(this) result(x)
            class(plot_data_2d), intent(in) :: this
            real(real64), allocatable, dimension(:) :: x
        end function

        module function pd2d_get_c_array(this) result(x)
            class(plot_data_2d), intent(in) :: this
            real(real64), allocatable, dimension(:) :: x
        end function
    end interface

! ******************************************************************************
! FPLOT_PLOT_DATA_3D.F90
! ------------------------------------------------------------------------------
    !> @brief Defines a three-dimensional plot data set.
    type, extends(scatter_plot_data) :: plot_data_3d
    private
        !> An N-by-3 matrix containing the x, y, and z data points.
        real(real64), allocatable, dimension(:,:) :: m_data
    contains
        !> @brief Gets the number of data points.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure integer(int32) function get_count(class(plot_data_3d) this)
        !! @endcode
        !!
        !! @param[in] this The plot_data_3d object.
        !! @return The number of data points.
        procedure, public :: get_count => pd3d_get_data_count
        !> @brief Gets the requested X data point.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure real(real64) function get_x(class(plot_data_3d), this, integer(int32) index)
        !! @endcode
        !!
        !! @param[in] this The plot_data_3d object.
        !! @param[in] index The index of the data point to retrieve.
        !! @return The requested data point.
        procedure, public :: get_x => pd3d_get_x_data
        !> @brief Sets the requested X data point.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_x(class(plot_data_3d) this, integer(int32) index, real(real64) x)
        !! @endcode
        !!
        !! @param[in,out] this The plot_data_3d object.
        !! @param[in] index The index of the data point to replace.
        !! @param[in] x The data point.
        procedure, public :: set_x => pd3d_set_x_data
        !> @brief Gets the requested Y data point.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure real(real64) function get_y(class(plot_data_3d) this, this, integer(int32) index)
        !! @endcode
        !!
        !! @param[in] this The plot_data_3d object.
        !! @param[in] index The index of the data point to retrieve.
        !! @return The requested data point.
        procedure, public :: get_y => pd3d_get_y_data
        !> @brief Sets the requested Y data point.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_y(class(plot_data_3d) this, integer(int32) index, real(real64) x)
        !! @endcode
        !!
        !! @param[in,out] this The plot_data_3d object.
        !! @param[in] index The index of the data point to replace.
        !! @param[in] x The data point.
        procedure, public :: set_y => pd3d_set_y_data
        !> @brief Gets the requested Z data point.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure real(real64) function get_z(class(plot_data_3d) this, this, integer(int32) index)
        !! @endcode
        !!
        !! @param[in] this The plot_data_3d object.
        !! @param[in] index The index of the data point to retrieve.
        !! @return The requested data point.
        procedure, public :: get_z => pd3d_get_z_data
        !> @brief Sets the requested Z data point.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_z(class(plot_data_3d) this, integer(int32) index, real(real64) x)
        !! @endcode
        !!
        !! @param[in,out] this The plot_data_3d object.
        !! @param[in] index The index of the data point to replace.
        !! @param[in] x The data point.
        procedure, public :: set_z => pd3d_set_z_data
        !> @brief Gets the GNUPLOT command string defining which axes the data
        !! is to be plotted against.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable :: get_axes_string(class(plot_data_3d) this)
        !! @endcode
        !!
        !! @param[in] this The plot_data_3d object.
        !! @return The command string.
        procedure, public :: get_axes_string => pd3d_get_axes_cmd
        !> @brief Gets the GNUPLOT command string containing the actual data
        !! to plot.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable :: get_data_string(class(plot_data_3d) this)
        !! @endcode
        !!
        !! @param[in] this The plot_data_3d object.
        !! @return The command string.
        procedure, public :: get_data_string => pd3d_get_data_cmd
        !> @brief Defines the data set.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine define_data(class(plot_data_3d) this, real(real64) x(:), real(real64) y(:), real(real64) z(:), optional class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The plot_data_2d object.
        !! @param[in] x An N-element array containing the x coordinate data.
        !! @param[in] y An N-element array containing the y coordinate data.
        !! @param[in] z An N-element array containing the z coordinate data.
        !! @param[out] err An optional errors-based object that if provided can be
        !!  used to retrieve information relating to any errors encountered during
        !!  execution.  If not provided, a default implementation of the errors
        !!  class is used internally to provide error handling.  Possible errors and
        !!  warning messages that may be encountered are as follows.
        !!  - PLOT_OUT_OF_MEMORY_ERROR: Occurs if insufficient memory is available.
        !!  - PLOT_ARRAY_SIZE_MISMATCH_ERROR: Occurs if @p x, @p y, and @p z are
        !!      not the same size.
        procedure, public :: define_data => pd3d_set_data_1
        !> @brief Gets the stored X data array.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! real(real64)(:) function get_x_data(class(plot_data_3d) this)
        !! @endcode
        !!
        !! @param[in] this The plot_data_3d object.
        !! @return A copy of the stored data array.
        procedure, public :: get_x_data => pd3d_get_x_array
        !> @brief Gets the stored Y data array.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! real(real64)(:) function get_y_data(class(plot_data_3d) this)
        !! @endcode
        !!
        !! @param[in] this The plot_data_3d object.
        !! @return A copy of the stored data array.
        procedure, public :: get_y_data => pd3d_get_y_array
        !> @brief Gets the stored Z data array.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! real(real64)(:) function get_z_data(class(plot_data_3d) this)
        !! @endcode
        !!
        !! @param[in] this The plot_data_3d object.
        !! @return A copy of the stored data array.
        procedure, public :: get_z_data => pd3d_get_z_array
        !> @brief Gets the stored color scaling data array.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! real(real64)(:) function get_color_data(class(plot_data_3d) this)
        !! @endcode
        !!
        !! @param[in] this The plot_data_3d object.
        !! @return A copy of the stored data array.
        procedure, public :: get_color_data => pd3d_get_c_array
    end type

! ------------------------------------------------------------------------------
    interface
        pure module function pd3d_get_data_count(this) result(x)
            class(plot_data_3d), intent(in) :: this
            integer(int32) :: x
        end function

        pure module function pd3d_get_x_data(this, index) result(x)
            class(plot_data_3d), intent(in) :: this
            integer(int32), intent(in) :: index
            real(real64) :: x
        end function

        module subroutine pd3d_set_x_data(this, index, x)
            class(plot_data_3d), intent(inout) :: this
            integer(int32), intent(in) :: index
            real(real64), intent(in) :: x
        end subroutine

        pure module function pd3d_get_y_data(this, index) result(x)
            class(plot_data_3d), intent(in) :: this
            integer(int32), intent(in) :: index
            real(real64) :: x
        end function

        module subroutine pd3d_set_y_data(this, index, x)
            class(plot_data_3d), intent(inout) :: this
            integer(int32), intent(in) :: index
            real(real64), intent(in) :: x
        end subroutine

        pure module function pd3d_get_z_data(this, index) result(x)
            class(plot_data_3d), intent(in) :: this
            integer(int32), intent(in) :: index
            real(real64) :: x
        end function

        module subroutine pd3d_set_z_data(this, index, x)
            class(plot_data_3d), intent(inout) :: this
            integer(int32), intent(in) :: index
            real(real64), intent(in) :: x
        end subroutine

        module function pd3d_get_axes_cmd(this) result(x)
            class(plot_data_3d), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        module function pd3d_get_data_cmd(this) result(x)
            class(plot_data_3d), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        module subroutine pd3d_set_data_1(this, x, y, z, c, err)
            class(plot_data_3d), intent(inout) :: this
            real(real64), intent(in), dimension(:) :: x, y, z
            real(real64), intent(in), dimension(:), optional :: c
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module function pd3d_get_x_array(this) result(x)
            class(plot_data_3d), intent(in) :: this
            real(real64), allocatable, dimension(:) :: x
        end function

        module function pd3d_get_y_array(this) result(x)
            class(plot_data_3d), intent(in) :: this
            real(real64), allocatable, dimension(:) :: x
        end function

        module function pd3d_get_z_array(this) result(x)
            class(plot_data_3d), intent(in) :: this
            real(real64), allocatable, dimension(:) :: x
        end function

        module function pd3d_get_c_array(this) result(x)
            class(plot_data_3d), intent(in) :: this
            real(real64), allocatable, dimension(:) :: x
        end function
    end interface

! ******************************************************************************
! FPLOT_SURFACE_PLOT_DATA.F90
! ------------------------------------------------------------------------------
    !> @brief Provides a three-dimensional surface plot data set.
    type, extends(plot_data) :: surface_plot_data
    private
        !> Stores the x-coordinate data
        real(real64), allocatable, dimension(:,:) :: m_x
        !> Stores the y-coordinate data
        real(real64), allocatable, dimension(:,:) :: m_y
        !> Stores the z-coordinate data
        real(real64), allocatable, dimension(:,:) :: m_z
        !> Set to true to display a wireframe of the surface; else, just a
        !! smooth surface will be drawn
        logical :: m_wireframe = .false.
    contains
        !> @brief Gets the size of the stored data set.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure integer(int32) function get_size(class(surface_plot_data) this, integer(int32) dim)
        !! @endcode
        !!
        !! @param[in] this The suface_plot_data object.
        !! @param[in] dim The dimension of interest.  Notice, data is stored as a
        !!  2D matrix (i.e. only 1 and 2 are valid inputs).
        !! @return The size of the requested dimension.
        procedure, public :: get_size => surfd_get_size
        !> @brief Gets the requested X data point.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure real(real64) function get_x(class(surface_plot_data) this, integer(int32) i, integer(int32) j)
        !! @endcode
        !!
        !! @param[in] this The surface_plot_data object.
        !! @param[in] i The row index.
        !! @param[in] j The column index.
        !! @return The value.
        procedure, public :: get_x => surfd_get_x
        !> @brief Sets the requested X data point.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_x(class(surface_plot_data) this, integer(int32) i, integer(int32) j, real(real64) x)
        !! @endcode
        !!
        !! @param[in,out] this The surface_plot_data object.
        !! @param[in] i The row index.
        !! @param[in] j The column index.
        !! @param[in] x The value.
        procedure, public :: set_x => surfd_set_x
        !> @brief Gets the requested Y data point.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure real(real64) function get_y(class(surface_plot_data) this, integer(int32) i, integer(int32) j)
        !! @endcode
        !!
        !! @param[in] this The surface_plot_data object.
        !! @param[in] i The row index.
        !! @param[in] j The column index.
        !! @return The value.
        procedure, public :: get_y => surfd_get_y
        !> @brief Sets the requested Y data point.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_y(class(surface_plot_data) this, integer(int32) i, integer(int32) j, real(real64) x)
        !! @endcode
        !!
        !! @param[in,out] this The surface_plot_data object.
        !! @param[in] i The row index.
        !! @param[in] j The column index.
        !! @param[in] x The value.
        procedure, public :: set_y => surfd_set_y
        !> @brief Gets the requested Z data point.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure real(real64) function get_z(class(surface_plot_data) this, integer(int32) i, integer(int32) j)
        !! @endcode
        !!
        !! @param[in] this The surface_plot_data object.
        !! @param[in] i The row index.
        !! @param[in] j The column index.
        !! @return The value.
        procedure, public :: get_z => surfd_get_z
        !> @brief Sets the requested Z data point.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_z(class(surface_plot_data) this, integer(int32) i, integer(int32) j, real(real64) x)
        !! @endcode
        !!
        !! @param[in,out] this The surface_plot_data object.
        !! @param[in] i The row index.
        !! @param[in] j The column index.
        !! @param[in] x The value.
        procedure, public :: set_z => surfd_set_z
        !> @brief Gets a value determining if a wireframe mesh should be
        !! displayed.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function get_wireframe(class(surface_plot_data) this)
        !! @endcode
        !!
        !! @param[in] this The surface_plot_data object.
        !! @return Returns true if a wireframe mesh should be displayed; else, false
        !!  to display a solid surface.
        procedure, public :: get_use_wireframe => surfd_get_wireframe
        !> @brief Sets a value determining if a wireframe mesh should be
        !! displayed.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_wireframe(class(surface_plot_data) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The surface_plot_data object.
        !! @param[in] x Set to true if a wireframe mesh should be displayed; else,
        !!  false to display a solid surface.
        procedure, public :: set_use_wireframe => surfd_set_wireframe
        !> @brief Gets the GNUPLOT command string to represent this
        !! surface_plot_data object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_command_string(class(surface_plot_data) this)
        !! @endcode
        !!
        !! @param[in] this The surface_plot_data object.
        !! @return The command string.
        procedure, public :: get_command_string => surfd_get_cmd
        !> @brief Gets the GNUPLOT command string containing the actual data
        !! to plot.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_data_string(class(surface_plot_data) this)
        !! @endcode
        !!
        !! @param[in] this The surface_plot_data object.
        !! @return The GNUPLOT command string.
        procedure, public :: get_data_string => surfd_get_data_cmd
        !> @brief Defines the data set.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine define_data(class(surface_plot_data) this, real(real64) x(:,:), real(real64) y(:,:), real(real64) z(:,:))
        !! @endcode
        !!
        !! @param[in,out] this The surface_plot_data object.
        !! @param[in] x An M-by-N matrix containing the x-coordinate data.
        !! @param[in] y An M-by-N matrix containing the y-coordinate data.
        !! @param[in] z An M-by-N matrix containing the z-coordinate data.
        !! @param[out] err An optional errors-based object that if provided can be
        !!  used to retrieve information relating to any errors encountered during
        !!  execution.  If not provided, a default implementation of the errors
        !!  class is used internally to provide error handling.  Possible errors and
        !!  warning messages that may be encountered are as follows.
        !!  - PLOT_OUT_OF_MEMORY_ERROR: Occurs if insufficient memory is available.
        !!  - PLOT_ARRAY_SIZE_MISMATCH_ERROR: Occurs if @p x, @p y, and @p z are
        !!      not the same size.
        procedure, public :: define_data => surfd_set_data_1
    end type

! ------------------------------------------------------------------------------
    interface
        pure module function surfd_get_size(this, dim) result(x)
            class(surface_plot_data), intent(in) :: this
            integer(int32), intent(in) :: dim
            integer(int32) :: x
        end function

        pure module function surfd_get_x(this, i, j) result(x)
            class(surface_plot_data), intent(in) :: this
            integer(int32), intent(in) :: i, j
            real(real64) :: x
        end function

        module subroutine surfd_set_x(this, i, j, x)
            class(surface_plot_data), intent(inout) :: this
            integer(int32), intent(in) :: i, j
            real(real64), intent(in) :: x
        end subroutine

        pure module function surfd_get_y(this, i, j) result(x)
            class(surface_plot_data), intent(in) :: this
            integer(int32), intent(in) :: i, j
            real(real64) :: x
        end function

        module subroutine surfd_set_y(this, i, j, x)
            class(surface_plot_data), intent(inout) :: this
            integer(int32), intent(in) :: i, j
            real(real64), intent(in) :: x
        end subroutine

        pure module function surfd_get_z(this, i, j) result(x)
            class(surface_plot_data), intent(in) :: this
            integer(int32), intent(in) :: i, j
            real(real64) :: x
        end function

        module subroutine surfd_set_z(this, i, j, x)
            class(surface_plot_data), intent(inout) :: this
            integer(int32), intent(in) :: i, j
            real(real64), intent(in) :: x
        end subroutine

        pure module function surfd_get_wireframe(this) result(x)
            class(surface_plot_data), intent(in) :: this
            logical :: x
        end function

        module subroutine surfd_set_wireframe(this, x)
            class(surface_plot_data), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        module function surfd_get_cmd(this) result(x)
            class(surface_plot_data), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        module function surfd_get_data_cmd(this) result(x)
            class(surface_plot_data), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        module subroutine surfd_set_data_1(this, x, y, z, err)
            class(surface_plot_data), intent(inout) :: this
            real(real64), intent(in), dimension(:,:) :: x, y, z
            class(errors), intent(inout), optional, target :: err
        end subroutine
    end interface

! ******************************************************************************
! FPLOT_PLOT_2D.F90
! ------------------------------------------------------------------------------
    !> @brief A plot object defining a 2D plot.
    type, extends(plot) :: plot_2d
    private
        !> The x-axis.
        type(x_axis), pointer :: m_xAxis => null()
        !> The y-axis.
        type(y_axis), pointer :: m_yAxis => null()
        !> The secondary y-axis.
        type(y2_axis), pointer :: m_y2Axis => null()
        !> Display the secondary y axis?
        logical :: m_useY2 = .false.
        !> Set to square scaling
        logical :: m_set2square = .false.
    contains
        !> @brief Cleans up resources held by the plot_2d object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine p2d_clean_up(type(plot_2d) this)
        !! @endcode
        !!
        !! @param[in,out] this The plot_2d object.
        final :: p2d_clean_up
        !> @brief Initializes the plot_2d object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine initialize(class(plot_2d) this, optional integer(int32) term, optional class(errors) err)
        !! @endcode
        !!
        !! @param[in] this The plot_2d object.
        !! @param[in] term An optional input that is used to define the terminal.
        !!  The default terminal is a WXT terminal.  The acceptable inputs are:
        !!  - GNUPLOT_TERMINAL_PNG
        !!  - GNUPLOT_TERMINAL_QT
        !!  - GNUPLOT_TERMINAL_WIN32
        !!  - GNUPLOT_TERMINAL_WXT
        !!  - GNUPLOT_TERMINAL_LATEX
        !! @param[in] fname A filename to pass to the terminal in the event the
        !!  terminal is a file type (e.g. GNUPLOT_TERMINAL_PNG).
        !! @param[out] err An optional errors-based object that if provided can be
        !!  used to retrieve information relating to any errors encountered during
        !!  execution.  If not provided, a default implementation of the errors
        !!  class is used internally to provide error handling.  Possible errors and
        !!  warning messages that may be encountered are as follows.
        !! - PLOT_OUT_OF_MEMORY_ERROR: Occurs if insufficient memory is available.
        procedure, public :: initialize => p2d_init
        !> @brief Gets the GNUPLOT command string to represent this plot_2d
        !! object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_command_string(class(plot_2d) this)
        !! @endcode
        !!
        !! @param[in] this The plot_2d object.
        !! @return The command string.
        procedure, public :: get_command_string => p2d_get_cmd
        !> @brief Gets the x-axis object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! class(plot_axis) function, pointer get_x_axis(class(plot_2d) this)
        !! @endcode
        !!
        !! @param[in] this The plot_2d object.
        !! @return A pointer to the x-axis object.
        !!
        procedure, public :: get_x_axis => p2d_get_x_axis
        !> @brief Gets the y-axis object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! class(plot_axis) function, pointer get_y_axis(class(plot_2d) this)
        !! @endcode
        !!
        !! @param[in] this The plot_2d object.
        !! @return A pointer to the y-axis object.
        procedure, public :: get_y_axis => p2d_get_y_axis
        !> @brief Gets the secondary y-axis object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! class(plot_axis) function, pointer get_y2_axis(class(plot_2d) this)
        !! @endcode
        !!
        !! @param[in] this The plot_2d object.
        !! @return A pointer to the secondary y-axis object.
        procedure, public :: get_y2_axis => p2d_get_y2_axis
        !> @brief Gets a flag determining if the secondary y-axis should be
        !! displayed.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function get_use_y2_axis(class(plot_2d) this)
        !! @endcode
        !!
        !! @param[in] this The plot_2d object.
        !! @return Returns true if the axis should be displayed; else, false.
        procedure, public :: get_use_y2_axis => p2d_get_use_y2
        !> @brief Sets a flag determining if the secondary y-axis should be
        !! displayed.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_use_y2_axis(class(plot_2d) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The plot_2d object.
        !! @param[in] x Set to true if the axis should be displayed; else, false.
        procedure, public :: set_use_y2_axis => p2d_set_use_y2
        !> @brief Gets a logical flag determining if the axes size should be
        !!  squared off.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! logical get_square_axes(class(plot_2d) this)
        !! @endcode
        !!
        !! @param[in] this The plot_2d object.
        !! @return Returns true if the axes are to be sized to a square; else,
        !!  false.
        procedure, public :: get_square_axes => p2d_get_square_axes
        !> @brief Sets a logical flag determining if the axes size should be
        !!  squared off.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_square_axes(class(plot_2d) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The plot_2d object.
        !! @param[in] Set to true if the axes are to be sized to a square; else,
        !!  false.
        procedure, public :: set_square_axes => p2d_set_square_axes
    end type

! ------------------------------------------------------------------------------
    interface
        module subroutine p2d_clean_up(this)
            type(plot_2d), intent(inout) :: this
        end subroutine

        module subroutine p2d_init(this, term, fname, err)
            class(plot_2d), intent(inout) :: this
            integer(int32), intent(in), optional :: term
            character(len = *), intent(in), optional :: fname
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module function p2d_get_cmd(this) result(x)
            class(plot_2d), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        module function p2d_get_x_axis(this) result(ptr)
            class(plot_2d), intent(in) :: this
            class(plot_axis), pointer :: ptr
        end function

        module function p2d_get_y_axis(this) result(ptr)
            class(plot_2d), intent(in) :: this
            class(plot_axis), pointer :: ptr
        end function

        module function p2d_get_y2_axis(this) result(ptr)
            class(plot_2d), intent(in) :: this
            class(plot_axis), pointer :: ptr
        end function

        pure module function p2d_get_use_y2(this) result(x)
            class(plot_2d), intent(in) :: this
            logical :: x
        end function

        module subroutine p2d_set_use_y2(this, x)
            class(plot_2d), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        pure module function p2d_get_square_axes(this) result(rst)
            class(plot_2d), intent(in) :: this
            logical :: rst
        end function

        module subroutine p2d_set_square_axes(this, x)
            class(plot_2d), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine
    end interface

! ******************************************************************************
! FPLOT_PLOT_3D.F90
! ------------------------------------------------------------------------------
    !> @brief A plot object defining a 3D plot.
    type, extends(plot) :: plot_3d
    private
        !> The x-axis.
        type(x_axis), pointer :: m_xAxis => null()
        !> The y-axis.
        type(y_axis), pointer :: m_yAxis => null()
        !> The z-axis.
        type(z_axis), pointer :: m_zAxis => null()
        !> The elevation angle.
        real(real64) :: m_elevation = 60.0d0
        !> The azimuth.
        real(real64) :: m_azimuth = 30.0d0
        !> Z-axis intersect X-Y plane?
        logical :: m_zIntersect = .true.
        !> Set map projection
        logical :: m_setMap = .false.
        !> Plot coordinate system.
        integer(int32) :: m_csys = COORDINATES_CARTESIAN
    contains
        !> @brief Cleans up resources held by the plot_3d object.
        !!
        !! @param[in,out] this The plot_3d object.
        final :: p3d_clean_up
        !> @brief Initializes the plot_3d object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine initialize(class(plot_3d) this, optional integer(int32) term, optional class(errors) err)
        !! @endcode
        !!
        !! @param[in] this The plot_3d object.
        !! @param[in] term An optional input that is used to define the terminal.
        !!  The default terminal is a WXT terminal.  The acceptable inputs are:
        !!  - GNUPLOT_TERMINAL_PNG
        !!  - GNUPLOT_TERMINAL_QT
        !!  - GNUPLOT_TERMINAL_WIN32
        !!  - GNUPLOT_TERMINAL_WXT
        !!  - GNUPLOT_TERMINAL_LATEX
        !! @param[in] fname A filename to pass to the terminal in the event the
        !!  terminal is a file type (e.g. GNUPLOT_TERMINAL_PNG).
        !! @param[out] err An optional errors-based object that if provided can be
        !!  used to retrieve information relating to any errors encountered during
        !!  execution.  If not provided, a default implementation of the errors
        !!  class is used internally to provide error handling.  Possible errors and
        !!  warning messages that may be encountered are as follows.
        !! - PLOT_OUT_OF_MEMORY_ERROR: Occurs if insufficient memory is available.
        procedure, public :: initialize => p3d_init
        !> @brief Gets the GNUPLOT command string to represent this plot_3d
        !! object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_command_string(class(plot_3d) this)
        !! @endcode
        !!
        !! @param[in] this The plot_3d object.
        !! @return The command string.
        procedure, public :: get_command_string => p3d_get_cmd
        !> @brief Gets the x-axis object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! class(plot_axis) function, pointer get_x_axis(class(plot_3d) this)
        !! @endcode
        !!
        !! @param[in] this The plot_3d object.
        !! @return A pointer to the x-axis object.
        procedure, public :: get_x_axis => p3d_get_x_axis
        !> @brief Gets the y-axis object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! class(plot_axis) function, pointer get_y_axis(class(plot_3d) this)
        !! @endcode
        !!
        !! @param[in] this The plot_3d object.
        !! @return A pointer to the y-axis object.
        procedure, public :: get_y_axis => p3d_get_y_axis
        !> @brief Gets the z-axis object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! class(plot_axis) function, pointer get_z_axis(class(plot_3d) this)
        !! @endcode
        !!
        !! @param[in] this The plot_3d object.
        !! @return A pointer to the z-axis object.
        procedure, public :: get_z_axis => p3d_get_z_axis
        !> @brief Gets the plot elevation angle.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! real(real64) function get_elevation(class(plot_3d) this)
        !! @endcode
        !!
        !! @param[in] this The plot_3d object.
        !! @return The elevation angle, in degrees.
        procedure, public :: get_elevation => p3d_get_elevation
        !> @brief Sets the plot elevation angle.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_elevation(class(plot_3d) this, real(real64) x)
        !! @endcode
        !!
        !! @param[in,out] this The plot_3d object.
        !! @param[in] x The elevation angle, in degrees.
        procedure, public :: set_elevation => p3d_set_elevation
        !> @brief Gets the plot azimuth angle.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! real(real64) function get_azimuth(class(plot_3d) this)
        !! @endcode
        !!
        !! @param[in] this The plot_3d object.
        !! @return The azimuth angle, in degrees.
        procedure, public :: get_azimuth => p3d_get_azimuth
        !> @brief Sets the plot azimuth angle.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_azimuth(class(plot_3d) this, real(real64) x)
        !! @endcode
        !!
        !! @param[in,out] this The plot_3d object.
        !! @param[in] x The azimuth angle, in degrees.
        procedure, public :: set_azimuth => p3d_set_azimuth
        !> @brief Gets a value determining if the z-axis should intersect the
        !! x-y plane.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function get_z_intersect_xy(class(plot_3d) this)
        !! @endcode
        !!
        !! @param[in] this The plot_3d object.
        !! @return Returns true if the z-axis should intersect the x-y plane; else,
        !!  false to allow the z-axis to float.
        procedure, public :: get_z_intersect_xy => p3d_get_z_axis_intersect
        !> @brief Sets a value determining if the z-axis should intersect the
        !! x-y plane.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_z_intersect_xy(class(plot_3d) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The plot_3d object.
        !! @param[in] x Set to true if the z-axis should intersect the x-y plane;
        !!  else, false to allow the z-axis to float.
        procedure, public :: set_z_intersect_xy => p3d_set_z_axis_intersect
        !> @brief Gets a value determining if the view should be set to a 2D
        !! map view.  If true, the azimuth and elevation terms are ignored.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function get_use_map_view(class(plot_3d) this)
        !! @endcode
        !!
        !! @param[in] this The plot_3d object.
        !! @return Returns true if the map view will be used; else, false.
        procedure, public :: get_use_map_view => p3d_get_use_map_view
        !> @brief Sets a value determining if the view should be set to a 2D
        !! map view.  If true, the azimuth and elevation terms are ignored.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_use_map_view(class(plot_3d) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The plot_3d object.
        !! @param[in] x Returns true if the map view will be used; else, false.
        procedure, public :: set_use_map_view => p3d_set_use_map_view
        !> @brief Gets a value determining the coordinate system.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure integer(int32) function get_coordinate_system(class(plot_3d) this)
        !! @endcode
        !!
        !! @param[in] this The plot_3d object.
        !! @return The coordinate system ID, which must be one of the following.
        !! - COORDINATES_CARTESIAN
        !! - COORDINATES_CYLINDRICAL
        !! - COORDINATES_SPHERICAL
        procedure, public :: get_coordinate_system => p3d_get_csys
        !> @brief Sets a value determining the coordinate system.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_coordinate_system(class(plot_3d) this, integer(int32) x)
        !! @endcode
        !!
        !! @param[in,out] this The plot_3d object.
        !! @param[in] x The coordinate system ID, which must be one of the 
        !!  following.
        !! - COORDINATES_CARTESIAN
        !! - COORDINATES_CYLINDRICAL
        !! - COORDINATES_SPHERICAL
        procedure, public :: set_coordinate_system => p3d_set_csys
    end type

! ------------------------------------------------------------------------------
    interface
        module subroutine p3d_clean_up(this)
            type(plot_3d), intent(inout) :: this
        end subroutine

        module subroutine p3d_init(this, term, fname, err)
            class(plot_3d), intent(inout) :: this
            integer(int32), intent(in), optional :: term
            character(len = *), intent(in), optional :: fname
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module function p3d_get_cmd(this) result(x)
            class(plot_3d), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        module function p3d_get_x_axis(this) result(ptr)
            class(plot_3d), intent(in) :: this
            class(plot_axis), pointer :: ptr
        end function

        module function p3d_get_y_axis(this) result(ptr)
            class(plot_3d), intent(in) :: this
            class(plot_axis), pointer :: ptr
        end function

        module function p3d_get_z_axis(this) result(ptr)
            class(plot_3d), intent(in) :: this
            class(plot_axis), pointer :: ptr
        end function

        pure module function p3d_get_elevation(this) result(x)
            class(plot_3d), intent(in) :: this
            real(real64) :: x
        end function

        module subroutine p3d_set_elevation(this, x)
            class(plot_3d), intent(inout) :: this
            real(real64), intent(in) :: x
        end subroutine

        pure module function p3d_get_azimuth(this) result(x)
            class(plot_3d), intent(in) :: this
            real(real64) :: x
        end function

        module subroutine p3d_set_azimuth(this, x)
            class(plot_3d), intent(inout) :: this
            real(real64), intent(in) :: x
        end subroutine

        pure module function p3d_get_z_axis_intersect(this) result(x)
            class(plot_3d), intent(in) :: this
            logical :: x
        end function

        module subroutine p3d_set_z_axis_intersect(this, x)
            class(plot_3d), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        pure module function p3d_get_use_map_view(this) result(rst)
            class(plot_3d), intent(in) :: this
            logical :: rst
        end function
        
        module subroutine p3d_set_use_map_view(this, x)
            class(plot_3d), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        pure module function p3d_get_csys(this) result(rst)
            class(plot_3d), intent(in) :: this
            integer(int32) :: rst
        end function

        module subroutine p3d_set_csys(this, x)
            class(plot_3d), intent(inout) :: this
            integer(int32), intent(in) :: x
        end subroutine
    end interface

! ******************************************************************************
! FPLOT_SURFACE_PLOT.F90
! ------------------------------------------------------------------------------
    !> @brief A plot object defining a 3D surface plot.
    type, extends(plot_3d) :: surface_plot
    private
        !> Show hidden lines
        logical :: m_showHidden = .false.
        !> The colormap
        ! class(colormap), pointer :: m_colormap
        !> Smooth the surface?
        logical :: m_smooth = .true.
        !> Show a contour plot as well as the surface plot?
        logical :: m_contour = .false.
        ! !> Show the colorbar?
        ! logical :: m_showColorbar = .true.
        !> Use lighting?
        logical :: m_useLighting = .false.
        !> Lighting intensity (0 - 1) - default is 0.5
        real(real32) :: m_lightIntensity = 0.5
        !> Specular highlight intensity (0 - 1)
        real(real32) :: m_specular = 0.5
        !> Defines the translucency value.  Must exist on (0, 1].
        real(real32) :: m_transparency = 1.0
    contains
        !> @brief Cleans up resources held by the surface_plot object.
        !!
        !! @param[in,out] this The surface_plot object.
        ! final :: surf_clean_up
        !> @brief Initializes the surface_plot object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine initialize(class(surface_plot) this, optional integer(int32) term, optional class(errors) err)
        !! @endcode
        !!
        !! @param[in] this The surface_plot object.
        !! @param[in] term An optional input that is used to define the terminal.
        !!  The default terminal is a WXT terminal.  The acceptable inputs are:
        !!  - GNUPLOT_TERMINAL_PNG
        !!  - GNUPLOT_TERMINAL_QT
        !!  - GNUPLOT_TERMINAL_WIN32
        !!  - GNUPLOT_TERMINAL_WXT
        !!  - GNUPLOT_TERMINAL_LATEX
        !! @param[in] fname A filename to pass to the terminal in the event the
        !!  terminal is a file type (e.g. GNUPLOT_TERMINAL_PNG).
        !! @param[out] err An optional errors-based object that if provided can be
        !!  used to retrieve information relating to any errors encountered during
        !!  execution.  If not provided, a default implementation of the errors
        !!  class is used internally to provide error handling.  Possible errors and
        !!  warning messages that may be encountered are as follows.
        !! - PLOT_OUT_OF_MEMORY_ERROR: Occurs if insufficient memory is available.
        procedure, public :: initialize => surf_init
        !> @brief Gets a value indicating if hidden lines should be shown.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function get_show_hidden(class(surface_plot) this)
        !! @endcode
        !!
        !! @param[in] this The surface_plot object.
        !! @return Returns true if hidden lines should be shown; else, false.
        procedure, public :: get_show_hidden => surf_get_show_hidden
        !> @brief Sets a value indicating if hidden lines should be shown.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_show_hidden(class(surface_plot) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The surface_plot object.
        !! @param[in] x Set to true if hidden lines should be shown; else, false.
        procedure, public :: set_show_hidden => surf_set_show_hidden
        !> @brief Gets the GNUPLOT command string to represent this plot_3d
        !! object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_command_string(class(surface_plot) this)
        !! @endcode
        !!
        !! @param[in] this The surface_plot object.
        !! @return The command string.
        procedure, public :: get_command_string => surf_get_cmd
        !> @brief Gets a value determining if the plotted surfaces should be
        !! smoothed.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function get_allow_smoothing(class(surface_plot) this)
        !! @endcode
        !!
        !! @param[in] this The surface_plot object.
        !! @return Returns true if the surface should be smoothed; else, false.
        procedure, public :: get_allow_smoothing => surf_get_smooth
        !> @brief Sets a value determining if the plotted surfaces should be
        !! smoothed.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_allow_smoothing(class(surface_plot) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The surface_plot object.
        !! @param[in] x Set to true if the surface should be smoothed; else, false.
        procedure, public :: set_allow_smoothing => surf_set_smooth
        !> @brief Gets a value determining if a contour plot should be drawn in
        !! conjunction with the surface plot.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function get_show_contours(class(surface_plot) this)
        !! @endcode
        !!
        !! @param[in] this The surface_plot object.
        !! @return Returns true if the contour plot should be drawn; else, false to
        !!  only draw the surface.
        procedure, public :: get_show_contours => surf_get_show_contours
        !> @brief Sets a value determining if a contour plot should be drawn in
        !! conjunction with the surface plot.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_show_contours(class(surface_plot) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The surface_plot object.
        !! @param[in] x Set to true if the contour plot should be drawn; else, false
        !!  to only draw the surface.
        procedure, public :: set_show_contours => surf_set_show_contours
        !> @brief Gets a value indicating if lighting, beyond the ambient
        !! light source, is to be used.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function get_use_lighting(class(surface_plot) this)
        !! @endcode
        !!
        !! @param[in] this The surface_plot object.
        !! @return True if lighting should be used; else, false.
        procedure, public :: get_use_lighting => surf_get_use_lighting
        !> @brief Sets a value indicating if lighting, beyond the ambient
        !! light source, is to be used.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_use_lighting(class(surface_plot) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The surface_plot object.
        !! @param[in] x True if lighting should be used; else, false.
        procedure, public :: set_use_lighting => surf_set_use_lighting
        !> @brief Gets the ratio of the strength of the light source relative
        !! to the ambient light.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure real(real32) function get_light_intensity(class(surface_plot) this)
        !! @endcode
        !!
        !! @param[in] this The surface_plot object.
        !! @return The light intensity ratio.
        procedure, public :: get_light_intensity => surf_get_light_intensity
        !> @brief Sets the ratio of the strength of the light source relative
        !! to the ambient light.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_light_intensity(class(surface_plot) this, real(real32) x)
        !! @endcode
        !!
        !! @param[in,out] this The surface_plot object.
        !! @param[in] x The light intensity ratio.  The value must exist in the
        !!  set [0, 1]; else, it will be clipped to lie within the range.
        procedure, public :: set_light_intensity => surf_set_light_intensity
        !> @brief Gets the ratio of the strength of the specular light source
        !! relative to the ambient light.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure real(real32) function get_specular_intensity(class(surface_plot) this)
        !! @endcode
        !!
        !! @param[in] this The surface_plot object.
        !! @return The specular light intensity ratio.
        procedure, public :: get_specular_intensity => surf_get_specular_intensity
        !> @brief Sets the ratio of the strength of the specular light source
        !! relative to the ambient light.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_specular_intensity(class(surface_plot) this, real(real32) x)
        !! @endcode
        !!
        !! @param[in,out] this The surface_plot object.
        !! @param[in] x The specular light intensity ratio.  The value must
        !!  exist in the set [0, 1]; else, it will be clipped to lie within the
        !!  range.
        procedure, public :: set_specular_intensity => surf_set_specular_intensity
        !> @brief Gets a factor defining the transparency of plotted surfaces.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure real(real32) function get_transparency(class(surface_plot) this)
        !! @endcode
        !!
        !! @param[in] this The surface_plot object.
        !! @return A value existing on the set (0 1] defining the level of
        !!  transparency.  A value of 1 indicates a fully opaque surface.
        procedure, public :: get_transparency => surf_get_transparency
        !> @brief Sets a factor defining the transparency of plotted surfaces.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_transparency(class(surface_plot) this, real(real32) x)
        !! @endcode
        !!
        !! @param[in,out] this The surface_plot object.
        !! @param[in] x A value existing on the set (0 1] defining the level of
        !!  transparency.  A value of 1 indicates a fully opaque surface.  
        !!  Any values supplied outside of the set are clipped to fit within
        !!  (0 1].
        procedure, public :: set_transparency => surf_set_transparency
    end type

! ------------------------------------------------------------------------------
    interface
        ! module subroutine surf_clean_up(this)
        !     type(surface_plot), intent(inout) :: this
        ! end subroutine

        module subroutine surf_init(this, term, fname, err)
            class(surface_plot), intent(inout) :: this
            integer(int32), intent(in), optional :: term
            character(len = *), intent(in), optional :: fname
            class(errors), intent(inout), optional, target :: err
        end subroutine

        pure module function surf_get_show_hidden(this) result(x)
            class(surface_plot), intent(in) :: this
            logical :: x
        end function

        module subroutine surf_set_show_hidden(this, x)
            class(surface_plot), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        module function surf_get_cmd(this) result(x)
            class(surface_plot), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        ! module function surf_get_colormap(this) result(x)
        !     class(surface_plot), intent(in) :: this
        !     class(colormap), pointer :: x
        ! end function

        ! module subroutine surf_set_colormap(this, x, err)
        !     class(surface_plot), intent(inout) :: this
        !     class(colormap), intent(in) :: x
        !     class(errors), intent(inout), optional, target :: err
        ! end subroutine

        pure module function surf_get_smooth(this) result(x)
            class(surface_plot), intent(in) :: this
            logical :: x
        end function

        module subroutine surf_set_smooth(this, x)
            class(surface_plot), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        pure module function surf_get_show_contours(this) result(x)
            class(surface_plot), intent(in) :: this
            logical :: x
        end function

        module subroutine surf_set_show_contours(this, x)
            class(surface_plot), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        ! pure module function surf_get_show_colorbar(this) result(x)
        !     class(surface_plot), intent(in) :: this
        !     logical :: x
        ! end function

        ! module subroutine surf_set_show_colorbar(this, x)
        !     class(surface_plot), intent(inout) :: this
        !     logical, intent(in) :: x
        ! end subroutine

        pure module function surf_get_use_lighting(this) result(x)
            class(surface_plot), intent(in) :: this
            logical :: x
        end function

        module subroutine surf_set_use_lighting(this, x)
            class(surface_plot), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        pure module function surf_get_light_intensity(this) result(x)
            class(surface_plot), intent(in) :: this
            real(real32) :: x
        end function

        module subroutine surf_set_light_intensity(this, x)
            class(surface_plot), intent(inout) :: this
            real(real32), intent(in) :: x
        end subroutine

        pure module function surf_get_specular_intensity(this) result(x)
            class(surface_plot), intent(in) :: this
            real(real32) :: x
        end function

        module subroutine surf_set_specular_intensity(this, x)
            class(surface_plot), intent(inout) :: this
            real(real32), intent(in) :: x
        end subroutine

        pure module function surf_get_transparency(this) result(x)
            class(surface_plot), intent(in) :: this
            real(real32) :: x
        end function

        module subroutine surf_set_transparency(this, x)
            class(surface_plot), intent(inout) :: this
            real(real32), intent(in) :: x
        end subroutine
    end interface

! ******************************************************************************
! FPLOT_AXIS.F90
! ------------------------------------------------------------------------------
    !> @brief An x-axis object.
    !!
    !! @par Syntax
    !! @code{.f90}
    !! character(len = :) function, allocatable get_id_string(class(x_axis) this)
    !! @endcode
    !!
    !! @param[in] this The x_axis object.
    !! @return The string.
    type, extends(plot_axis) :: x_axis
        !> The ID character
        character :: m_id = "x"
    contains
        !> @brief Gets the axis identification string.
        procedure, public :: get_id_string => xa_get_id
    end type

! ------------------------------------------------------------------------------
    !> @brief A y-axis object.
    !!
    !! @par Syntax
    !! @code{.f90}
    !! character(len = :) function, allocatable get_id_string(class(y_axis) this)
    !! @endcode
    !!
    !! @param[in] this The y_axis object.
    !! @return The string.
    type, extends(plot_axis) :: y_axis
        !> The ID character
        character :: m_id = "y"
    contains
        !> @brief Gets the axis identification string.
        procedure, public :: get_id_string => ya_get_id
    end type

! ------------------------------------------------------------------------------
    !> @brief A secondary y-axis object.
    !!
    !! @par Syntax
    !! @code{.f90}
    !! character(len = :) function, allocatable get_id_string(class(y2_axis) this)
    !! @endcode
    !!
    !! @param[in] this The y2_axis object.
    !! @return The string.
    type, extends(plot_axis) :: y2_axis
        !> The ID character
        character(len = 2) :: m_id = "y2"
    contains
        !> @brief Gets the axis identification string.
        procedure, public :: get_id_string => y2a_get_id
    end type

! ------------------------------------------------------------------------------
    !> @brief A z-axis object.
    !!
    !! @par Syntax
    !! @code{.f90}
    !! character(len = :) function, allocatable get_id_string(class(z_axis) this)
    !! @endcode
    !!
    !! @param[in] this The z_axis object.
    !! @return The string.
    type, extends(plot_axis) :: z_axis
        !> The ID character
        character :: m_id = "z"
    contains
        !> @brief Gets the axis identification string.
        procedure, public :: get_id_string => za_get_id
    end type

! ------------------------------------------------------------------------------
    interface
        module function xa_get_id(this) result(x)
            class(x_axis), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        module function ya_get_id(this) result(x)
            class(y_axis), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        module function y2a_get_id(this) result(x)
            class(y2_axis), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        module function za_get_id(this) result(x)
            class(z_axis), intent(in) :: this
            character(len = :), allocatable :: x
        end function
    end interface

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

        !> @brief Retrieves a numeric value from a scatter_plot_data object.
        !!
        !! @param[in] this The scatter_plot_data object.
        !! @param[in] index The index of the value to retrieve.
        !! @return The requested value.
        pure function spd_get_value(this, index) result(x)
            use, intrinsic :: iso_fortran_env, only : int32, real64
            import scatter_plot_data
            class(scatter_plot_data), intent(in) :: this
            integer(int32), intent(in) :: index
            real(real64) :: x
        end function

        !> @brief Sets a numeric value into a scatter_plot_data object.
        !!
        !! @param[in,out] this The scatter_plot_data object.
        !! @param[in] index The index of the value to retrieve.
        !! @param[in] x The value.
        subroutine spd_set_value(this, index, x)
            use, intrinsic :: iso_fortran_env, only : int32, real64
            import scatter_plot_data
            class(scatter_plot_data), intent(inout) :: this
            integer(int32), intent(in) :: index
            real(real64), intent(in) :: x
        end subroutine

        !> @brief Retrieves an integer value from a scatter_plot_data object.
        !!
        !! @param[in] this The scatter_plot_data object.
        !! @return The requested value.
        pure function spd_get_int_value(this) result(x)
            use, intrinsic :: iso_fortran_env, only : int32
            import scatter_plot_data
            class(scatter_plot_data), intent(in) :: this
            integer(int32) :: x
        end function

        !> @brief Retrieves a string from a scatter_plot_data object.
        !!
        !! @param[in] this The scatter_plot_data object.
        !! @return The string.
        function spd_get_string_result(this) result(x)
            import scatter_plot_data
            class(scatter_plot_data), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        !> @brief Retrieves a string from a colormap.
        !!
        !! @param[in] this The colormap object.
        !! @return The string.
        function cm_get_string_result(this) result(x)
            import colormap
            class(colormap), intent(in) :: this
            character(len = :), allocatable :: x
        end function
    end interface

! ******************************************************************************
! FPLOT_MULTIPLOT.F90
! ------------------------------------------------------------------------------
    !> @brief Defines a multi-plot layout.
    type, extends(plot_object) :: multiplot
        !> The collection of plot objects.
        type(list) :: m_plots
        !> The number of rows of plots.
        integer(int32) :: m_rows = 0
        !> The number of columns of plots.
        integer(int32) :: m_cols = 0
        !> The page title.
        character(len = PLOTDATA_MAX_NAME_LENGTH) :: m_title
        !> Has a title?
        logical :: m_hasTitle = .false.
        !> The BNUPLOT terminal object to target.
        class(terminal), pointer :: m_terminal => null()
    contains
        final :: mp_clean
        procedure, public :: get_command_string => mp_get_command
        !> @brief Initializes the multiplot object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine initialize(class(multiplot) this, optional class(terminal) term, optional class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The multiplot object.
        !! @param[in] term An optional input that is used to define the terminal.
        !!  The default terminal is a WXT terminal.  The acceptable inputs are:
        !!  - GNUPLOT_TERMINAL_PNG
        !!  - GNUPLOT_TERMINAL_QT
        !!  - GNUPLOT_TERMINAL_WIN32
        !!  - GNUPLOT_TERMINAL_WXT
        !!  - GNUPLOT_TERMINAL_LATEX
        !! @param[in,out] err An optional errors-based object that if provided can be
        !!  used to retrieve information relating to any errors encountered during
        !!  execution.  If not provided, a default implementation of the errors
        !!  class is used internally to provide error handling.  Possible errors and
        !!  warning messages that may be encountered are as follows.
        !! - PLOT_OUT_OF_MEMORY_ERROR: Occurs if insufficient memory is available.
        procedure, public :: initialize => mp_init
        !> @brief Gets the number of rows of plots.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure integer(int32) function get_row_count(class(multiplot) this)
        !! @endcode
        !!
        !! @param[in] this The multiplot object.
        !! @return The number of rows.
        procedure, public :: get_row_count => mp_get_rows
        !> @brief Gets the number of columns of plots.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure integer(int32) function get_column_count(class(multiplot) this)
        !! @endcode
        !!
        !! @param[in] this The multiplot object.
        !! @return The number of columns.
        procedure, public :: get_column_count => mp_get_cols
        !> @brief Gets the number of plots.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure integer(int32) function get_plot_count(class(multiplot) this)
        !! @endcode
        !!
        !! @param[in] this The multiplot object.
        !! @return The number of plots.
        procedure, public :: get_plot_count => mp_get_count
        !> @brief Gets the multiplot's title.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_title(class(multiplot))
        !! @endcode
        !!
        !! @param[in] this The multiplot object.
        !! @return The multiplot's title.
        procedure, public :: get_title => mp_get_title
        !> @brief Sets the multiplot's title.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_title(class(multiplot) this, character(len = *) txt)
        !! @endcode
        !!
        !! @param[in,out] this The multiplot object.
        !! @param[in] txt The multiplot's title.  The number of characters must be less
        !! than or equal to PLOTDATA_MAX_NAME_LENGTH; else, the text string is
        !! truncated.
        procedure, public :: set_title => mp_set_title
        !> @brief Launches GNUPLOT and draws the multiplot per the current state of
        !! the command list.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine draw(class(multiplot) this, optional logical persist, optional class(errors) err)
        !! @endcode
        !!
        !! @param[in] this The multiplot object.
        !! @param[in] persist An optional parameter that can be used to keep GNUPLOT
        !!  open.  Set to true to force GNUPLOT to remain open; else, set to false
        !!  to allow GNUPLOT to close after drawing.  The default is true.
        !! @param[in,out] err An optional errors-based object that if provided can be
        !!  used to retrieve information relating to any errors encountered during
        !!  execution.  If not provided, a default implementation of the errors
        !!  class is used internally to provide error handling.  Possible errors and
        !!  warning messages that may be encountered are as follows.
        !!  - PLOT_GNUPLOT_FILE_ERROR: Occurs if the command file cannot be written.
        procedure, public :: draw => mp_draw
        !> @brief Gets the requested plot object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! class(plot) pointer function get(class(multiplot) this, integer(int32) i, integer(int32) j)
        !! @endcode
        !!
        !! @param[in] this The multiplot object.
        !! @param[in] i The row index of the plot to retrieve.
        !! @param[in] j The column index of the plot to retrieve.
        !! @return A pointer to the requested plot object.
        procedure, public :: get => mp_get
        !> @brief Sets the requested plot object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set(class(multiplot) this, integer(int32) i, integer(int32) j, class(plot) pointer x)
        !! @endcode
        !!
        !! @param[in,out] this The multiplot object.
        !! @param[in] i The row index of the plot to retrieve.
        !! @param[in] j The column index of the plot to retrieve.
        !! @param[in] x A pointer to the requested plot object.
        procedure, public :: set => mp_set
        !> @brief Gets a value determining if a title has been defined for the
        !!  multiplot object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function is_title_defined(class(multiplot) this)
        !! @endcode
        !!
        !! @param[in] this The multiplot object.
        !! @return Returns true if a title has been defined for this multiplot; else,
        !!  returns false.
        procedure, public :: is_title_defined => mp_has_title
        !> @brief Gets the GNUPLOT terminal object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! class(terminal) function, pointer get_terminal(class(multiplot) this)
        !! @endcode
        !!
        !! @param[in] this The multiplot object.
        !! @return A pointer to the GNUPLOT terminal object.
        procedure, public :: get_terminal => mp_get_term
        !> @brief Saves a GNUPLOT command file.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine save_file(class(multiplot) this, character(len = *) fname, optional class(errors) err)
        !! @endcode
        !!
        !! @param[in] this The multiplot object.
        !! @param[in] fname The filename.
        !! @param[in,out] err An optional errors-based object that if provided can be
        !!  used to retrieve information relating to any errors encountered during
        !!  execution.  If not provided, a default implementation of the errors
        !!  class is used internally to provide error handling.  Possible errors and
        !!  warning messages that may be encountered are as follows.
        !!  - PLOT_GNUPLOT_FILE_ERROR: Occurs if the command file cannot be written.
        procedure, public :: save_file => mp_save
        !> @brief Gets the name of the font used for plot text.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_font_name(class(multiplot) this)
        !! @endcode
        !!
        !! @param[in] this The multiplot object.
        !! @return The font name.
        procedure, public :: get_font_name => mp_get_font
        !> @brief Sets the name of the font used for plot text.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_font_name(class(multiplot) this, character(len = *) x)
        !! @endcode
        !!
        !! @param[in,out] this The multiplot object.
        !! @param[in] x The font name.
        procedure, public :: set_font_name => mp_set_font
        !> @brief Gets the size of the font used by the plot.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! integer(int32) function get_font_size(class(multiplot) this)
        !! @endcode
        !!
        !! @param[in] this The multiplot object.
        !! @return The size of the font, in points.
        procedure, public :: get_font_size => mp_get_font_size
        !> @brief Sets the size of the font used by the plot.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_font_size(class(multiplot) this, integer(int32) x)
        !! @endcode
        !!
        !! @param[in,out] this The multiplot object.
        !! @param[in] x The font size, in points.  If a value of zero is provided,
        !! the font size is reset to its default value; or, if a negative value
        !! is provided, the absolute value of the supplied value is utilized.
        procedure, public :: set_font_size => mp_set_font_size
    end type

! ------------------------------------------------------------------------------
    interface
        module function mp_get_command(this) result(x)
            class(multiplot), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        module subroutine mp_init(this, m, n, term, err)
            class(multiplot), intent(inout) :: this
            integer(int32), intent(in) :: m, n
            integer(int32), intent(in), optional :: term
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine mp_clean(this)
            type(multiplot), intent(inout) :: this
        end subroutine

        pure module function mp_get_rows(this) result(x)
            class(multiplot), intent(in) :: this
            integer(int32) :: x
        end function

        pure module function mp_get_cols(this) result(x)
            class(multiplot), intent(in) :: this
            integer(int32) :: x
        end function

        pure module function mp_get_count(this) result(x)
            class(multiplot), intent(in) :: this
            integer(int32) :: x
        end function

        module function mp_get_title(this) result(x)
            class(multiplot), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        module subroutine mp_set_title(this, x)
            class(multiplot), intent(inout) :: this
            character(len = *), intent(in) :: x
        end subroutine

        module subroutine mp_draw(this, persist, err)
            class(multiplot), intent(in) :: this
            logical, intent(in), optional :: persist
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module function mp_get(this, i, j) result(x)
            class(multiplot), intent(in) :: this
            integer(int32), intent(in) :: i, j
            class(plot), pointer :: x
        end function

        module subroutine mp_set(this, i, j, x)
            class(multiplot), intent(inout) :: this
            integer(int32), intent(in) :: i, j
            class(plot), intent(in) :: x
        end subroutine

        pure module function mp_has_title(this) result(x)
            class(multiplot), intent(in) :: this
            logical :: x
        end function

        module function mp_get_term(this) result(x)
            class(multiplot), intent(in) :: this
            class(terminal), pointer :: x
        end function

        module subroutine mp_save(this, fname, err)
            class(multiplot), intent(in) :: this
            character(len = *), intent(in) :: fname
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module function mp_get_font(this) result(x)
            class(multiplot), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        module subroutine mp_set_font(this, x)
            class(multiplot), intent(inout) :: this
            character(len = *), intent(in) :: x
        end subroutine

        module function mp_get_font_size(this) result(x)
            class(multiplot), intent(in) :: this
            integer(int32) :: x
        end function

        module subroutine mp_set_font_size(this, x)
            class(multiplot), intent(inout) :: this
            integer(int32), intent(in) :: x
        end subroutine
    end interface

! ******************************************************************************
! FPLOT_PLOT_DATA_ERRORS.F90
! ------------------------------------------------------------------------------
    !> @brief Defines a 2D error bar based data set.
    type, extends(plot_data_colored) :: plot_data_error_bars
        !> Display x error bars?
        logical :: m_xBars = .false.
        !> Display y error bars?
        logical :: m_yBars = .false.
        !> A matrix containing the raw and error data.  Column 1 is for the
        !! x coordinate, column 2 for the y coordinate, and the remaining 
        !! columns are for the error data (x, then y if applicable)
        real(real64), allocatable, dimension(:,:) :: m_data
        !> Display an error box for the case where x and y errors are defined.
        logical :: m_box = .false.
        !> Plot error bars using a defined range vs. a +/- value.
        logical :: m_range = .false.
    contains
        procedure, public :: get_command_string => pde_get_cmd
        procedure, public :: get_data_string => pde_get_data_cmd
        !> @brief Defines the x error data.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine define_x_error_data(class(plot_data_error_bars) this, real(real64) x(:), real(real64) y(:), real(real64) xerr(:), optional class(errors) err)
        !! @endcode
        !! @par Alternative Syntax
        !! @code{.f90}
        !! subroutine define_x_error_data(class(plot_data_error_bars) this, real(real64) x(:), real(real64) y(:), real(real64) xmin(:), real(real64) xmax(:), optional class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The plot_data_error_bars object.
        !! @param[in] x An N-element array containing the x coordinates of the 
        !!  data.
        !! @param[in] y An N-element array containing the y coordinates of the 
        !!  data.
        !! @param[in] xerr An N-element array containing the x errors at each
        !!  data point.
        !! @param[in] xmin = An N-element array containing the minimum x values
        !!  at each data point.
        !! @param[in] xmax = An N-element array containing the maximum x values
        !!  at each data point.
        !! @param[in,out] err An optional errors-based object that if provided can be
        !!  used to retrieve information relating to any errors encountered during
        !!  execution.  If not provided, a default implementation of the errors
        !!  class is used internally to provide error handling.  Possible errors and
        !!  warning messages that may be encountered are as follows.
        !!  - PLOT_ARRAY_SIZE_MISMATCH_ERROR: Occurs if the input arrays are not
        !!      the same size.
        !!  - PLOT_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory
        !!      available.
        generic, public :: define_x_error_data => pde_define_x_err, &
            pde_define_x_err_lim
        !> @brief Defines the y error data.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine define_y_error_data(class(plot_data_error_bars) this, real(real64) x(:), real(real64) y(:), real(real64) yerr(:), optional class(errors) err)
        !! @endcode
        !! @par Alternative Syntax
        !! @code{.f90}
        !! subroutine define_y_error_data(class(plot_data_error_bars) this, real(real64) x(:), real(real64) y(:), real(real64) ymin(:), real(real64) ymax(:), optional class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The plot_data_error_bars object.
        !! @param[in] x An N-element array containing the x coordinates of the 
        !!  data.
        !! @param[in] y An N-element array containing the y coordinates of the 
        !!  data.
        !! @param[in] yerr An N-element array containing the y errors at each
        !!  data point.
        !! @param[in] ymin = An N-element array containing the minimum y values
        !!  at each data point.
        !! @param[in] ymax = An N-element array containing the maximum y values
        !!  at each data point.
        !! @param[in,out] err An optional errors-based object that if provided can be
        !!  used to retrieve information relating to any errors encountered during
        !!  execution.  If not provided, a default implementation of the errors
        !!  class is used internally to provide error handling.  Possible errors and
        !!  warning messages that may be encountered are as follows.
        !!  - PLOT_ARRAY_SIZE_MISMATCH_ERROR: Occurs if the input arrays are not
        !!      the same size.
        !!  - PLOT_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory
        !!      available.
        generic, public :: define_y_error_data => pde_define_y_err, &
            pde_define_y_err_lim
        !> @brief Defines the x and y error data.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine define_xy_error_data(class(plot_data_error_bars) this, real(real64) x(:), real(real64) y(:), real(real64) xerr(:), real(real64) yerr(:), optional class(errors) err)
        !! @endcode
        !! @par Alternative Syntax
        !! @code{.f90}
        !! subroutine define_xy_error_data(class(plot_data_error_bars) this, real(real64) x(:), real(real64) y(:), real(real64) xmin(:), real(real64) xmax(:), real(real64) ymin(:), real(real64) ymax(:), optional class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The plot_data_error_bars object.
        !! @param[in] x An N-element array containing the x coordinates of the 
        !!  data.
        !! @param[in] y An N-element array containing the y coordinates of the 
        !!  data.
        !! @param[in] xerr An N-element array containing the x errors at each
        !!  data point.
        !! @param[in] yerr An N-element array containing the y errors at each
        !!  data point.
        !! @param[in] xmin = An N-element array containing the minimum x values
        !!  at each data point.
        !! @param[in] xmax = An N-element array containing the maximum x values
        !!  at each data point.
        !! @param[in] ymin = An N-element array containing the minimum y values
        !!  at each data point.
        !! @param[in] ymax = An N-element array containing the maximum y values
        !!  at each data point.
        !! @param[in,out] err An optional errors-based object that if provided can be
        !!  used to retrieve information relating to any errors encountered during
        !!  execution.  If not provided, a default implementation of the errors
        !!  class is used internally to provide error handling.  Possible errors and
        !!  warning messages that may be encountered are as follows.
        !!  - PLOT_ARRAY_SIZE_MISMATCH_ERROR: Occurs if the input arrays are not
        !!      the same size.
        !!  - PLOT_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory
        !!      available.
        generic, public :: define_xy_error_data => pde_define_xy_err, &
            pde_define_xy_err_lim
        !> @brief Tests to see if the x error bar data has been defined, and as
        !!  a result, if the x error data is to be plotted.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function get_plot_x_error_bars(class(plot_data_error_bars) this)
        !! @endcode
        !!
        !! @param[in] this The plot_data_error_bars object.
        !! @return Returns true if the x error bars are to be plotted; else,
        !!  false.
        procedure, public :: get_plot_x_error_bars => pde_get_plot_x_err
        !> @brief Tests to see if the y error bar data has been defined, and as
        !!  a result, if the y error data is to be plotted.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function get_plot_y_error_bars(class(plot_data_error_bars) this)
        !! @endcode
        !!
        !! @param[in] this The plot_data_error_bars object.
        !! @return Returns true if the y error bars are to be plotted; else,
        !!  false.
        procedure, public :: get_plot_y_error_bars => pde_get_plot_y_err
        !> @brief Gets the number of stored data points.
        !!
        !! @param[in] this The plot_data_error_bars object.
        !! @return The number of data points.
        procedure, public :: get_count => pde_get_count
        !> @brief Tests to see if the x and y error boxes should be utilized.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function get_use_error_box(class(plot_data_error_bars) this)
        !! @endcode
        !!
        !! @param[in] this The plot_data_error_bars object.
        !! @return Returns true if the error boxes are to be plotted; else,
        !!  false.
        !!
        !! @par Remarks
        !! Notice, the error boxes are only utilized if there is both x and y
        !! error data defined, regardless of the value of this property.
        procedure, public :: get_use_error_box => pde_get_box
        !> @brief Deterimines if the x and y error boxes should be utilized.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_use_error_box(class(plot_data_error_bars) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The plot_data_error_bars object.
        !! @param[in] x Set to true if the error boxes are to be plotted; else,
        !!  false.
        !!
        !! @par Remarks
        !! Notice, the error boxes are only utilized if there is both x and y
        !! error data defined, regardless of the value of this property.
        procedure, public :: set_use_error_box => pde_set_box
        !> @brief Gets a value determining if a defined range is being used
        !! to define the error bar extremes.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function get_use_range(class(plot_data_error_bars) this)
        !! @endcode
        !!
        !! @param[in] this The plot_data_error_bars object.
        !! @return True if a defined range is being used; else, false.
        procedure, public :: get_use_range => pde_get_use_range

        procedure :: pde_define_x_err
        procedure :: pde_define_y_err
        procedure :: pde_define_xy_err
        procedure :: pde_define_x_err_lim
        procedure :: pde_define_y_err_lim
        procedure :: pde_define_xy_err_lim
    end type

! ------------------------------------------------------------------------------
    interface
        module function pde_get_cmd(this) result(cmd)
            class(plot_data_error_bars), intent(in) :: this
            character(len = :), allocatable :: cmd
        end function

        module function pde_get_data_cmd(this) result(cmd)
            class(plot_data_error_bars), intent(in) :: this
            character(len = :), allocatable :: cmd
        end function

        module subroutine pde_define_x_err(this, x, y, xerr, err)
            class(plot_data_error_bars), intent(inout) :: this
            real(real64), intent(in), dimension(:) :: x, y, xerr
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine pde_define_y_err(this, x, y, yerr, err)
            class(plot_data_error_bars), intent(inout) :: this
            real(real64), intent(in), dimension(:) :: x, y, yerr
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine pde_define_xy_err(this, x, y, xerr, yerr, err)
            class(plot_data_error_bars), intent(inout) :: this
            real(real64), intent(in), dimension(:) :: x, y, xerr, yerr
            class(errors), intent(inout), optional, target :: err
        end subroutine

        pure module function pde_get_plot_x_err(this) result(x)
            class(plot_data_error_bars), intent(in) :: this
            logical :: x
        end function

        pure module function pde_get_plot_y_err(this) result(x)
            class(plot_data_error_bars), intent(in) :: this
            logical :: x
        end function

        pure module function pde_get_count(this) result(x)
            class(plot_data_error_bars), intent(in) :: this
            integer(int32) :: x
        end function

        pure module function pde_get_box(this) result(x)
            class(plot_data_error_bars), intent(in) :: this
            logical :: x
        end function

        module subroutine pde_set_box(this, x)
            class(plot_data_error_bars), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        pure module function pde_get_use_range(this) result(x)
            class(plot_data_error_bars), intent(in) :: this
            logical :: x
        end function

        module subroutine pde_define_x_err_lim(this, x, y, xmin, xmax, err)
            class(plot_data_error_bars), intent(inout) :: this
            real(real64), intent(in), dimension(:) :: x, y, xmin, xmax
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine pde_define_y_err_lim(this, x, y, ymin, ymax, err)
            class(plot_data_error_bars), intent(inout) :: this
            real(real64), intent(in), dimension(:) :: x, y, ymin, ymax
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine pde_define_xy_err_lim(this, x, y, xmin, xmax, ymin, &
                ymax, err)
            class(plot_data_error_bars), intent(inout) :: this
            real(real64), intent(in), dimension(:) :: x, y, xmin, xmax, &
                ymin, ymax
            class(errors), intent(inout), optional, target :: err
        end subroutine
    end interface

! ******************************************************************************
! FPLOT_PLOT_DATA_BAR.F90
! ------------------------------------------------------------------------------
    !> @brief Defines a data set tailored to bar charts.
    type, extends(plot_data_colored) :: plot_data_bar
    private
        !> @brief An array containing axis labels to associate with each bar.
        type(varying_string), allocatable, dimension(:) :: m_axisLabels
        !> @brief An array of data defining each bar - the matrix contains
        !!  multiple columns to allow multiple bars per label.
        real(real64), allocatable, dimension(:,:) :: m_barData
        !> @brief Determines if the axis labels should be used - only applicable
        !! if there is existing data stored in m_axisLabels & m_axisLabels
        !! is the same size as m_barData.
        logical :: m_useAxisLabels = .true.
        !> Draw against the secondary y axis?
        logical :: m_useY2 = .false.
        !> @brief Determines if each bar is filled.
        logical :: m_filled = .true.
        !> @brief The alpha value (transparency) for each bar
        real(real32) :: m_alpha = 1.0
    contains
        procedure, public :: get_count => pdb_get_count
        procedure, public :: get => pdb_get_data
        procedure, public :: set => pdb_set_data
        procedure, public :: get_data => pdb_get_data_set
        procedure, public :: get_label => pdb_get_label
        procedure, public :: set_label => pdb_set_label
        procedure, public :: get_use_labels => pdb_get_use_labels
        procedure, public :: set_use_labels => pdb_set_use_labels
        procedure, public :: get_command_string => pdb_get_cmd
        procedure, public :: get_data_string => pdb_get_data_cmd
        procedure, public :: get_axes_string => pdb_get_axes_cmd
        procedure, public :: get_bar_per_label_count => pdb_get_col_count
        procedure, public :: get_draw_against_y2 => pdb_get_use_y2
        procedure, public :: set_draw_against_y2 => pdb_set_use_y2
        procedure, public :: get_is_filled => pdb_get_is_filled
        procedure, public :: set_is_filled => pdb_set_is_filled
        procedure, public :: get_transparency => pdb_get_alpha
        procedure, public :: set_transparency => pdb_set_alpha
        generic, public :: define_data => pdb_set_data_1, pdb_set_data_2, &
            pdb_set_data_3
        procedure :: pdb_set_data_1
        procedure :: pdb_set_data_2
        procedure :: pdb_set_data_3
        procedure :: set_data_1 => pdb_set_data_1_core
        procedure :: set_data_2 => pdb_set_data_2_core
        procedure :: set_data_3 => pdb_set_data_3_core
    end type

! ------------------------------------------------------------------------------
    interface
        pure module function pdb_get_count(this) result(x)
            class(plot_data_bar), intent(in) :: this
            integer(int32) :: x
        end function

        pure module function pdb_get_data(this, index, col) result(x)
            class(plot_data_bar), intent(in) :: this
            integer(int32), intent(in) :: index, col
            real(real64) :: x
        end function

        module subroutine pdb_set_data(this, index, col, x)
            class(plot_data_bar), intent(inout) :: this
            integer(int32), intent(in) :: index, col
            real(real64), intent(in) :: x
        end subroutine

        pure module function pdb_get_data_set(this, col) result(x)
            class(plot_data_bar), intent(in) :: this
            integer(int32), intent(in) :: col
            real(real64), allocatable, dimension(:) :: x
        end function

        pure module function pdb_get_label(this, index) result(x)
            class(plot_data_bar), intent(in) :: this
            integer(int32), intent(in) :: index
            character(len = :), allocatable :: x
        end function

        module subroutine pdb_set_label(this, index, txt)
            class(plot_data_bar), intent(inout) :: this
            integer(int32) :: index
            character(len = *), intent(in) :: txt
        end subroutine

        pure module function pdb_get_use_labels(this) result(x)
            class(plot_data_bar), intent(in) :: this
            logical :: x
        end function

        module subroutine pdb_set_use_labels(this, x)
            class(plot_data_bar), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        module function pdb_get_cmd(this) result(x)
            class(plot_data_bar), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        module function pdb_get_data_cmd(this) result(x)
            class(plot_data_bar), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        module function pdb_get_axes_cmd(this) result(x)
            class(plot_data_bar), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        pure module function pdb_get_col_count(this) result(x)
            class(plot_data_bar), intent(in) :: this
            integer(int32) :: x
        end function

        pure module function pdb_get_use_y2(this) result(x)
            class(plot_data_bar), intent(in) :: this
            logical :: x
        end function

        module subroutine pdb_set_use_y2(this, x)
            class(plot_data_bar), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        pure module function pdb_get_is_filled(this) result(x)
            class(plot_data_bar), intent(in) :: this
            logical :: x
        end function

        module subroutine pdb_set_is_filled(this, x)
            class(plot_data_bar), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        module subroutine pdb_set_data_1(this, x, err)
            class(plot_data_bar), intent(inout) :: this
            real(real64), intent(in), dimension(:) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine pdb_set_data_2(this, labels, x, err)
            class(plot_data_bar), intent(inout) :: this
            class(varying_string), intent(in), dimension(:) :: labels
            real(real64), intent(in), dimension(:) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine pdb_set_data_3(this, labels, x, fmt, err)
            class(plot_data_bar), intent(inout) :: this
            real(real64), intent(in), dimension(:) :: labels
            real(real64), intent(in), dimension(:) :: x
            character(len = *), intent(in), optional :: fmt
            class(errors), intent(inout), optional, target :: err
        end subroutine

        pure module function pdb_get_alpha(this) result(x)
            class(plot_data_bar), intent(in) :: this
            real(real32) :: x
        end function

        module subroutine pdb_set_alpha(this, x)
            class(plot_data_bar), intent(inout) :: this
            real(real32), intent(in) :: x
        end subroutine

        module subroutine pdb_set_data_1_core(this, x, err)
            class(plot_data_bar), intent(inout) :: this
            real(real64), intent(in), dimension(:) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine pdb_set_data_2_core(this, labels, x, err)
            class(plot_data_bar), intent(inout) :: this
            class(varying_string), intent(in), dimension(:) :: labels
            real(real64), intent(in), dimension(:) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine pdb_set_data_3_core(this, labels, x, fmt, err)
            class(plot_data_bar), intent(inout) :: this
            real(real64), intent(in), dimension(:) :: labels
            real(real64), intent(in), dimension(:) :: x
            character(len = *), intent(in), optional :: fmt
            class(errors), intent(inout), optional, target :: err
        end subroutine
    end interface

! ******************************************************************************
! FPLOT_PLOT_DATA_HISTOGRAM.F90
! ------------------------------------------------------------------------------
    !> @brief A container for plotting data in the form of a histogram.
    type, extends(plot_data_bar) :: plot_data_histogram
    private
        !> @brief The number of bins.
        integer(int32) :: m_binCount = 10
        !> @brief The numerical label format string.
        character(len = :), allocatable :: m_numberFmt
    contains
        procedure, public :: get_bin_count => pdh_get_bin_count
        procedure, public :: set_bin_count => pdh_set_bin_count
        procedure, public :: bin_data => pdh_bin_data
        procedure, public :: get_extreme_values => pdh_get_extremes
        procedure, public :: get_number_format => pdh_get_num_fmt
        procedure, public :: set_number_format => pdh_set_num_fmt
        procedure :: set_data_1 => pdh_set_data_1
        procedure :: set_data_2 => pdh_set_data_2
        procedure :: set_data_3 => pdh_set_data_3
    end type

! ------------------------------------------------------------------------------
    interface
        pure module function pdh_get_bin_count(this) result(x)
            class(plot_data_histogram), intent(in) :: this
            integer(int32) :: x
        end function

        module subroutine pdh_set_bin_count(this, x)
            class(plot_data_histogram), intent(inout) :: this
            integer(int32), intent(in) :: x
        end subroutine

        module function pdh_bin_data(this, x, err) result(bx)
            class(plot_data_histogram), intent(in) :: this
            real(real64), intent(in), dimension(:) :: x
            class(errors), intent(inout), optional, target :: err
            real(real64), allocatable, dimension(:,:) :: bx
        end function

        pure module function pdh_get_extremes(this) result(x)
            class(plot_data_histogram), intent(in) :: this
            real(real64), dimension(2) :: x
        end function

        module subroutine pdh_set_data_1(this, x, err)
            class(plot_data_histogram), intent(inout) :: this
            real(real64), intent(in), dimension(:) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine pdh_set_data_2(this, labels, x, err)
            class(plot_data_histogram), intent(inout) :: this
            class(varying_string), intent(in), dimension(:) :: labels
            real(real64), intent(in), dimension(:) :: x
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine pdh_set_data_3(this, labels, x, fmt, err)
            class(plot_data_histogram), intent(inout) :: this
            real(real64), intent(in), dimension(:) :: labels
            real(real64), intent(in), dimension(:) :: x
            character(len = *), intent(in), optional :: fmt
            class(errors), intent(inout), optional, target :: err
        end subroutine

        pure module function pdh_get_num_fmt(this) result(x)
            class(plot_data_histogram), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        module subroutine pdh_set_num_fmt(this, x)
            class(plot_data_histogram), intent(inout) :: this
            character(len = *), intent(in) :: x
        end subroutine
    end interface

! ******************************************************************************
! FPLOT_PLOT_BAR.F90
! ------------------------------------------------------------------------------
    !> @brief Defines a 2D plot tailored towards bar plotting.
    type, extends(plot_2d) :: plot_bar
    private
        !> @brief A relative scaling of the width of a single bar.  The value
        !! must be between 0 and 1 with 1 being full width.
        real(real32) :: m_barWidth = 0.75d0
    contains
        procedure, public :: get_bar_width => pb_get_bar_width
        procedure, public :: set_bar_width => pb_set_bar_width
        procedure, public :: get_command_string => pb_get_cmd
    end type

! ------------------------------------------------------------------------------
    interface
        pure module function pb_get_bar_width(this) result(x)
            class(plot_bar), intent(in) :: this
            real(real32) :: x
        end function

        module subroutine pb_set_bar_width(this, x)
            class(plot_bar), intent(inout) :: this
            real(real32), intent(in) :: x
        end subroutine

        module function pb_get_cmd(this) result(x)
            class(plot_bar), intent(in) :: this
            character(len = :), allocatable :: x
        end function
    end interface


! ******************************************************************************
! FPLOT_TRIANGULATIONS_DELAUNAY_2D.F90
! ------------------------------------------------------------------------------
    !> @brief Provides a container for a 2D Delaunay triangulation.
    !!
    !! @par Remarks
    !! This type utilizes the GEOMPACK triangulation code available at 
    !! https://people.sc.fsu.edu/~jburkardt/f77_src/geompack/geompack.html.
    type delaunay_tri_2d
    private
        !> @brief An array of the x-coordinates of each point.
        real(real64), allocatable, dimension(:) :: m_x
        !> @brief An array of the y-coordinates of each point.
        real(real64), allocatable, dimension(:) :: m_y
        !> @brief A 3-column matrix containing the indices of each triangle's
        !! vertex.
        integer(int32), allocatable, dimension(:,:) :: m_indices
    contains
        !> @brief Creates an unconstrained 2D Delaunay triangulation given a 
        !! set of x-y points.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine create(class(delaunay_tri_2d) this, real(real64) x(:), real(real64) y(:), class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The delaunay_tri_2d object.
        !! @param[in] x An N-element array containing the x-coordinates of each
        !!  data point.
        !! @param[in] y An N-element array containing the y-coordinates of each
        !!  data point. 
        !! @param[in,out] err An optional errors-based object that if provided 
        !!  can be used to retrieve information relating to any errors 
        !!  encountered during execution.  If not provided, a default 
        !!  implementation of the errors class is used internally to provide 
        !!  error handling.  Possible errors and warning messages that may be 
        !!  encountered are as follows.
        !!  - PLOT_ARRAY_SIZE_MISMATCH_ERROR: Occurs if the input arrays are not
        !!      the same size.
        !!  - PLOT_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory
        !!      available.
        procedure, public :: create => d2d_init
        !> @brief Gets the number of points in the triangulation.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! integer(int32) function get_point_count(class(delaunay_tri_2d) this)
        !! @endcode
        !!
        !! @param[in] this The delaunay_tri_2d object.
        !! @return The number of points in the triangulation.
        procedure, public :: get_point_count => d2d_get_pt_count
        !> @brief Gets the number of triangles in the triangulation.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! integer(int32) function get_triangle_count(class(delaunay_tri_2d) this)
        !! @endcode
        !!
        !! @param[in] this The delaunay_tri_2d object.
        !! @return The number of triangles in the triangulation.
        procedure, public :: get_triangle_count => d2d_get_tri_count
        !> @brief Gets the x-coordinates of each point.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! real(real64)(:) function get_points_x(class(delaunay_tri_2d) this)
        !! @endcode
        !!
        !! @param[in] this The delaunay_tri_2d object.
        !! @return An array of the x-coordinates of each point.
        procedure, public :: get_points_x => d2d_get_x_pts
        !> @brief Gets the y-coordinates of each point.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! real(real64)(:) function get_points_y(class(delaunay_tri_2d) this)
        !! @endcode
        !!
        !! @param[in] this The delaunay_tri_2d object.
        !! @return An array of the y-coordinates of each point.
        procedure, public :: get_points_y => d2d_get_y_pts
        !> @brief Gets a list of the indices of each triangle vertex.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! integer(int32)(:,:) function get_indices(class(delaunay_tri_2d) this)
        !! @endcode
        !!
        !! @param[in] this The delaunay_tri_2d object.
        !! @return An N-by-3 matrix with each column containing the index of the
        !!  vertex of each triangle where N is the number of triangles.
        procedure, public :: get_indices => d2d_get_tris
        !> @brief Finds the triangle that contains the specified point.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! integer(int32) function find_triangle(class(delaunay_tri_2d) this, real(real64) x, real(real64) y)
        !! @endcode
        !!
        !! @param[in] this The delaunay_tri_2d object.
        !! @param[in] x The x-coordinate of the point.
        !! @param[in] y The y-coordinate of the point.
        !!
        !! @return Returns the index of the triangle containing the specified
        !!  point.  If no triangle contains the specified point, a value of
        !!  -1 is returned.
        procedure, public :: find_triangle => d2d_get_tri_with_pt
    end type

! ----------
    interface
        module subroutine d2d_init(this, x, y, err)
            class(delaunay_tri_2d), intent(inout) :: this
            real(real64), intent(in), dimension(:) :: x, y
            class(errors), intent(inout), target, optional :: err
        end subroutine

        pure module function d2d_get_pt_count(this) result(rst)
            class(delaunay_tri_2d), intent(in) :: this
            integer(int32) :: rst
        end function

        pure module function d2d_get_tri_count(this) result(rst)
            class(delaunay_tri_2d), intent(in) :: this
            integer(int32) :: rst
        end function

        pure module function d2d_get_x_pts(this) result(rst)
            class(delaunay_tri_2d), intent(in) :: this
            real(real64), allocatable, dimension(:) :: rst
        end function

        pure module function d2d_get_y_pts(this) result(rst)
            class(delaunay_tri_2d), intent(in) :: this
            real(real64), allocatable, dimension(:) :: rst
        end function

        pure module function d2d_get_tris(this) result(rst)
            class(delaunay_tri_2d), intent(in) :: this
            integer(int32), allocatable, dimension(:,:) :: rst
        end function

        pure module function d2d_get_tri_with_pt(this, x, y) result(rst)
            class(delaunay_tri_2d), intent(in) :: this
            real(real64), intent(in) :: x, y
            integer(int32) :: rst
        end function
    end interface

! ******************************************************************************
! FPLOT_PLOT_DATA_TRI_2D.F90
! ------------------------------------------------------------------------------
    !> @brief Defines a 2D triangulated data set.
    type, extends(plot_data_colored) :: plot_data_tri_2d
    private
        !> @brief An array of the x-coordinates of each point.
        real(real64), allocatable, dimension(:) :: m_x
        !> @brief An array of the y-coordinates of each point.
        real(real64), allocatable, dimension(:) :: m_y
        !> @brief A 3-column matrix containing the indices of each triangle's
        !! vertex.
        integer(int32), allocatable, dimension(:,:) :: m_indices
        !> @brief The line width.
        real(real32) :: m_lineWidth = 1.0
        !> @brief The line style
        integer(int32) :: m_lineStyle = LINE_SOLID
    contains
        procedure, public :: get_data_string => pdt2d_get_data_cmd
        procedure, public :: get_command_string => pdt2d_get_cmd
        !> @brief Defines the data to plot.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine(class(plot_data_tri_2d) this, class(delaunay_tri_2d) tri)
        !! @endcode
        !!
        !! @param[in,out] this The plot_data_tri_2d object.
        !! @param[in] tri The triangulation data to plot.
        procedure, public :: define_data => pdt2d_define_data
        !> @brief Gets the width of the lines used to draw the triangulation.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! real(real32) function get_line_width(class(plot_data_tri_2d) this)
        !! @endcode
        !!
        !! @param[in] this The plot_data_tri_2d object.
        !! @return The line width.
        procedure, public :: get_line_width => pdt2d_get_line_width
        !> @brief Sets the width of the lines used to draw the triangulation.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_line_width(class(plot_data_tri_2d) this, real(real32) x)
        !! @endcode
        !!
        !! @param[in,out] this The plot_data_tri_2d object.
        !! @param[in] x The line width.
        procedure, public :: set_line_width => pdt2d_set_line_width
        !> @brief
        !!
        !! @par Syntax
        !! @code{.f90}
        !! integer(int32) function get_line_style(class(plot_data_tri_2d) this)
        !! @endcode
        !!
        !! @param[in] this The plot_data_tri_2d object.
        !! @return The line sytle flag.
        procedure, public :: get_line_style => pdt2d_get_line_style
        !> @brief
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_line_style(class(plot_data_tri_2d) this, integer(int32) x)
        !! @endcode
        !!
        !! @param[in,out] this The plot_data_tri_2d object.
        !! @param[in] x The line style.  The line style must be one of the
        !!      following:
        !!  - LINE_DASHED
        !!  - LINE_DASH_DOTTED
        !!  - LINE_DASH_DOT_DOT
        !!  - LINE_DOTTED
        !!  - LINE_SOLID
        procedure, public :: set_line_style => pdt2d_set_line_style
    end type

! --------------------
    interface
        module function pdt2d_get_data_cmd(this) result(x)
            class(plot_data_tri_2d), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        module function pdt2d_get_cmd(this) result(x)
            class(plot_data_tri_2d), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        module subroutine pdt2d_define_data(this, tri)
            class(plot_data_tri_2d), intent(inout) :: this
            class(delaunay_tri_2d), intent(in) :: tri
        end subroutine

        module function pdt2d_get_axes_cmd(this) result(x)
            class(plot_data_tri_2d), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        pure module function pdt2d_get_line_width(this) result(rst)
            class(plot_data_tri_2d), intent(in) :: this
            real(real32) :: rst
        end function

        module subroutine pdt2d_set_line_width(this, x)
            class(plot_data_tri_2d), intent(inout) :: this
            real(real32), intent(in) :: x
        end subroutine

        pure module function pdt2d_get_line_style(this) result(rst)
            class(plot_data_tri_2d), intent(in) :: this
            integer(int32) :: rst
        end function

        module subroutine pdt2d_set_line_style(this, x)
            class(plot_data_tri_2d), intent(inout) :: this
            integer(int32), intent(in) :: x
        end subroutine
    end interface

! ******************************************************************************
! FPLOT_DELAUNAY_TRI_SURFACE.F90
! ------------------------------------------------------------------------------
    !> @brief Provides a type describing a triangulated surface.
    type, extends(delaunay_tri_2d) :: delaunay_tri_surface
    private
        !> @brief An array of the z-coordinates of each point.
        real(real64), allocatable, dimension(:) :: m_z
    contains
        !> @brief Defines the function values that correspond to the x and y
        !! data points.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine define_function_values(class(delaunay_tri_surface) this, real(real64) z(:), class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The delaunay_tri_surface object.
        !! @param[in] z An N-element array containing the function values for
        !!  each x and y coordinate.  Notice, the x and y coordinates must 
        !!  already be defined prior to calling this routine.
        !! @param[in,out] err An optional errors-based object that if provided 
        !!  can be used to retrieve information relating to any errors 
        !!  encountered during execution.  If not provided, a default 
        !!  implementation of the errors class is used internally to provide 
        !!  error handling.  Possible errors and warning messages that may be 
        !!  encountered are as follows.
        !!  - PLOT_ARRAY_SIZE_MISMATCH_ERROR: Occurs if @p z is not the same
        !!      size as the number of x-y data points.
        !!  - PLOT_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory
        !!      available.
        !!  - PLOT_INVALID_OPERATION_ERROR: Occurs if the x-y point data has
        !!      not been defined.
        procedure, public :: define_function_values => dts_define_fcn
        !> @brief Gets the z-coordinates of each point.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! real(real64)(:) function get_points_x(class(delaunay_tri_surface) this)
        !! @endcode
        !!
        !! @param[in] this The delaunay_tri_2d object.
        !! @return An array of the z-coordinates of each point.
        procedure, public :: get_points_z => dts_get_z
        !> @brief Evaluates the function at the requested point by means of 
        !!  linear interpolation.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! real(real64) function evaluate(class(delaunay_tri_surface) this, real(real64) x, real(real64) y)
        !! @endcode
        !!
        !! @param[in] this The delaunay_tri_surface object.
        !! @param[in] x The x-coordinate at which to evaluate the function.
        !! @param[in] y The y-coordinate at which to evaluate the function.
        !!
        !! @return The function value.  If the point (@p x, @p y) does not lie
        !!  within the range of defined values, then a value of NaN is returned.
        generic, public :: evaluate => dts_interp_1, dts_interp_2

        procedure :: dts_interp_1
        procedure :: dts_interp_2
    end type

! --------------------
    interface
        module subroutine dts_define_fcn(this, z, err)
            class(delaunay_tri_surface), intent(inout) :: this
            real(real64), intent(in), dimension(:) :: z
            class(errors), intent(inout), optional, target :: err
        end subroutine

        pure module function dts_get_z(this) result(rst)
            class(delaunay_tri_surface), intent(in) :: this
            real(real64), allocatable, dimension(:) :: rst
        end function

        pure module function dts_interp_1(this, x, y) result(z)
            class(delaunay_tri_surface), intent(in) :: this
            real(real64), intent(in) :: x, y
            real(real64) :: z
        end function

        pure module function dts_interp_2(this, x, y) result(z)
            class(delaunay_tri_surface), intent(in) :: this
            real(real64), intent(in), dimension(:) :: x, y
            real(real64), allocatable, dimension(:) :: z
        end function
    end interface

! ******************************************************************************
! FPLOT_TRI_SURFACE_PLOT_DATA.F90
! ------------------------------------------------------------------------------
    !> @brief Provides a three-dimensional surface plot data set constructed of
    !! triangulated points.
    type, extends(plot_data) :: tri_surface_plot_data
    private
        !> @brief An array of the x-coordinates of each point.
        real(real64), allocatable, dimension(:) :: m_x
        !> @brief An array of the y-coordinates of each point.
        real(real64), allocatable, dimension(:) :: m_y
        !> @brief An array of the z-coordinates of each point.
        real(real64), allocatable, dimension(:) :: m_z
        !> @brief A 3-column matrix containing the indices of each triangle's
        !! vertex.
        integer(int32), allocatable, dimension(:,:) :: m_indices
        !> @brief Determines if the surface should be drawn as a wireframe.
        logical :: m_wireframe = .true.
    contains
        procedure, public :: get_data_string => tspd_get_data_cmd
        procedure, public :: get_command_string => tspd_get_cmd
        !> @brief Gets a value determining if a wireframe mesh should be 
        !!  displayed.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! logical function get_use_wireframe(class(tri_surface_plot_data) this)
        !! @endcode
        !!
        !! @param[in] this The tri_surface_plot_data object.
        !! @return Returns true if the plot is to be drawn as a wireframe; else,
        !!  false to draw as a surface.
        procedure, public :: get_use_wireframe => tspd_get_wireframe
        !> @brief Sets a value determining if a wireframe mesh should be 
        !!  displayed.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_use_wireframe(class(tri_surface_plot_data) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The tri_surface_plot_data object.
        !! @param[in] x Set to true if the plot is to be drawn as a wireframe;
        !!  else, set to false to draw as a surface.
        procedure, public :: set_use_wireframe => tspd_set_wireframe
        !> @brief Defines the data to plot.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine define_data(class(tri_surface_plot_data) this, class(delaunay_tri_surface) tri)
        !! @endcode
        !!
        !! @param[in,out] this The tri_surface_plot_data object.
        !! @param[in] tri The triangulation to plot.
        procedure, public :: define_data => tspd_define_data
    end type

! --------------------
    interface
        module function tspd_get_data_cmd(this) result(x)
            class(tri_surface_plot_data), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        module function tspd_get_cmd(this) result(x)
            class(tri_surface_plot_data), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        pure module function tspd_get_wireframe(this) result(rst)
            class(tri_surface_plot_data), intent(in) :: this
            logical :: rst
        end function

        module subroutine tspd_set_wireframe(this, x)
            class(tri_surface_plot_data), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        module subroutine tspd_define_data(this, tri)
            class(tri_surface_plot_data), intent(inout) :: this
            class(delaunay_tri_surface), intent(in) :: tri
        end subroutine
    end interface

! ******************************************************************************
! FPLOT_VECTOR_FIELD_PLOT_DATA.F90
! ------------------------------------------------------------------------------
    ! REF:
    ! http://www.gnuplotting.org/vector-field-from-data-file/
    ! http://gnuplot.sourceforge.net/demo_5.4/vector.html
    ! http://www.gnuplot.info/docs_5.4/Gnuplot_5_4.pdf (pg 79)

    !> @brief Defines a two-dimensional vector-field plot data set.
    type, extends(plot_data_colored) :: vector_field_plot_data
    private
        !> @brief An M-by-N-by-4 array containing the x, y, dx, and dy plot
        !! data points.  Optionally, a 5th page can be added to define the
        !! color for each arrow.
        real(real64), allocatable, dimension(:,:,:) :: m_data
        !> @brief The vector size (scaling factor).
        real(real64) :: m_arrowSize = 1.0d0
        !> @brief Fill the arrow heads?
        logical :: m_filledHeads = .false.
    contains
        !> @brief Gets the GNUPLOT command string containing the actual data
        !! to plot.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_data_string(class(vector_field_plot_data) this)
        !! @endcode
        !!
        !! @param[in] this The vector_field_plot_data object.
        !! @return The command string.
        procedure, public :: get_data_string => vfpd_get_data_cmd
        !> @brief Gets the GNUPLOT command string to represent this
        !! vector_field_plot_data object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_command_string(class(vector_field_plot_data) this)
        !! @endcode
        !!
        !! @param[in] this The vector_field_plot_data object.
        !! @return The command string.
        procedure, public :: get_command_string => vfpd_get_cmd
        !> @brief Defines the data set.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine define_data(class(vector_field_plot_data) this, real(real64) x(:,:), real(real64) y(:,:), real(real64) dx(:,:), real(real64) dy(:,:), real(real64) c(:,:), class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The vector_field_plot_data object.
        !! @param[in] x An M-by-N matrix containing the x-locations of each arrow's origin.
        !! @param[in] y An M-by-N matrix containing the y-locations of each arrow's origin.
        !! @param[in] dx An M-by-N matrix containing the x-direction of each arrow.
        !! @param[in] dy An M-by-N matrix containing the y-direction of each arrow.
        !! @param[in] c An optional M-by-N matrix containing information on how to color the
        !!  arrows.  The colors are determined by the active colormap.
        !! @param[in,out] err An optional errors-based object that if provided can be
        !!  used to retrieve information relating to any errors encountered during
        !!  execution.  If not provided, a default implementation of the errors
        !!  class is used internally to provide error handling.  Possible errors and
        !!  warning messages that may be encountered are as follows.
        !!  - PLOT_OUT_OF_MEMORY_ERROR: Occurs if insufficient memory is available.
        !!  - PLOT_ARRAY_SIZE_MISMATCH_ERROR: Occurs if the input matrices are
        !!      not the same size.
        procedure, public :: define_data => vfpd_define_data
        !> @brief Gets the scaling factor used to determine the arrow size.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! real(real64) get_arrow_size(class(vector_field_plot_data) this)
        !! @endcode
        !!
        !! @param[in] this The vector_field_plot_data object.
        !! @return The scaling factor.
        procedure, public :: get_arrow_size => vfpd_get_arrow_size
        !> @brief Sets the scaling factor used to determine the arrow size.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_arrow_size(class(vector_field_plot_data) this, real(real64) x)
        !! @endcode
        !!
        !! @param[in,out] this The vector_field_plot_data object.
        !! @param[in] x The scaling factor.
        procedure, public :: set_arrow_size => vfpd_set_arrow_size
        !> @brief Gets a value determining if the arrow heads should be filled.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! logical get_fill_arrow(class(vector_field_plot_data) this)
        !! @endcode
        !!
        !! @param[in] this The vector_field_plot_data object.
        !! @return True if the arrow heads should be filled; else, false.
        procedure, public :: get_fill_arrow => vfpd_get_fill_arrow
        !> @brief Sets a value determining if the arrow heads should be filled.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_fill_arrow(class(vector_field_plot_data) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The vector_field_plot_data object.
        !! @param[in] x True if the arrow heads should be filled; else, false.
        procedure, public :: set_fill_arrow => vfpd_set_fill_arrow
        !> @brief Gets a value indicating if data-dependent coloring should be
        !! used.  This is defined by supplying information on how to scale the
        !! coloring when calling define_data.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! logical get_use_data_dependent_colors(class(vector_field_plot_data) this)
        !! @endcode
        !!
        !! @param[in] this The vector_field_plot_data object.
        !! return Returns true if data-dependent coloring is being used; else,
        !!  false.
        procedure, public :: get_use_data_dependent_colors => &
            vfpd_get_use_data_dependent_colors
    end type

! --------------------
    interface
         module function vfpd_get_data_cmd(this) result(x)
            class(vector_field_plot_data), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        module function vfpd_get_cmd(this) result(x)
            class(vector_field_plot_data), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        module subroutine vfpd_define_data(this, x, y, dx, dy, c, err)
            class(vector_field_plot_data), intent(inout) :: this
            real(real64), intent(in), dimension(:,:) :: x, y, dx, dy
            real(real64), intent(in), dimension(:,:), optional :: c
            class(errors), intent(inout), optional, target :: err
        end subroutine

        pure module function vfpd_get_arrow_size(this) result(rst)
            class(vector_field_plot_data), intent(in) :: this
            real(real64) :: rst
        end function

        module subroutine vfpd_set_arrow_size(this, x)
            class(vector_field_plot_data), intent(inout) :: this
            real(real64), intent(in) :: x
        end subroutine

        pure module function vfpd_get_fill_arrow(this) result(rst)
            class(vector_field_plot_data), intent(in) :: this
            logical :: rst
        end function

        module subroutine vfpd_set_fill_arrow(this, x)
            class(vector_field_plot_data), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        pure module function vfpd_get_use_data_dependent_colors(this) result(rst)
            class(vector_field_plot_data), intent(in) :: this
            logical :: rst
        end function
    end interface

! ******************************************************************************
! FPLOT_PLOT_POLAR.F90
! ------------------------------------------------------------------------------
    !> @brief Defines a 2D polar plot.
    type, extends(plot) :: plot_polar
    private
        !> @brief Allow the plot to autoscale?
        logical :: m_autoscale = .true.
        !> @brief The minimum radius value - only applicable if m_autoscale is
        !!  false.
        real(real64) :: m_minrad = 0.0d0
        !> @brief The maximum radius value - only applicable if m_autoscale is
        !!  false.
        real(real64) :: m_maxrad = 1.0d0
        !> @brief The location for theta = 0
        character(len = :), allocatable :: m_thetaStart 
        !> @brief The direction for theta
        character(len = :), allocatable :: m_thetaDirection
    contains
        final :: plr_clean_up
        !> @brief Initializes the plot_polar object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine initialize(class(plot_polar) this, optional integer(int32) term, optional class(errors) err)
        !! @endcode
        !!
        !! @param[in] this The plot_polar object.
        !! @param[in] term An optional input that is used to define the terminal.
        !!  The default terminal is a WXT terminal.  The acceptable inputs are:
        !!  - GNUPLOT_TERMINAL_PNG
        !!  - GNUPLOT_TERMINAL_QT
        !!  - GNUPLOT_TERMINAL_WIN32
        !!  - GNUPLOT_TERMINAL_WXT
        !!  - GNUPLOT_TERMINAL_LATEX
        !! @param[in] fname A filename to pass to the terminal in the event the
        !!  terminal is a file type (e.g. GNUPLOT_TERMINAL_PNG).
        !! @param[out] err An optional errors-based object that if provided can be
        !!  used to retrieve information relating to any errors encountered during
        !!  execution.  If not provided, a default implementation of the errors
        !!  class is used internally to provide error handling.  Possible errors and
        !!  warning messages that may be encountered are as follows.
        !! - PLOT_OUT_OF_MEMORY_ERROR: Occurs if insufficient memory is available.
        procedure, public :: initialize => plr_init
        !> @brief Gets the GNUPLOT command string to represent this plot_polar
        !! object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_command_string(class(plot_polar) this)
        !! @endcode
        !!
        !! @param[in] this The plot_polar object.
        !! @return The command string.
        procedure, public :: get_command_string => plr_get_cmd
        !> @brief Gets a logical value determining if the axis should be 
        !! automatically scaled to fit the data.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! logical get_autoscale(class(plot_polar) this)
        !! @endcode
        !!
        !! @param[in] this The plot_polar object.
        !! @return Returns true if the plot will autoscale; else, false.
        procedure, public :: get_autoscale => plr_get_autoscale
        !> @brief Sets a logical value determining if the axis should be 
        !! automatically scaled to fit the data.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_autoscale(class(plot_polar) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The plot_polar object.
        !! @param[in] x Set to true if the plot will autoscale; else, false.
        procedure, public :: set_autoscale => plr_set_autoscale
        !> @brief Gets the radial axis limits if autoscaling is inactive.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! real(real64)(2) get_radial_limits(class(plot_polar) this)
        !! @endcode
        !!
        !! @param[in] this The plot_polar object.
        !! @returns A 2-element array containing the minimum and maximum limit
        !!  values in that order.
        procedure, public :: get_radial_limits => plr_get_limits
        !> @brief Sets the radial axis limits if autoscaling is inactive.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_radial_limits(class(plot_polar) this, real(real64) x(2))
        !! @endcode
        !!
        !! @param[in,out] this The plot_polar object.
        !! @param[in] A 2-element array containing the minimum and maximum limit
        !!  values.
        procedure, public :: set_radial_limits => plr_set_limits
        !> @brief Gets the position for theta = 0.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) get_theta_start_position(class(plot_polar) this)
        !! @endcode
        !!
        !! @param[in] this The plot_polar object.
        !! @return The starting position.  It is one of the following flags.
        !!  - POLAR_THETA_BOTTOM
        !!  - POLAR_THETA_TOP
        !!  - POLAR_THETA_RIGHT
        !!  - POLAR_THETA_LEFT
        procedure, public :: get_theta_start_position => plr_get_theta_start
        !> @brief Sets the position for theta = 0.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_theta_start_position(class(plot_polar) this, character(len = *) x)
        !! @endcode
        !!
        !! @param[in,out] this The plot_polar object.
        !! @param[in] x The starting position.  It must be one of the following 
        !!  flags.
        !!  - POLAR_THETA_BOTTOM
        !!  - POLAR_THETA_TOP
        !!  - POLAR_THETA_RIGHT
        !!  - POLAR_THETA_LEFT
        procedure, public :: set_theta_start_position => plr_set_theta_start
        !> @brief Gets the theta direction.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) get_theta_direction(class(plot_polar) this)
        !! @endcode
        !!
        !! @param[in] this The plot_polar object.
        !! @return The direction.  It is one of the following flags.
        !!  - POLAR_THETA_CCW
        !!  - POLAR_THETA_CW
        procedure, public :: get_theta_direction => plr_get_theta_direction
        !> @brief Sets the theta direction.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_theta_direction(class(plot_polar) this, character(len = *) x)
        !! @endcode
        !!
        !! @param[in,out] this The plot_polar object.
        !! @param[in] x The direction.  It must be one of the following flags.
        !!  - POLAR_THETA_CCW
        !!  - POLAR_THETA_CW
        procedure, public :: set_theta_direction => plr_set_theta_direction
    end type

! --------------------
    interface
        module subroutine plr_clean_up(this)
            type(plot_polar), intent(inout) :: this
        end subroutine

        module subroutine plr_init(this, term, fname, err)
            class(plot_polar), intent(inout) :: this
            integer(int32), intent(in), optional :: term
            character(len = *), intent(in), optional :: fname
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module function plr_get_cmd(this) result(x)
            class(plot_polar), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        pure module function plr_get_autoscale(this) result(rst)
            class(plot_polar), intent(in) :: this
            logical :: rst
        end function

        module subroutine plr_set_autoscale(this, x)
            class(plot_polar), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        pure module function plr_get_limits(this) result(rst)
            class(plot_polar), intent(in) :: this
            real(real64) :: rst(2)
        end function

        module subroutine plr_set_limits(this, x)
            class(plot_polar), intent(inout) :: this
            real(real64), intent(in) :: x(2)
        end subroutine

        pure module function plr_get_theta_start(this) result(rst)
            class(plot_polar), intent(in) :: this
            character(len = :), allocatable :: rst
        end function

        module subroutine plr_set_theta_start(this, x)
            class(plot_polar), intent(inout) :: this
            character(len = *), intent(in) :: x
        end subroutine

        pure module function plr_get_theta_direction(this) result(rst)
            class(plot_polar), intent(in) :: this
            character(len = :), allocatable :: rst
        end function

        module subroutine plr_set_theta_direction(this, x)
            class(plot_polar), intent(inout) :: this
            character(len = *), intent(in) :: x
        end subroutine
    end interface

! ******************************************************************************
! FPLOT_FILLED_PLOT_DATA.F90
! ------------------------------------------------------------------------------
    !> @brief Defines a two-dimensional filled plot data set.
    type, extends(plot_data_colored) :: filled_plot_data
    private
        !> Plot against the secondary y-axis
        logical :: m_useY2 = .false.
        !> The data set (column 1 = x, column 2 = y, column 3 = constraint y)
        real(real64), allocatable, dimension(:,:) :: m_data
    contains
        !> @brief Gets the GNUPLOT command string defining which axes the data
        !! is to be plotted against.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_axis_string(class(filled_plot_data) this)
        !! @endcode
        !!
        !! @param[in] this The filled_plot_data object.
        !! @return The command string.
        procedure, public :: get_axes_string => fpd_get_axes_cmd
        !> @brief Gets a value determining if the data should be plotted against
        !! the secondary y-axis.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function get_draw_against_y2(class(filled_plot_data) this)
        !! @endcode
        !!
        !! @param[in] this The filled_plot_data object.
        !! @return Returns true if the data should be plotted against the secondary
        !!  y-axis; else, false to plot against the primary y-axis.
        procedure, public :: get_draw_against_y2 => fpd_get_draw_against_y2
        !> @brief Sets a value determining if the data should be plotted against
        !! the secondary y-axis.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_draw_against_y2(class(filled_plot_data) this, logical x)
        !! @endcode
        !!
        !! @param[in,out] this The filled_plot_data object.
        !! @param[in] x Set to true if the data should be plotted against the
        !!  secondary y-axis; else, false to plot against the primary y-axis.
        procedure, public :: set_draw_against_y2 => fpd_set_draw_against_y2
        !> @brief Gets the GNUPLOT command string to represent this
        !! filled_plot_data object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_command_string(class(filled_plot_data) this)
        !! @endcode
        !!
        !! @param[in] this The filled_plot_data object.
        !! @return The command string.
        procedure, public :: get_command_string => fpd_get_cmd
        !> @brief Gets the GNUPLOT command string containing the actual data
        !! to plot.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! character(len = :) function, allocatable get_data_string(class(filled_plot_data) this)
        !! @endcode
        !!
        !! @param[in] this The filled_plot_data object.
        !! @return The command string.
        procedure, public :: get_data_string => fpd_get_data_cmd
        !> @brief Defines the data set.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine define_data(class(filled_plot_data) this, real(real64) x(:), real(real64) y(:), real(real64) yc(:))
        !! @endcode
        !!
        !! @param[in,out] this The filled_plot_data object.
        !! @param[in] x An N-element array containing the x coordinate data.
        !! @param[in] y An N-element array containing the y coordinate data.
        !! @param[in] yc An N-element array containing the constraining curve y coordinate data.
        !! @param[out] err An optional errors-based object that if provided can be
        !!  used to retrieve information relating to any errors encountered during
        !!  execution.  If not provided, a default implementation of the errors
        !!  class is used internally to provide error handling.  Possible errors and
        !!  warning messages that may be encountered are as follows.
        !!  - PLOT_OUT_OF_MEMORY_ERROR: Occurs if insufficient memory is available.
        !!  - PLOT_ARRAY_SIZE_MISMATCH_ERROR: Occurs if @p x and @p y are not the
        !!      same size.
        procedure, public :: define_data => fpd_define_data
    end type

! --------------------
    interface
        module function fpd_get_axes_cmd(this) result(x)
            class(filled_plot_data), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        pure module function fpd_get_draw_against_y2(this) result(x)
            class(filled_plot_data), intent(in) :: this
            logical :: x
        end function

        module subroutine fpd_set_draw_against_y2(this, x)
            class(filled_plot_data), intent(inout) :: this
            logical, intent(in) :: x
        end subroutine

        module function fpd_get_cmd(this) result(x)
            class(filled_plot_data), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        module function fpd_get_data_cmd(this) result(x)
            class(filled_plot_data), intent(in) :: this
            character(len = :), allocatable :: x
        end function

        module subroutine fpd_define_data(this, x, y, yc, err)
            class(filled_plot_data), intent(inout) :: this
            real(real64), intent(in), dimension(:) :: x, y, yc
            class(errors), intent(inout), optional, target :: err
        end subroutine
    end interface

! ******************************************************************************
! FPLOT_SIMPLIFY.F90
! ------------------------------------------------------------------------------
    !> @brief Simplifies a 2D or 3D polyline by removing points too close to 
    !!  discern given a specified tolerance.
    !!
    !! @par Overload 1
    !! Simplifies a 2D polyline by removing points too close to discern given
    !! a specified tolerance.
    !!
    !! @param[in] x An N-element array containing the x-coordinates of the vertices
    !!  making up the polyline.
    !! @param[in] y An N-element array containing the y-coordinates of the vertices
    !!  making up the polyline.
    !! @param[in] tol The distance tolerance to use when simplifying the polyline.
    !!  This value must be positive, and larger than machine epsilon.
    !! @param[in,out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - PLOT_OUT_OF_MEMORY_ERROR: Occurs if insufficient memory is available.
    !!  - PLOT_ARRAY_SIZE_MISMATCH_ERROR: Occurs if the input array sizes are not
    !!      compatible.
    !!  - PLOT_INVALID_INPUT_ERROR: Occurs if @p tol is not positive and greater
    !!      than machine epsilon.
    !!
    !! @return A matrix containing the simplified polyline vertices.  The first
    !! column of the matrix contains the x-coordinates, and the second column
    !! contains the y-coordinates.
    !!
    !! @par Overload 2
    !! Simplifies a 3D polyline by removing points too close to discern 
    !! given a specified tolerance.
    !!
    !! @param[in] x An N-element array containing the x-coordinates of the vertices
    !!  making up the polyline.
    !! @param[in] y An N-element array containing the y-coordinates of the vertices
    !!  making up the polyline.
    !! @param[in] z An N-element array containing the z-coordinates of the vertices
    !!  making up the polyline.
    !! @param[in] tol The distance tolerance to use when simplifying the polyline.
    !!  This value must be positive, and larger than machine epsilon.
    !! @param[in,out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - PLOT_OUT_OF_MEMORY_ERROR: Occurs if insufficient memory is available.
    !!  - PLOT_ARRAY_SIZE_MISMATCH_ERROR: Occurs if the input array sizes are not
    !!      compatible.
    !!  - PLOT_INVALID_INPUT_ERROR: Occurs if @p tol is not positive and greater
    !!      than machine epsilon.
    !!
    !! @return A matrix containing the simplified polyline vertices.  The first
    !! column of the matrix contains the x-coordinates, the second column
    !! contains the y-coordinates, and the third column contains the z-coordinates.
    !!
    !! @par Overload 3
    !! Simplifies a 2D or 3D polyline by removing points too close to discern 
    !! given a specified tolerance.
    !!
    !! @param[in] xy An N-by-2 or N-by-3 matrix containing the polyline vertex data.
    !! @param[in] tol The distance tolerance to use when simplifying the polyline.
    !!  This value must be positive, and larger than machine epsilon.
    !! @param[in,out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - PLOT_OUT_OF_MEMORY_ERROR: Occurs if insufficient memory is available.
    !!  - PLOT_ARRAY_SIZE_MISMATCH_ERROR: Occurs if the input array sizes are not
    !!      compatible.
    !!  - PLOT_INVALID_INPUT_ERROR: Occurs if @p tol is not positive and greater
    !!      than machine epsilon.
    !!
    !! @return A matrix containing the simplified polyline vertices.  The first
    !! column of the matrix contains the x-coordinates, the second column
    !! contains the y-coordinates, and if necessary, the third column contains
    !! the z-coordinates.
    interface simplify_polyline
        module procedure :: simplify_polyline_2d1
        module procedure :: simplify_polyline_3d1
        module procedure :: simplify_polyline_mtx
    end interface

    interface
        module function simplify_polyline_2d1(x, y, tol, err) result(ln)
            real(real64), intent(in), dimension(:) :: x, y
            real(real64), intent(in) :: tol
            class(errors), intent(inout), optional, target :: err
            real(real64), allocatable, dimension(:,:) :: ln
        end function

        module function simplify_polyline_3d1(x, y, z, tol, err) result(ln)
            real(real64), intent(in), dimension(:) :: x, y, z
            real(real64), intent(in) :: tol
            class(errors), intent(inout), optional, target :: err
            real(real64), allocatable, dimension(:,:) :: ln
        end function

        module function simplify_polyline_mtx(xy, tol, err) result(ln)
            real(real64), intent(in), dimension(:,:) :: xy
            real(real64), intent(in) :: tol
            class(errors), intent(inout), optional, target :: err
            real(real64), allocatable, dimension(:,:) :: ln
        end function
    end interface

! ------------------------------------------------------------------------------
end module
