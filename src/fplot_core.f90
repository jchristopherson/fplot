module fplot_core
    !! FPLOT is a Fortran library providing a means of interacting with
    !! [GNUPLOT](http://www.gnuplot.info/) from a Fortran program.  The library
    !! is designed in an object-oriented manner, and as such utilizes language 
    !! features that require a compiler that supports the 2003 standard.  
    !! Additionally, it is expected that Gnuplot is installed on the system 
    !! path.  For full functionallity, a minimum of GNUPLOT v5.2 is expected.
    use fplot_constants
    use fplot_core_routines
    use fplot_colors
    use fplot_plot_object
    use fplot_plot_data
    use fplot_plot_axis
    use fplot_terminal
    use fplot_windows_terminal
    use fplot_qt_terminal
    use fplot_wxt_terminal
    use fplot_png_terminal
    use fplot_latex_terminal
    use fplot_label
    use fplot_arrow
    use fplot_legend
    use fplot_plot_data_2d
    use fplot_plot_data_3d
    use fplot_surface_plot_data
    use fplot_plot_data_error_bars
    use fplot_plot_data_bar
    use fplot_plot_data_histogram
    use fplot_colormap
    use fplot_filled_plot_data
    use fplot_triangulations_delaunay_2d
    use fplot_plot_data_tri_2d
    use fplot_delaunay_tri_surface
    use fplot_tri_surface_plot_data
    use fplot_vector_field_plot_data
    use fplot_plot
    use fplot_plot_2d
    use fplot_plot_3d
    use fplot_surface_plot
    use fplot_multiplot
    use fplot_plot_bar
    use fplot_plot_polar
    use fplot_stats_plots
    implicit none
    private
    
    ! FPLOT_CONSTANTS.F90
    public :: GNUPLOT_TERMINAL_WIN32
    public :: GNUPLOT_TERMINAL_WXT
    public :: GNUPLOT_TERMINAL_QT
    public :: GNUPLOT_TERMINAL_PNG
    public :: GNUPLOT_TERMINAL_LATEX
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

    ! FPLOT_CORE_ROUTINES.F90
    public :: linspace
    public :: logspace
    public :: meshgrid

    ! FPLOT_COLORS.F90
    public :: color
    public :: operator(==)
    public :: operator(/=)
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
    public :: color_list

    ! FPLOT_PLOT_OBJECT.F90
    public :: plot_object
    public :: get_string_result

    ! FPLOT_PLOT_DATA.F90
    public :: plot_data
    public :: pd_get_string_result
    public :: plot_data_colored
    public :: scatter_plot_data
    public :: spd_get_int_value
    public :: spd_get_string_result
    public :: spd_get_value
    public :: spd_set_value

    ! FPLOT_PLOT_AXIS.F90
    public :: plot_axis
    public :: pa_get_string_result
    public :: x_axis
    public :: y_axis
    public :: y2_axis
    public :: z_axis

    ! FPLOT_TERMINAL.F90
    public :: terminal
    public :: term_get_string_result

    ! FPLOT_WINDOWS_TERMINAL.F90
    public :: windows_terminal
    
    ! FPLOT_QT_TERMINAL.F90
    public :: qt_terminal
    
    ! FPLOT_WXT_TERMINAL.F90
    public :: wxt_terminal

    ! FPLOT_PNG_TERMINAL.F90
    public :: png_terminal

    ! FPLOT_LATEX_TERMINAL.F90
    public :: latex_terminal

    ! FPLOT_LABEL.F90
    public :: plot_label

    ! FPLOT_ARROW.F90
    public :: plot_arrow

    ! FPLOT_LEGEND.F90
    public :: legend

    ! FPLOT_PLOT_DATA_2D.F90
    public :: plot_data_2d
    
    ! FPLOT_PLOT_DATA_3D.F90
    public :: plot_data_3d
    
    ! FPLOT_SURFACE_PLOT_DATA.F90
    public :: surface_plot_data

    ! FPLOT_PLOT_DATA_ERROR_BARS.F90
    public :: plot_data_error_bars
    
    ! FPLOT_PLOT_DATA_BAR.F90
    public :: plot_data_bar

    ! FPLOT_PLOT_DATA_HISTOGRAM.F90
    public :: plot_data_histogram

    ! FPLOT_COLORMAP.F90
    public :: cmap
    public :: colormap
    public :: cm_get_string_result
    public :: rainbow_colormap
    public :: hot_colormap
    public :: cool_colormap
    public :: parula_colormap
    public :: grey_colormap
    public :: earth_colormap
    public :: custom_colormap

    ! FPLOT_FILLED_PLOT_DATA.F90
    public :: filled_plot_data

    ! FPLOT_TRIANGULATIONS_DELAUNAY_2D.F90
    public :: delaunay_tri_2d

    ! FPLOT_PLOT_DATA_TRI_2D.F90
    public :: plot_data_tri_2d

    ! FPLOT_DELAUNAY_TRI_SURFACE.F90
    public :: delaunay_tri_surface
    
    ! FPLOT_TRI_SURFACE_PLOT_DATA.F90
    public :: tri_surface_plot_data

    ! FPLOT_VECTOR_FIELD_PLOT_DATA.F90
    public :: vector_field_plot_data

    ! FPLOT_PLOT.F90
    public :: plot

    ! FPLOT_PLOT_2D.F90
    public :: plot_2d

    ! FPLOT_PLOT_3D.F90
    public :: plot_3d

    ! FPLOT_SURFACE_PLOT.F90
    public :: surface_plot

    ! FPLOT_MULTIPLOT.F90
    public :: multiplot
    
    ! FPLOT_PLOT_BAR.F90
    public :: plot_bar
    
    ! FPLOT_PLOT_POLAR.F90    
    public :: plot_polar

    ! FPLOT_STATS_PLOTS.F90
    public :: correlation_plot
end module
