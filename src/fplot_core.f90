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
    use fplot_plot_data_box_whisker
    use fplot_plot_data_function
    use strings
    use fplot_svg_terminal
    implicit none
    
end module
