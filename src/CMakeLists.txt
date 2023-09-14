# Locate the source directory
set(dir ${CMAKE_CURRENT_SOURCE_DIR})

# Define the source files
set(FPLOT_SOURCES
    ${dir}/fplot_core.f90
    ${dir}/fplot_core_routines.f90
    ${dir}/fplot_colors.f90
    ${dir}/fplot_terminal.f90
    ${dir}/fplot_windows_terminal.f90
    ${dir}/fplot_qt_terminal.f90
    ${dir}/fplot_wxt_terminal.f90
    ${dir}/fplot_png_terminal.f90
    ${dir}/fplot_latex_terminal.f90
    ${dir}/fplot_plot_data.f90
    ${dir}/fplot_plot_axis.f90
    ${dir}/fplot_legend.f90
    ${dir}/fplot_plot.f90
    ${dir}/fplot_colormap.f90
    ${dir}/fplot_scatter_plot_data.f90
    ${dir}/fplot_simplify.f90
    ${dir}/fplot_plot_data_2d.f90
    ${dir}/fplot_plot_data_3d.f90
    ${dir}/fplot_surface_plot_data.f90
    ${dir}/fplot_plot_2d.f90
    ${dir}/fplot_plot_3d.f90
    ${dir}/fplot_surface_plot.f90
    ${dir}/fplot_axis.f90
    ${dir}/fplot_label.f90
    ${dir}/fplot_multiplot.f90
    ${dir}/fplot_plot_data_error_bars.f90
    ${dir}/fplot_plot_data_colored.f90
    ${dir}/fplot_plot_data_bar.f90
    ${dir}/fplot_plot_data_histogram.f90
    ${dir}/fplot_plot_bar.f90
    ${dir}/fplot_plot_data_tri_2d.f90
    ${dir}/fplot_delaunay_tri_surface.f90
    ${dir}/fplot_tri_surface_plot_data.f90
    ${dir}/fplot_vector_field_plot_data.f90
    ${dir}/fplot_plot_polar.f90
    ${dir}/fplot_filled_plot_data.f90
    # ${dir}/tripack.f
    ${dir}/fplot_string_builder.f90
    ${dir}/fplot_string_builder_implementation.f90
    ${dir}/fplot_triangulations_delaunay_2d.f90
)
set(FPLOT_SOURCES ${FPLOT_SOURCES} PARENT_SCOPE)
