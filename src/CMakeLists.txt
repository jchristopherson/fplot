# Define the source files
set(fplot_sources
    fplot_core.f90
    fplot_core_routines.f90
    fplot_errors.f90
    fplot_colors.f90
    fplot_terminal.f90
    fplot_windows_terminal.f90
    fplot_qt_terminal.f90
    fplot_wxt_terminal.f90
    fplot_png_terminal.f90
    fplot_latex_terminal.f90
    fplot_plot_data.f90
    fplot_plot_axis.f90
    fplot_legend.f90
    fplot_plot.f90
    fplot_colormap.f90
    fplot_scatter_plot_data.f90
    fplot_plot_data_2d.f90
    fplot_plot_data_3d.f90
    fplot_surface_plot_data.f90
    fplot_plot_2d.f90
    fplot_plot_3d.f90
    fplot_surface_plot.f90
    fplot_axis.f90
    fplot_label.f90
    fplot_multiplot.f90
    fplot_plot_data_error_bars.f90
    fplot_plot_data_colored.f90
    fplot_simplify.f90
    fplot_plot_data_bar.f90
    fplot_plot_data_histogram.f90
    fplot_plot_bar.f90
    delaunay.c
    fplot_delaunay.f90
    fplot_plot_data_tri_2d.f90
)

# Build the library
add_library(fplot ${fplot_sources})
target_link_libraries(fplot ${FERROR_LIBRARIES} ${FCORE_LIBRARIES})


# ------------------------------------------------------------------------------
# INSTALLATION
# ------------------------------------------------------------------------------
# Define target information
set_property(TARGET fplot PROPERTY VERSION ${FPLOT_VERSION})
set_property(TARGET fplot PROPERTY SOVERSION ${FPLOT_MAJOR_VERSION})
set_property(TARGET fplot PROPERTY INTERFACE_collections_MAJOR_VERSION ${FPLOT_MAJOR_VERSION})
set_property(TARGET fplot APPEND PROPERTY COMPATIBLE_INTERFACE_STRING fplot_MAJOR_VERSION)

# Locate the "include" directories
set(fplot_INCLUDE_DIRS ${PROJECT_SOURCE_DIR}/include)

# Define the installation instructions
install(TARGETS fplot EXPORT fplotTargets
    RUNTIME DESTINATION fplot/bin
    LIBRARY DESTINATION fplot/lib
    ARCHIVE DESTINATION fplot/lib
    CONFIGURATIONS Release
    INCLUDES DESTINATION fplot/include
)
install(
    DIRECTORY ${PROJECT_SOURCE_DIR}/include
    DESTINATION ${CMAKE_INSTALL_PREFIX}/fplot
    COMPONENT Devel
)

# Include the documentation
install(DIRECTORY ${PROJECT_SOURCE_DIR}/doc/html DESTINATION fplot/doc)

# Define the version file
include(CMakePackageConfigHelpers)
write_basic_package_version_file(
    "${CMAKE_BINARY_DIR}/fplotConfigVersion.cmake"
    VERSION ${FPLOT_VERSION}
    COMPATIBILITY AnyNewerVersion
)

export(EXPORT fplotTargets
    FILE "${CMAKE_BINARY_DIR}/fplotTargets.cmake"
)

# Define the configuration file
configure_file(
    "${PROJECT_SOURCE_DIR}/fplotConfig.cmake.in"
    "${CMAKE_BINARY_DIR}/fplotConfig.cmake"
    COPYONLY
)

set(ConfigPackageLocation fplot/lib/cmake/fplot)
install(
    EXPORT fplotTargets
    FILE fplotTargets.cmake
    DESTINATION ${ConfigPackageLocation}
)
install(
    FILES
        "${CMAKE_BINARY_DIR}/fplotConfig.cmake"
        "${CMAKE_BINARY_DIR}/fplotConfigVersion.cmake"
    DESTINATION
        ${ConfigPackageLocation}
    COMPONENT
        Devel
)
