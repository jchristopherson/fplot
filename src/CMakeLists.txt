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
    ${dir}/fplot_simplify.f90
    ${dir}/fplot_plot_2d.f90
    ${dir}/fplot_plot_3d.f90
    ${dir}/fplot_surface_plot.f90
    ${dir}/fplot_label.f90
    ${dir}/fplot_multiplot.f90
    ${dir}/fplot_plot_data_error_bars.f90
    ${dir}/fplot_plot_data_bar.f90
    ${dir}/fplot_plot_data_histogram.f90
    ${dir}/fplot_plot_bar.f90
    ${dir}/fplot_plot_data_tri_2d.f90
    ${dir}/fplot_delaunay_tri_surface.f90
    ${dir}/fplot_tri_surface_plot_data.f90
    ${dir}/fplot_vector_field_plot_data.f90
    ${dir}/fplot_plot_polar.f90
    ${dir}/fplot_filled_plot_data.f90
    ${dir}/fplot_triangulations_delaunay_2d.f90
    ${dir}/fplot_arrow.f90
    ${dir}/fplot_errors.f90
    ${dir}/fplot_constants.f90
    ${dir}/fplot_plot_object.f90
    ${dir}/fplot_plot_data_2d.f90
    ${dir}/fplot_plot_data_3d.f90
    ${dir}/fplot_surface_plot_data.f90
    ${dir}/fplot_stats_plots.f90
)
set(FPLOT_SOURCES ${FPLOT_SOURCES} PARENT_SCOPE)

# Dependencies
include(FetchContent)
find_package(ferror QUIET)
find_package(collections QUIET)
find_package(geompack QUIET)
find_package(fstring QUIET)
if (NOT ferror_FOUND)
    message(STATUS "FERROR could not be found.  A reference version will be employed.")
    FetchContent_Declare(
        ferror
        GIT_REPOSITORY "https://github.com/jchristopherson/ferror"
    )
    FetchContent_MakeAvailable(ferror)
    set(FERROR_LIBRARIES ferror)
    set(BUILD_FERROR TRUE)
else()
    set(FERROR_LIBRARIES ferror::ferror)
    set(BUILD_FERROR FALSE)
endif()

if (NOT collections_FOUND)
    message(STATUS "COLLECTIONS could not be found.  A reference version will be employed.")
    FetchContent_Declare(
        collections
        GIT_REPOSITORY "https://github.com/jchristopherson/collections"
        GIT_TAG main
    )
    FetchContent_MakeAvailable(collections)
    set(COLLECTIONS_LIBRARIES collections)
    set(BUILD_COLLECTIONS TRUE)
else()
    set(COLLECTIONS_LIBRARIES collections::collections)
    set(BUILD_COLLECTIONS FALSE)
endif()

if (NOT geompack_FOUND)
    message(STATUS "GEOMPACK could not be found.  A reference version will be employed.")
    FetchContent_Declare(
        geompack
        GIT_REPOSITORY "https://github.com/jchristopherson/geompack"
        GIT_TAG main
    )
    FetchContent_MakeAvailable(geompack)
    set(GEOMPACK_LIBRARIES geompack)
    set(BUILD_GEOMPACK TRUE)
else()
    set(GEOMPACK_LIBRARIES geompack::geompack)
    set(BUILD_GEOMPACK FALSE)
endif()

if (NOT fstring_FOUND)
    message(STATUS "FSTRING could not be found.  A reference version will be employed.")
    FetchContent_Declare(
        fstring
        GIT_REPOSITORY "https://github.com/jchristopherson/fstring"
        GIT_TAG main
    )
    FetchContent_MakeAvailable(fstring)
    set(FSTRING_LIBRARIES fstring)
    set(BUILD_FSTRING TRUE)
else()
    set(FSTRING_LIBRARIES fstring::fstring)
    set(BUILD_FSTRING FALSE)
endif()

FetchContent_Declare(
    forcolormap
    GIT_REPOSITORY "https://github.com/vmagnin/forcolormap"
    GIT_TAG main
)
FetchContent_MakeAvailable(forcolormap)

# Build
add_library(${PROJECT_NAME} ${FPLOT_SOURCES})
target_link_libraries(
    ${PROJECT_NAME}
    PUBLIC
    ${FERROR_LIBRARIES}
)
target_link_libraries(
    ${PROJECT_NAME}
    PRIVATE
    ${COLLECTIONS_LIBRARIES}
    ${GEOMPACK_LIBRARIES}
    ${FSTRING_LIBRARIES}
    $<BUILD_INTERFACE:forcolormap>
)

set_target_properties(
    ${PROJECT_NAME}
    PROPERTIES
    POSITION_INDEPENDENT_CODE ON
    WINDOWS_EXPORT_ALL_SYMBOLS ON
)

set(FPLOT_MOD_DIR ${CMAKE_CURRENT_BINARY_DIR}/mod_files/)
if (NOT EXISTS "${FPLOT_MOD_DIR}")
    file(MAKE_DIRECTORY "${FPLOT_MOD_DIR}")
endif()

set_target_properties(
    ${PROJECT_NAME} PROPERTIES
    Fortran_MODULE_DIRECTORY ${FPLOT_MOD_DIR}
)
target_include_directories(
    ${PROJECT_NAME} PUBLIC
    $<BUILD_INTERFACE:${FPLOT_MOD_DIR}>
    $<INSTALL_INTERFACE:${CMAKE_INSTALL_MODULEDIR}>
)

# Install
install(
    TARGETS ${PROJECT_NAME}
    EXPORT ${PROJECT_NAME}-targets
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
)
install(DIRECTORY ${FPLOT_MOD_DIR} DESTINATION ${CMAKE_INSTALL_MODULEDIR})

if (${BUILD_FERROR} AND ${BUILD_SHARED_LIBS})
    install(IMPORTED_RUNTIME_ARTIFACTS ${FERROR_LIBRARIES})
endif()

if (${BUILD_COLLECTIONS} AND ${BUILD_SHARED_LIBS})
    install(IMPORTED_RUNTIME_ARTIFACTS ${COLLECTIONS_LIBRARIES})
endif()

if (${BUILD_GEOMPACK} AND ${BUILD_SHARED_LIBS})
    install(IMPORTED_RUNTIME_ARTIFACTS ${GEOMPACK_LIBRARIES})
endif()

if (${BUILD_FSTRING} AND ${BUILD_SHARED_LIBS})
    install(IMPORTED_RUNTIME_ARTIFACTS ${FSTRING_LIBRARIES})
endif()
