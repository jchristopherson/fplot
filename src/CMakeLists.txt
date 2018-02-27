# Define the source files
set(fplot_sources fplot_core.f90 fplot_errors.f90 ${strings_Sources})

# Build the library
add_library(fplot ${fplot_sources})
target_link_libraries(fplot ${FERROR_LIBRARIES} ${COLLECTIONS_LIBRARIES})


# ------------------------------------------------------------------------------
# INSTALLATION
# ------------------------------------------------------------------------------
