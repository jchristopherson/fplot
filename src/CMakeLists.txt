# Locate the dependencies
add_subdirectory(external/strings)
add_subdirectory(external/ferror)

# Define the source files
set(fplot_sources fplot_core.f90 fplot_errors.f90 fplot_list.f90)

# Build the library
add_library(fplot ${fplot_sources})
target_link_libraries(fplot strings ferror)