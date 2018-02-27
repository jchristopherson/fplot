# Inform the user we're building the examples
message(STATUS "Building FPLOT examples.")

# Example 1
add_executable(fplot_2d_1 fplot_2d_1.f90)
target_link_libraries(fplot_2d_1 fplot)

# Example 2
add_executable(fplot_2d_2 fplot_2d_2.f90)
target_link_libraries(fplot_2d_2 fplot)

# Example 3
add_executable(fplot_png_1 fplot_png_1.f90)
target_link_libraries(fplot_png_1 fplot)

# Example 4
add_executable(fplot_3d_1 fplot_3d_1.f90)
target_link_libraries(fplot_3d_1 fplot)

# Example 5
add_executable(fplot_surf_1 fplot_surf_1.f90)
target_link_libraries(fplot_surf_1 fplot)