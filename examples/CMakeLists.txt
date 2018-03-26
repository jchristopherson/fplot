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

# Example 6
add_executable(fplot_2d_3 fplot_2d_3.f90)
target_link_libraries(fplot_2d_3 fplot)

# Example 7
add_executable(fplot_2d_4 fplot_2d_4.f90)
target_link_libraries(fplot_2d_4 fplot)

# Example 8
add_executable(fplot_2d_5 fplot_2d_5.f90)
target_link_libraries(fplot_2d_5 fplot)

# Example 9
add_executable(fplot_2d_6 fplot_2d_6.f90)
target_link_libraries(fplot_2d_6 fplot)

# Example 10
add_executable(fplot_2d_7 fplot_2d_7.f90)
target_link_libraries(fplot_2d_7 fplot)

# Example 11
add_executable(fplot_2d_8 fplot_2d_8.f90)
target_link_libraries(fplot_2d_8 fplot)

# Example 12
add_executable(fplot_surf_2 fplot_surf_2.f90)
target_link_libraries(fplot_surf_2 fplot)

# Copy the FERROR & COLLECTIONS shared object to the runtime directory on Windows
if (WIN32)
    add_custom_command(
        TARGET fplot_2d_1 POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
        ${ferror_LibLocation} $<TARGET_FILE_DIR:fplot_2d_1>
    )
    add_custom_command(
        TARGET fplot_2d_1 POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
        ${collections_LibLocation} $<TARGET_FILE_DIR:fplot_2d_1>
    )
endif()