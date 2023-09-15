include("${PROJECT_SOURCE_DIR}/cmake/helper.cmake")

set(fplot_test_sources
    fplot_test.f90
    fplot_3d_test.f90
)

add_executable(fplot_test ${fplot_test_sources})
link_library(fplot_test ${PROJECT_NAME} ${PROJECT_INCLUDE_DIR})
add_test(
    NAME fplot_test
    WORKING_DIRECTORY ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
    COMMAND $<TARGET_FILE:fplot_test>
)