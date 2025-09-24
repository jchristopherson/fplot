add_subdirectory(fortran_test_helper)

set(fplot_test_sources
    fplot_test.f90
    fplot_3d_test.f90
    fplot_arrow_test.f90
)

find_package(fotran_test_helper QUIET)
if (NOT fortran_test_helper_FOUND)
    include(FetchContent)
    FetchContent_Declare(
        fortran_test_helper
        GIT_TAG "origin/main"
        GIT_REPOSITORY https://github.com/jchristopherson/fortran_test_helper
    )
    FetchContent_MakeAvailable(fortran_test_helper)
    set(FTH_LIBRARIES fortran_test_helper)
else()
    set(FTH_LIBRARIES fortran_test_helper::fortran_test_helper)
endif()

add_executable(fplot_test ${fplot_test_sources})
target_link_libraries(
    fplot_test
    fplot
    ${FTH_LIBRARIES}
)
add_test(
    NAME fplot_test
    WORKING_DIRECTORY ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
    COMMAND $<TARGET_FILE:fplot_test>
)

if (${BUILD_SHARED_LIBS} AND WIN32)
    add_custom_command(
        TARGET fplot_test
        POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy
        $<TARGET_RUNTIME_DLLS:fplot_test>
        $<TARGET_FILE_DIR:fplot_test>
        COMMAND_EXPAND_LISTS
    )
endif()
