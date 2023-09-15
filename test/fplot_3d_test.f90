module fplot_3d_test
    use fplot_core
    use iso_fortran_env
    implicit none
contains
! ------------------------------------------------------------------------------
function test_3d_coordinate_system() result(rst)
    ! Arguments
    logical :: rst

    ! Local Variables
    type(plot_3d) :: plt

    ! Initialization
    call plt%initialize()
    rst = .true.

    ! Test
    call plt%set_coordinate_system(COORDINATES_CYLINDRICAL)
    if (plt%get_coordinate_system() /= COORDINATES_CYLINDRICAL) then
        rst = .false.
        print "(A)", "TEST FAILED: test_3d_coordinate_system -1"
    end if

    call plt%set_coordinate_system(COORDINATES_SPHERICAL)
    if (plt%get_coordinate_system() /= COORDINATES_SPHERICAL) then
        rst = .false.
        print "(A)", "TEST FAILED: test_3d_coordinate_system -2"
    end if

    call plt%set_coordinate_system(COORDINATES_CARTESIAN)
    if (plt%get_coordinate_system() /= COORDINATES_CARTESIAN) then
        rst = .false.
        print "(A)", "TEST FAILED: test_3d_coordinate_system -3"
    end if
end function

! ------------------------------------------------------------------------------
end module