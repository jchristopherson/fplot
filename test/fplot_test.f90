program main
    use fplot_3d_test
    implicit none

    ! Local Variables
    logical :: check

    ! Tests
    check = test_3d_coordinate_system()
    if (.not.check) stop 1
end program