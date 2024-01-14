program main
    use fplot_3d_test
    use fplot_arrow_test
    implicit none

    ! Local Variables
    logical :: check

    ! Tests - Started on Sept. 15, 2023.
    ! TO DO: Add tests as this project evolves.
    check = test_3d_coordinate_system()
    if (.not.check) stop 1

    check = test_arrow()
    if (.not.check) stop 2
end program