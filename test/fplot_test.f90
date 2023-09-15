program main
    use fplot_3d_test
    implicit none

    ! Local Variables
    logical :: check

    ! Tests - Started on Sept. 15, 2023.
    ! TO DO: Add tests as this project evolves.
    check = test_3d_coordinate_system()
    if (.not.check) stop 1
end program