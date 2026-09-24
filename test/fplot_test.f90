program main
    use fplot_3d_test
    use fplot_arrow_test
    use fplot_delaunay_test
    implicit none

    ! Local Variables
    logical :: check

    ! Tests - Started on Sept. 15, 2023.
    ! TO DO: Add tests as this project evolves.
    check = test_3d_coordinate_system()
    if (.not.check) stop 1

    check = test_arrow()
    if (.not.check) stop 2

    check = test_r8tris2_square()
    if (.not.check) stop 3

    check = test_r8tris2_triangle_count()
    if (.not.check) stop 4

    check = test_delaunay_tri_2d()
    if (.not.check) stop 5
end program