module fplot_delaunay_test
    use iso_fortran_env
    use delaunay
    use fplot_core
    implicit none
contains
! ------------------------------------------------------------------------------
function test_r8tris2_square() result(rst)
    ! Arguments
    logical :: rst

    ! Parameters
    integer(int32), parameter :: npts = 4

    ! Local Variables
    real(real64) :: xy(2, npts)
    integer(int32) :: ntri
    integer(int32) :: tri(3, 2 * npts), nbr(3, 2 * npts)

    ! Initialization
    rst = .true.

    ! Define the corners of a unit square
    xy(:,1) = [0.0d0, 0.0d0]
    xy(:,2) = [1.0d0, 0.0d0]
    xy(:,3) = [1.0d0, 1.0d0]
    xy(:,4) = [0.0d0, 1.0d0]

    ! Test
    call r8tris2(npts, xy, ntri, tri, nbr)

    ! A convex quadrilateral should always produce exactly 2 triangles
    if (ntri /= 2) then
        rst = .false.
        print "(A)", "TEST FAILED: test_r8tris2_square -1"
    end if

    ! Every reported vertex index must be within the valid point range
    if (any(tri(:,1:ntri) < 1) .or. any(tri(:,1:ntri) > npts)) then
        rst = .false.
        print "(A)", "TEST FAILED: test_r8tris2_square -2"
    end if
end function

! ------------------------------------------------------------------------------
function test_r8tris2_triangle_count() result(rst)
    ! Arguments
    logical :: rst

    ! Parameters
    integer(int32), parameter :: npts = 6

    ! Local Variables
    real(real64) :: xy(2, npts)
    integer(int32) :: ntri
    integer(int32) :: tri(3, 2 * npts), nbr(3, 2 * npts)

    ! Initialization
    rst = .true.

    ! Define a set of points: 5 on a pentagon, and 1 in the center
    xy(:,1) = [0.0d0, 1.0d0]
    xy(:,2) = [0.951057d0, 0.309017d0]
    xy(:,3) = [0.587785d0, -0.809017d0]
    xy(:,4) = [-0.587785d0, -0.809017d0]
    xy(:,5) = [-0.951057d0, 0.309017d0]
    xy(:,6) = [0.0d0, 0.0d0]

    ! Test
    call r8tris2(npts, xy, ntri, tri, nbr)

    ! For a point set with NB boundary points, the triangle count should
    ! be 2 * NODE_NUM - NB - 2.  Here, all 5 outer points lie on the
    ! convex hull, so NB = 5, and the expected count is 2*6 - 5 - 2 = 5
    if (ntri /= 5) then
        rst = .false.
        print "(A)", "TEST FAILED: test_r8tris2_triangle_count -1"
    end if
end function

! ------------------------------------------------------------------------------
function test_delaunay_tri_2d() result(rst)
    ! Arguments
    logical :: rst

    ! Parameters
    integer(int32), parameter :: npts = 4
    real(real64), parameter :: x(npts) = [0.0d0, 1.0d0, 1.0d0, 0.0d0]
    real(real64), parameter :: y(npts) = [0.0d0, 0.0d0, 1.0d0, 1.0d0]

    ! Local Variables
    type(delaunay_tri_2d) :: tri
    integer(int32) :: i

    ! Initialization
    rst = .true.
    call tri%create(x, y)

    ! Test
    if (tri%get_point_count() /= npts) then
        rst = .false.
        print "(A)", "TEST FAILED: test_delaunay_tri_2d -1"
    end if

    if (tri%get_triangle_count() /= 2) then
        rst = .false.
        print "(A)", "TEST FAILED: test_delaunay_tri_2d -2"
    end if

    ! The center of the square must lie within one of the triangles
    i = tri%find_triangle(0.5d0, 0.5d0)
    if (i == -1) then
        rst = .false.
        print "(A)", "TEST FAILED: test_delaunay_tri_2d -3"
    end if

    ! A point well outside the square must not be found in any triangle
    i = tri%find_triangle(10.0d0, 10.0d0)
    if (i /= -1) then
        rst = .false.
        print "(A)", "TEST FAILED: test_delaunay_tri_2d -4"
    end if
end function

! ------------------------------------------------------------------------------
end module
