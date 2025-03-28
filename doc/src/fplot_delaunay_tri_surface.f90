! fplot_delaunay_tri_surface.f90

module fplot_delaunay_tri_surface
    use iso_fortran_env
    use ieee_arithmetic
    use fplot_triangulations_delaunay_2d
    use fplot_errors
    use ferror
    implicit none
    private
    public :: delaunay_tri_surface

    type, extends(delaunay_tri_2d) :: delaunay_tri_surface
        !! Provides a type describing a triangulated surface.
        real(real64), private, allocatable, dimension(:) :: m_z
            !! An array of the z-coordinates of each point.
    contains
        procedure, public :: define_function_values => dts_define_fcn
        procedure, public :: get_points_z => dts_get_z
        generic, public :: evaluate => dts_interp_1, dts_interp_2

        procedure, private :: dts_interp_1
        procedure, private :: dts_interp_2
    end type

contains
! ------------------------------------------------------------------------------
    subroutine dts_define_fcn(this, z, err)
        !! Defines the function values that correspond to the x and y
        !! data points.
        class(delaunay_tri_surface), intent(inout) :: this
            !! The delaunay_tri_surface object.
        real(real64), intent(in), dimension(:) :: z
            !! An N-element array containing the function values for
            !! each x and y coordinate.  Notice, the x and y coordinates must 
            !! already be defined prior to calling this routine.
        class(errors), intent(inout), optional, target :: err
            !! An error handling object.

        ! Local Variables
        integer(int32) :: n, flag
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        
        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        n = this%get_point_count()

        ! Input Check
        if (n == 0) then
            call errmgr%report_error("dts_define_fcn", &
                "No x-y coordinates have been defined.", &
                PLOT_INVALID_OPERATION_ERROR)
            return
        end if
        if (size(z) /= n) then
            call report_array_size_mismatch_error(errmgr, "dts_define_fcn", &
                "z", n, size(z))
            return
        end if

        ! Store the data
        if (allocated(this%m_z)) deallocate(this%m_z)
        allocate(this%m_z(n), stat = flag)
        if (flag /= 0) then
            call report_memory_error(errmgr, "dts_define_fcn", flag)
            return
        end if
        this%m_z = z
    end subroutine

! ------------------------------------------------------------------------------
    pure function dts_get_z(this) result(rst)
        !! Gets the z-coordinates of each point.
        class(delaunay_tri_surface), intent(in) :: this
            !! The delaunay_tri_surface object.
        real(real64), allocatable, dimension(:) :: rst
            !! An array of the z-coordinates of each point.
        if (allocated(this%m_z)) then
            rst = this%m_z
        else
            allocate(rst(0))
        end if
    end function

! ------------------------------------------------------------------------------
    ! Interpolation Routine - Barycentric Coordinate Approach
    ! https://www.iue.tuwien.ac.at/phd/nentchev/node25.html
    ! https://academic.csuohio.edu/duffy_s/CVE_512_11.pdf
    pure function dts_interp_1(this, x, y) result(z)
        !! Evaluates the function at the requested point by means of 
        !! linear interpolation.
        class(delaunay_tri_surface), intent(in) :: this
            !! The delaunay_tri_surface object.
        real(real64), intent(in) :: x
            !! The x-coordinate at which to evaluate the function.
        real(real64), intent(in) :: y
            !! The y-coordinate at which to evaluate the function.
        real(real64) :: z
            !! The function value.  If the point (x, y) does not lie within the 
            !! range of defined values, then a value of NaN is returned.

        ! Local Variables
        integer(int32) :: i, n1, n2, n3
        real(real64) :: x1, x2, x3, y1, y2, y3, z1, z2, z3
        integer(int32), allocatable, dimension(:,:) :: indices
        real(real64), allocatable, dimension(:) :: xc, yc, zc

        ! Initialization
        z = ieee_value(z, ieee_quiet_nan)
        indices = this%get_indices()
        xc = this%get_points_x()
        yc = this%get_points_y()
        zc = this%get_points_z()

        ! Quick Return
        if (this%get_triangle_count() == 0 .or. &
            this%get_point_count() == 0 .or. &
            size(zc) == 0) return

        ! Locate the triangle to which the point (x, y) belongs.  If no triangle
        ! is found, simply return NaN
        i = this%find_triangle(x, y)
        if (i == -1) return

        ! Get the triangle vertices
        n1 = indices(i, 1)
        n2 = indices(i, 2)
        n3 = indices(i, 3)

        x1 = xc(n1)
        y1 = yc(n1)
        z1 = zc(n1)

        x2 = xc(n2)
        y2 = yc(n2)
        z2 = zc(n2)

        x3 = xc(n3)
        y3 = yc(n3)
        z3 = zc(n3)

        ! Perform the interpolation
        z = linear_interp(x1, y1, z1, x2, y2, z2, x3, y3, z3, x, y)
    end function

! --------------------
    pure function dts_interp_2(this, x, y) result(z)
        !! Evaluates the function at the requested point by means of 
        !! linear interpolation.
        class(delaunay_tri_surface), intent(in) :: this
            !! The delaunay_tri_surface object.
        real(real64), intent(in), dimension(:) :: x
            !! The x data coordinates.
        real(real64), intent(in), dimension(:) :: y
            !! The x data coordinates.
        real(real64), allocatable, dimension(:) :: z
            !! The interpolated z coordinate points.  If the point (x, y) does 
            !! not lie within the range of defined values, then a value of NaN 
            !! is returned.

        ! Local Variables
        integer(int32) :: i, j, n1, n2, n3, nxy
        real(real64) :: x1, x2, x3, y1, y2, y3, z1, z2, z3, nan
        integer(int32), allocatable, dimension(:,:) :: indices
        real(real64), allocatable, dimension(:) :: xc, yc, zc

        ! Initialization
        nxy = min(size(x), size(y))
        nan = ieee_value(nan, ieee_quiet_nan)
        allocate(z(nxy))
        z = nan
        indices = this%get_indices()
        xc = this%get_points_x()
        yc = this%get_points_y()
        zc = this%get_points_z()

        ! Quick Return
        if (this%get_triangle_count() == 0 .or. &
            this%get_point_count() == 0 .or. &
            size(zc) == 0) return

        ! Locate the triangle to which the point (x, y) belongs.  If no triangle
        ! is found, simply return NaN
        do i = 1, nxy
            ! Find the index of the triangle
            j = this%find_triangle(x(i), y(i))

            if (j == -1) cycle  ! Skip if we couldn't find a triangle

            ! Get the vertices
            n1 = indices(j, 1)
            n2 = indices(j, 2)
            n3 = indices(j, 3)

            x1 = xc(n1)
            y1 = yc(n1)
            z1 = zc(n1)

            x2 = xc(n2)
            y2 = yc(n2)
            z2 = zc(n2)

            x3 = xc(n3)
            y3 = yc(n3)
            z3 = zc(n3)

            ! Perform the interpolation
            z(i) = linear_interp(x1, y1, z1, x2, y2, z2, x3, y3, z3, x(i), y(i))
        end do
    end function

! ------------------------------------------------------------------------------
    ! Utilizes linear shape functions to interpolate on a triangle given its
    ! vertex locations, and the desired interpolation location.  Notice, the
    ! interpolation location is expected to lie within the triangle.  This is
    ! not checked.
    pure elemental function linear_interp(x1, y1, z1, x2, y2, z2, x3, &
            y3, z3, x, y) result(z)
        real(real64), intent(in) :: x1, y1, z1, x2, y2, z2, x3, y3, z3, x, y
        real(real64) :: a1, a2, a3, j, z

        j = (x2 - x1) * y3 + (x1 - x3) * y2 + (x3 - x2) * y1
        a1 = (x2 * y3 - x3 * y2 + (y2 - y3) * x + (x3 - x2) * y)
        a2 = (x3 * y1 - x1 * y3 + (y3 - y1) * x + (x1 - x3) * y)
        a3 = (x1 * y2 - x2 * y1 + (y1 - y2) * x + (x2 - x1) * y)
        z = (a1 * z1 + a2 * z2 + a3 * z3) / j
    end function

! ------------------------------------------------------------------------------
end module
