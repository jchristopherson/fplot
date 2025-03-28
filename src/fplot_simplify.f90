! fplot_simplify.f90

! References:
! - https://www.codeproject.com/Articles/114797/Polyline-Simplification
! - https://en.wikipedia.org/wiki/Ramer%E2%80%93Douglas%E2%80%93Peucker_algorithm

module fplot_simplify
    use iso_fortran_env
    use ferror
    use fplot_errors
    implicit none
    private
    public :: simplify_polyline

    interface simplify_polyline
        module procedure :: simplify_polyline_2d1
        module procedure :: simplify_polyline_3d1
        module procedure :: simplify_polyline_mtx
    end interface

contains
    function simplify_polyline_2d1(x, y, tol, err) result(ln)
        !! Simplifies a 2D polyline by removing points too close to 
        !! discern given a specified tolerance.
        real(real64), intent(in), dimension(:) :: x
            !! An N-element array containing the x-coordinates of the vertices
            !! making up the polyline.
        real(real64), intent(in), dimension(:) :: y
            !! An N-element array containing the y-coordinates of the vertices
            !! making up the polyline.
        real(real64), intent(in) :: tol
            !! The distance tolerance to use when simplifying the polyline.
            !! This value must be positive, and larger than machine epsilon.
        class(errors), intent(inout), optional, target :: err
            !! An error handling object.
        real(real64), allocatable, dimension(:,:) :: ln
            !! A matrix containing the simplified polyline vertices.  The first
            !! column of the matrix contains the x-coordinates, and the second 
            !! column contains the y-coordinates.

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        integer(int32) :: n
        real(real64) :: eps
        
        ! Initialization
        n = size(x)
        eps = epsilon(eps)
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Input Check
        if (size(y) /= n) then
            call report_array_size_mismatch_error(errmgr, &
                "simplify_polyline_2d1", "y", n, size(y))
            return
        end if

        if (tol < eps) then
            call errmgr%report_error("simplify_polyline_2d1", &
                "The tolerance value is either negative or less " // &
                "than machine precision.", PLOT_INVALID_INPUT_ERROR)
            return
        end if

        ! Process
        ln = radial_distance_2d(x, y, tol, err)
    end function


    function simplify_polyline_3d1(x, y, z, tol, err) result(ln)
        !! Simplifies a 3D polyline by removing points too close to 
        !! discern given a specified tolerance.
        real(real64), intent(in), dimension(:) :: x
            !! An N-element array containing the x-coordinates of the vertices
            !! making up the polyline.
        real(real64), intent(in), dimension(:) :: y
            !! An N-element array containing the y-coordinates of the vertices
            !! making up the polyline.
        real(real64), intent(in), dimension(:) :: z
            !! An N-element array containing the z-coordinates of the vertices
            !! making up the polyline.
        real(real64), intent(in) :: tol
            !! The distance tolerance to use when simplifying the polyline.
            !! This value must be positive, and larger than machine epsilon.
        class(errors), intent(inout), optional, target :: err
            !! An error handling object.
        real(real64), allocatable, dimension(:,:) :: ln
            !! A matrix containing the simplified polyline vertices.  The first
            !! column of the matrix contains the x-coordinates, the second 
            !! column contains the y-coordinates, and the third column contains 
            !! the z-coordinates.

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        integer(int32) :: n
        real(real64) :: eps
        
        ! Initialization
        n = size(x)
        eps = epsilon(eps)
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Input Check
        if (size(y) /= n) then
            call report_array_size_mismatch_error(errmgr, &
                "simplify_polyline_3d1", "y", n, size(y))
            return
        end if

        if (size(z) /= n) then
            call report_array_size_mismatch_error(errmgr, &
                "simplify_polyline_3d1", "z", n, size(z))
            return
        end if

        if (tol < eps) then
            call errmgr%report_error("simplify_polyline_3d1", &
                "The tolerance value is either negative or less " // &
                "than machine precision.", PLOT_INVALID_INPUT_ERROR)
            return
        end if

        ! Process
        ln = radial_distance_3d(x, y, z, tol, errmgr)
    end function


    
    function simplify_polyline_mtx(xy, tol, err) result(ln)
        !! Simplifies a 2D or 3D polyline by removing points too close to 
        !! discern given a specified tolerance.
        real(real64), intent(in), dimension(:,:) :: xy
            !! An N-by-2 or N-by-3 matrix containing the polyline vertex data.
        real(real64), intent(in) :: tol
            !! The distance tolerance to use when simplifying the polyline.
            !! This value must be positive, and larger than machine epsilon.
        class(errors), intent(inout), optional, target :: err
            !! An error handling object.
        real(real64), allocatable, dimension(:,:) :: ln
            !! A matrix containing the simplified polyline vertices.  The first
            !! column of the matrix contains the x-coordinates, the second 
            !! column contains the y-coordinates, and if necessary, the third 
            !! column contains the z-coordinates.

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        
        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Ensure there are at least 2 columns of data in XY
        if (size(xy, 2) < 2) then
            call report_matrix_size_mismatch_error(errmgr, &
                "simplify_polyline_mtx", "xy", size(xy, 1), 2, size(xy, 1), &
                size(xy, 2))
            return
        end if

        ! Process
        if (size(xy, 2) == 2) then
            ln = simplify_polyline_2d1(xy(:,1), xy(:,2), tol, errmgr)
        else
            ln = simplify_polyline_3d1(xy(:,1), xy(:,2), xy(:,3), tol, errmgr)
        end if
    end function



    function radial_distance_2d(x, y, tol, err) result(pts)
        ! Arguments
        real(real64), intent(in), dimension(:) :: x, y
        real(real64), intent(in) :: tol
        class(errors), intent(inout) :: err
        real(real64), allocatable, dimension(:,:) :: pts

        ! Local Variables
        integer(int32) :: i, j, n, nvalid, flag
        logical, allocatable, dimension(:) :: valid
        real(real64) :: r, xref, yref

        ! Initialization
        n = size(x)
        if (n == 0) return
        i = 2
        xref = x(1)
        yref = y(1)
        nvalid = 1

        ! Local Memory Allocation
        allocate(valid(n), stat = flag)
        if (flag /= 0) then
            call report_memory_error(err, "radial_distance_2d", flag)
            return
        end if
        valid(1) = .true.

        ! Cycle through and determine which points to keep
        do
            if (i > n) exit
            r = pythag2(x(i), y(i), xref, yref)
            if (r < tol) then
                ! The point is too close, reject it
                valid(i) = .false.
            else
                ! The point is outside the tolerance, and is OK
                valid(i) = .true.
                nvalid = nvalid + 1

                ! Move the reference point
                xref = x(i)
                yref = y(i)
            end if
            i = i + 1
        end do

        ! Allocate space, and collect all valid points
        allocate(pts(nvalid, 2), stat = flag)
        if (flag /= 0) then
            call report_memory_error(err, "radial_distance_2d", flag)
            return
        end if
        j = 1
        do i = 1, n
            if (valid(i)) then
                pts(j,1) = x(i)
                pts(j,2) = y(i)
                j = j + 1
            end if
        end do
    end function


    function radial_distance_3d(x, y, z, tol, err) result(pts)
        ! Arguments
        real(real64), intent(in), dimension(:) :: x, y, z
        real(real64), intent(in) :: tol
        class(errors), intent(inout) :: err
        real(real64), allocatable, dimension(:,:) :: pts

        ! Local Variables
        integer(int32) :: i, j, n, nvalid, flag
        logical, allocatable, dimension(:) :: valid
        real(real64) :: r, xref, yref, zref

        ! Initialization
        n = size(x)
        if (n == 0) return
        i = 2
        xref = x(1)
        yref = y(1)
        zref = z(1)
        nvalid = 1

        ! Local Memory Allocation
        allocate(valid(n), stat = flag)
        if (flag /= 0) then
            call report_memory_error(err, "radial_distance_3d", flag)
            return
        end if
        valid(1) = .true.

        ! Cycle through and determine which points to keep
        do
            if (i > n) exit
            r = pythag3(x(i), y(i), z(i), xref, yref, zref)
            if (r < tol) then
                ! The point is too close, reject it
                valid(i) = .false.
            else
                ! The point is outside the tolerance, and is OK
                valid(i) = .true.
                nvalid = nvalid + 1

                ! Move the reference point
                xref = x(i)
                yref = y(i)
                zref = z(i)
            end if
            i = i + 1
        end do

        ! Allocate space, and collect all valid points
        allocate(pts(nvalid, 3), stat = flag)
        if (flag /= 0) then
            call report_memory_error(err, "radial_distance_3d", flag)
            return
        end if
        j = 1
        do i = 1, n
            if (valid(i)) then
                pts(j,1) = x(i)
                pts(j,2) = y(i)
                pts(j,3) = z(i)
                j = j + 1
            end if
        end do
    end function

    pure function pythag2(x, y, xo, yo) result(r)
        ! Arguments
        real(real64), intent(in) :: x, y, xo, yo
        real(real64) :: r

        ! Local Variables
        real(real64) :: w, xabs, yabs

        ! Process
        xabs = abs(x - xo)
        yabs = abs(y - yo)
        w = max(xabs, yabs)
        if (w < epsilon(w)) then
            r = xabs + yabs
        else
            r = w * sqrt((xabs / w)**2 + (yabs / w)**2)
        end if
    end function

    pure function pythag3(x, y, z, xo, yo, zo) result(r)
        ! Arguments
        real(real64), intent(in) :: x, y, z, xo, yo, zo
        real(real64) :: r

        ! Local Variables
        real(real64) :: w, xabs, yabs, zabs

        ! Process
        xabs = abs(x - xo)
        yabs = abs(y - yo)
        zabs = abs(z - zo)
        w = max(xabs, yabs, zabs)
        if (w < epsilon(w)) then
            r = xabs + yabs + zabs
        else
            r = w * sqrt((xabs / w)**2 + (yabs / w)**2 + (zabs / w)**2)
        end if
    end function

end module
