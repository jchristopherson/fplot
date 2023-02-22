submodule (fplot_core) fplot_triangulations_delaunay_2d
contains
! ------------------------------------------------------------------------------
    module subroutine d2d_init(this, x, y, err)
        ! Arguments
        class(delaunay_tri_2d), intent(inout) :: this
        real(real64), intent(in), dimension(:) :: x, y
        class(errors), intent(inout), target, optional :: err

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg
        integer(int32) :: i, npts, ntri, flag
        real(real64), allocatable, dimension(:,:) :: nodexy
        integer(int32), allocatable, dimension(:,:) :: trinode, trinbr
        
        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        npts = size(x)

        ! Input Check
        if (size(y) /= npts) then
            write(errmsg, 200) &
                "Expected the y-coordinate array to have ", npts, &
                " elements, but found ", size(y), " instead."
            call errmgr%report_error("d2d_init", trim(errmsg), &
                PLOT_ARRAY_SIZE_MISMATCH_ERROR)
            return
        end if

        ! Clean up incase of an existing triangulation
        if (allocated(this%m_x)) deallocate(this%m_x)
        if (allocated(this%m_y)) deallocate(this%m_y)
        if (allocated(this%m_indices)) deallocate(this%m_indices)

        ! Allocate workspace arrays for the triangulation
        allocate(nodexy(2, npts), stat = flag)
        if (flag == 0) allocate(trinode(3, 2 * npts), stat = flag)
        if (flag == 0) allocate(trinbr(3, 2 * npts), stat = flag)
        if (flag /= 0) go to 100

        ! Generate the points list
        do i = 1, npts
            nodexy(1,i) = x(i)
            nodexy(2,i) = y(i)
        end do

        ! Compute the triangulation
        call r8tris2(npts, nodexy, ntri, trinode, trinbr)

        ! Populate the remainder of the object
        allocate(this%m_x(npts), stat = flag)
        if (flag == 0) allocate(this%m_y(npts), stat = flag)
        if (flag == 0) allocate(this%m_indices(ntri, 3), stat = flag)

        do i = 1, npts
            this%m_x(i) = nodexy(1,i)
            this%m_y(i) = nodexy(2, i)
        end do

        do i = 1, ntri
            this%m_indices(i,:) = trinode(:,i)
        end do

        ! End
        return

        ! Memory Error Handler
    100 continue
        call errmgr%report_error("d2d_init", "Insufficient memory available.", &
            PLOT_OUT_OF_MEMORY_ERROR)
        return
        
200     format(A, I0, A, I0, A)
    end subroutine

! ------------------------------------------------------------------------------
    pure module function d2d_get_pt_count(this) result(rst)
        class(delaunay_tri_2d), intent(in) :: this
        integer(int32) :: rst
        if (allocated(this%m_x)) then
            rst = size(this%m_x)
        else
            rst = 0
        end if
    end function

! ------------------------------------------------------------------------------
    pure module function d2d_get_tri_count(this) result(rst)
        class(delaunay_tri_2d), intent(in) :: this
        integer(int32) :: rst
        if (allocated(this%m_indices)) then
            rst = size(this%m_indices, 1)
        else
            rst = 0
        end if
    end function

! ------------------------------------------------------------------------------
    pure module function d2d_get_x_pts(this) result(rst)
        class(delaunay_tri_2d), intent(in) :: this
        real(real64), allocatable, dimension(:) :: rst
        if (allocated(this%m_x)) then
            rst = this%m_x
        else
            allocate(rst(0))
        end if
    end function

! ------------------------------------------------------------------------------
    pure module function d2d_get_y_pts(this) result(rst)
        class(delaunay_tri_2d), intent(in) :: this
        real(real64), allocatable, dimension(:) :: rst
        if (allocated(this%m_y)) then
            rst = this%m_y
        else
            allocate(rst(0))
        end if
    end function

! ------------------------------------------------------------------------------
    pure module function d2d_get_tris(this) result(rst)
        class(delaunay_tri_2d), intent(in) :: this
        integer(int32), allocatable, dimension(:,:) :: rst
        if (allocated(this%m_indices)) then
            rst = this%m_indices
        else
            allocate(rst(0, 0))
        end if
    end function

! ------------------------------------------------------------------------------
    pure module function d2d_get_tri_with_pt(this, x, y) result(rst)
        ! Arguments
        class(delaunay_tri_2d), intent(in) :: this
        real(real64), intent(in) :: x, y
        integer(int32) :: rst

        ! Local Variables
        integer(int32) :: i, j
        real(real64) :: x1, y1, x2, y2, x3, y3
        logical :: check
        
        ! Initialization
        rst = -1

        ! Process
        do i = 1, this%get_triangle_count()
            j = this%m_indices(i, 1)
            x1 = this%m_x(j)
            y1 = this%m_y(j)

            j = this%m_indices(i, 2)
            x2 = this%m_x(j)
            y2 = this%m_y(j)

            j = this%m_indices(i, 3)
            x3 = this%m_x(j)
            y3 = this%m_y(j)

            check = point_inside_triangle(x1, y1, x2, y2, x3, y3, x, y)
            if (check) then
                rst = i
            end if
        end do
    end function

! ------------------------------------------------------------------------------
    ! Determine if a point lies within a triangle.
    ! https://stackoverflow.com/questions/2049582/how-to-determine-if-a-point-is-in-a-2d-triangle
    ! https://en.wikipedia.org/wiki/Barycentric_coordinate_system
    pure elemental function point_inside_triangle(x1, y1, x2, y2, x3, y3, &
            x, y) result(rst)
        ! Arguments
        real(real64), intent(in) :: x1, y1, x2, y2, x3, y3, x, y
        logical :: rst

        ! Local Variables
        real(real64) :: lambda1, lambda2, dT

        ! Initialization
        dT = (y2 - y3) * (x1 - x3) + (x3 - x2) * (y1 - y3)
        lambda1 = ((y2 - y3) * (x - x3) + (x3 - x2) * (y - y3)) / dT
        lambda2 = ((y3 - y1) * (x - x3) + (x1 - x3) * (y - y3)) / dT

        ! The point is within the triangle if:
        ! 0 <= lambda1 <= 1
        ! 0 <= lambda2 <= 1
        ! 0 <= lambda1 + lambda2 <= 1
        rst = (lambda1 <= 1.0d0 .and. lambda1 >= 0.0d0) .and. &
            (lambda2 <= 1.0d0 .and. lambda2 >= 0.0d0) .and. &
            (lambda1 + lambda2 >= 0.0d0 .and. lambda1 + lambda2 <= 1.0d0)
    end function

! ------------------------------------------------------------------------------
end submodule