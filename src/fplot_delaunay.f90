! fplot_delaunay.f90

submodule (fplot_core) fplot_delaunay
    use iso_c_binding

    type, bind(C) :: del_point2d_t
        real(c_double) :: x
        real(c_double) :: y
    end type

    type, bind(C) :: delaunay2d_t
        integer(c_int) :: num_points
        type(c_ptr) :: points
        integer(c_int) :: num_faces
        type(c_ptr) :: faces
    end type

    type, bind(C) :: tri_delaunay2d_t
        integer(c_int) :: num_points
        type(c_ptr) :: points
        integer(c_int) :: num_triangles
        type(c_ptr) :: tris
    end type

    interface
        function delaunay2d_from(points, num_points) &
                bind(C, name = "delaunay2d_from") result(rst)
            use iso_c_binding
            import del_point2d_t
            integer(c_int), intent(in), value :: num_points
            type(del_point2d_t), intent(in) :: points(num_points)
            type(c_ptr) :: rst
        end function

        subroutine delaunay2d_release(del) bind(C, name = "delaunay2d_release")
            use iso_c_binding
            type(c_ptr), value :: del
        end subroutine

        function tri_delaunay2d_from(del) result(rst) &
                bind(C, name = "tri_delaunay2d_from")
            use iso_c_binding
            type(c_ptr), intent(in), value :: del
            type(c_ptr) :: rst
        end function

        subroutine tri_delaunay2d_release(tdel) &
                bind(C, name = "tri_delaunay2d_release")
            use iso_c_binding
            type(c_ptr), value :: tdel
        end subroutine
    end interface

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
        integer(int32) :: i, j, npts, ntri, flag
        type(del_point2d_t), allocatable, dimension(:) :: pts
        type(c_ptr) :: dptr, tptr
        type(tri_delaunay2d_t), pointer :: mesh
        type(del_point2d_t), pointer, dimension(:) :: nodes
        integer(c_int), pointer, dimension(:) :: tris
        
        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        npts = size(x)

        ! Input Check
        if (size(y) /= npts) then
            write(errmsg, '(AI0AI0A)') &
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

        ! Set up the triangulation
        allocate(pts(npts), stat = flag)
        if (flag /= 0) go to 100
        do i = 1, npts
            pts(i)%x = x(i)
            pts(i)%y = y(i)
        end do
        
        ! Construct the triangulation
        dptr = delaunay2d_from(pts, npts)
        tptr = tri_delaunay2d_from(dptr)

        ! Populate the remainder of the object
        call c_f_pointer(tptr, mesh)
        ntri = mesh%num_triangles
        npts = mesh%num_points

        allocate(this%m_x(npts), stat = flag)
        if (flag == 0) allocate(this%m_y(npts), stat = flag)
        if (flag == 0) allocate(this%m_indices(ntri, 3), stat = flag)
        
        call c_f_pointer(mesh%points, nodes, [npts])
        do i = 1, npts
            this%m_x(i) = nodes(i)%x
            this%m_y(i) = nodes(i)%y
        end do

        call c_f_pointer(mesh%tris, tris, [3 * ntri])
        j = 1
        do i = 1, ntri
            ! +1 accounts for C zero-based indexing
            this%m_indices(i,1) = tris(j) + 1
            this%m_indices(i,2) = tris(j + 1) + 1
            this%m_indices(i,3) = tris(j + 2) + 1
            j = j + 3
        end do

        ! Clean up
        call delaunay2d_release(dptr)
        call delaunay2d_release(tptr)

        ! End
        return

        ! Memory Error Handler
    100 continue
        call errmgr%report_error("d2d_init", "Insufficient memory available.", &
            PLOT_OUT_OF_MEMORY_ERROR)
        return
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
    module function d2d_get_data_cmd(this) result(x)
        class(delaunay_tri_2d), intent(in) :: this
        character(len = :), allocatable :: x
    end function

! ------------------------------------------------------------------------------
    module function d2d_get_cmd(this) result(x)
        class(delaunay_tri_2d), intent(in) :: this
        character(len = :), allocatable :: x
    end function

! ------------------------------------------------------------------------------
end submodule
