! fplot_tri_surface_plot_data.f90

submodule (fplot_core) fplot_tri_surface_plot_data
contains
! ------------------------------------------------------------------------------
    module function tspd_get_data_cmd(this) result(x)
        ! Arguments
        class(tri_surface_plot_data), intent(in) :: this
        character(len = :), allocatable :: x

        ! Local Variables
        ! Local Variables
        type(string_builder) :: str
        integer(int32) :: i, j, n
        character :: delimiter, nl

        ! Initialization
        call str%initialize()
        n = size(this%m_indices, 1)
        delimiter = achar(9)
        nl = new_line(nl)

        ! Process
        ! https://stackoverflow.com/questions/42784369/drawing-triangular-mesh-using-gnuplot
        ! http://www.gnuplot.info/faq/faq.html#x1-530005.10
        do i = 1, n
            ! Line 1-2
            ! Vertex 1
            j = this%m_indices(i, 1)
            call str%append(to_string(this%m_x(j)))
            call str%append(delimiter)
            call str%append(to_string(this%m_y(j)))
            call str%append(delimiter)
            call str%append(to_string(this%m_z(j)))
            call str%append(nl)

            ! Vertex 2
            j = this%m_indices(i, 2)
            call str%append(to_string(this%m_x(j)))
            call str%append(delimiter)
            call str%append(to_string(this%m_y(j)))
            call str%append(delimiter)
            call str%append(to_string(this%m_z(j)))
            call str%append(nl)

            ! Line 2-3
            ! Vertex 2
            call str%append(nl)
            j = this%m_indices(i, 2)
            call str%append(to_string(this%m_x(j)))
            call str%append(delimiter)
            call str%append(to_string(this%m_y(j)))
            call str%append(delimiter)
            call str%append(to_string(this%m_z(j)))
            call str%append(nl)


            ! Vertex 3
            j = this%m_indices(i, 3)
            call str%append(to_string(this%m_x(j)))
            call str%append(delimiter)
            call str%append(to_string(this%m_y(j)))
            call str%append(delimiter)
            call str%append(to_string(this%m_z(j)))
            call str%append(nl)

            ! Line 3-1
            ! Vertex 3
            call str%append(nl)
            j = this%m_indices(i, 3)
            call str%append(to_string(this%m_x(j)))
            call str%append(delimiter)
            call str%append(to_string(this%m_y(j)))
            call str%append(delimiter)
            call str%append(to_string(this%m_z(j)))
            call str%append(nl)

            ! Vertex 1
            j = this%m_indices(i, 1)
            call str%append(to_string(this%m_x(j)))
            call str%append(delimiter)
            call str%append(to_string(this%m_y(j)))
            call str%append(delimiter)
            call str%append(to_string(this%m_z(j)))
            call str%append(nl)

            ! Add in the two blank lines
            if (i /= n) then
                call str%append(nl)
                call str%append(nl)
            end if
        end do

        ! End
        x = char(str%to_string())
    end function

! ------------------------------------------------------------------------------
    module function tspd_get_cmd(this) result(x)
        ! Arguments
        class(tri_surface_plot_data), intent(in) :: this
        character(len = :), allocatable :: x

        ! Local Variables
        type(string_builder) :: str
        integer(int32) :: n

        ! Initialization
        call str%initialize()

        ! Title
        n = len_trim(this%get_name())
        if (n > 0) then
            call str%append(' "-" title "')
            call str%append(this%get_name())
            call str%append('"')
        else
            call str%append(' "-" notitle')
        end if

        ! PM3D or wireframe?
        if (this%get_use_wireframe()) then
            call str%append(" with lines")
        else
            call str%append(" with pm3d")
        end if

        ! End
        x = char(str%to_string())
    end function

! ------------------------------------------------------------------------------
    pure module function tspd_get_wireframe(this) result(rst)
        class(tri_surface_plot_data), intent(in) :: this
        logical :: rst
        rst = this%m_wireframe
    end function

! ------------------------------------------------------------------------------
    module subroutine tspd_set_wireframe(this, x)
        class(tri_surface_plot_data), intent(inout) :: this
        logical, intent(in) :: x
        this%m_wireframe = x
    end subroutine

! ------------------------------------------------------------------------------
    module subroutine tspd_define_data(this, tri)
        ! Arguments
        class(tri_surface_plot_data), intent(inout) :: this
        class(delaunay_tri_surface), intent(in) :: tri

        ! Process
        if (allocated(this%m_x)) deallocate(this%m_x)
        if (allocated(this%m_y)) deallocate(this%m_y)
        if (allocated(this%m_z)) deallocate(this%m_z)
        if (allocated(this%m_indices)) deallocate(this%m_indices)

        this%m_x = tri%get_points_x()
        this%m_y = tri%get_points_y()
        this%m_z = tri%get_points_z()
        this%m_indices = tri%get_indices()
    end subroutine

! ------------------------------------------------------------------------------
end submodule
