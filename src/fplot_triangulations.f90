!> @brief Provides various triangulation routines.
!!
!! @par Remarks
!! This type utilizes the GEOMPACK triangulation code available at 
!! https://people.sc.fsu.edu/~jburkardt/f77_src/geompack/geompack.html.
module fplot_triangulations
    use iso_fortran_env
    use ferror
    implicit none
    private
    public :: delaunay_tri_2d
    
    !> @brief Provides a container for a 2D Delaunay triangulation.
    !!
    !! @par Remarks
    !! This type utilizes the GEOMPACK triangulation code available at 
    !! https://people.sc.fsu.edu/~jburkardt/f77_src/geompack/geompack.html.
    !!
    !! @par Example
    !! @code{.f90}
    !! program example
    !!     use iso_fortran_env
    !!     use fplot_core
    !!     use triangulations
    !!     implicit none
    !!
    !!     ! Parameters
    !!     integer(int32), parameter :: npts = 1000
    !!     real(real64), parameter :: pi = 2.0d0 * acos(0.0d0)
    !!
    !!     ! Local Variables
    !!     type(delaunay_tri_2d) :: tri
    !!     real(real64) :: x(npts), y(npts), theta(npts), radius(npts)
    !!     type(plot_2d) :: plt
    !!     type(plot_data_tri_2d) :: ds
    !!
    !!     ! Initialization
    !!     call random_number(theta)
    !!     theta = 2.0d0 * pi * theta
    !!
    !!     call random_number(radius)
    !!     radius = radius + 0.5d0
    !!
    !!     x = radius * cos(theta)
    !!     y = radius * sin(theta)
    !!
    !!     ! Create a 2D triangulation from the data
    !!     call tri%create(x, y)
    !!
    !!     ! Display the number of points and elements
    !!     print '(AI0AI0A)', "The triangulation consists of ", &
    !!         tri%get_point_count(), " points, and ", tri%get_triangle_count(), &
    !!         " triangles."
    !!
    !!     ! Plot the triangulation
    !!     call plt%initialize()
    !!     call plt%set_font_size(14)
    !!
    !!     call ds%define_data(tri)
    !!     call plt%push(ds)
    !!
    !!     call plt%draw()
    !! end program
    !! @endcode
    !! The above program produces the following output.
    !! @code{.txt}
    !! The triangulation consists of 1000 points, and 1970 triangles.
    !! @endcode
    !! @image html example_delaunay_2d_1.png
    type delaunay_tri_2d
    private
        !> @brief An array of the x-coordinates of each point.
        real(real64), allocatable, dimension(:) :: m_x
        !> @brief An array of the y-coordinates of each point.
        real(real64), allocatable, dimension(:) :: m_y
        !> @brief A 3-column matrix containing the indices of each triangle's
        !! vertex.
        integer(int32), allocatable, dimension(:,:) :: m_indices
    contains
        !> @brief Creates an unconstrained 2D Delaunay triangulation given a 
        !! set of x-y points.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine create(class(delaunay_tri_2d) this, real(real64) x(:), real(real64) y(:), class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The delaunay_tri_2d object.
        !! @param[in] x An N-element array containing the x-coordinates of each
        !!  data point.
        !! @param[in] y An N-element array containing the y-coordinates of each
        !!  data point. 
        !! @param[in,out] err An optional errors-based object that if provided 
        !!  can be used to retrieve information relating to any errors 
        !!  encountered during execution.  If not provided, a default 
        !!  implementation of the errors class is used internally to provide 
        !!  error handling.  Possible errors and warning messages that may be 
        !!  encountered are as follows.
        !!  - PLOT_ARRAY_SIZE_MISMATCH_ERROR: Occurs if the input arrays are not
        !!      the same size.
        !!  - PLOT_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory
        !!      available.
        procedure, public :: create => d2d_init
        !> @brief Gets the number of points in the triangulation.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! integer(int32) function get_point_count(class(delaunay_tri_2d) this)
        !! @endcode
        !!
        !! @param[in] this The delaunay_tri_2d object.
        !! @return The number of points in the triangulation.
        procedure, public :: get_point_count => d2d_get_pt_count
        !> @brief Gets the number of triangles in the triangulation.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! integer(int32) function get_triangle_count(class(delaunay_tri_2d) this)
        !! @endcode
        !!
        !! @param[in] this The delaunay_tri_2d object.
        !! @return The number of triangles in the triangulation.
        procedure, public :: get_triangle_count => d2d_get_tri_count
        !> @brief Gets the x-coordinates of each point.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! real(real64)(:) function get_points_x(class(delaunay_tri_2d) this)
        !! @endcode
        !!
        !! @param[in] this The delaunay_tri_2d object.
        !! @return An array of the x-coordinates of each point.
        procedure, public :: get_points_x => d2d_get_x_pts
        !> @brief Gets the y-coordinates of each point.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! real(real64)(:) function get_points_y(class(delaunay_tri_2d) this)
        !! @endcode
        !!
        !! @param[in] this The delaunay_tri_2d object.
        !! @return An array of the y-coordinates of each point.
        procedure, public :: get_points_y => d2d_get_y_pts
        !> @brief Gets a list of the indices of each triangle vertex.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! integer(int32)(:,:) function get_indices(class(delaunay_tri_2d) this)
        !! @endcode
        !!
        !! @param[in] this The delaunay_tri_2d object.
        !! @return An N-by-3 matrix with each column containing the index of the
        !!  vertex of each triangle where N is the number of triangles.
        procedure, public :: get_indices => d2d_get_tris
        !> @brief Finds the triangle that contains the specified point.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! integer(int32) function find_triangle(class(delaunay_tri_2d) this, real(real64) x, real(real64) y)
        !! @endcode
        !!
        !! @param[in] this The delaunay_tri_2d object.
        !! @param[in] x The x-coordinate of the point.
        !! @param[in] y The y-coordinate of the point.
        !!
        !! @return Returns the index of the triangle containing the specified
        !!  point.  If no triangle contains the specified point, a value of
        !!  -1 is returned.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use fplot_core
        !!     use iso_fortran_env
        !!     use triangulations
        !!     implicit none
        !!
        !!     ! Parameters
        !!     integer(int32), parameter :: npts = 1000
        !!     real(real64), parameter :: pi = 2.0d0 * acos(0.0d0)
        !!     real(real64), parameter :: xpt = 0.75d0
        !!     real(real64), parameter :: ypt = 0.75d0
        !!
        !!     ! Local Variables
        !!     type(delaunay_tri_2d) :: tri
        !!     real(real64) :: x(npts), y(npts), theta(npts), radius(npts), &
        !!         xtri(3), ytri(3)
        !!     integer(int32) :: ind, n1, n2, n3
        !!     integer(int32), allocatable, dimension(:,:) :: indices
        !!     type(plot_2d) :: plt
        !!     type(plot_data_tri_2d) :: ds
        !!     type(plot_data_2d) :: dtri, dpt
        !!
        !!     ! Initialization
        !!     call random_number(theta)
        !!     theta = 2.0d0 * pi * theta
        !!
        !!     call random_number(radius)
        !!     radius = radius + 0.5d0
        !!
        !!     x = radius * cos(theta)
        !!     y = radius * sin(theta)
        !!
        !!     ! Create a 2D triangulation from the data
        !!     call tri%create(x, y)
        !!
        !!     ! Find the index of the triangle containing (xpt, ypt)
        !!     ind = tri%find_triangle(xpt, ypt)
        !!     if (ind == -1) then
        !!         print '(A)', "No triangle was found that included the specified point."
        !!     end if
        !!
        !!     ! Get the vertices of this triangle
        !!     indices = tri%get_indices()
        !!     n1 = indices(ind, 1)
        !!     n2 = indices(ind, 2)
        !!     n3 = indices(ind, 3)
        !!     xtri = [x(n1), x(n2), x(n3)]
        !!     ytri = [y(n1), y(n2), y(n3)]
        !!
        !!     ! Plot the triangulation, the point of interest, and highlight the triangle
        !!     call plt%initialize()
        !!     call plt%set_font_size(14)
        !!
        !!     call ds%define_data(tri)
        !!     call plt%push(ds)
        !!
        !!     call dtri%define_data(xtri, ytri)
        !!     call dtri%set_draw_line(.false.)
        !!     call dtri%set_draw_markers(.true.)
        !!     call dtri%set_marker_style(MARKER_FILLED_CIRCLE)
        !!     call dtri%set_marker_scaling(1.2)
        !!     call dtri%set_line_color(CLR_LIME)
        !!     call plt%push(dtri)
        !!
        !!     call dpt%define_data([xpt], [ypt])
        !!     call dpt%set_draw_line(.false.)
        !!     call dpt%set_draw_markers(.true.)
        !!     call dpt%set_marker_style(MARKER_X)
        !!     call dpt%set_marker_scaling(3.0)
        !!     call dpt%set_line_width(2.0)
        !!     call plt%push(dpt)
        !!
        !!     call plt%draw()
        !! end program
        !! @endcode
        !! @image html example_delaunay_2d_2a.png
        !! @image html example_delaunay_2d_2b.png
        procedure, public :: find_triangle => d2d_get_tri_with_pt
    end type

! ----------
    interface
        module subroutine d2d_init(this, x, y, err)
            class(delaunay_tri_2d), intent(inout) :: this
            real(real64), intent(in), dimension(:) :: x, y
            class(errors), intent(inout), target, optional :: err
        end subroutine

        pure module function d2d_get_pt_count(this) result(rst)
            class(delaunay_tri_2d), intent(in) :: this
            integer(int32) :: rst
        end function

        pure module function d2d_get_tri_count(this) result(rst)
            class(delaunay_tri_2d), intent(in) :: this
            integer(int32) :: rst
        end function

        pure module function d2d_get_x_pts(this) result(rst)
            class(delaunay_tri_2d), intent(in) :: this
            real(real64), allocatable, dimension(:) :: rst
        end function

        pure module function d2d_get_y_pts(this) result(rst)
            class(delaunay_tri_2d), intent(in) :: this
            real(real64), allocatable, dimension(:) :: rst
        end function

        pure module function d2d_get_tris(this) result(rst)
            class(delaunay_tri_2d), intent(in) :: this
            integer(int32), allocatable, dimension(:,:) :: rst
        end function

        pure module function d2d_get_tri_with_pt(this, x, y) result(rst)
            class(delaunay_tri_2d), intent(in) :: this
            real(real64), intent(in) :: x, y
            integer(int32) :: rst
        end function
    end interface
    
end module