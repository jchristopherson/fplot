! fplot_plot_data_tri_2d.f90

module fplot_plot_data_tri_2d
    use iso_fortran_env
    use fplot_plot_data
    use fplot_constants
    use fplot_triangulations_delaunay_2d
    use fplot_colors
    use strings
    implicit none
    private
    public :: plot_data_tri_2d

    type, extends(plot_data_colored) :: plot_data_tri_2d
        !! Defines a 2D triangulated data set.
        real(real64), private, allocatable, dimension(:) :: m_x
            !! An array of the x-coordinates of each point.
        real(real64), private, allocatable, dimension(:) :: m_y
            !! An array of the y-coordinates of each point.
        integer(int32), private, allocatable, dimension(:,:) :: m_indices
            !! A 3-column matrix containing the indices of each triangle's
            !! vertex.
        real(real32), private :: m_lineWidth = 1.0
            !! The line width.
        integer(int32), private :: m_lineStyle = LINE_SOLID
            !! The line style
    contains
        procedure, public :: get_data_string => pdt2d_get_data_cmd
        procedure, public :: get_command_string => pdt2d_get_cmd
        procedure, public :: define_data => pdt2d_define_data
        procedure, public :: get_line_width => pdt2d_get_line_width
        procedure, public :: set_line_width => pdt2d_set_line_width
        procedure, public :: get_line_style => pdt2d_get_line_style
        procedure, public :: set_line_style => pdt2d_set_line_style
    end type

contains
! ------------------------------------------------------------------------------
    module function pdt2d_get_data_cmd(this) result(x)
        !! Gets the GNUPLOT command string describing the data to plot.
        class(plot_data_tri_2d), intent(in) :: this
            !! The plot_data_tri_2d object.
        character(len = :), allocatable :: x
            !! The command string.

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
        ! https://codeyarns.com/2011/01/25/gnuplot-plotting-a-3d-triangulation/
        do i = 1, n
            ! Line 1-2
            ! Vertex 1
            j = this%m_indices(i, 1)
            call str%append(to_string(this%m_x(j)))
            call str%append(delimiter)
            call str%append(to_string(this%m_y(j)))
            call str%append(delimiter)
            call str%append("0.0")
            call str%append(nl)

            ! Vertex 2
            j = this%m_indices(i, 2)
            call str%append(to_string(this%m_x(j)))
            call str%append(delimiter)
            call str%append(to_string(this%m_y(j)))
            call str%append(delimiter)
            call str%append("0.0")
            call str%append(nl)

            ! Line 2-3
            ! Vertex 2
            call str%append(nl)
            j = this%m_indices(i, 2)
            call str%append(to_string(this%m_x(j)))
            call str%append(delimiter)
            call str%append(to_string(this%m_y(j)))
            call str%append(delimiter)
            call str%append("0.0")
            call str%append(nl)


            ! Vertex 3
            j = this%m_indices(i, 3)
            call str%append(to_string(this%m_x(j)))
            call str%append(delimiter)
            call str%append(to_string(this%m_y(j)))
            call str%append(delimiter)
            call str%append("0.0")
            call str%append(nl)

            ! Line 3-1
            ! Vertex 3
            call str%append(nl)
            j = this%m_indices(i, 3)
            call str%append(to_string(this%m_x(j)))
            call str%append(delimiter)
            call str%append(to_string(this%m_y(j)))
            call str%append(delimiter)
            call str%append("0.0")
            call str%append(nl)

            ! Vertex 1
            j = this%m_indices(i, 1)
            call str%append(to_string(this%m_x(j)))
            call str%append(delimiter)
            call str%append(to_string(this%m_y(j)))
            call str%append(delimiter)
            call str%append("0.0")
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
    module function pdt2d_get_cmd(this) result(x)
        !! Gets the GNUPLOT command string for the object.
        class(plot_data_tri_2d), intent(in) :: this
            !! The plot_data_tri_2d object.
        character(len = :), allocatable :: x
            !! The command string.

        ! Local Variables
        type(string_builder) :: str
        integer(int32) :: n
        type(color) :: clr

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

        ! Lines
        call str%append(" with lines")

        ! Line Width
        call str%append(" lw ")
        call str%append(to_string(this%get_line_width()))

        ! Line Color
        clr = this%get_line_color()
        call str%append(' lc rgb "#')
        call str%append(clr%to_hex_string())
        call str%append('"')

        ! Line Style
        call str%append(" lt ")
        call str%append(to_string(this%get_line_style()))
        if (this%get_line_style() /= LINE_SOLID) then
            call str%append(" dashtype ")
            call str%append(to_string(this%get_line_style()))
        end if

        ! End
        x = char(str%to_string())
    end function

! ------------------------------------------------------------------------------
    module subroutine pdt2d_define_data(this, tri)
        !! Defines the data to plot.
        class(plot_data_tri_2d), intent(inout) :: this
            !! The plot_data_tri_2d object.
        class(delaunay_tri_2d), intent(in) :: tri
            !! The triangulation data to plot.

        ! Process
        if (allocated(this%m_x)) deallocate(this%m_x)
        if (allocated(this%m_y)) deallocate(this%m_y)
        if (allocated(this%m_indices)) deallocate(this%m_indices)

        this%m_x = tri%get_points_x()
        this%m_y = tri%get_points_y()
        this%m_indices = tri%get_indices()
    end subroutine
    
! ------------------------------------------------------------------------------
    pure module function pdt2d_get_line_width(this) result(rst)
        !! Gets the width of the lines used to draw the triangulation.
        class(plot_data_tri_2d), intent(in) :: this
            !! The plot_data_tri_2d object.
        real(real32) :: rst
            !! The line width.
        rst = this%m_lineWidth
    end function

! --------------------
    module subroutine pdt2d_set_line_width(this, x)
        !! Sets the width of the lines used to draw the triangulation.
        class(plot_data_tri_2d), intent(inout) :: this
            !! The plot_data_tri_2d object.
        real(real32), intent(in) :: x
            !! The line width.
        if (x <= 0.0d0) then
            this%m_lineWidth = 1.0d0
        else
            this%m_lineWidth = x
        end if
    end subroutine
! ------------------------------------------------------------------------------
    pure module function pdt2d_get_line_style(this) result(rst)
        !! Gets the line style.
        class(plot_data_tri_2d), intent(in) :: this
            !! The plot_data_tri_2d object.
        integer(int32) :: rst
            !! The line style.  The line style must be one of the following
            !! constants.
            !!
            !! - LINE_DASHED
            !!
            !!  - LINE_DASH_DOTTED
            !!
            !!  - LINE_DASH_DOT_DOT
            !!
            !!  - LINE_DOTTED
            !!
            !!  - LINE_SOLID
        rst = this%m_lineStyle
    end function

! --------------------
    module subroutine pdt2d_set_line_style(this, x)
        !! Sets the line style.
        class(plot_data_tri_2d), intent(inout) :: this
            !! The plot_data_tri_2d object.
        integer(int32), intent(in) :: x
            !! The line style.  The line style must be one of the following
            !! constants.
            !!
            !! - LINE_DASHED
            !!
            !!  - LINE_DASH_DOTTED
            !!
            !!  - LINE_DASH_DOT_DOT
            !!
            !!  - LINE_DOTTED
            !!
            !!  - LINE_SOLID
        if (x == LINE_DASHED .or. &
            x == LINE_DASH_DOTTED .or. &
            x == LINE_DASH_DOT_DOT .or. &
            x == LINE_DOTTED .or. &
            x == LINE_SOLID) then
            ! Only reset the line style if it is a valid type.
            this%m_lineStyle = x
        end if
    end subroutine

! ------------------------------------------------------------------------------
end module
