module fplot_svg_terminal
    use iso_fortran_env
    use fplot_terminal
    use fplot_constants
    use strings
    implicit none
    private
    public :: svg_terminal

    type, extends(file_terminal) :: svg_terminal
        !! An SVG terminal.
        character(len = 47), private :: m_id = 'svg enhanced background "white" dynamic mousing'
            !! The terminal ID string
    contains
        procedure, public :: get_id_string => svg_get_term_string
    end type

contains
! ------------------------------------------------------------------------------
    function svg_get_term_string(this) result(x)
        !! Retrieves a GNUPLOT terminal identifier string.
        class(svg_terminal), intent(in) :: this
            !! The svg_terminal object.
        character(len = :), allocatable :: x
            !! The string.
        integer(int32) :: n
        n = len_trim(this%m_id)
        allocate(character(len = n) :: x)
        x = this%m_id
    end function

! ------------------------------------------------------------------------------
end module