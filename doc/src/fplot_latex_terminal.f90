module fplot_latex_terminal
    use iso_fortran_env
    use fplot_terminal
    use fplot_constants
    use strings
    implicit none
    private
    public :: latex_terminal

    type, extends(file_terminal) :: latex_terminal
        !! A LATEX terminal.
        character(len = 14), private :: m_id = "epslatex color"
            !! The terminal ID string
    contains
        procedure, public :: get_id_string => tex_get_term_string
    end type

contains
! ------------------------------------------------------------------------------
    function tex_get_term_string(this) result(x)
        !! Retrieves a GNUPLOT terminal identifier string.
        class(latex_terminal), intent(in) :: this
            !! The latex_terminal object.
        character(len = :), allocatable :: x
            !! The string.
        integer(int32) :: n
        n = len_trim(this%m_id)
        allocate(character(len = n) :: x)
        x = this%m_id
    end function

! ------------------------------------------------------------------------------
end module
