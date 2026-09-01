! fplot_png_terminal.f90

module fplot_png_terminal
    use iso_fortran_env
    use strings
    use fplot_terminal
    use fplot_constants
    implicit none
    private
    public :: png_terminal

    type, extends(file_terminal) :: png_terminal
        !! Defines a terminal used for producing PNG outputs.
        character(len = 17), private :: m_id = "pngcairo enhanced"
            !! The terminal ID string
    contains
        procedure, public :: get_id_string => png_get_term_string
    end type

contains
! ------------------------------------------------------------------------------
    function png_get_term_string(this) result(x)
        !! Retrieves a GNUPLOT terminal identifier string.
        class(png_terminal), intent(in) :: this
            !! The png_terminal object.
        character(len = :), allocatable :: x
            !! The string.
        integer(int32) :: n
        n = len(this%m_id)
        allocate(character(len = n) :: x)
        x = this%m_id
    end function

! ------------------------------------------------------------------------------
end module