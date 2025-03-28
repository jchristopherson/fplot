! fplot_windows_terminal.f90

module fplot_windows_terminal
    use iso_fortran_env
    use fplot_terminal
    implicit none
    private
    public :: windows_terminal

    type, extends(terminal) :: windows_terminal
        !! A Windows-specific terminal.
        character(len = 3), private :: m_id = "win"
            !! The terminal ID string
    contains
        procedure, public :: get_id_string => wt_get_term_string
    end type

contains

    function wt_get_term_string(this) result(x)
        !! Retrieves a GNUPLOT terminal identifier string.
        class(windows_terminal), intent(in) :: this
            !! The windows_terminal object.
        character(len = :), allocatable :: x
            !! The string.
        integer(int32) :: n
        n = len_trim(this%m_id)
        allocate(character(len = n) :: x)
        x = this%m_id
    end function

end module
