! fplot_wxt_terminal.f90

module fplot_wxt_terminal
    use iso_fortran_env
    use fplot_terminal
    implicit none
    private
    public :: wxt_terminal

    type, extends(terminal) :: wxt_terminal
        !! A WXT terminal.
        character(len = 3), private :: m_id = "wxt"
            !! The terminal ID string
    contains
        procedure, public :: get_id_string => wxt_get_term_string
    end type

contains
    function wxt_get_term_string(this) result(x)
        !! Retrieves a GNUPLOT terminal identifier string.
        class(wxt_terminal), intent(in) :: this
            !! The wxt_terminal object.
        character(len = :), allocatable :: x
            !! The string.
        integer(int32) :: n
        n = len_trim(this%m_id)
        allocate(character(len = n) :: x)
        x = this%m_id
    end function
end module
