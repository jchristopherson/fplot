! fplot_qt_terminal.f90

module fplot_qt_terminal
    use iso_fortran_env
    use fplot_terminal
    implicit none
    private
    public :: qt_terminal

    type, extends(terminal) :: qt_terminal
        !! Defines a terminal that utilizes QT.
        character(len = 2), private :: m_id = "qt"
            !! The terminal ID string
    contains
        procedure, public :: get_id_string => qt_get_term_string
    end type

contains
    function qt_get_term_string(this) result(x)
        !! Retrieves a GNUPLOT terminal identifier string.
        class(qt_terminal), intent(in) :: this
            !! The qt_terminal object.
        character(len = :), allocatable :: x
            !! The string.
        integer(int32) :: n
        n = len_trim(this%m_id)
        allocate(character(len = n) :: x)
        x = this%m_id
    end function
end module
