! fplot_qt_terminal.f90

submodule (fplot_core) fplot_qt_terminal
contains
    pure module function qt_get_term_string(this) result(x)
        class(qt_terminal), intent(in) :: this
        character(len = :), allocatable :: x
        x = this%m_id
    end function
end submodule
