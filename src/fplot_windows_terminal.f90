! fplot_windows_terminal.f90

submodule (fplot_core) fplot_windows_terminal
contains

    pure module function wt_get_term_string(this) result(x)
        class(windows_terminal), intent(in) :: this
        character(len = :), allocatable :: x
        x = this%m_id
    end function

end submodule
