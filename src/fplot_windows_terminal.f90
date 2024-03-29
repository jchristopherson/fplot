! fplot_windows_terminal.f90

submodule (fplot_core) fplot_windows_terminal
contains

    module function wt_get_term_string(this) result(x)
        class(windows_terminal), intent(in) :: this
        character(len = :), allocatable :: x
        integer(int32) :: n
        n = len_trim(this%m_id)
        allocate(character(len = n) :: x)
        x = this%m_id
    end function

end submodule
