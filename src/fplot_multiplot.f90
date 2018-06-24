! fplot_multiplot.f90

submodule (fplot_core) fplot_multiplot
contains
! ------------------------------------------------------------------------------
    module function mp_get_command(this) result(x)
        class(multiplot), intent(in) :: this
        character(len = :), allocatable :: x
    end function

! ------------------------------------------------------------------------------
    module subroutine mp_init(this, m, n)
        class(multiplot), intent(inout) :: this
        integer(int32), intent(in) :: m, n
    end subroutine
    
! ------------------------------------------------------------------------------
    pure module function mp_get_rows(this) result(x)
        class(multiplot), intent(in) :: this
        integer(int32) :: x
    end function

! --------------------
    pure module function mp_get_cols(this) result(x)
        class(multiplot), intent(in) :: this
        integer(int32) :: x
    end function
    
! ------------------------------------------------------------------------------
    module function mp_get_title(this) result(x)
        class(multiplot), intent(in) :: this
        character(len = :), allocatable :: x
    end function

! --------------------
    module subroutine mp_set_title(this, x)
        class(multiplot), intent(inout) :: this
        character(len = *), intent(in) :: x
    end subroutine
    
! ------------------------------------------------------------------------------
    module subroutine mp_draw(this, persist, err)
        class(multiplot), intent(in) :: this
        logical, intent(in), optional :: persist
        class(errors), intent(inout), optional, target :: err
    end subroutine

! ------------------------------------------------------------------------------
    module function mp_get(this, i, j) result(x)
        class(multiplot), intent(in) :: this
        integer(int32), intent(in) :: i, j
        class(plot), pointer :: x
    end function

! --------------------
    module subroutine mp_set(this, i, j, x)
        class(multiplot), intent(inout) :: this
        integer(int32), intent(in) :: i, j
        class(plot), intent(in) :: x
    end subroutine
    
! ------------------------------------------------------------------------------
end submodule
