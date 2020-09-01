! fplot_plot_bar.f90

submodule (fplot_core) fplot_plot_bar
contains
! ------------------------------------------------------------------------------
pure module function pb_get_bar_width(this) result(x)
    class(plot_bar), intent(in) :: this
    real(real32) :: x
    x = this%m_barWidth
end function

! ------------------------------------------------------------------------------
module subroutine pb_set_bar_width(this, x)
    class(plot_bar), intent(inout) :: this
    real(real32), intent(in) :: x
    if (x > 1.0) then
        this%m_barWidth = 1.0
    else if (x < 0.0) then
        this%m_barWidth = 0.0
    else
        this%m_barWidth = x
    end if
end subroutine

! ------------------------------------------------------------------------------
module function pb_get_cmd(this) result(x)
    ! Arguments
    class(plot_bar), intent(in) :: this
    character(len = :), allocatable :: x

    ! Local Variables
    type(string_builder) :: str

    ! Initialization
    call str%initialize()

    ! Box Width
    call str%append(new_line('a'))
    call str%append("set boxwidth ")
    call str%append(to_string(this%get_bar_width()))
    call str%append(" relative")

    ! Call the base routine to establish the remainder of the plot
    call str%append(this%plot_2d%get_command_string())

    ! End
    x = str%to_string()
end function

! ------------------------------------------------------------------------------
! TO DO YET:
! - clustering
! - stacking
! - lighting
end submodule
