program example
    use iso_fortran_env
    use fplot_core
    implicit none

    ! Variables
    integer(int32), parameter :: ndata = 5
    integer(int32), parameter :: nsets = 4
    integer(int32) :: i
    real(real64) :: x(ndata, nsets)
    type(string) :: labels(ndata)
    type(plot_bar) :: plt
    type(plot_data_bar) :: pd
    class(legend), pointer :: lgnd

    ! Build the plot
    call random_number(x)
    do i = 1, ndata
        labels(i) = "T" // to_string(i)
    end do

    call plt%initialize()
    call plt%set_bar_width(0.9)
    call plt%set_draw_border(.false.)
    call plt%set_show_y_major_grid(.true.)
    call plt%set_spacing(3)

    lgnd => plt%get_legend()
    call lgnd%set_is_visible(.true.)
    call lgnd%set_draw_border(.false.)
    call lgnd%set_draw_inside_axes(.false.)
    call lgnd%set_vertical_position(LEGEND_BOTTOM)
    call lgnd%set_horizontal_position(LEGEND_CENTER)
    call lgnd%set_layout(LEGEND_ARRANGE_HORIZONTALLY)

    call pd%define_data(labels, x)
    do i = 1, nsets
        call pd%set_series_name(i, "Series " // char(to_string(i)))
    end do
    call plt%push(pd)
    call plt%draw()
end program