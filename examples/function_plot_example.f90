program example
    use iso_fortran_env
    use fplot_core
    implicit none

    ! Local Variables
    type(plot_2d) :: plt
    type(plot_data_function) :: fcn1, fcn2
    class(legend), pointer :: lgnd

    ! Define the functions to plot
    call fcn1%define_data("sin(x)", "f(x)", 0.0d0, 10.0d0)
    call fcn1%set_line_width(2.0)

    call fcn2%define_data("cos(x)", "g(x)", 0.0d0, 7.5d0)
    call fcn2%set_line_width(2.0)
    call fcn2%set_line_style(LINE_DASHED)
    call fcn2%set_draw_markers(.true.)
    call fcn2%set_marker_scaling(1.5)
    call fcn2%set_marker_frequency(8)

    ! Plot
    call plt%initialize()
    
    lgnd => plt%get_legend()
    call lgnd%set_is_visible(.true.)
    call lgnd%set_draw_border(.false.)
    call lgnd%set_vertical_position(LEGEND_BOTTOM)

    call plt%push(fcn1)
    call plt%push(fcn2)
    call plt%draw()
end program