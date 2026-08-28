program example
    use iso_fortran_env
    use fplot_core
    implicit none

    ! Variables
    integer(int32), parameter :: npts = 20
    real(real64) :: y(npts)
    type(plot_2d) :: plt
    type(plot_data_2d) :: pd1, pd2

    ! Process
    call random_number(y)
    
    call plt%initialize()
    
    call pd1%define_data(y)
    call pd1%set_draw_line(.false.)
    call pd1%set_draw_markers(.true.)
    call pd1%set_marker_scaling(1.5)
    call plt%push(pd1)

    call pd2%define_data(y)
    call pd2%set_draw_impulses(.true.)
    call pd2%set_line_width(2.0)
    call plt%push(pd2)

    call plt%draw()
end program