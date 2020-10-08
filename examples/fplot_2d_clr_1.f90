! fplot_2d_clr_1.f90

program example
    use iso_fortran_env
    use fplot_core
    implicit none

    ! Parameters
    integer(int32), parameter :: npts = 1000

    ! Local Variables
    type(color) :: cmap(6), c(npts), c1, c2
    real(real64) :: nmap(6), x(npts), y(npts), x1, x2
    integer(int32) :: i, ind1, ind2
    type(plot_2d) :: plt
    type(plot_data_2d) :: ds

    ! Define the color map
    cmap = [CLR_BLUE, CLR_CYAN, CLR_GREEN, CLR_YELLOW, CLR_ORANGE, CLR_RED]
    nmap = [-1.0d0, -2.0d0 / 3.0d0, -1.0d0 / 3.0d0, &
        1.0d0 / 3.0d0, 2.0d0 / 3.0d0, 1.0d0]

    ! Build the data set
    x = linspace(0.0d0, 1.0d1, npts)
    y = sin(x)

    ! Generate the color list for each data point
    do i = 1, npts
        call find_indices(y(i), nmap, ind1, ind2)
        if (ind1 == ind2) then
            c(i) = cmap(ind1)
        else
            c(i) = interp_color(cmap(ind1), cmap(ind2), nmap(ind1), &
                nmap(ind2), y(i))
        end if
    end do

    ! Plot the data set
    call plt%initialize()
    call ds%define_data(x, y)
    call plt%push(ds)
    call plt%save_file("test.plt")
    call plt%draw()
contains
    ! Color Interpolation Routine
    pure function interp_color(c1, c2, x1, x2, x) result(rst)
        type(color), intent(in) :: c1, c2
        real(real64), intent(in) :: x1, x2, x
        type(color) :: rst

        rst%red = int(lin_interp(x1, x2, real(c1%red, real64), &
            real(c2%red, real64), x), int32)
        rst%green = int(lin_interp(x1, x2, real(c1%green, real64), &
            real(c2%green, real64), x), int32)
        rst%blue = int(lin_interp(x1, x2, real(c1%blue, real64), &
            real(c2%blue, real64), x), int32)

        if (rst%red < 0) rst%red = 0
        if (rst%red > 255) rst%red = 255

        if (rst%green < 0) rst%green = 0
        if (rst%green > 255) rst%green = 255

        if (rst%blue < 0) rst%blue = 0
        if (rst%blue > 255) rst%blue = 255
    end function

    ! Linear Interpolation Routine
    pure function lin_interp(x1, x2, y1, y2, x) result(y)
        real(real64), intent(in) :: x1, x2, y1, y2, x
        real(real64) :: y
        if (x == x1) then
            y = y1
        else if (x == x2) then
            y = y2
        else
            y = (y2 - y1) * (x - x1) / (x2 - x1) + y1
        end if
    end function

    ! Find Indices
    subroutine find_indices(xc, xpts, i1, i2)
        real(real64), intent(in) :: xc, xpts(:)
        integer(int32), intent(out) :: i1, i2

        i2 = 1
        do i2 = 1, size(xpts)
            if (xpts(i2) > xc) exit
        end do
        i1 = i2 - 1
        if (i1 < 1) i1 = 1
    end subroutine
end program
