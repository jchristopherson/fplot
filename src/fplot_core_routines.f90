! fplot_core_routines.f90

submodule (fplot_core) fplot_core_routines
contains
! ------------------------------------------------------------------------------
    pure module function linspace(start, finish, npts) result(x)
        ! Arguments
        real(real64), intent(in) :: start, finish
        integer(int32), intent(in) :: npts
        real(real64), dimension(npts) :: x

        ! Local Variables
        integer(int32) :: i
        real(real64) :: dx

        ! Process
        dx = (finish - start) / (npts - 1.0d0)
        x(1) = start
        do i = 2, npts
            x(i) = x(i-1) + dx
        end do
    end function

end submodule
