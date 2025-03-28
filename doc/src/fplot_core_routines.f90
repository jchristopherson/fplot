! fplot_core_routines.f90

module fplot_core_routines
    use iso_fortran_env
    implicit none
    private
    public :: linspace
    public :: logspace
    public :: meshgrid

contains
! ------------------------------------------------------------------------------
    pure function linspace(start, finish, npts) result(x)
        !! Constructs a linearly spaced array.
        real(real64), intent(in) :: start
            !! The first value in the array.
        real(real64), intent(in) :: finish
            !! The last value in the array.
        integer(int32), intent(in) :: npts
            !! The number of values in the array.
        real(real64), allocatable, dimension(:) :: x
            !! The resulting array.

        ! Local Variables
        integer(int32) :: i
        real(real64) :: dx

        ! Process
        allocate(x(npts))
        dx = (finish - start) / (npts - 1.0d0)
        x(1) = start
        do i = 2, npts
            x(i) = x(i-1) + dx
        end do
    end function

! ------------------------------------------------------------------------------
    pure function logspace(start, finish, npts) result(x)
        !! Construcst a logarithmically spaced array.
        real(real64), intent(in) :: start
            !! The exponent of the first value in the array.
        real(real64), intent(in) :: finish
            !! The exponent of the final value in the array.
        integer(int32), intent(in) :: npts
            !! The number of values in the array.
        real(real64), allocatable, dimension(:) :: x
            !! The resulting array.

        ! Local Variables
        integer(int32) :: i
        real(real64) :: dx, exponent

        ! Process
        allocate(x(npts))
        dx = (finish - start) / (npts - 1.0d0)
        exponent = start
        do i = 1, npts
            x(i) = 1.0d1**exponent
            exponent = exponent + dx
        end do
    end function

! ------------------------------------------------------------------------------
    pure function meshgrid(x, y) result(xy)
        !! Constructs two matrices (X and Y) from x and y data arrays.
        real(real64), intent(in), dimension(:) :: x
            !! An M-element array of x data points.
        real(real64), intent(in), dimension(:) :: y
            !! An N-element array of y data points.
        real(real64), allocatable, dimension(:,:,:) :: xy
            !! An N-by-M-by-2 array containing the x data matrix on the first 
            !! page of the array, and the y data matrix on the second page.

        ! Local Variables
        integer(int32) :: i, nx, ny

        ! Process
        nx = size(x)
        ny = size(y)
        allocate(xy(ny, nx, 2))
        do i = 1, ny
            xy(i,:,1) = x
        end do
        do i = 1, nx
            xy(:,i,2) = y
        end do
    end function

end module
