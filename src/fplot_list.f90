! fplot_list.f90

!> @brief \b fplot_list
!!
!! @par Purpose
!! This module provides a collection suitable for supporting plotting
!! operations.
module fplot_list
    use, intrinsic :: iso_fortran_env, only : int32
    use ferror, only : errors
    use fplot_errors
    implicit none
    private
    public :: container
    public :: list

! ******************************************************************************
! CONSTANTS
! ------------------------------------------------------------------------------
    !> @brief The default buffer size.
    integer(int32), parameter :: DEFAULT_BUFFER_SIZE = 10

! ******************************************************************************
! TYPES
! ------------------------------------------------------------------------------
    !> @brief A container type allowing storage of most any Fortran type.
    type container
    private
        !> A pointer to a polymorphic variable allowing storage of any type.
        class(*), pointer :: m_data => null()
    contains
        !> @brief Retrieves the stored data.
        procedure, public :: get => cntr_get_item
        !> @brief Stores the specified item in the container.
        procedure, public :: set => cntr_set_item
    end type

! ------------------------------------------------------------------------------
    !> @brief A generic list container.
    type list
    private
        !> A collection of container objects.
        type(container), allocatable, dimension(:) :: m_list
        !> The actual number of items in m_list
        integer(int32) :: m_count = 0
    contains
        !> @brief Gets the number of items in the list.
        procedure, public :: get_count => list_get_count
        !> @brief Gets the capacity of the list.
        procedure, public :: get_capacity => list_get_capacity
        !> @brief Sets the capacity of the list.
        procedure, public :: set_capacity => list_set_capacity
        !> @brief Gets an item from the list.
        procedure, public :: get => list_get
        !> @brief Sets an item into the list.
        procedure, public :: set => list_set
        !> @brief Pushes an item onto the end of the list.
        procedure, public :: push => list_push
        !> @brief Pops the last item from the end of the list.
        procedure, public :: pop => list_pop
        !> @brief Inserts an item into the list.
        procedure, public :: insert => list_insert
        !> @brief Removes an item from the list.
        procedure, public :: remove => list_remove
        !> @brief Clears the contents of the list.
        procedure, public :: clear => list_clear
    end type

contains
! ******************************************************************************
! CONTAINER MEMBERS
! ------------------------------------------------------------------------------
    !> @brief Retrieves the stored data represented as a pointer to a
    !! polymorhpic type.
    !!
    !! @param[in] this The container object.
    !! @return A pointer to the stored data.
    function cntr_get_item(this) result(x)
        class(container), intent(in) :: this
        class(*), pointer :: x
        x => this%m_data
    end function

! --------------------
    !> @brief Stores the specified item in the container.
    !!
    !! @param[in,out] this The container object.
    !! @param[in] x The object to store.
    subroutine cntr_set_item(this, x)
        class(container), intent(inout) :: this
        class(*), intent(in), target :: x
        this%m_data => x
    end subroutine

! ******************************************************************************
! LIST MEMBERS
! ------------------------------------------------------------------------------
    !> @brief Gets the number of items in the list.
    !!
    !! @param[in] this The list object.
    !! @return The number of items stored in the list.
    pure function list_get_count(this) result(x)
        class(list), intent(in) :: this
        integer(int32) :: x
        x = this%m_count
    end function

! ------------------------------------------------------------------------------
    !> @brief Gets the capacity of the list.
    !!
    !! @param[in] this The list object.
    !! @return The capacity of the list.
    pure function list_get_capacity(this) result(x)
        class(list), intent(in) :: this
        integer(int32) :: x
        if (allocated(this%m_list)) then
            x = size(this%m_list)
        else
            x = 0
        end if
    end function

! --------------------
    !> @brief Sets the capacity of the list.
    !!
    !! @param[in,out] this The list object.
    !! @param[in] n The desired capacity of the list.  This value must not be
    !!  less than the number of items already stored in the list.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !! - PLOT_INVALID_INPUT_ERROR: Occurs if @p n is less than the number of
    !!      items already stored in the list.
    !! - PLOT_OUT_OF_MEMORY_ERROR: Occurs if insufficient memory is available.
    subroutine list_set_capacity(this, n, err)
        ! Arguments
        class(list), intent(inout) :: this
        integer(int32), intent(in) :: n
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: flag, m
        type(container), allocatable, dimension(:) :: copy
        class(errors), pointer :: errmgr
        type(errors), target :: deferr

        ! Initialization
        m = this%get_count()
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Input Check
        if (n < m) then
            call errmgr%report_error("list_set_capacity", &
                "Reducing the capacity of the list is not allowed.", &
                PLOT_INVALID_INPUT_ERROR)
            return
        end if

        ! Process
        if (allocated(this%m_list)) then
            allocate(copy(m), stat = flag)
            if (flag == 0) then
                copy = this%m_list
                deallocate(this%m_list)
                allocate(this%m_list(n), stat = flag)
                if (flag == 0) this%m_list(1:m) = copy
            end if
        else
            allocate(this%m_list(n), stat = flag)
        end if
        if (flag /= 0) then
            call errmgr%report_error("list_set_capacity", &
                "Insufficient memory available.", PLOT_OUT_OF_MEMORY_ERROR)
            return
        end if
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets an item from the list.
    !!
    !! @param[in] this The list object.
    !! @param[in] i The index of the item.
    !! @return A container object containing requested item.
    function list_get(this, i) result(x)
        class(list), intent(in) :: this
        integer(int32), intent(in) :: i
        type(container) :: x
        x = this%m_list(i)
    end function

! --------------------
    !> @brief Sets an item into the list.
    !!
    !! @param[in,out] this The list object.
    !! @param[in] i The index of the item.
    !! @param[in] x The item to place into the list.
    subroutine list_set(this, i, x)
        ! Arguments
        class(list), intent(inout) :: this
        integer(int32), intent(in) :: i
        class(*), intent(in) :: x

        ! Store the object
        type(container) :: obj
        call obj%set(x)
        this%m_list(i) = obj
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Pushes an item onto the end of the list.
    !!
    !! @param[in,out] this The list object.
    !! @param[in] x The object to add to the list.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !! - PLOT_OUT_OF_MEMORY_ERROR: Occurs if insufficient memory is available.
    subroutine list_push(this, x, err)
        ! Arguments
        class(list), intent(inout) :: this
        class(*), intent(in) :: x
        class(errors), intent(inout), optional, target :: err

        ! Ensure we've got sufficient capacity; else, increase accordingly
        if (this%get_capacity() <= this%get_count() + 1) then
            call this%set_capacity(this%get_count() + DEFAULT_BUFFER_SIZE, err)
        end if

        ! Store the value
        this%m_count = this%m_count + 1
        call this%set(this%get_count(), x)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Pops the last item from the end of the list.
    !!
    !! @param[in,out] this The list object.
    subroutine list_pop(this)
        ! Arguments
        class(list), intent(inout) :: this

        ! Process
        if (this%m_count > 0) then
            this%m_count = this%m_count - 1
        end if
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Inserts an item into the list.
    !!
    !! @param[in,out] this The list object.
    !! @param[in] i The index at which to insert the item.
    !! @param[in] x The item to insert.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !! - PLOT_INVALID_INPUT_ERROR: Occurs if @p i is less than or equal to 0,
    !!      or if @p i is larger than 1 element beyond the current size of the
    !!      list.
    !! - PLOT_OUT_OF_MEMORY_ERROR: Occurs if insufficient memory is available.
    subroutine list_insert(this, i, x, err)
        ! Arguments
        class(list), intent(inout) :: this
        integer(int32), intent(in) :: i
        class(*), intent(in) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: n
        type(container) :: obj
        class(errors), pointer :: errmgr
        type(errors), target :: deferr

        ! Initialization
        n = this%get_count()
        call obj%set(x)
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Input Check
        if (i <= 0 .or. i > n + 1) then
            call errmgr%report_error("list_insert", &
                "The supplied index is outside the bounds of the list.", &
                PLOT_INVALID_INPUT_ERROR)
            return
        end if

        ! Ensure sufficient capacity
        if (this%get_capacity() <= n + 1) then
            call this%set_capacity(n + DEFAULT_BUFFER_SIZE, errmgr)
            if (errmgr%has_error_occurred()) return
        end if

        ! Shift everything back by one element, and insert the specified item
        this%m_list(i+1:n+1) = this%m_list(i:n)
        this%m_list(i) = obj
        this%m_count = this%m_count + 1
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Removes an item from the list.
    !!
    !! @param[in,out] this The list object.
    !! @param[in] i The index of the item to remove.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !! - PLOT_INVALID_INPUT_ERROR: Occurs if @p i is less than or equal to 0,
    !!      or if @p i is larger than 1 element beyond the current size of the
    !!      list.
    !! - PLOT_INVALID_OPERATION_ERROR: Occurs if attempting to remove an item
    !!      when there are no items left in the list to remove.
    subroutine list_remove(this, i, err)
        ! Arguments
        class(list), intent(inout) :: this
        integer(int32), intent(in) :: i
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: n
        class(errors), pointer :: errmgr
        type(errors), target :: deferr

        ! Initialization
        n = this%get_count()
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Ensure there are items to remove
        if (n == 0) then
            call errmgr%report_error("list_remove", &
                "Attempted to remove items from an already empty list.", &
                PLOT_INVALID_OPERATION_ERROR)
            return
        end if

        ! Input Check
        if (i <= 0 .or. i > n) then
            call errmgr%report_error("list_remove", &
                "The supplied index is outside the bounds of the list.", &
                PLOT_INVALID_INPUT_ERROR)
            return
        end if

        ! Shift everything down by one element
        this%m_list(i:n-1) = this%m_list(i+1:n)
        this%m_count = this%m_count - 1
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Clears the contents of the list.
    !!
    !! @param[in,out] this The list object.
    subroutine list_clear(this)
        class(list), intent(inout) :: this
        this%m_count = 0
    end subroutine

! ------------------------------------------------------------------------------
end module
