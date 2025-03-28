! fplot_surface_plot.f90

module fplot_surface_plot
    use iso_fortran_env
    use fplot_plot_3d
    use fplot_errors
    use fplot_legend
    use ferror
    use strings
    implicit none
    private
    public :: surface_plot

    type, extends(plot_3d) :: surface_plot
        logical, private :: m_showHidden = .false.
            !! Show hidden lines?
        logical, private :: m_smooth = .true.
            !! Smooth the surface?
        logical, private :: m_contour = .false.
            !! Show a contour plot as well as the surface plot?
        logical, private :: m_useLighting = .false.
            !! Use lighting?
        real(real32), private :: m_lightIntensity = 0.5
            !! Lighting intensity (0 - 1) - default is 0.5
        real(real32), private :: m_specular = 0.5
            !! Specular highlight intensity (0 - 1).
        real(real32), private :: m_transparency = 1.0
            !! Defines the translucency value.  Must exist on (0, 1].
    contains
        procedure, public :: initialize => surf_init
        procedure, public :: get_show_hidden => surf_get_show_hidden
        procedure, public :: set_show_hidden => surf_set_show_hidden
        procedure, public :: get_command_string => surf_get_cmd
        procedure, public :: get_allow_smoothing => surf_get_smooth
        procedure, public :: set_allow_smoothing => surf_set_smooth
        procedure, public :: get_show_contours => surf_get_show_contours
        procedure, public :: set_show_contours => surf_set_show_contours
        procedure, public :: get_use_lighting => surf_get_use_lighting
        procedure, public :: set_use_lighting => surf_set_use_lighting
        procedure, public :: get_light_intensity => surf_get_light_intensity
        procedure, public :: set_light_intensity => surf_set_light_intensity
        procedure, public :: get_specular_intensity => surf_get_specular_intensity
        procedure, public :: set_specular_intensity => surf_set_specular_intensity
        procedure, public :: get_transparency => surf_get_transparency
        procedure, public :: set_transparency => surf_set_transparency
    end type

contains
! ------------------------------------------------------------------------------
    subroutine surf_init(this, term, fname, err)
        !! Initializes the surface_plot object.
        class(surface_plot), intent(inout) :: this
            !! The surface_plot object.
        integer(int32), intent(in), optional :: term
            !! An optional input that is used to define the terminal.
            !!  The default terminal is a WXT terminal.  The acceptable inputs 
            !! are:
            !!
            !!  - GNUPLOT_TERMINAL_PNG
            !!
            !!  - GNUPLOT_TERMINAL_QT
            !!
            !!  - GNUPLOT_TERMINAL_WIN32
            !!
            !!  - GNUPLOT_TERMINAL_WXT
            !!
            !!  - GNUPLOT_TERMINAL_LATEX
        character(len = *), intent(in), optional :: fname
            !! A filename to pass to the terminal in the event the
            !! terminal is a file type (e.g. GNUPLOT_TERMINAL_PNG).
        class(errors), intent(inout), optional, target :: err
            !! An error handling object.

        ! Local Variables
        type(legend), pointer :: lgnd

        ! Initialize the base class
        call this%plot_3d%initialize(term, fname, err)

        ! Do not display the legend
        lgnd => this%get_legend()
        call lgnd%set_is_visible(.false.)
    end subroutine

! ------------------------------------------------------------------------------
    pure function surf_get_show_hidden(this) result(x)
        !! Gets a value indicating if hidden lines should be shown.
        class(surface_plot), intent(in) :: this
            !! The surface_plot object.
        logical :: x
            !! Returns true if hidden lines should be shown; else, false.
        x = this%m_showHidden
    end function

! ------------------------------------------------------------------------------
    subroutine surf_set_show_hidden(this, x)
        !! Sets a value indicating if hidden lines should be shown.
        class(surface_plot), intent(inout) :: this
            !! The surface_plot object.
        logical, intent(in) :: x
            !! Set to true if hidden lines should be shown; else, false.
        this%m_showHidden = x
    end subroutine

! ------------------------------------------------------------------------------
    function surf_get_cmd(this) result(x)
        !! Gets the GNUPLOT command string to represent this plot_3d
        !! object.
        class(surface_plot), intent(in) :: this
            !! The surface_plot object.
        character(len = :), allocatable :: x
            !! The command string.

        ! Local Variables
        type(string_builder) :: str
        ! class(colormap), pointer :: clr

        ! Initialization
        call str%initialize()

        ! Call the base routine
        call str%append(this%plot%get_command_string())

        ! Hidden Stuff
        call str%append(new_line('a'))
        if (this%get_show_hidden()) then
            call str%append("unset hidden3d")
        else
            call str%append("set hidden3d")
        end if

        ! Define the colormap
        ! clr => this%get_colormap()
        ! if (associated(clr)) then
        !     call str%append(new_line('a'))
        !     call str%append(clr%get_command_string())
        ! end if

        ! Allow for smoothing interpolation
        if (this%get_allow_smoothing()) then
            call str%append(new_line('a'))
            call str%append("set pm3d interpolate 0,0")
        end if

        ! Draw a contour plot as well?
        if (this%get_show_contours()) then
            call str%append(new_line('a'))
            call str%append("set contour")
        end if

        ! Show colorbar
        ! if (.not.this%get_show_colorbar()) then
        !     call str%append(new_line('a'))
        !     call str%append("unset colorbox")
        ! end if

        ! Lighting
        if (this%get_use_lighting()) then
            call str%append(new_line('a'))
            call str%append("set pm3d lighting primary ")
            call str%append(to_string(this%get_light_intensity()))
            call str%append(" specular ")
            call str%append(to_string(this%get_specular_intensity()))
        end if

        ! Translucent
        if (this%get_transparency() < 1.0 .and. this%get_transparency() > 0.0) then
            call str%append(new_line('a'))
            call str%append("set style fill transparent solid ")
            call str%append(to_string(this%get_transparency()))
        end if

        ! Call the base class to define the rest of the plot commands
        call str%append(new_line('a'))
        call str%append(this%plot_3d%get_command_string())

        ! Output
        x = char(str%to_string())
    end function

! ------------------------------------------------------------------------------
!     module function surf_get_colormap(this) result(x)
!         class(surface_plot), intent(in) :: this
!         class(colormap), pointer :: x
!         x => this%m_colormap
!     end function

! ! --------------------
!     module subroutine surf_set_colormap(this, x, err)
!         ! Arguments
!         class(surface_plot), intent(inout) :: this
!         class(colormap), intent(in) :: x
!         class(errors), intent(inout), optional, target :: err

!         ! Local Variables
!         integer(int32) :: flag
!         class(errors), pointer :: errmgr
!         type(errors), target :: deferr

!         ! Initialization
!         if (present(err)) then
!             errmgr => err
!         else
!             errmgr => deferr
!         end if

!         ! Process
!         if (associated(this%m_colormap)) deallocate(this%m_colormap)
!         allocate(this%m_colormap, stat = flag, source = x)
!         if (flag /= 0) then
!             call errmgr%report_error("surf_set_colormap", &
!                 "Insufficient memory available.", PLOT_OUT_OF_MEMORY_ERROR)
!             return
!         end if
!     end subroutine

! ------------------------------------------------------------------------------
    pure function surf_get_smooth(this) result(x)
        !! Gets a value determining if the plotted surfaces should be
        !! smoothed.
        class(surface_plot), intent(in) :: this
            !! The surface_plot object.
        logical :: x
            !! Returns true if the surface should be smoothed; else, false.
        x = this%m_smooth
    end function

! --------------------
    subroutine surf_set_smooth(this, x)
        !! Sets a value determining if the plotted surfaces should be
        !! smoothed.
        class(surface_plot), intent(inout) :: this
            !! The surface_plot object.
        logical, intent(in) :: x
            !! Set to true if the surface should be smoothed; else, false.
        this%m_smooth = x
    end subroutine

! ------------------------------------------------------------------------------
    pure function surf_get_show_contours(this) result(x)
        !! Gets a value determining if a contour plot should be drawn in
        !! conjunction with the surface plot.
        class(surface_plot), intent(in) :: this
            !! The surface_plot object.
        logical :: x
            !! Returns true if the contour plot should be drawn; else, false to
            !! only draw the surface.
        x = this%m_contour
    end function

! --------------------
    subroutine surf_set_show_contours(this, x)
        !! Sets a value determining if a contour plot should be drawn in
        !! conjunction with the surface plot.
        class(surface_plot), intent(inout) :: this
            !! The surface_plot object.
        logical, intent(in) :: x
            !! Set to true if the contour plot should be drawn; else, false to
            !! only draw the surface.
        this%m_contour = x
    end subroutine

! ------------------------------------------------------------------------------
!     pure module function surf_get_show_colorbar(this) result(x)
!         class(surface_plot), intent(in) :: this
!         logical :: x
!         x = this%m_showColorbar
!     end function

! ! --------------------
!     module subroutine surf_set_show_colorbar(this, x)
!         class(surface_plot), intent(inout) :: this
!         logical, intent(in) :: x
!         this%m_showColorbar = x
!     end subroutine

! ------------------------------------------------------------------------------
    pure function surf_get_use_lighting(this) result(x)
        !! Gets a value indicating if lighting, beyond the ambient
        !! light source, is to be used.
        class(surface_plot), intent(in) :: this
            !! The surface_plot object.
        logical :: x
            !! True if lighting should be used; else, false.
        x = this%m_useLighting
    end function

! --------------------
    subroutine surf_set_use_lighting(this, x)
        !! Sets a value indicating if lighting, beyond the ambient
        !! light source, is to be used.
        class(surface_plot), intent(inout) :: this
            !! The surface_plot object.
        logical, intent(in) :: x
            !! True if lighting should be used; else, false.
        this%m_useLighting = x
    end subroutine

! ------------------------------------------------------------------------------
    pure function surf_get_light_intensity(this) result(x)
        !! Gets the ratio of the strength of the light source relative
        !! to the ambient light.
        class(surface_plot), intent(in) :: this
            !! The surface_plot object.
        real(real32) :: x
            !! The light intensity ratio.
        x = this%m_lightIntensity
    end function

! --------------------
    subroutine surf_set_light_intensity(this, x)
        !! Sets the ratio of the strength of the light source relative
        !! to the ambient light.
        class(surface_plot), intent(inout) :: this
            !! The surface_plot object.
        real(real32), intent(in) :: x
            !! The light intensity ratio.  The value must exist in the
            !! set [0, 1]; else, it will be clipped to lie within the range.
        if (x < 0.0) then
            this%m_lightIntensity = 0.0
        else if (x > 1.0) then
            this%m_lightIntensity = 1.0
        else
            this%m_lightIntensity = x
        end if
    end subroutine

! ------------------------------------------------------------------------------
    pure function surf_get_specular_intensity(this) result(x)
        !! Gets the ratio of the strength of the specular light source
        !! relative to the ambient light.
        class(surface_plot), intent(in) :: this
            !! The surface_plot object.
        real(real32) :: x
            !! The specular light intensity ratio.
        x = this%m_specular
    end function

! --------------------
    subroutine surf_set_specular_intensity(this, x)
        !! Sets the ratio of the strength of the specular light source
        !! relative to the ambient light.
        class(surface_plot), intent(inout) :: this
            !! The surface_plot object.
        real(real32), intent(in) :: x
            !! The specular light intensity ratio.  The value must exist in the 
            !! set [0, 1]; else, it will be clipped to lie within the range.
        if (x < 0.0) then
            this%m_specular = 0.0
        else if (x > 1.0) then
            this%m_specular = 1.0
        else
            this%m_specular = x
        end if
    end subroutine

! ------------------------------------------------------------------------------
    pure function surf_get_transparency(this) result(x)
        !! Gets a factor defining the transparency of plotted surfaces.
        class(surface_plot), intent(in) :: this
            !! The surface_plot object.
        real(real32) :: x
            !! A value existing on the set (0 1] defining the level of
            !! transparency.  A value of 1 indicates a fully opaque surface.
        x = this%m_transparency
    end function

! --------------------
    subroutine surf_set_transparency(this, x)
        !! Sets a factor defining the transparency of plotted surfaces.
        class(surface_plot), intent(inout) :: this
            !! The surface_plot object.
        real(real32), intent(in) :: x
            !! A value existing on the set (0 1] defining the level of
            !! transparency.  A value of 1 indicates a fully opaque surface.  
            !! Any values supplied outside of the set are clipped to fit within
            !! (0 1].
        if (x > 1.0) then
            this%m_transparency = 1.0
        else if (x <= 0.0) then
            this%m_transparency = 0.1
        else
            this%m_transparency = x
        end if
    end subroutine

! ------------------------------------------------------------------------------
end module