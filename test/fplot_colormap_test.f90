! Copyright (c) 2017-2026 Jason Christopherson
! Licensed under the MIT License. See the LICENSE file in the project root.

module fplot_colormap_test
    use iso_fortran_env, only : real32
    use fplot_colormap, only : rainbow_colormap
    implicit none
contains
    function test_colormap_accessors() result(rst)
        logical :: rst
        type(rainbow_colormap) :: map

        rst = .false.
        if (map%get_colorbar_thickness() /= 0.02) return
        if (map%get_colorbar_length() /= 0.4) return
        if (any(map%get_axes_size() /= [0.85, 0.85])) return
        if (any(map%get_axes_origin() /= [0.05, 0.05])) return

        call map%set_colorbar_thickness(0.25)
        call map%set_colorbar_length(0.75)
        call map%set_axes_size([0.25, 0.75])
        call map%set_axes_origin([0.75, 0.25])
        if (map%get_colorbar_thickness() /= 0.25) return
        if (map%get_colorbar_length() /= 0.75) return
        if (any(map%get_axes_size() /= [0.25, 0.75])) return
        if (any(map%get_axes_origin() /= [0.75, 0.25])) return

        call map%set_colorbar_thickness(-0.5)
        call map%set_colorbar_length(1.5)
        call map%set_axes_size([-0.5, 1.5])
        call map%set_axes_origin([1.5, -0.5])
        if (map%get_colorbar_thickness() /= 0.0) return
        if (map%get_colorbar_length() /= 1.0) return
        if (any(map%get_axes_size() /= [0.0, 1.0])) return
        if (any(map%get_axes_origin() /= [1.0, 0.0])) return

        call map%set_colorbar_thickness(1.5)
        call map%set_colorbar_length(-0.5)
        if (map%get_colorbar_thickness() /= 1.0) return
        if (map%get_colorbar_length() /= 0.0) return
        rst = .true.
    end function
end module