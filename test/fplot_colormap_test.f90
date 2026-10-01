module fplot_colormap_test
    use iso_fortran_env, only : real32
    use fplot_colormap, only : rainbow_colormap
    implicit none
contains
    function test_colormap_accessors() result(rst)
        logical :: rst
        type(rainbow_colormap) :: map

        rst = .false.
        if (map%get_colorbar_thickness() /= 0.02_real32) return
        if (map%get_colorbar_length() /= 0.4_real32) return
        if (any(map%get_axes_size() /= [0.85_real32, 0.85_real32])) return
        if (any(map%get_axes_origin() /= [0.05_real32, 0.05_real32])) return

        call map%set_colorbar_thickness(0.25_real32)
        call map%set_colorbar_length(0.75_real32)
        call map%set_axes_size([0.25_real32, 0.75_real32])
        call map%set_axes_origin([0.75_real32, 0.25_real32])
        if (map%get_colorbar_thickness() /= 0.25_real32) return
        if (map%get_colorbar_length() /= 0.75_real32) return
        if (any(map%get_axes_size() /= [0.25_real32, 0.75_real32])) return
        if (any(map%get_axes_origin() /= [0.75_real32, 0.25_real32])) return

        call map%set_colorbar_thickness(-0.5_real32)
        call map%set_colorbar_length(1.5_real32)
        call map%set_axes_size([-0.5_real32, 1.5_real32])
        call map%set_axes_origin([1.5_real32, -0.5_real32])
        if (map%get_colorbar_thickness() /= 0.0_real32) return
        if (map%get_colorbar_length() /= 1.0_real32) return
        if (any(map%get_axes_size() /= [0.0_real32, 1.0_real32])) return
        if (any(map%get_axes_origin() /= [1.0_real32, 0.0_real32])) return

        call map%set_colorbar_thickness(1.5_real32)
        call map%set_colorbar_length(-0.5_real32)
        if (map%get_colorbar_thickness() /= 1.0_real32) return
        if (map%get_colorbar_length() /= 0.0_real32) return
        rst = .true.
    end function
end module