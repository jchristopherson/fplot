project('fplot', 'fortran')

fcore_proj = subproject('fcore')
fcore_dep = fcore_proj.get_variable('fcore_dep')

ferror_proj = subproject('ferror')
ferror_dep = ferror_proj.get_variable('ferror_dep')

subdir('src')

fplot_lib = static_library('fplot', sources,
  dependencies: [fcore_dep, ferror_dep])

fplot_dep = declare_dependency(link_with: fplot_lib)

subdir('examples')
