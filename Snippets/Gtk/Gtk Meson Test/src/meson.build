gtktest_sources = [
  'gtktest-application.c',
  'gtktest-application.h',
  config_h,
]

gtktest_deps = [
  dependency('gtk4'),
]

gtktest_lib = library(
  'gtktest',
  gtktest_sources,
  dependencies: gtktest_deps,
  install: true,
  install_dir: pkglibdir,
  install_rpath: pkglibdir + ':' + libdir,
)

gtktest_inc = include_directories('.')
gtktest_dep = declare_dependency(
  sources: [config_h],
  include_directories: gtktest_inc,
  link_with: gtktest_lib,
  dependencies: gtktest_deps,
)

executable(
  'gtktest',
  'main.c',
  dependencies: gtktest_dep,
  install: true,
  install_rpath: pkglibdir,
  gui_app: true,
)
