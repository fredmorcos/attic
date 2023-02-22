project(
  'gtktest',
  'c',
  version: '0.1.0',
  license: 'GPL3',
  default_options: [
    'buildtype=debugoptimized',
    'c_std=c17',
    'warning_level=3',
  ])

extra_args = ['-Wpedantic']

add_global_arguments(extra_args, language: 'c')

i18n = import('i18n')

app_id = 'com.fredmorcos.GtkTest'
app_path = '/com/fredmorcos/GtkTest/'

prefix = get_option('prefix')
libdir = prefix / get_option('libdir')
pkglibdir = prefix / libdir / meson.project_name()

conf = configuration_data()
conf.set_quoted('PACKAGE_VERSION', '@VCS_TAG@')
conf.set_quoted('GETTEXT_PACKAGE', 'gtktest')
conf.set_quoted('GTKTEST_APPLICATION_ID', app_id)
conf.set('GTKTEST_APPLICATION_ID_RAW', app_id)
conf.set('GTKTEST_APPLICATION_PATH_RAW', app_path)
conf.set_quoted('LOCALEDIR', prefix / get_option('localedir'))
conf.set('BINDIR', prefix / get_option('bindir'))

config_h_in = configure_file(output: 'config.h.in', configuration: conf)
config_h = vcs_tag(input: config_h_in, output: 'config.h')

gtk_ver = 'GDK_VERSION_4_4'
glib_ver = 'GLIB_VERSION_2_70'

add_project_arguments(['-I' + meson.build_root()], language: 'c')

subdir('src')
