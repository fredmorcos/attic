from distutils.core import setup

setup(
  name = 'AutoCal',
  version = '0.1.1',
  description = 'Automatic Calendar and Event Manager',
  author = 'Fred Morcos',
  author_email = 'fred.morcos@gmail.com',
  url = 'http://autocal.googlecode.com/',
  packages = ['libautocal', 'libautocalui'],
  package_dir = {'libautocal': 'libautocal', 'libautocalui': 'libautocalui'},
  scripts = ['autocal'],
  license = 'GPLv3',
  data_files = [
    ('share/autocal', ['data/autocal.svg']),
    ('share/doc/autocal', ['README', 'LICENSE', 'ROADMAP', 'ChangeLog',
                           'AUTHORS']),
    ('share/applications', ['autocal.desktop']),
    ('share/pixmaps', ['data/autocal.png'])
  ]
)
