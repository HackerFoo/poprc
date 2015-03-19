import os

def FlagsForFile(filename, **kwargs):

  flags = [
    '-Wall',
    '-Wextra',
    '-pedantic',
    '-I.',
    '-isystem',
    '/usr/include',
    '-isystem',
    '/usr/local/include',
    '-DUSE_LLVM',
    '-DUSE_LINENOISE',
  ]

  ext = os.path.splitext( filename )[1]

  if ext == '.c':
    flags += ['-xc']
  elif ext == '.cpp':
    flags += ['-xc++']
    flags += ['-std=c++11']
    flags += ['-I/opt/local/libexec/llvm-3.6/include']
  elif ext == '.h':
    flags += []
  else:
    flags = []

  return {
    'flags':    flags,
    'do_cache': True
  }
