import os

def FlagsForFile(filename, **kwargs):

  flags = [
    '-Wall',
    '-Wextra',
    '-pedantic',
    '-D', 'USE_LLVM',
    '-D', 'USE_LINENOISE',
    '-isystem', '/opt/local/include',
    '-isystem', '/opt/local/libexec/llvm-3.6/include/c++/v1',
    '-isystem', '/opt/local/libexec/llvm-3.6/lib/clang/3.6.0/include',
    '-isystem', '/usr/include'
  ]

  ext = os.path.splitext( filename )[1]

  if ext == '.c':
    flags += ['-xc']
  elif ext == '.cpp':
    flags += ['-xc++']
    flags += ['-std=c++11']
    flags += ['-isystem', '/opt/local/libexec/llvm-3.6/include']
  elif ext == '.h':
    flags += []
  else:
    flags = []

  return {
    'flags':    flags,
    'do_cache': True
  }
