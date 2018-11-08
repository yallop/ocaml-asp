#! /usr/bin/python3

# Matthias Klose
# Modified to only exclude module imports from a given module.

# Copyright 2004 Toby Dickenson
#
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject
# to the following conditions:
#
# The above copyright notice and this permission notice shall be included
# in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
# CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
# TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
# SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

import os, sys, pprint
import modulefinder
import imp

class mymf(modulefinder.ModuleFinder):
    def __init__(self,*args,**kwargs):
        self._depgraph = {}
        self._types = {}
        self._last_caller = None
        modulefinder.ModuleFinder.__init__(self, *args, **kwargs)
        
    def import_hook(self, name, caller=None, fromlist=None, level=-1):
        old_last_caller = self._last_caller
        try:
            self._last_caller = caller
            return modulefinder.ModuleFinder.import_hook(self, name, caller,
                                                         fromlist, level)
        finally:
            self._last_caller = old_last_caller
            
    def import_module(self, partnam, fqname, parent):
        m = modulefinder.ModuleFinder.import_module(self,
                                                    partnam, fqname, parent)
        if m is not None and self._last_caller:
            caller = self._last_caller.__name__
            if '.' in caller:
                caller = caller[:caller.index('.')]
            callee =  m.__name__
            if '.' in callee:
                callee = callee[:callee.index('.')]
            #print "XXX last_caller", caller, "MOD", callee
            #self._depgraph.setdefault(self._last_caller.__name__,{})[r.__name__] = 1
            #if caller in ('pdb', 'doctest') or callee in ('pdb', 'doctest'):
            #    print caller, "-->", callee
            if caller != callee:
                self._depgraph.setdefault(caller,{})[callee] = 1
        return m
    
    def find_module(self, name, path, parent=None):
        if parent is not None:
            # assert path is not None
            fullname = parent.__name__+'.'+name
        elif name == "__init__":
            fullname = os.path.basename(path[0])
        else:
            fullname = name
        if self._last_caller:
            caller = self._last_caller.__name__
            if fullname in excluded_imports.get(caller, []):
                #self.msgout(3, "find_module -> Excluded", fullname)
                raise ImportError(name)

        if fullname in self.excludes:
            #self.msgout(3, "find_module -> Excluded", fullname)
            raise ImportError(name)

        if path is None:
            if name in sys.builtin_module_names:
                return (None, None, ("", "", imp.C_BUILTIN))

            path = self.path
        return imp.find_module(name, path)

    def load_module(self, fqname, fp, pathname, file_info):
        suffix, mode, type = file_info
        m = modulefinder.ModuleFinder.load_module(self, fqname,
                                                  fp, pathname, file_info)
        if m is not None:
            self._types[m.__name__] = type
        return m

    def load_package(self, fqname, pathname):
        m = modulefinder.ModuleFinder.load_package(self, fqname,pathname)
        if m is not None:
            self._types[m.__name__] = imp.PKG_DIRECTORY
        return m
 
def reduce_depgraph(dg):
    pass

# guarded imports, which don't need to be included in python-minimal
excluded_imports = {
    'argparse': set(('gettext',)),
    'codecs': set(('encodings',)),
    'collections': set(('cPickle', 'pickle', 'doctest')),
    'compileall': set(('concurrent',)),
    'copy': set(('reprlib',)),
    #'functools': set(('_dummy_thread',)),
    'hashlib': set(('logging', '_hashlib')),
    #'hashlib': set(('_hashlib', '_md5', '_sha', '_sha256','_sha512',)),
    'heapq': set(('doctest',)),
    #'io': set(('_dummy_thread',)),
    'logging': set(('multiprocessing',)),
    'os': set(('nt', 'ntpath', 'os2', 'os2emxpath', 'mac', 'macpath',
               'riscos', 'riscospath', 'riscosenviron')),
    'optparse': set(('gettext',)),
    'pickle': set(('argparse', 'doctest', 'pprint')),
    'platform': set(('ctypes', 'plistlib', 'tempfile')),
    'reprlib': set(('_dummy_thread',)),
    #'socket': set(('_ssl',)),
    '_sitebuiltins': set(('pydoc',)),
    'subprocess': set(('dummy_threading',)),
    'sysconfig': set(('pprint','_osx_support')),
    'tempfile': set(('_dummy_thread', 'shutil')),
    'functools': set(('typing',)),
    'platform': set(('distutils','plistlib')),
    }

def main(argv):
    # Parse command line
    import getopt
    try:
        opts, args = getopt.getopt(sys.argv[1:], "dmp:qx:")
    except getopt.error as msg:
        print(msg)
        return

    # Process options
    debug = 1
    domods = 0
    addpath = []
    exclude = []
    for o, a in opts:
        if o == '-d':
            debug = debug + 1
        if o == '-m':
            domods = 1
        if o == '-p':
            addpath = addpath + a.split(os.pathsep)
        if o == '-q':
            debug = 0
        if o == '-x':
            exclude.append(a)

    path = sys.path[:]
    path = addpath + path

    if debug > 1:
        print(("version:", sys.version))
        print("path:")
        for item in path:
            print(("   ", repr(item)))

    #exclude = ['__builtin__', 'sys', 'os']
    exclude = []
    mf = mymf(path, debug, exclude)
    for arg in args:
        mf.run_script(arg)

    depgraph = reduce_depgraph(mf._depgraph)
    
    pprint.pprint({'depgraph':mf._depgraph, 'types':mf._types})
    
if __name__=='__main__':
    main(sys.argv[1:])
