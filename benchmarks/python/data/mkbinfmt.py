# mkbinfmt.py
import imp, sys, os.path

magic = "".join(["\\x%.2x" % c for c in imp.get_magic()])

name = sys.argv[1]
 
binfmt = '''\
package %s
interpreter /usr/bin/%s
magic %s\
''' % (name, name, magic)

#filename = '/usr/share/binfmts/' + name
#open(filename,'w+').write(binfmt)

sys.stdout.write(binfmt)
sys.stdout.write('\n')
