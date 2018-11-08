
import sys

def get_listed(fn):
    modules = set()
    for line in open(fn).readlines():
        modules.add(line.split()[1])
    return modules

def get_dependencies(fn):
    t = eval(open(fn).read())
    modules = set()
    depgraph = t['depgraph']
    for mod, deps in list(depgraph.items()):
        if mod != '__main__':
            modules.add(mod)
        modules.update(list(deps.keys()))
    return depgraph, modules

def main():
    mods = get_listed(sys.argv[1])
    depgraph, deps = get_dependencies(sys.argv[2])
    print(("Listed modules:", sorted(mods)))
    print("")
    print(("Dependent modules:", sorted(deps)))
    print("")

    missing = deps.difference(mods)
    if missing:
        print("Missing modules in python-minimal:")
        print(missing)
    for m in missing:
        users = []
        for caller, callees in list(depgraph.items()):
            if m in callees:
                users.append(caller)
        print((m, "used in: ", users))
    sys.exit(len(missing))

main()

