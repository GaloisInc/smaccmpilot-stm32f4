import re
import os
start_sentinel = re.compile("\s*--BEGIN MESSAGE MODULES")
end_sentinel = re.compile("\s*--END MESSAGE MODULES")

def mods_in_dir(path, prefix):
    for dirname, dirnames, filenames in os.walk(path):
        hss = [ prefix + '.' + (f[:-3]) for f in filenames if f.endswith(".hs") ]
    return hss

def run(cabal_file, module_path):
    out = ''
    state = 'begin'
    with open(cabal_file, 'r') as f:
        for l in f:
            if state == 'begin':
                out += l
                if start_sentinel.match(l) != None:
                    state = 'start sentinel'
            elif state == 'start sentinel':
                if end_sentinel.match(l) != None:
                    out += l
                    state = 'end sentinel'
                else:
                    native = mods_in_dir(module_path+'/Native/Messages',
                                          'SMACCMPilot.Mavlink.Native.Messages')
                    ivory  = mods_in_dir(module_path+'/Ivory/Messages',
                                          'SMACCMPilot.Mavlink.Ivory.Messages')
                    spacer = '                        '
                    out += spacer
                    out += (",\n" + spacer).join(native+ivory)
                    out += ",\n"
            elif state == 'end sentinel':
                out += l
    with open(cabal_file, 'w') as f:
        f.write(out)

if __name__ == "__main__":
    from optparse import OptionParser

    parser = OptionParser("%prog [options] cabalfile")

    parser.add_option("-p", "--path", dest="path", default="./src/SMACCMPilot/Mavlink",
                    help="path to SMACCMPilot.Mavlink haskell modules")

    (opts, args) = parser.parse_args()
    if len(args) == 1:
        cabal_file = args[0]
        module_path = opts.path
        run(cabal_file, module_path)
    else:
        parser.error("Must specify cabal file")
