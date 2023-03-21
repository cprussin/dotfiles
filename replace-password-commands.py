import json
import subprocess
import sys


def replace_password_commands(o):
    for k, v in o.items():
        if isinstance(v, dict):
            if 'keyCommand' in v:
                out = subprocess.check_output(v['keyCommand'])
                o[k] = out.decode('UTF-8').strip()
            else:
                o[k] = replace_password_commands(o[k])
    return o


def main():
    obj = json.load(open(sys.argv[1]))
    print(json.dumps(replace_password_commands(obj)))


if __name__ == '__main__':
    main()
