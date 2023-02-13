import json
import subprocess
import sys


def replace_password_commands(o):
    if 'passwordCommand' in o:
        out = subprocess.check_output(o['passwordCommand'])
        o['password'] = out.decode('UTF-8').strip()
        del o['passwordCommand']
    for k, v in o.items():
        if isinstance(v, dict):
            o[k] = replace_password_commands(o[k])
    return o


def main():
    obj = json.load(open(sys.argv[1]))
    print(json.dumps(replace_password_commands(obj)))


if __name__ == '__main__':
    main()
