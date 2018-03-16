#!/usr/bin/env python3

import sys, os
import re
import subprocess
import yaml
from collections import OrderedDict

import argparse
parser = argparse.ArgumentParser()
parser.add_argument("path")
parser.add_argument("-t", "--type", choices=['local', 'git'])
parser.add_argument("-r", "--remove", action="store_true")
parser.add_argument("-b", "--branch")
args = parser.parse_args()

def ordered_load(stream, Loader=yaml.Loader, object_pairs_hook=OrderedDict):
    class OrderedLoader(Loader):
        pass
    def construct_mapping(loader, node):
        loader.flatten_mapping(node)
        return object_pairs_hook(loader.construct_pairs(node))
    OrderedLoader.add_constructor(
        yaml.resolver.BaseResolver.DEFAULT_MAPPING_TAG,
        construct_mapping)
    return yaml.load(stream, OrderedLoader)

def ordered_dump(data, stream=None, Dumper=yaml.Dumper, **kwds):
    class OrderedDumper(Dumper):
        pass
    def _dict_representer(dumper, data):
        return dumper.represent_mapping(
            yaml.resolver.BaseResolver.DEFAULT_MAPPING_TAG,
            data.items())
    OrderedDumper.add_representer(OrderedDict, _dict_representer)
    return yaml.dump(data, stream, OrderedDumper, **kwds)

def main():
    override_type = 'local' if not args.type else args.type
    path = args.path

    folder_path = os.path.abspath(path)
    pubspec_path = os.path.join(folder_path, 'pubspec.yaml')
    pubspec = yaml.load(open(pubspec_path, 'rb'))
    pub_name = pubspec['name']

    if not args.remove:
        print("Adding %s override for %s…\n" % (override_type, pub_name))
    else:
        print("Removing override for %s…\n" % (pub_name))

    local_pubspec_path = os.path.join('.', 'pubspec.yaml')
    local_pubspec = ordered_load(open(local_pubspec_path, 'rb'))
    local_pubspec_raw = open(local_pubspec_path, 'rb').read().decode('utf-8')
    current_overrides = local_pubspec['dependency_overrides']
    current_overrides_raw = ordered_dump({'dependency_overrides': current_overrides}, default_flow_style=False)

    if not args.remove:
        if override_type == 'git':
            current_overrides.update(get_git_override(folder_path, pub_name))
        elif override_type == 'local':
            current_overrides.update(get_local_override(folder_path, pub_name))
    else:
        current_overrides.pop(pub_name, None)

    new_overrides_raw = ordered_dump({'dependency_overrides': current_overrides}, default_flow_style=False)
    if current_overrides_raw not in local_pubspec_raw:
        print("Please dont use comments, blanks and unnecessary quotes in overrides section")
        print("But you can manualy replace section to:")
        print(new_overrides_raw)
        exit(1)
    local_pubspec_raw = local_pubspec_raw.replace(current_overrides_raw, new_overrides_raw)
    open(local_pubspec_path, 'wb').write(local_pubspec_raw.encode('utf-8'))

def get_local_override(folder_path, pub_name):
    override_local = {pub_name: {'path': folder_path}}
    return override_local

def get_git_override(folder_path, pub_name):
    git_config_path = os.path.join(folder_path, '.git', 'config')
    git_config = open(git_config_path, 'rb').read().decode('utf-8')
    url_regex = re.compile(r'url = (.*)')

    matches = url_regex.findall(git_config)
    git_url = matches[0]
    if not args.branch:
        cwd = os.getcwd()
        os.chdir(folder_path)
        git_ref = subprocess.check_output(['git', 'rev-parse', '--abbrev-ref', 'HEAD']).decode('utf-8').strip()
        os.chdir(cwd)
    else:
        git_ref = args.branch

    override_git = {pub_name: {'git': {'url': git_url, 'ref': git_ref}}}
    return override_git
    

if __name__ == "__main__":
    main()
