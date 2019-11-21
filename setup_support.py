#!/usr/bin/env python
from subprocess import check_output, check_call, CalledProcessError
import argparse
import json
import logging
import os
import re
import sys


def which(prog, paths=None, default=''):
    """Locate executable.

    :param prog: program to find
    :type prog: str
    :param paths: if not None then we use this value instead of PATH to look
        for the executable.
    :type paths: str | None
    :param default: default value to return if not found
    :type default: str | None | T

    :return: absolute path to the program on success, found by searching for an
      executable in the directories listed in the environment variable PATH
      or default value if not found
    :rtype: str | None | T
    """
    def is_exe(fpath):
        return os.path.isfile(fpath) and os.access(fpath, os.X_OK)

    def possible_names(fpath):
        names = [fpath]
        if sys.platform == 'win32':
            names.extend([fpath + ext for ext in
                          os.environ.get('PATHEXT', '').split(';')])
        return names

    fpath, fname = os.path.split(prog)
    if fpath:
        # Full path given, check if executable
        for progname in possible_names(prog):
            if is_exe(progname):
                return progname
    else:
        # Check for all directories listed in $PATH
        if paths is None:
            paths = os.environ["PATH"]

        for pathdir in paths.split(os.pathsep):
            exe_file = os.path.join(pathdir, prog)
            for progname in possible_names(exe_file):
                if is_exe(progname):
                    return progname

    # Not found.
    return default


SUPPORTED_LIBRARY_TYPES = {'static', 'static-pic', 'relocatable'}


def decode_library_types(arg):
    library_types = arg.split(',')
    library_type_set = set(library_types)

    # Make sure that all requested library types are supported
    unsupported = library_type_set - SUPPORTED_LIBRARY_TYPES
    if unsupported:
        raise ValueError('Unsupported library types: {}'
                         .format(', '.join(sorted(unsupported))))

    # Make sure that the given list of library types contains no double entries
    if len(library_types) != len(library_type_set):
        raise ValueError('Library types cannot be requested twice')

    # Return the list, and not the set, to keep the original ordering
    return library_types


class Config(object):
    def __init__(self, args=None):
        self.object_dir = os.path.abspath(os.getcwd())
        self.source_dir = os.path.abspath(os.path.dirname(sys.argv[0]))
        self.json_cache = os.path.join(self.object_dir, 'setup.json')
        if args is not None:
            self.load_cache = args.load_cache
        else:
            self.load_cache = True
        self.cache_loaded = False
        self.init_data(args)

    @classmethod
    def add_arguments(cls, parser):
        parser.add_argument('--jobs', '-j',
                            help='gprbuild parallelism', default='0')
        parser.add_argument('--reconfigure',
                            help='ignore previous setup results',
                            dest='load_cache',
                            default=True,
                            action="store_false")
        parser.add_argument('--target',
                            help='target',
                            default=None)
        parser.add_argument('--prefix',
                            help='installation prefix',
                            default='auto')
        parser.add_argument('--integrated',
                            help='installation in platform specific subdir',
                            default=False,
                            action="store_true")
        parser.add_argument('--library-types',
                            default=['static', 'static-pic', 'relocatable'],
                            type=decode_library_types,
                            help='Comma-separated list of library types to'
                                 ' build. By default, build all of them.')

    def init_data(self, args):
        if self.load_cache and os.path.isfile(self.json_cache):
            with open(self.json_cache, 'r') as fd:
                try:
                    data = json.load(fd)
                except Exception:
                    data = {}
            # Check that the setup is not changed
            if (data.get('source_dir', '') == self.source_dir and
                    data.get('object_dir', '') == self.object_dir):
                self.data = data
                self.cache_loaded = True
                return

        self.data = {}
        self.data['source_dir'] = self.source_dir
        self.data['object_dir'] = self.object_dir

        if args is not None:
            if args.target is None:
                self.data['target'] = self.run('gcc', '-dumpmachine',
                                               grab=True)
            else:
                self.data['target'] = args.target

            self.data['jobs'] = args.jobs
            self.data['integrated'] = args.integrated
            self.data['canonical_target'] = self.run(
                'gprconfig', '--config=ada',
                '--target=%s' % self.data['target'],
                '--mi-show-compilers',
                grab=r' 1 normalized_target:(\S*)')

            default_prefix = os.path.dirname(
                os.path.dirname(
                    self.run('gprconfig', '--config=ada',
                             '--target=%s' % self.data['target'],
                             '--mi-show-compilers',
                             grab=r' 1 path:(.*)')))
            if args.prefix == 'auto':
                self.data['prefix'] = default_prefix
            else:
                self.data['prefix'] = args.prefix

            # The first element in library_types list define the default type
            # of library that will be used. Do not rely on the default set in
            # the project file.
            self.data['library_types'] = args.library_types

    def set_data(self, name, value, sub=None):
        if sub is None:
            self.data[name] = value
        else:
            if sub not in self.data:
                self.data[sub] = {}
            self.data[sub][name] = value

    def save_data(self):
        with open(self.json_cache, 'w') as fd:
            json.dump(self.data, fd)

    def gprcmd(self, cmd, project, *args, **kwargs):
        cmd = list(cmd) + list(args)

        # Handle out of source dir builds
        if self.source_dir != self.object_dir:
            cmd.append('-P%s' % os.path.join(self.source_dir, project))
            cmd.append('--relocate-build-tree')
        else:
            cmd.append('-P%s' % project)

        # Always pass --target so that 'Target value is enforced.
        cmd.append('--target=%s' % self.data['canonical_target'])

        # Pass common variables values computed during configure
        # step and stored in setup.json
        if 'gprbuild' in self.data:
            for name, value in self.data['gprbuild'].items():
                cmd.append('-X%s=%s' % (name, value))

        # Additional scenario variables coming usually from variants
        if 'gpr_vars' in kwargs:
            for name, value in kwargs['gpr_vars'].items():
                cmd.append('-X%s=%s' % (name, value))

        return self.run(*cmd, **kwargs)

    @property
    def prefix(self):
        prefix = self.data['prefix']
        # In integrated mode always install in a subdirectory which is
        # target specific
        if self.data['integrated']:
            prefix = os.path.join(prefix, self.data['canonical_target'])
        return prefix

    def gprbuild(self, project, *args, **kwargs):
        cmd = ['gprbuild', '-j%s' % self.data['jobs'], '-p']
        return self.gprcmd(cmd, project, *args, **kwargs)

    def gprclean(self, project, *args, **kwargs):
        cmd = ['gprclean', '-q']
        return self.gprcmd(cmd, project, *args, **kwargs)

    def gprinstall(self, project, *args, **kwargs):
        # Sources are shared between all variants and put in
        # include/<project_name> where project_name is the base name of
        # project file.
        cmd = ['gprinstall', '-p', '-f',
               '--prefix=%s' % self.prefix,
               '--sources-subdir=include/%s' % project[:-4]]

        return self.gprcmd(cmd, project, *args, **kwargs)

    def gpruninstall(self, project, *args, **kwargs):
        cmd = ['gprinstall', '-p', '-f', '--prefix=%s' % self.prefix]
        cmd.append('--uninstall')
        return self.gprcmd(cmd, project, *args, **kwargs)

    def run(self, *args, **kwargs):
        grab = kwargs.get('grab', False)

        cmd = list(args)

        cmd[0] = which(cmd[0])
        assert cmd[0], "cannot find program: %s" % args[0]

        if grab:
            output = check_output(cmd).decode('utf-8').strip()
            if not isinstance(grab, bool):
                output = re.findall(grab, output)[0]
            return output
        else:
            logging.info('Launch: %s' % " ".join(cmd))
            return check_call(cmd)


class SetupApp(object):

    def __init__(self):
        pass

    def variants(self, config, cmd):
        return [([], {})]

    def build(self, args):
        config = Config(args)
        if not config.cache_loaded:
            self.update_config(config, args)
            config.save_data()

        for gpr_args, gpr_vars in self.variants(config, 'build'):
            config.gprbuild(
                self.project,
                *(gpr_args + args.gpr_opts + ['-gargs']),
                gpr_vars=gpr_vars)
        return 0

    def clean(self, args):
        config = Config()
        if not config.cache_loaded:
            logging.info('nothing to clean')
            return 0

        for gpr_args, gpr_vars in self.variants(config, 'clean'):
            config.gprclean(self.project,
                            *(gpr_args + args.gpr_opts),
                            gpr_vars=gpr_vars)

    def install(self, args):
        config = Config()
        if not config.cache_loaded:
            logging.info('nothing to install')
            return 0
        if args.prefix is not None:
            config.set_data('prefix', args.prefix)

        logging.info('%-26s %s',
                     'Installation directory', config.data['prefix'])
        for gpr_args, gpr_vars in self.variants(config, 'install'):
            config.gprinstall(self.project,
                              *(gpr_args + args.gpr_opts),
                              gpr_vars=gpr_vars)

    def uninstall(self, args):
        config = Config()
        if not config.cache_loaded:
            logging.info('nothing to uninstall')
            return 0
        if args.prefix is not None:
            config.set_data('prefix', args.prefix)

        config.gpruninstall(self.project, *args.gpr_opts)

    def create(self):
        self.main = argparse.ArgumentParser(description=self.description)
        self.parser = self.main.add_subparsers(
            title='commands',
            description='available commands (do ./setup.py CMD --help '
                        'for help on command CMD)')

        # Build command
        self.build_cmd = self.parser.add_parser('build',
                                                help='build %s' % self.name)
        self.build_cmd.add_argument(
            '--gpr-opts',
            nargs=argparse.REMAINDER,
            default=[],
            help="pass remaining arguments to gprbuild")
        Config.add_arguments(self.build_cmd)
        self.build_cmd.set_defaults(command=self.build)

        # Clean command
        self.clean_cmd = self.parser.add_parser(
            'clean',
            help='clean %s' % self.name)
        self.clean_cmd.add_argument(
            '--gpr-opts',
            default=[],
            nargs=argparse.REMAINDER,
            help="pass remaining arguments to gprclean")
        self.clean_cmd.set_defaults(command=self.clean)

        # Install command
        self.install_cmd = self.parser.add_parser(
            'install',
            help='install %s' % self.name)
        self.install_cmd.add_argument(
                '--gpr-opts',
                default=[],
                nargs=argparse.REMAINDER,
                help="pass remaining arguments to gprinstall")
        self.install_cmd.add_argument('--prefix',
                                      help='installation prefix',
                                      default=None)
        self.install_cmd.set_defaults(command=self.install)

        # Uninstall command
        self.uninstall_cmd = self.parser.add_parser(
            'uninstall',
            help='uninstall %s' % self.name)
        self.uninstall_cmd.add_argument(
            '--gpr-opts',
            default=[],
            nargs=argparse.REMAINDER,
            help="pass remaining arguments to gpruninstall")
        self.uninstall_cmd.add_argument('--prefix',
                                        help='un-installation prefix',
                                        default=None)
        self.uninstall_cmd.set_defaults(command=self.uninstall)
        logging.basicConfig(level=logging.DEBUG, format='%(message)s')

    def run(self):
        self.create()
        args = self.main.parse_args()
        try:
            return args.command(args)
        except CalledProcessError as e:
            logging.error('process failed with status: %s', e.returncode)
            return 1
        except AssertionError as e:
            logging.error('requirement missing: %s', e)
            return 1
