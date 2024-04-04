#!/usr/bin/env python
import logging
import sys
import re
import os
import json
import shutil
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from setup_support import SetupApp, Config

PYTHON_DATA_SCRIPT = """
from distutils.sysconfig import (get_config_var, get_python_inc,
                                 get_config_vars, PREFIX)
import json
result = {'config_vars': get_config_vars(),
          'python_inc': get_python_inc(),
          'python_inc_plat': get_python_inc(plat_specific=True),
          'prefix': PREFIX}
print(json.dumps(result))
"""


def fetch_python_config(config):
    logging.info('Fetch Python information...')
    python_output = config.run(config.data['python_exec'], '-c',
                               PYTHON_DATA_SCRIPT,
                               grab=True)
    python_data = json.loads(python_output)
    config_vars = python_data['config_vars']
    python_version = config_vars["VERSION"]
    python_ldversion = config_vars.get("LDVERSION", python_version)
    logging.info('  %-24s %s', 'Python version:', python_version)

    # Current python location
    current_prefix = python_data['prefix']

    # Fetch prefix during the build process. Some paths of interest might
    # still reference a location used during the Python build process.
    build_prefix = ([sys.prefix] +
                    re.findall(r"'--prefix=([^']+)'",
                               config_vars.get('CONFIG_ARGS', '')))[-1]

    def relocate(path):
        if os.path.isabs(path):
            rel_path = os.path.relpath(path, build_prefix)
            if not rel_path.startswith(os.pardir):
                # If the input path is relative to the original build
                # directory, replace build prefix by the current one.
                return os.path.join(current_prefix, rel_path)
            else:
                # Otherwise, return it unchanged
                return path
        else:
            # The input path is relative so assume it's relative to the current
            # python prefix.
            return os.path.join(current_prefix, path)

    # Retrieve cflags, and linker flags
    static_dir = relocate(config_vars.get('LIBPL', 'libs'))
    logging.info('  %-24s %s', 'Static dir', static_dir)

    shared_dir = relocate(config_vars.get('LIBDIR', '.'))
    logging.info('  %-24s %s', 'Shared dir', shared_dir)

    # Add first relocated include dirs followed by non-relocated version.
    # Indeed, when using venv and maybe virtualenv, includes are not copied.
    cflags = "-I" + relocate(python_data['python_inc'])
    if python_data['python_inc'] != python_data['python_inc_plat']:
        cflags += " -I" + relocate(python_data['python_inc_plat'])
        cflags += " -I" + python_data['python_inc_plat']
    cflags += " -I" + python_data['python_inc']

    logging.info('  %-24s %s', 'CFLAGS', cflags)

    if python_version.startswith('3'):
        # In python 3.x MODLIBS seems to drag in too many libraries
        python_libs = [config_vars[v] for v in ("LIBS", "SYSLIBS")
                       if v in config_vars and config_vars[v]]
    else:
        python_libs = [config_vars[v] for v in ("LIBS", "SYSLIBS", "MODLIBS")
                       if v in config_vars and config_vars[v]]
    python_libs = " ".join(python_libs)

    python_shared_libs = "-L%s -lpython%s %s" % (shared_dir,
                                                 python_ldversion,
                                                 python_libs)
    python_static_libs = python_libs
    libpython_a = os.path.join(
        static_dir,
        config_vars.get('LIBRARY', 'libpython%s.a' % python_version))

    if os.path.isfile(libpython_a):
        config.set_data('GNATCOLL_PYTHON_STATIC_LIB',
                        libpython_a, sub='gprbuild')
    else:
        logging.info('static python library not found')

    if sys.platform.startswith('linux'):
        # On Linux platform, even when linking with the static libpython,
        # symbols not used by the application itself should be exported so
        # that shared library present in Python can use the Python C API.
        python_static_libs += ' -export-dynamic'
        python_shared_libs += ' -export-dynamic'

    logging.info('  %-24s %s', 'Shared linker flags', python_shared_libs)
    logging.info('  %-24s %s', 'Static linker flags', python_static_libs)

    # User does not have the choice between linking with static libpython
    # and shared libpython. If --enable-shared or --enable-framework was
    # passed to Python's configure during Python build, then we should
    # link with the shared libpython, otherwise with the static one.
    # Indeed otherwise some C modules might not work as expected or even
    # crash. On Windows always link with shared version of libpython
    # (if the static is present, this is just an indirection to the shared)
    if '--enable-shared' in config_vars.get('CONFIG_ARGS', '') or \
            '--enable-framework' in config_vars.get('CONFIG_ARGS', '') or \
            sys.platform.startswith('win'):
        logging.info('Force link to shared python library')
        config.set_data('GNATCOLL_PYTHON_LIBS',
                        python_shared_libs, sub='gprbuild')
        config.set_data('GNATCOLL_LIBPYTHON_KIND', 'shared',
                        sub='gprbuild')
    else:
        logging.info('Force link to static python library')
        config.set_data('GNATCOLL_PYTHON_LIBS',
                        python_static_libs, sub='gprbuild')
        config.set_data('GNATCOLL_LIBPYTHON_KIND', 'static',
                        sub='gprbuild')
    config.set_data('GNATCOLL_PYTHON_CFLAGS', cflags, sub='gprbuild')


class GNATCollPython(SetupApp):
    name = 'gnatcoll_python'
    project = 'gnatcoll_python.gpr'
    description = 'GNATColl Python bindings'

    def create(self):
        super(GNATCollPython, self).create()
        self.build_cmd.add_argument(
            '--python-exec',
            help='set python executable location',
            metavar='PATH',
            default=sys.executable)
        self.build_cmd.add_argument(
            '--debug',
            help='build project in debug mode',
            action="store_true",
            default=False)

    def update_config(self, config, args):
        # Fetch python information
        config.set_data('python_exec', args.python_exec)
        fetch_python_config(config)

        logging.info('%-26s %s',
                     'Libraries kind', ", ".join(config.data['library_types']))

        # Set library version
        with open(os.path.join(config.source_dir, '..',
                               'version_information'), 'r') as fd:
            version = fd.read().strip()
        config.set_data('GNATCOLL_VERSION', version, sub='gprbuild')
        logging.info('%-26s %s', 'Version', version)

        # Set build mode
        config.set_data('BUILD', 'DEBUG' if args.debug else 'PROD',
                        sub='gprbuild')
        logging.info('%-26s %s', 'Build mode',
                     config.data['gprbuild']['BUILD'])

        # Set GNATCOLL_OS
        if 'darwin' in config.data['canonical_target']:
            gnatcoll_os = 'osx'
        elif 'windows' in config.data['canonical_target']:
            gnatcoll_os = 'windows'
        else:
            # Assume this is an Unix system
            gnatcoll_os = 'unix'
        config.set_data('GNATCOLL_OS', gnatcoll_os, sub='gprbuild')

    def variants(self, config, cmd):
        result = []
        for library_type in config.data['library_types']:
            gpr_vars = {'LIBRARY_TYPE': library_type,
                        'XMLADA_BUILD': library_type,
                        'GPR_BUILD': library_type}
            if cmd == 'install':
                result.append((['--build-name=%s' % library_type,
                                '--build-var=LIBRARY_TYPE'],
                               gpr_vars))
            else:
                result.append(([], gpr_vars))
        return result

    def install(self, args):
        config = Config()
        has_static_python = "GNATCOLL_PYTHON_STATIC_LIB" in config.data["gprbuild"]
        if has_static_python:
            python_la = config.data["gprbuild"]["GNATCOLL_PYTHON_STATIC_LIB"]
            prefix = config.data["prefix"]
            target = os.path.join(
                "..", "..", "lib", "gnatcoll_python.static",
                os.path.basename(python_la)
            )
            config.set_data("GNATCOLL_PYTHON_STATIC_LIB", target, sub='gprbuild')
            config.save_data()
        super(GNATCollPython, self).install(args)

        if has_static_python:
            # Copy over the libpython*.la
            shutil.copy(
                python_la, os.path.join(prefix, "lib", "gnatcoll_python.static"))


if __name__ == '__main__':
    app = GNATCollPython()
    sys.exit(app.run())
