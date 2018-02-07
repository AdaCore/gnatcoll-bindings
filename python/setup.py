#!/usr/bin/env python
import logging
import sys
import re
import os
import json
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from setup_support import SetupApp

PYTHON_DATA_SCRIPT = """
from distutils.sysconfig import get_config_var, get_python_inc, get_config_vars
import json
result = {'config_vars': get_config_vars(),
          'python_inc': get_python_inc(),
          'python_inc_plat': get_python_inc(plat_specific=True)}
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
    logging.info('  %-24s %s', 'Python version:', python_version)

    # Current python location
    current_prefix = sys.prefix

    # Fetch prefix during the buid process. Some paths of interest might still
    # reference a location used during the Python build process.
    build_prefix = ([sys.prefix] +
                    re.findall(r"'--prefix=([^']+)'",
                               config_vars.get('CONFIG_ARGS', '')))[-1]

    def relocate(path):
        if os.path.isabs(path):
            rel_path = os.path.relpath(path, build_prefix)
            if not rel_path.startswith(os.pardir):
                # The input path is relative to the original build directory
                # replace build prefix by current one.
                return os.path.join(current_prefix, rel_path)
            else:
                # The input path is not relative to original build directory
                # thus return it unchanged.
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

    cflags = "-I" + relocate(python_data['python_inc'])
    if python_data['python_inc'] != python_data['python_inc_plat']:
        cflags += " -I" + relocate(python_data['python_inc_plat'])
    logging.info('  %-24s %s', 'CFLAGS', cflags)

    python_libs = [config_vars[v] for v in ("LIBS", "SYSLIBS", "MODLIBS")
                   if v in config_vars and config_vars[v]]
    python_libs = " ".join(python_libs)

    python_shared_libs = "-L%s -lpython%s %s" % (shared_dir,
                                                 python_version,
                                                 python_libs)
    if os.path.isfile(
            os.path.join(static_dir, 'libpython%s.a' % python_version)):
        python_static_libs = '%s/libpython%s.a %s' % (static_dir,
                                                      python_version,
                                                      python_libs)
    if sys.platform.startswith('linux'):
        # On Linux platform, even when linking with the static libpython,
        # symbols not used by the application itself should be exported so
        # that shared library present in Python can use the Python C API.
        python_static_libs += ' -export-dynamic'
        python_shared_libs += ' -export-dynamic'

    logging.info('  %-24s %s', 'Shared linker flags', python_shared_libs)
    logging.info('  %-24s %s', 'Static linker flags', python_static_libs)

    config.set_data('GNATCOLL_PYTHON_CFLAGS', cflags, sub='gprbuild')
    config.set_data('GNATCOLL_PYTHON_STATIC_LIBS',
                    python_static_libs, sub='gprbuild')
    config.set_data('GNATCOLL_PYTHON_SHARED_LIBS',
                    python_shared_libs, sub='gprbuild')


class GNATCollPython(SetupApp):
    name = 'gnatcoll_python'
    project = 'gnatcoll_python.gpr'
    description = 'GNATColl Python bindings'

    def create(self):
        super(GNATCollPython, self).create()
        self.build_cmd.add_argument(
            '--disable-shared',
            help='if set disable build of shared libraries',
            dest='enable_shared',
            default=True,
            action="store_false")
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

        # Compute list of library types to build
        # The first element in library_types list define the default type of
        # library that will be used. Do not rely on the default set in the
        # project file.
        if args.enable_shared:
            config.set_data('library_types',
                            ['static', 'static-pic', 'relocatable'])
        else:
            config.set_data('library_types',
                            ['static'])
        logging.info('%-26s %s',
                     'Libraries kind', ", ".join(config.data['library_types']))

        # Set library version
        with open(os.path.join(config.source_dir, '..',
                               'version_information'), 'rb') as fd:
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


if __name__ == '__main__':
    app = GNATCollPython()
    sys.exit(app.run())
