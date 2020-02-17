#!/usr/bin/env python
import logging
import os
import sys
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from setup_support import SetupApp


class GNATCollIconv(SetupApp):
    name = 'gnatcoll_iconv'
    project = 'gnatcoll_iconv.gpr'
    description = 'GNATColl Iconv bindings'

    def create(self):
        super(GNATCollIconv, self).create()
        self.build_cmd.add_argument(
            '--debug',
            help='build project in debug mode',
            action="store_true",
            default=False)
        self.build_cmd.add_argument(
            '--force-libiconv',
            help='if set force use of libiconv. By default on linux system '
            'we rely on libc rather than libiconv',
            action="store_true",
            default=False)

    def update_config(self, config, args):
        logging.info('%-26s %s',
                     'Libraries kind', ", ".join(config.data['library_types']))

        # Set library version
        with open(os.path.join(config.source_dir, '..',
                               'version_information'), 'r') as fd:
            version = fd.read().strip()
        config.set_data('GNATCOLL_VERSION', version, sub='gprbuild')

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

        # Set GNATCOLL_ICONV_OPT
        if 'linux' in config.data['canonical_target'] and \
                not args.force_libiconv:
            config.set_data('GNATCOLL_ICONV_OPT', '', sub='gprbuild')
        else:
            config.set_data('GNATCOLL_ICONV_OPT', '-liconv', sub='gprbuild')

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
    app = GNATCollIconv()
    sys.exit(app.run())
