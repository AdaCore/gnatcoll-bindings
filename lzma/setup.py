#!/usr/bin/env python
import logging
import os
import sys
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from setup_support import SetupApp


class GNATCollLZMA(SetupApp):
    name = 'gnatcoll_lzma'
    project = 'gnatcoll_lzma.gpr'
    description = 'GNATColl LZMA bindings'

    def create(self):
        super(GNATCollLZMA, self).create()
        self.build_cmd.add_argument(
            '--debug',
            help='build project in debug mode',
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

    def variants(self, config, cmd):
        result = []
        for library_type in config.data['library_types']:
            gpr_vars = {'LIBRARY_TYPE': library_type,
                        'GPR_BUILD': library_type}
            if cmd == 'install':
                result.append((['--build-name=%s' % library_type,
                                '--build-var=LIBRARY_TYPE'],
                               gpr_vars))
            else:
                result.append(([], gpr_vars))
        return result


if __name__ == '__main__':
    app = GNATCollLZMA()
    sys.exit(app.run())
