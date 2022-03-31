import logging
import os
import sys
import traceback

from e3.fs import mkdir
from e3.os.fs import df
from e3.os.process import Run
from e3.testsuite.driver import TestDriver
from e3.testsuite.process import check_call
from e3.testsuite.result import TestStatus


# Root directory of respectively the testsuite and the gnatcoll
# repository.
TESTSUITE_ROOT_DIR = os.path.dirname(
    os.path.dirname(os.path.abspath(__file__))
)
GNATCOLL_ROOT_DIR = os.path.dirname(TESTSUITE_ROOT_DIR)

PROJECT = {
    "gmp": "gnatcoll_gmp.gpr",
    "iconv": "gnatcoll_iconv.gpr",
    "lzma": "gnatcoll_lzma.gpr",
    "omp": "gnatcoll_omp.gpr",
    "zlib": "gnatcoll_zlib.gpr",
}


def make_gnatcoll(work_dir, build_mode, gcov=False):
    """Build gnatcoll core with or without gcov instrumentation.

    :param work_dir: working directory. gnatcoll is built in `build` subdir
        and installed in `install` subdir
    :type work_dir: str
    :param build_mode: build mode to use in order to build the gnatcoll
        project
    :param gcov: if True, build it with gcov instrumentation
    :type gcov: bool
    :return: a triplet (project path, source path, object path)
    :rtype: (str, str, str)
    :raise AssertError: in case compilation of installation fails
    """
    params = f"({build_mode}, gcov={gcov})"

    # Create build tree structure
    build_dir = os.path.join(work_dir, "build")
    install_dir = os.path.join(work_dir, "install")
    mkdir(build_dir)
    mkdir(install_dir)

    # Compute make invocation
    for binding in PROJECT.keys():
        logging.info(f"Compiling gnatcoll {binding} {params}")
        setup = os.path.join(GNATCOLL_ROOT_DIR, binding, "setup.py")
        obj_dir = os.path.join(build_dir, binding)
        mkdir(obj_dir)

        build_cmd = [sys.executable, setup, "build", "--library-types=static"]
        install_cmd = [
            sys.executable,
            setup,
            "install",
            "--prefix",
            install_dir,
        ]

        if gcov:
            build_cmd += [
                "--gpr-opts",
                "-cargs",
                "-fprofile-arcs",
                "-ftest-coverage",
                "-largs",
                "-lgcov",
                "-gargs",
                f"-XBUILD={build_mode}",
            ]
        else:
            build_cmd += ["--gpr-opts", f"-XBUILD={build_mode}"]

        # Build & Install
        p = Run(build_cmd, cwd=obj_dir)
        assert p.status == 0, "gnatcoll %s build failed:\n%s" % (
            binding,
            p.out,
        )
        logging.debug("build:\n%s", p.out)

        p = Run(install_cmd, cwd=obj_dir)
        assert p.status == 0, "gnatcoll %s installation failed:\n%s" % (
            binding,
            p.out,
        )
        logging.debug("install:\n%s", p.out)

    return (
        os.path.join(install_dir, "share", "gpr"),
        os.path.join(install_dir, "include"),
        build_dir,
    )


def gprbuild(
    driver,
    project_file=None,
    cwd=None,
    gcov=False,
    scenario=None,
    gpr_project_path=None,
    **kwargs
):
    """Launch gprbuild.

    :param project_file: project file to compile. If None, we looks first for
        a test.gpr in the test dir and otherwise fallback on the common
        test.gpr project of the support subdir of the testsuite.
    :type project_file: str
    :param cwd: directory in which to run gprbuild. If None the gprbuild build
        is run in the default working dir for the test.
    :type cwd: str | None
    :param gcov: if True link with gcov libraries
    :type gcov: bool
    :param scenario: scenario variable values
    :type scenario: dict
    :param gpr_project_path: if not None prepent this value to GPR_PROJECT_PATH
    :type gpr_project_path: None | str
    :param kwargs: additional keyword arguements are passed to
        e3.testsuite.process.check_call function
    :return: True on successful completion
    :rtype: bool
    """
    if scenario is None:
        scenario = {}

    if cwd is None:
        cwd = driver.test_env["working_dir"]
    mkdir(cwd)

    if project_file is None:
        project_file = os.path.join(driver.test_env["test_dir"], "test.gpr")
        if not os.path.isfile(project_file):
            project_file = os.path.join(cwd, "test.gpr")
            with open(
                os.path.join(TESTSUITE_ROOT_DIR, "support", "test.gpr"), "r"
            ) as fd:
                content = fd.read()
            with open(project_file, "w") as fd:
                for component in driver.test_env.get("components", []):
                    fd.write('with "%s";\n' % PROJECT[component])
                fd.write(content)
            scenario["TEST_SOURCES"] = driver.test_env["test_dir"]
    scenario["SUPPORT_SOURCES"] = os.path.join(TESTSUITE_ROOT_DIR, "support")

    gprbuild_cmd = [
        "gprbuild",
        "--relocate-build-tree",
        "-p",
        "-P",
        project_file,
    ]
    for k, v in scenario.items():
        gprbuild_cmd.append("-X%s=%s" % (k, v))
    if gcov:
        gprbuild_cmd += [
            "-largs",
            "-lgcov",
            "-cargs",
            "-fprofile-arcs",
            "-ftest-coverage",
            "-g",
        ]

    # Adjust process environment
    env = None
    if gpr_project_path:
        new_gpr_path = gpr_project_path
        if "GPR_PROJECT_PATH" in os.environ:
            new_gpr_path += os.path.pathsep + os.environ["GPR_PROJECT_PATH"]
        env = {"GPR_PROJECT_PATH": new_gpr_path}

    check_call(
        driver, gprbuild_cmd, cwd=cwd, env=env, ignore_environ=False, **kwargs
    )
    # If we get there it means the build succeeded.
    return True


class GNATcollTestDriver(TestDriver):
    """Abstract class to share some common facilities."""

    def should_skip(self):
        """Handle of 'skip' in test.yaml.

        :return: None if the test should not be skipped, a TestStatus
            otherwise.
        :rtype: None | TestStatus
        """
        if "skip" in self.test_env:
            eval_env = {
                "env": self.env,
                "test_env": self.test_env,
                "disk_space": lambda: df(self.env.working_dir),
            }

            for status, expr in self.test_env["skip"]:
                try:
                    if eval(expr, eval_env):
                        return TestStatus[status]
                except Exception:
                    logging.error(traceback.format_exc())
                    return TestStatus.ERROR
        return None
