#! /usr/bin/env python3
from e3.testsuite import Testsuite
from e3.testsuite.driver import TestDriver
from e3.testsuite.result import TestStatus
from e3.testsuite.process import check_call
from e3.env import Env
from e3.fs import mkdir, cp, ls
import logging
import sys
import os

ROOT_DIR = os.path.dirname(os.path.abspath(__file__))


class DefaultDriver(TestDriver):
    @property
    def project_file(self):
        result = os.path.join(self.test_env["test_dir"], "test.gpr")
        if not os.path.isfile(result):
            result = os.path.join(self.env.root_dir, "support", "test.gpr")
        return result

    @property
    def build_dir(self):
        return self.test_env["working_dir"]

    @property
    def test_source_dir(self):
        return self.test_env["test_dir"]

    @property
    def support_source_dir(self):
        return os.path.join(self.env.root_dir, "support")

    def build(self, prev, slot):
        self.logger = logging.getLogger(f"test.{self.test_env['test_name']}")

        env = {
            "TEST_SOURCES": self.test_source_dir,
            "SUPPORT_SOURCES": self.support_source_dir,
        }

        mkdir(self.build_dir)
        py_files = ls(os.path.join(self.test_source_dir, "*.py"))
        if py_files:
            cp(py_files, self.build_dir)
        check_call(
            self,
            ["gprbuild", "-P", self.project_file, "--relocate-build-tree", "-p"],
            cwd=self.build_dir,
            timeout=300,
            env=env,
            ignore_environ=False,
        )

    def run(self, prev, slot):
        if self.result.status == TestStatus.ERROR:  # means that status was not set
            test_py = os.path.join(self.build_dir, "test.py")
            if os.path.isfile(test_py):
                self.test_process = check_call(
                    self, [sys.executable, "./test.py"], cwd=self.build_dir, timeout=60
                )
            else:
                self.test_process = check_call(
                    self,
                    [os.path.join(self.build_dir, "obj", "test")]
                    + self.test_env.get("test_args", []),
                    timeout=60,
                    cwd=self.build_dir,
                )

    def analyze(self, prev, slot):
        if self.result.status == TestStatus.ERROR:  # means that status was not set
            if "<=== TEST PASSED ===>" not in self.test_process.out:
                self.result.set_status(TestStatus.FAIL)
            else:
                self.result.set_status(TestStatus.PASS)
            self.push_result()

    def add_test(self, dag):
        self.add_fragment(dag, "build")
        self.add_fragment(dag, "run", after=["build"])
        self.add_fragment(dag, "analyze", after=["run"])


class GNATCOLLPython3Test(Testsuite):
    """Testsuite for the bc(1) calculator."""

    test_driver_map = {"default": DefaultDriver}
    default_driver = "default"
    tests_subdir = "tests"


if __name__ == "__main__":
    Env.add_search_path("GPR_PROJECT_PATH", os.path.join(ROOT_DIR, "support"))
    assert (
        "ADA_PYTHON_HOME" in os.environ
    ), "ADA_PYTHON_HOME should point to Python distrib used to build the binding"
    sys.exit(GNATCOLLPython3Test().testsuite_main())
