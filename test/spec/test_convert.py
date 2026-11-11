import pathlib
import unittest

import convert

HERE = pathlib.Path(__file__).parent


class ConvertTest(unittest.TestCase):
    def test_gives_the_reviewed_test_cases(self):
        self.assertEqual(
            convert.convert(HERE / "commonmark-0.31.2.json", HERE / "gfm-27d942c.txt",
                            HERE / "cjk-friendly-e9762c3.json"),
            (HERE / "expected.eld").read_text(encoding="utf-8"))


if __name__ == "__main__":
    unittest.main()
