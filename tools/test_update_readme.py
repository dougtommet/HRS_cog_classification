import contextlib
import io
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

import update_readme as updater


class UpdateReadmeTests(unittest.TestCase):
    def test_inventory_uses_disk_not_index_and_excludes_archive(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root / "Reports/Archive").mkdir(parents=True)
            current = "Reports/Test_2026-10-18.html"
            (root / current).touch()
            (root / "Reports/Archive/Test_2026-10-17.html").touch()
            config = {"A0": {"outputs": [("Report", "Reports/Test_*.html")]}}
            with patch.object(updater, "ROOT", root), patch.object(
                updater, "ANALYSES", config
            ):
                files = updater.local_outputs()
            self.assertEqual(files, [current])
            self.assertEqual(updater.newest(files, "Reports/Test_*.html"), current)

    def test_latest_plain_render_uses_date_and_time(self):
        files = [
            "Reports/Test_2026-10-17_2300.docx",
            "Reports/Test_2026-10-18.docx",
            "Reports/Test_2026-10-18_0900.docx",
            "Reports/Test_2026-10-18_1000.docx",
            "Reports/Test_2026-10-19_1200_edited.docx",
        ]
        self.assertEqual(
            updater.newest(files, "Reports/Test_*.docx"), files[3]
        )

    def test_fixed_filename_and_missing_report(self):
        self.assertEqual(
            updater.newest(["Figures/fig1.png"], "Figures/fig1.png"),
            "Figures/fig1.png",
        )
        self.assertIsNone(updater.newest([], "Reports/Test_*.html"))

    def test_warns_for_untracked_and_unstaged_but_not_staged(self):
        files = ["Reports/New_2026-10-18.html",
                 "Reports/Changed_2026-10-18.html",
                 "Reports/Staged_2026-10-18.html"]
        config = {"A0": {"outputs": [(str(i), path)
                                    for i, path in enumerate(files)]}}
        output = io.StringIO()
        with patch.object(updater, "ANALYSES", config), patch.object(
            updater, "git",
            side_effect=["\0".join(files[1:]) + "\0", files[1] + "\0"],
        ), contextlib.redirect_stderr(output):
            updater.warn_unstaged_outputs(files)
        self.assertIn("untracked: " + files[0], output.getvalue())
        self.assertIn("unstaged changes: " + files[1], output.getvalue())
        self.assertNotIn(files[2], output.getvalue())

    def test_links_follow_local_selection(self):
        cfg = {"outputs": [("Report", "Reports/Test_*.html")]}
        result = updater.most_recent_lines(
            cfg, ["Reports/Test_2026-10-18.html"]
        )
        self.assertEqual(result, [
            "  - Most recent: " + updater.GITHACK + "Reports/Test_2026-10-18.html"
        ])


if __name__ == "__main__":
    unittest.main()
