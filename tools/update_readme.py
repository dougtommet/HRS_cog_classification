#!/usr/bin/env python3
"""Refresh the auto-maintained lines of README.md and References/instructions.md.

For each analysis section (### Analysis A#: ...) in README.md this rewrites:
  - the "Most recent" link(s) under "Final rendered output", pointing at the
    newest dated report that is tracked in git (committed or staged);
  - "Date last updated", from the latest commit touching the analysis's
    source files (today, if any of them has uncommitted changes).

Everything else in the README is hand-written and left alone. The finished
Project Overview section is then copied into References/instructions.md.

Usage (from the project root):
  python3 tools/update_readme.py          # rewrite files in place
  python3 tools/update_readme.py --check  # exit 1 if either file is stale

Links assume the files will be pushed to BRANCH on GitHub. Standard library only.
"""

import argparse
import datetime
import difflib
import re
import subprocess
import sys
import urllib.parse
from fnmatch import fnmatch
from pathlib import Path

OWNER = "dougtommet"
REPO = "HRS_cog_classification"
BRANCH = "main"

ROOT = Path(__file__).resolve().parent.parent
README = ROOT / "README.md"
INSTRUCTIONS = ROOT / "References" / "instructions.md"

# outputs:  (label, glob) pairs; the newest tracked match of each glob is linked.
#           Labels are shown only when an analysis has more than one output.
# fallback: the full "Most recent" line used when no output is tracked yet.
# sources:  git pathspecs whose commits define "Date last updated".
ANALYSES = {
    "A0": {
        "outputs": [("Report", "Reports/A0_HRS_data_processing_*.html")],
        "fallback": "no driver render committed yet. Pre-driver render (in `R/`): {githack}R/A0_000-Main_control.html",
        "sources": ["R/A0_*.R", "R/A0_*.qmd", "Analysis0_Driver.R"],
    },
    "A1": {
        "outputs": [("Report", "Reports/HRS_cognition_*.html")],
        "sources": ["R/A1_*.R", "R/A1_*.qmd", "R/0[0-3]*.R", "R/0[0-3]*.qmd", "R/_0*.qmd", "Analysis1_Driver.R"],
    },
    "A2": {
        "outputs": [
            ("Manuscript", "Reports/MS_Main_*.docx"),
            ("Tables and figures", "Reports/MS_Tab_Fig_Apndx_*.docx"),
            ("Appendix 1", "Reports/MS_Appendix_1_*.docx"),
            ("Appendix 2", "Reports/MS_Appendix_2_*.docx"),
            ("Appendix 3", "Reports/MS_Appendix_3_*.docx"),
        ],
        "sources": ["R/MS_MAIN/*.R", "R/MS_MAIN/*.qmd", "Analysis2_Driver.R"],
    },
    "A3": {
        "outputs": [("Report", "Reports/PMM_Analysis_Report_*.html")],
        "sources": ["R/PMM_*.R", "R/PMM_*.qmd", "Analysis3_Driver.R"],
    },
    "A4": {
        "outputs": [(f"Figure {i}", f"Figures/Stata_Ad_Hoc_fig{i}.png") for i in range(1, 5)],
        "fallback": "no driver output committed. Legacy figures: {githack}Stata/fig1.png (also `fig2.png`–`fig4.png`)",
        "sources": ["Stata/*.do", "Analysis4_Driver.do"],
    },
    "A5": {
        "outputs": [("Report", "Reports/PsiMCA25_Sharing_*.html")],
        "sources": ["R/ΨMCA25-Sharing.qmd", "Analysis5_Driver.R"],
    },
    "A6": {
        "outputs": [("Report", "Reports/tmp_pmm_110_comparison_*.html")],
        "sources": ["R/tmp_*", "Analysis6_Driver.R"],
    },
    "A7": {
        "outputs": [("Report", "Reports/A7_HRS_cog_classification_*.html")],
        "sources": ["R/A7_*.R", "R/A7_*.qmd", "Analysis7_Driver.R"],
    },
    "A8": {
        "outputs": [("Slides", "Reports/Slides_A7summary_*.html")],
        "sources": ["R/A8_*.R", "R/A8_*.qmd", "Analysis8_Driver.R"],
    },
    "A9": {
        "outputs": [("Slides", "Reports/Slides2603_*.html")],
        "sources": ["R/Slides2603_*.R", "R/Slides2603_*.qmd", "Slides2603_Driver.R"],
    },
}

GITHACK = f"https://raw.githack.com/{OWNER}/{REPO}/{BRANCH}/"
RAW = f"https://raw.githubusercontent.com/{OWNER}/{REPO}/{BRANCH}/"
OFFICE_VIEWER = "https://view.officeapps.live.com/op/view.aspx?src="
DATE_RE = re.compile(r"\d{4}-\d{2}-\d{2}")


def git(*args):
    cmd = ["git", *args]
    result = subprocess.run(cmd, cwd=ROOT, capture_output=True, text=True)
    # An x86_64 Python on Apple Silicon runs /usr/bin/git under Rosetta, where
    # the xcrun shim fails; retry natively.
    if result.returncode != 0 and "xcrun" in result.stderr and sys.platform == "darwin":
        result = subprocess.run(["arch", "-arm64", *cmd], cwd=ROOT, capture_output=True, text=True)
    if result.returncode != 0:
        sys.exit(f"git {' '.join(args)} failed:\n{result.stderr}")
    return result.stdout


def link(path):
    if path.endswith(".docx"):
        return OFFICE_VIEWER + urllib.parse.quote(RAW + path, safe="")
    return GITHACK + path


def newest(tracked, pattern):
    """Newest tracked file matching pattern, by the last date in its name."""
    matches = [f for f in tracked if fnmatch(f, pattern)]
    if not matches:
        return None

    def key(f):
        dates = DATE_RE.findall(Path(f).name)
        return (dates[-1] if dates else "", f)

    return max(matches, key=key)


def most_recent_lines(cfg, tracked):
    found = [(label, f) for label, pattern in cfg["outputs"] if (f := newest(tracked, pattern))]
    if not found:
        fallback = cfg.get("fallback", "none committed yet.")
        return ["  - Most recent: " + fallback.format(githack=GITHACK)]
    if len(cfg["outputs"]) == 1:
        return ["  - Most recent: " + link(found[0][1])]
    heading = "  - Most recent"
    if all(f.endswith(".docx") for _, f in found):
        heading += " (opens in the Office Online viewer)"
    return [heading + ":"] + [f"    - [{label}]({link(f)})" for label, f in found]


def last_updated(cfg):
    if git("status", "--porcelain", "--", *cfg["sources"]).strip():
        return datetime.date.today().isoformat()
    return git("log", "-1", "--format=%ad", "--date=short", "--", *cfg["sources"]).strip() or "unknown"


def update_section(section, cfg, tracked):
    lines = section.split("\n")
    out, i = [], 0
    while i < len(lines):
        line = lines[i]
        if line.startswith("  - Most recent"):
            out.extend(most_recent_lines(cfg, tracked))
            i += 1
            while i < len(lines) and lines[i].startswith("    - "):
                i += 1
            continue
        if line.startswith("- Date last updated:"):
            line = "- Date last updated: " + last_updated(cfg)
        out.append(line)
        i += 1
    return "\n".join(out)


def update_readme(text, tracked):
    parts = re.split(r"(?m)^(?=### Analysis A\d+:)", text)
    for n, part in enumerate(parts):
        m = re.match(r"### Analysis (A\d+):", part)
        if m and m.group(1) in ANALYSES:
            parts[n] = update_section(part, ANALYSES[m.group(1)], tracked)
    return "".join(parts)


def overview(text):
    start = text.index("## Project Overview")
    return start, text.index("## Workflow Rules", start)


def sync_instructions(readme, instructions):
    r0, r1 = overview(readme)
    i0, i1 = overview(instructions)
    return instructions[:i0] + readme[r0:r1] + instructions[i1:]


def main():
    parser = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    parser.add_argument("--check", action="store_true", help="report stale files without writing")
    args = parser.parse_args()

    tracked = git("ls-files").splitlines()
    old_readme = README.read_text()
    old_instructions = INSTRUCTIONS.read_text()
    new_readme = update_readme(old_readme, tracked)
    new_instructions = sync_instructions(new_readme, old_instructions)

    stale = []
    for path, old, new in [(README, old_readme, new_readme), (INSTRUCTIONS, old_instructions, new_instructions)]:
        if old == new:
            continue
        rel = str(path.relative_to(ROOT))
        stale.append(rel)
        if args.check:
            sys.stdout.writelines(difflib.unified_diff(
                old.splitlines(True), new.splitlines(True), rel, rel + " (updated)"
            ))
        else:
            path.write_text(new)

    if not stale:
        print("README.md and References/instructions.md are up to date.")
    elif args.check:
        print("\nStale: " + ", ".join(stale) + ". Run python3 tools/update_readme.py")
        return 1
    else:
        print("Updated: " + ", ".join(stale))
    return 0


if __name__ == "__main__":
    sys.exit(main())
