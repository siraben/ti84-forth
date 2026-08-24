#!/usr/bin/env python3
"""Check invariants that make the Flash App build safe to install."""

from __future__ import annotations

import re
import sys
from pathlib import Path


APP_PAGE_SIZE = 0x4000
APP_ENTRY_OFFSET = 0x80
WORKSPACE_SIZE = 640
MAX_DICTIONARY = 0x4000
APP_SCRATCH_SIZE = 128
RETURN_STACK_SIZE = 294


def read_labels(path: Path) -> dict[str, int]:
    labels: dict[str, int] = {}
    pattern = re.compile(r"^([A-Z0-9_]+) = \$([0-9A-F]+)$")
    for line in path.read_text().splitlines():
        match = pattern.match(line)
        if match:
            labels[match.group(1)] = int(match.group(2), 16)
    return labels


def require(condition: bool, message: str) -> None:
    if not condition:
        raise SystemExit(message)


def main() -> None:
    if len(sys.argv) != 3:
        raise SystemExit("usage: check_flash_app.py RAW_APP LABEL_FILE")

    raw = Path(sys.argv[1]).read_bytes()
    labels = read_labels(Path(sys.argv[2]))
    required = {
        "APP_START",
        "APP_SCRATCH",
        "APP_VECTORS",
        "DISPATCH_XT",
        "HERE_START",
        "RETURN_STACK_TOP",
        "USERMEM",
    }
    missing = sorted(required - labels.keys())
    require(not missing, f"missing labels: {', '.join(missing)}")

    require(len(raw) <= APP_PAGE_SIZE, "Flash App exceeds one 16 KiB page")
    require(raw[:2] == b"\x80\x0f", "missing Flash App master header")
    require(b"TI84FTH " in raw[:APP_ENTRY_OFFSET], "wrong Flash App name")
    require(raw[0x1A:0x1D] == b"\x80\x81\x00", "expected one-page header")
    require(raw[APP_ENTRY_OFFSET] == 0xC3, "App entry is not JP app_start")
    target = raw[APP_ENTRY_OFFSET + 1] | (raw[APP_ENTRY_OFFSET + 2] << 8)
    require(target == labels["APP_START"], "App entry target disagrees with labels")

    require(
        labels["HERE_START"] == labels["USERMEM"] + WORKSPACE_SIZE,
        "workspace layout changed without updating the checker",
    )
    require(
        labels["RETURN_STACK_TOP"] < labels["HERE_START"],
        "return stack overlaps dictionary",
    )
    require(
        labels["USERMEM"] <= labels["APP_SCRATCH"] < labels["HERE_START"],
        "app scratch buffer is outside the workspace",
    )
    require(
        labels["APP_SCRATCH"] + APP_SCRATCH_SIZE
        <= labels["RETURN_STACK_TOP"] - RETURN_STACK_SIZE,
        "app scratch buffer overlaps the descending return stack",
    )
    require(
        labels["HERE_START"] + MAX_DICTIONARY <= 0x10000,
        "maximum dictionary would wrap the Z80 address space",
    )
    for label in ("APP_START", "APP_VECTORS", "DISPATCH_XT"):
        require(0x4000 <= labels[label] < 0x8000, f"{label} is not in App Flash")

    print(
        f"Flash App OK: {len(raw)} bytes used, "
        f"{APP_PAGE_SIZE - len(raw)} bytes free in page"
    )


if __name__ == "__main__":
    main()
