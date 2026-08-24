#!/usr/bin/env python3
"""Convert a Forth source file into a TI-83+/84+ program variable."""

import argparse
from pathlib import Path
import shutil
import subprocess


def source_to_asm(source: Path) -> Path:
    asm_path = source.with_suffix(".asm")
    data = source.read_bytes() + b"\0"
    lines = []
    for offset in range(0, len(data), 16):
        chunk = data[offset : offset + 16]
        lines.append(".db " + ", ".join(f"${byte:02x}" for byte in chunk))
    asm_path.write_text("\n".join(lines) + "\n", encoding="ascii")
    return asm_path


def assemble(asm_path: Path, spasm: str) -> Path:
    executable = shutil.which(spasm)
    if executable is None:
        raise SystemExit(f"assembler not found on PATH: {spasm}")
    output = asm_path.with_suffix(".8xp")
    output.unlink(missing_ok=True)
    result = subprocess.run([executable, str(asm_path), str(output)], check=False)
    # spasm-ng returns 1 for its expected "not assembly code" warning when
    # exporting an arbitrary source byte stream, even though the link file is
    # complete.  Accept that case only after validating the TI link checksum.
    valid_output = False
    if output.exists():
        data = output.read_bytes()
        if len(data) >= 57 and data.startswith(b"**TI83F*"):
            section_size = int.from_bytes(data[53:55], "little")
            section = data[55 : 55 + section_size]
            checksum = data[55 + section_size : 57 + section_size]
            valid_output = (
                len(section) == section_size
                and len(checksum) == 2
                and sum(section) & 0xFFFF == int.from_bytes(checksum, "little")
            )
    if result.returncode != 0 and not valid_output:
        raise subprocess.CalledProcessError(result.returncode, result.args)
    return output


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Convert Forth source to a TI-83+/84+ program variable."
    )
    parser.add_argument("file_path", type=Path, help="Forth source file")
    parser.add_argument(
        "--assemble", action="store_true", help="also assemble the generated .asm"
    )
    parser.add_argument(
        "--spasm", default="spasm", help="assembler command (default: spasm)"
    )
    args = parser.parse_args()

    stem = args.file_path.stem
    if not (1 <= len(stem) <= 8 and stem.isascii() and stem.isalnum()):
        parser.error("source basename must be 1-8 ASCII alphanumeric characters")

    asm_path = source_to_asm(args.file_path)
    if not args.assemble:
        print(f"Assembly file created: {asm_path}")
        return

    output = assemble(asm_path, args.spasm)
    asm_path.unlink()
    print(f"Program created: {output}")


if __name__ == "__main__":
    main()
