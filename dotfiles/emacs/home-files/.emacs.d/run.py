#!/usr/bin/env python3
"""init-run.py --- Load Emacs from this dev directory instead of .emacs.d.

This links (or copies) the dev config from this script's own directory together
with the mutable package/state directories from ``$HOME/.emacs.d`` into a
destination directory, then launches Emacs against it with ``--init-directory``.
"""

from __future__ import annotations

import argparse
import os
import shutil
import sys
from itertools import chain
from pathlib import Path
from typing import Iterable, NamedTuple, Sequence

# Directory holding this script -- the source of the dev config.
SCRIPT_DIR: Path = Path(__file__).resolve().parent

# The real Emacs user directory -- the source of mutable package/state.
EMACS_D: Path = Path.home() / ".emacs.d"

# Required targets linked from this script's directory.
REQUIRED: tuple[str, ...] = (
    "early-init.el",
    "init.el",
    "init-mini.el",
    "pew",
    "straight/versions/default.el",
)

# Optional targets linked from $HOME/.emacs.d (straight/ is handled separately
# since straight/versions/default.el must come from the dev directory above).
OPTIONAL: tuple[str, ...] = (
    "snippets",
    "tree-sitter",
)


class Target(NamedTuple):
    """A single link/copy job: ``src`` is placed at ``dst``."""

    src: Path
    dst: Path
    required: bool


def required_targets(dest: Path) -> Iterable[Target]:
    """Targets taken from this script's directory; all must exist."""
    return (Target(SCRIPT_DIR / rel, dest / rel, True) for rel in REQUIRED)


def optional_targets(dest: Path) -> Iterable[Target]:
    """Simple optional targets taken from ``$HOME/.emacs.d``."""
    return (Target(EMACS_D / rel, dest / rel, False) for rel in OPTIONAL)


def straight_targets(dest: Path) -> Iterable[Target]:
    """Everything under ``straight/`` except ``versions/default.el``.

    ``versions/default.el`` is a required target from the dev directory, so it
    is excluded here and sourced from ``$HOME/.emacs.d`` for everything else.
    """
    src_straight = EMACS_D / "straight"
    if not src_straight.exists():
        return ()

    def entry_targets(entry: Path) -> Iterable[Target]:
        if entry.name == "versions":
            return (
                Target(item, dest / "straight" / "versions" / item.name, False)
                for item in entry.iterdir()
                if item.name != "default.el"
            )
        return (Target(entry, dest / "straight" / entry.name, False),)

    return chain.from_iterable(entry_targets(e) for e in src_straight.iterdir())


def collect_targets(dest: Path) -> tuple[Target, ...]:
    """All link/copy jobs, in order, for the given destination."""
    return tuple(
        chain(
            required_targets(dest),
            optional_targets(dest),
            straight_targets(dest),
        )
    )


def missing_required(targets: Sequence[Target]) -> list[Path]:
    """Required source paths that do not exist."""
    return [t.src for t in targets if t.required and not t.src.exists()]


def pending(targets: Iterable[Target]) -> Iterable[Target]:
    """Targets whose destination does not yet exist (idempotency filter)."""
    return (t for t in targets if not (t.dst.is_symlink() or t.dst.exists()))


def warn_missing_optional(targets: Sequence[Target]) -> None:
    """Print a warning for every optional source path that is absent."""
    for src in (t.src for t in targets if not t.required and not t.src.exists()):
        print(f"Warning: optional target missing, skipping: {src}")


def place(target: Target, copy: bool) -> None:
    """Link or copy a single target; caller guarantees ``dst`` is absent."""
    target.dst.parent.mkdir(parents=True, exist_ok=True)

    if not copy:
        print(f"Linking {target.src} -> {target.dst}")
        target.dst.symlink_to(target.src)
        return

    # Targets may themselves be links; copy the real paths.
    real = target.src.resolve()
    print(f"Copying {real} -> {target.dst}")
    copier = shutil.copytree if real.is_dir() else shutil.copy2
    copier(real, target.dst)


def split_argv(argv: Sequence[str]) -> tuple[list[str], list[str]]:
    """Split ``argv`` on the first ``--`` into (our args, emacs passthrough).

    Everything after the first ``--`` is passed verbatim to the emacs
    executable, so it may contain its own flags without confusing argparse.
    """
    if "--" in argv:
        sep = argv.index("--")
        return list(argv[:sep]), list(argv[sep + 1 :])
    return list(argv), []


def parse_args(argv: Sequence[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Load Emacs from this dev directory instead of .emacs.d.",
        epilog="Arguments after -- are passed through to the emacs executable.",
    )
    parser.add_argument(
        "dest",
        nargs="?",
        default="emacs.d",
        help="directory name where linked/copied targets go (default: emacs.d)",
    )
    parser.add_argument(
        "-c",
        "--copy",
        action="store_true",
        help="copy targets instead of linking (real paths are resolved first)",
    )
    return parser.parse_args(argv)


def main(argv: Sequence[str] | None = None) -> None:
    our_argv, emacs_args = split_argv(sys.argv[1:] if argv is None else argv)
    args = parse_args(our_argv)
    dest = Path(args.dest)
    targets = collect_targets(dest)

    missing = missing_required(targets)
    if missing:
        print("Error: required target(s) missing:", file=sys.stderr)
        for src in missing:
            print(f"  {src}", file=sys.stderr)
        sys.exit(1)

    warn_missing_optional(targets)

    for target in pending(targets):
        place(target, args.copy)

    cmd = ["emacs", f"--init-directory={dest.resolve()}", "--debug-init", *emacs_args]
    print("Running:", " ".join(cmd))
    os.execvp("emacs", cmd)


if __name__ == "__main__":
    main()
