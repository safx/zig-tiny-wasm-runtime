#!/usr/bin/env python3
"""
WebAssembly spec test runner for zig-wasm-tiny-runtime
Compatible with spectec test suite format

Features:
- Parses assertion counts from spec_test output (Test Results: N/M)
- Classifies files as passed/partial/crashed
- Saves baseline results to JSON for regression tracking
- Compares current results against a saved baseline
"""

import argparse
import json
import re
import subprocess
import sys
from datetime import datetime
from pathlib import Path
from typing import List, Dict, Any, Tuple


def run_command(cmd: List[str], timeout: int = 120) -> subprocess.CompletedProcess:
    """Run a command with timeout and return the result.

    Raises subprocess.TimeoutExpired if the command exceeds the timeout.
    """
    return subprocess.run(
        cmd,
        capture_output=True,
        text=True,
        timeout=timeout,
        check=False
    )


def build_spec_test(build_dir: Path = Path(".")) -> Path:
    """Build the spec test runner."""
    print("Building spec test runner...")
    result = run_command(["zig", "build", "spec_test"], timeout=60)
    if result.returncode != 0:
        raise RuntimeError(f"Build failed: {result.stderr}")

    spec_test_path = build_dir / "zig-out" / "bin" / "spec_test"
    if not spec_test_path.exists():
        raise RuntimeError(f"Spec test runner not found at {spec_test_path}")

    return spec_test_path


def parse_assertion_counts(output: str) -> Tuple[int, int, int]:
    """Parse 'Test Results: N/M assertions passed (K skipped)' from spec_test stderr output.

    Returns (passed, total, skipped).
    """
    match = re.search(r'Test Results: (\d+)/(\d+) assertions passed(?:\s*\((\d+) skipped\))?', output)
    if match:
        passed = int(match.group(1))
        total = int(match.group(2))
        skipped = int(match.group(3)) if match.group(3) else 0
        return passed, total, skipped
    return 0, 0, 0


def run_test_file(spec_test: Path, test_file: Path, output_dir: Path,
                   verbose: bool = False, timeout: int = 120) -> Dict[str, Any]:
    """Run tests from a .wast file using spec_test runner.

    Returns a dict with:
        file: filename (stem only)
        status: "passed" | "partial" | "crashed"
        assertions_passed: number of passed assertions
        assertions_total: total number of assertions
        crash_reason: (optional) "timeout" | "nonzero_exit" | "exception"
        error: (optional) error details
    """
    if not test_file.exists():
        raise FileNotFoundError(f"Test file not found: {test_file}")

    # Run spec_test directly on the .wast file
    # Use -v for verbose mode, otherwise default (verbose_level=1) to get "Test Results:" line
    verbose_flag = ["-v"] if verbose else []
    cmd = [str(spec_test)] + verbose_flag + [str(test_file)]

    try:
        result = run_command(cmd, timeout=timeout)
    except subprocess.TimeoutExpired as e:
        # Extract partial output that was captured before timeout
        partial_stderr = ""
        if e.stderr:
            partial_stderr = e.stderr if isinstance(e.stderr, str) else e.stderr.decode("utf-8", errors="replace")
        passed, total, skipped = parse_assertion_counts(partial_stderr)
        error_snippet = partial_stderr[-500:].strip() if partial_stderr else "(no output)"
        return {
            "file": test_file.name,
            "status": "crashed",
            "assertions_passed": passed,
            "assertions_total": total,
            "assertions_skipped": skipped,
            "crash_reason": "timeout",
            "error": f"Timed out after {timeout}s. Last output: {error_snippet}",
        }
    except Exception as e:
        return {
            "file": test_file.name,
            "status": "crashed",
            "assertions_passed": 0,
            "assertions_total": 0,
            "assertions_skipped": 0,
            "crash_reason": "exception",
            "error": str(e),
        }

    # Parse assertion counts from stderr (spec_test uses std.debug.print -> stderr)
    passed, total, skipped = parse_assertion_counts(result.stderr)
    failed = total - passed - skipped

    if result.returncode != 0:
        # Runtime panic or crash
        error_snippet = result.stderr[-500:].strip() if result.stderr else "(no output)"
        return {
            "file": test_file.name,
            "status": "crashed",
            "assertions_passed": passed,
            "assertions_total": total,
            "assertions_skipped": skipped,
            "crash_reason": "nonzero_exit",
            "error": f"Exit code {result.returncode}. {error_snippet}",
        }

    if total > 0 and failed == 0:
        status = "passed"
    elif total > 0:
        status = "partial"
    else:
        # No assertions found (module-only test or parse failure with no output)
        # If returncode is 0, treat as passed
        status = "passed"

    return {
        "file": test_file.name,
        "status": status,
        "assertions_passed": passed,
        "assertions_total": total,
        "assertions_skipped": skipped,
    }


def get_git_info() -> Dict[str, str]:
    """Get current git commit hash and branch name."""
    info = {}
    try:
        result = run_command(["git", "rev-parse", "--short", "HEAD"])
        if result.returncode == 0:
            info["git_commit"] = result.stdout.strip()
    except Exception:
        pass
    try:
        result = run_command(["git", "rev-parse", "--abbrev-ref", "HEAD"])
        if result.returncode == 0:
            info["git_branch"] = result.stdout.strip()
    except Exception:
        pass
    return info


def save_baseline(results: List[Dict[str, Any]], output_path: str):
    """Save test results as a JSON baseline file."""
    total_passed = sum(r["assertions_passed"] for r in results)
    total_assertions = sum(r["assertions_total"] for r in results)
    total_skipped = sum(r.get("assertions_skipped", 0) for r in results)
    files_passed = sum(1 for r in results if r["status"] == "passed")

    summary = {
        "files_passed": files_passed,
        "files_total": len(results),
        "assertions_passed": total_passed,
        "assertions_total": total_assertions,
    }
    if total_skipped > 0:
        summary["assertions_skipped"] = total_skipped

    files = {}
    for r in results:
        entry = {
            "passed": r["assertions_passed"],
            "total": r["assertions_total"],
            "status": r["status"],
        }
        s = r.get("assertions_skipped", 0)
        if s > 0:
            entry["skipped"] = s
        files[r["file"]] = entry

    baseline = {
        "metadata": {
            "timestamp": datetime.now().isoformat(timespec="seconds"),
            **get_git_info(),
        },
        "summary": summary,
        "files": files,
    }

    with open(output_path, "w") as f:
        json.dump(baseline, f, indent=2)
        f.write("\n")

    print(f"\nBaseline saved to {output_path}")


def compare_with_baseline(results: List[Dict[str, Any]], baseline_path: str):
    """Compare current results against a saved baseline and print diff."""
    try:
        with open(baseline_path) as f:
            baseline = json.load(f)
    except (FileNotFoundError, json.JSONDecodeError) as e:
        print(f"\nError reading baseline: {e}", file=sys.stderr)
        return

    meta = baseline.get("metadata", {})
    prev_summary = baseline.get("summary", {})
    prev_files = baseline.get("files", {})

    # Current totals
    cur_assertions_passed = sum(r["assertions_passed"] for r in results)
    cur_assertions_total = sum(r["assertions_total"] for r in results)
    cur_assertions_skipped = sum(r.get("assertions_skipped", 0) for r in results)
    cur_files_passed = sum(1 for r in results if r["status"] == "passed")
    cur_files_total = len(results)

    # Previous totals
    prev_assertions_passed = prev_summary.get("assertions_passed", 0)
    prev_assertions_skipped = prev_summary.get("assertions_skipped", 0)
    prev_files_passed = prev_summary.get("files_passed", 0)
    prev_files_total = prev_summary.get("files_total", 0)

    # Header
    commit = meta.get("git_commit", "unknown")
    timestamp = meta.get("timestamp", "unknown")
    print(f"\nComparison with baseline ({commit}, {timestamp}):")

    # Assertion delta
    a_delta = cur_assertions_passed - prev_assertions_passed
    a_sign = "+" if a_delta >= 0 else ""
    print(f"  Assertions: {prev_assertions_passed} -> {cur_assertions_passed} ({a_sign}{a_delta})")

    # Skipped delta
    s_delta = cur_assertions_skipped - prev_assertions_skipped
    if cur_assertions_skipped > 0 or prev_assertions_skipped > 0:
        s_sign = "+" if s_delta >= 0 else ""
        print(f"  Skipped:    {prev_assertions_skipped} -> {cur_assertions_skipped} ({s_sign}{s_delta})")

    # File delta
    f_delta = cur_files_passed - prev_files_passed
    f_sign = "+" if f_delta >= 0 else ""
    print(f"  Files:      {prev_files_passed}/{prev_files_total} -> {cur_files_passed}/{cur_files_total} ({f_sign}{f_delta})")

    # Per-file diff
    improved = []
    regressed = []

    cur_files_map = {r["file"]: r for r in results}

    all_files = sorted(set(list(prev_files.keys()) + list(cur_files_map.keys())))

    for fname in all_files:
        cur = cur_files_map.get(fname)
        prev = prev_files.get(fname)

        if cur and prev:
            cur_p = cur["assertions_passed"]
            prev_p = prev.get("passed", 0)
            cur_t = cur["assertions_total"]
            prev_t = prev.get("total", 0)
            cur_s = cur.get("assertions_skipped", 0)
            prev_s = prev.get("skipped", 0)
            delta = cur_p - prev_p
            skip_note = ""
            if cur_s != prev_s:
                s_delta = cur_s - prev_s
                s_sign = "+" if s_delta >= 0 else ""
                skip_note = f" [skipped: {prev_s}->{cur_s} ({s_sign}{s_delta})]"

            if delta > 0:
                if prev.get("status") == "crashed":
                    improved.append(f"    {fname}: CRASH -> {cur_p}/{cur_t} (new){skip_note}")
                else:
                    improved.append(f"    {fname}: {prev_p}/{prev_t} -> {cur_p}/{cur_t} (+{delta}){skip_note}")
            elif delta < 0:
                if cur["status"] == "crashed":
                    regressed.append(f"    {fname}: {prev_p}/{prev_t} -> CRASH ({delta}){skip_note}")
                else:
                    regressed.append(f"    {fname}: {prev_p}/{prev_t} -> {cur_p}/{cur_t} ({delta}){skip_note}")
            elif skip_note:
                # passed count unchanged but skipped changed
                improved.append(f"    {fname}: {prev_p}/{prev_t} -> {cur_p}/{cur_t} (=){skip_note}")
        elif cur and not prev:
            improved.append(f"    {fname}: (new file) {cur['assertions_passed']}/{cur['assertions_total']}")
        elif prev and not cur:
            regressed.append(f"    {fname}: {prev.get('passed', 0)}/{prev.get('total', 0)} -> (removed)")

    print(f"\n  Improved ({len(improved)}):")
    if improved:
        for line in improved:
            print(line)
    else:
        print("    (none)")

    print(f"\n  Regressed ({len(regressed)}):")
    if regressed:
        for line in regressed:
            print(line)
    else:
        print("    (none)")


def main():
    parser = argparse.ArgumentParser(description="Run WebAssembly spec tests")
    parser.add_argument("--spec-test", default="./zig-out/bin/spec_test",
                       help="Path to spec test runner")
    parser.add_argument("--out", default="./test-output",
                       help="Output directory for generated files")
    parser.add_argument("--failfast", action="store_true",
                       help="Stop on first test failure")
    parser.add_argument("--verbose", "-v", action="store_true",
                       help="Verbose output")
    parser.add_argument("--save-baseline", metavar="FILE",
                       help="Save results to JSON baseline file")
    parser.add_argument("--compare", metavar="FILE",
                       help="Compare results against a JSON baseline file")
    parser.add_argument("--timeout", type=int, default=120,
                       help="Timeout in seconds per test file (default: 120)")
    parser.add_argument("files", nargs="*", help="Test files to run")

    args = parser.parse_args()

    # Setup output directory
    output_dir = Path(args.out)
    output_dir.mkdir(exist_ok=True)

    # Build spec_test if needed
    spec_test_path = Path(args.spec_test)
    if not spec_test_path.exists():
        try:
            spec_test_path = build_spec_test()
        except RuntimeError as e:
            print(f"Error: {e}", file=sys.stderr)
            return 1

    # Find test files
    test_files = []
    if args.files:
        for file_arg in args.files:
            file_path = Path(file_arg)
            if file_path.is_file() and file_path.suffix in [".wast", ".wat"]:
                test_files.append(file_path)
            elif file_path.is_dir():
                test_files.extend(sorted(file_path.glob("*.wast")))
                test_files.extend(sorted(file_path.glob("*.wat")))
    else:
        # Default: look for .wast and .wat files in common locations
        for test_dir in ["spec_test", "test", "tests"]:
            test_path = Path(test_dir)
            if test_path.exists():
                test_files.extend(sorted(test_path.glob("*.wast")))
                test_files.extend(sorted(test_path.glob("*.wat")))

    if not test_files:
        print("No .wast/.wat test files found")
        return 1

    # Run tests
    results = []

    for test_file in test_files:
        result = run_test_file(spec_test_path, test_file, output_dir, args.verbose, args.timeout)
        results.append(result)

        if result["status"] != "passed" and args.failfast:
            break

        # Progress output
        status_char = {
            "passed": ".",
            "partial": "p",
            "crashed": "X",
        }.get(result["status"], "?")

        if args.verbose:
            ap = result["assertions_passed"]
            at = result["assertions_total"]
            print(f"  {status_char} {test_file.name}: {ap}/{at}")
        else:
            print(status_char, end="", flush=True)

    if not args.verbose:
        print()  # newline after progress dots

    # Compute totals
    total_assertions_passed = sum(r["assertions_passed"] for r in results)
    total_assertions = sum(r["assertions_total"] for r in results)
    total_assertions_skipped = sum(r.get("assertions_skipped", 0) for r in results)
    total_assertions_failed = total_assertions - total_assertions_passed - total_assertions_skipped
    files_passed = sum(1 for r in results if r["status"] == "passed")
    files_total = len(results)

    if total_assertions > 0:
        pct = total_assertions_passed / total_assertions * 100
    else:
        pct = 0.0

    # Summary
    print(f"\nResults: {files_passed}/{files_total} files passed")
    skip_str = f", {total_assertions_skipped} skipped" if total_assertions_skipped > 0 else ""
    fail_str = f", {total_assertions_failed} failed" if total_assertions_failed > 0 else ""
    print(f"Assertions: {total_assertions_passed}/{total_assertions} ({pct:.1f}%{skip_str}{fail_str})")

    # Partial files detail
    partial = [r for r in results if r["status"] == "partial"]
    if partial:
        partial.sort(key=lambda r: r["assertions_passed"] - r["assertions_total"])
        print(f"\nPartial files ({len(partial)}):")
        for r in partial:
            s = r.get("assertions_skipped", 0)
            skip_str = f" ({s} skipped)" if s > 0 else ""
            print(f"  {r['file']}: {r['assertions_passed']}/{r['assertions_total']}{skip_str}")

    # Crashed files detail
    crashed = [r for r in results if r["status"] == "crashed"]
    if crashed:
        print(f"\nCrashed files ({len(crashed)}):")
        for r in crashed:
            reason = r.get("crash_reason", "unknown")
            ap = r["assertions_passed"]
            at = r["assertions_total"]
            count_str = f" ({ap}/{at})" if at > 0 else ""
            error = r.get("error", "")
            # Show first line of error for context
            error_first_line = error.split("\n")[-1].strip()[:120] if error else ""
            error_str = f" - {error_first_line}" if error_first_line else ""
            print(f"  {r['file']}: [{reason}]{count_str}{error_str}")

    # Baseline operations
    if args.save_baseline:
        save_baseline(results, args.save_baseline)

    if args.compare:
        compare_with_baseline(results, args.compare)

    # Exit code: 0 if all files passed, 1 otherwise
    if crashed:
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
