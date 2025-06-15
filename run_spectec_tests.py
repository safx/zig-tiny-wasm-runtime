#!/usr/bin/env python3
"""
WebAssembly spec test runner for zig-wasm-tiny-runtime
Compatible with spectec test suite format
"""

import argparse
import subprocess
import sys
from pathlib import Path
from typing import List, Dict, Any


def run_command(cmd: List[str], timeout: int = 30) -> subprocess.CompletedProcess:
    """Run a command with timeout and return the result."""
    try:
        result = subprocess.run(
            cmd, 
            capture_output=True, 
            text=True, 
            timeout=timeout,
            check=False
        )
        return result
    except subprocess.TimeoutExpired:
        raise RuntimeError(f"Command timed out: {' '.join(cmd)}")


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


def convert_wast_to_wasm(wast_file: Path, output_dir: Path) -> List[Path]:
    """Convert .wast file to .wasm files using wat2wasm."""
    wasm_files = []
    
    # Check if wat2wasm is available
    result = run_command(["which", "wat2wasm"])
    if result.returncode != 0:
        raise RuntimeError("wat2wasm not found. Please install wabt toolkit.")
    
    # For now, assume .wast files contain multiple modules that need to be split
    # This is a simplified implementation - real spectec tests may need more sophisticated parsing
    result = run_command(["wat2wasm", str(wast_file), "-o", str(output_dir / f"{wast_file.stem}.wasm")])
    if result.returncode == 0:
        wasm_files.append(output_dir / f"{wast_file.stem}.wasm")
    
    return wasm_files


def run_test_file(spec_test: Path, test_file: Path, output_dir: Path, verbose: bool = False) -> Dict[str, Any]:
    """Run tests from a .wast file using spec_test runner."""
    if not test_file.exists():
        raise FileNotFoundError(f"Test file not found: {test_file}")
    
    print(f"Processing test file: {test_file}")
    
    # Run spec_test directly on the .wast file
    try:
        verbose_flag = ["-v"] if verbose else ["-s"]
        cmd = [str(spec_test)] + verbose_flag + [str(test_file)]
        result = run_command(cmd)
        
        if result.returncode == 0:
            if verbose:
                print(f"  ✓ {test_file.name}")
            return {
                "file": str(test_file),
                "status": "passed",
                "tests_run": 1,
                "tests_passed": 1
            }
        else:
            error_msg = result.stderr.strip() or result.stdout.strip()
            if verbose:
                print(f"  ✗ {test_file.name}: {error_msg}")
            return {
                "file": str(test_file),
                "status": "failed",
                "tests_run": 1,
                "tests_passed": 0,
                "errors": [error_msg]
            }
    except Exception as e:
        error_msg = str(e)
        if verbose:
            print(f"  ✗ {test_file.name}: {error_msg}")
        return {
            "file": str(test_file),
            "status": "error",
            "error": error_msg,
            "tests_run": 0,
            "tests_passed": 0
        }


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
                test_files.extend(file_path.glob("*.wast"))
                test_files.extend(file_path.glob("*.wat"))
    else:
        # Default: look for .wast and .wat files in common locations
        for test_dir in ["spec_test", "test", "tests"]:
            test_path = Path(test_dir)
            if test_path.exists():
                test_files.extend(test_path.glob("*.wast"))
                test_files.extend(test_path.glob("*.wat"))
    
    if not test_files:
        print("No .wast/.wat test files found")
        return 1
    
    # Run tests
    total_tests = 0
    total_passed = 0
    failed_files = []
    
    for test_file in test_files:
        result = run_test_file(spec_test_path, test_file, output_dir, args.verbose)
        
        total_tests += result["tests_run"]
        total_passed += result["tests_passed"]
        
        if result["status"] != "passed":
            failed_files.append(result)
            if args.failfast:
                break
        
        if not args.verbose:
            status_char = "✓" if result["status"] == "passed" else "✗"
            print(f"{status_char} {test_file.name}: {result['tests_passed']}/{result['tests_run']}")
    
    # Summary
    print(f"\nResults: {total_passed}/{total_tests} tests passed")
    
    if failed_files:
        print(f"\nFailed files ({len(failed_files)}):")
        for failed in failed_files:
            print(f"  {failed['file']}")
            if "errors" in failed:
                for error in failed["errors"][:3]:  # Show first 3 errors
                    print(f"    {error}")
        return 1
    
    return 0


if __name__ == "__main__":
    sys.exit(main())