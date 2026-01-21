#!/usr/bin/env python3
"""
Generate a balanced shard manifest for integration tests.

Reads test timing data and distributes tests across N shards
to minimize total CI wall-clock time using greedy bin-packing.

Usage:
    generate-shard-manifest.py <timings-file> [--shards N]
    
Example:
    grep "TIMING:" test-run.log > timings.txt
    python3 scripts/generate-shard-manifest.py timings.txt --shards 4 > shards.json
"""

import argparse
import json
import sys
from datetime import datetime
from typing import Dict, List, Tuple


def parse_timings(filename: str) -> Dict[str, float]:
    """
    Parse timing data from test run logs.
    
    Expected format:
        TIMING: /tests/node/01-install.sh 12.345
    
    Returns:
        Dictionary mapping test paths to durations in seconds
    """
    tests = {}
    
    with open(filename) as f:
        for line_num, line in enumerate(f, 1):
            line = line.strip()
            
            if not line.startswith("TIMING:"):
                continue
            
            try:
                parts = line.split()
                if len(parts) < 3:
                    print(f"Warning: Skipping malformed line {line_num}: {line}", 
                          file=sys.stderr)
                    continue
                
                test_path = parts[1]
                duration = float(parts[2])
                
                # Convert absolute path to relative (strip /tests/ prefix)
                test_name = test_path.replace("/tests/", "")
                
                # Store the test
                tests[test_name] = duration
                
            except (ValueError, IndexError) as e:
                print(f"Warning: Error parsing line {line_num}: {e}", file=sys.stderr)
                continue
    
    return tests


def create_shards(tests: Dict[str, float], num_shards: int = 4) -> List[Dict]:
    """
    Distribute tests across shards using greedy bin-packing algorithm.
    
    Uses Longest Processing Time (LPT) approach:
    1. Sort tests by duration (longest first)
    2. Assign each test to the shard with smallest current total
    
    This approximates optimal balance and is fast (O(n log n)).
    
    Args:
        tests: Dictionary of test_name -> duration
        num_shards: Number of shards to create
    
    Returns:
        List of shard dictionaries with test lists and metadata
    """
    # Sort tests by duration (descending) for better balance
    sorted_tests = sorted(tests.items(), key=lambda x: x[1], reverse=True)
    
    # Initialize empty shards
    shards = [{
        "tests": [],
        "total_duration": 0.0
    } for _ in range(num_shards)]
    
    # Greedy assignment: always put next test in least-loaded shard
    for test_name, duration in sorted_tests:
        # Find shard with minimum current duration
        min_shard = min(shards, key=lambda s: s["total_duration"])
        
        # Assign test to this shard
        min_shard["tests"].append(test_name)
        min_shard["total_duration"] += duration
    
    return shards


def calculate_statistics(tests: Dict[str, float], shards: List[Dict]) -> Dict:
    """Calculate balance statistics for the sharding."""
    durations = [shard["total_duration"] for shard in shards]
    
    if not durations:
        return {}
    
    mean_duration = sum(durations) / len(durations)
    max_duration = max(durations)
    min_duration = min(durations)
    
    # Calculate variance (how imbalanced the shards are)
    variance = sum((d - mean_duration) ** 2 for d in durations) / len(durations)
    std_dev = variance ** 0.5
    
    # Coefficient of variation (normalized measure of spread)
    cv = (std_dev / mean_duration * 100) if mean_duration > 0 else 0
    
    return {
        "total_tests": len(tests),
        "total_duration": sum(tests.values()),
        "shard_count": len(shards),
        "mean_shard_duration": round(mean_duration, 2),
        "max_shard_duration": round(max_duration, 2),
        "min_shard_duration": round(min_duration, 2),
        "duration_std_dev": round(std_dev, 2),
        "balance_coefficient": round(cv, 2),  # Lower is better (<10% is good)
    }


def generate_manifest(tests: Dict[str, float], num_shards: int = 4) -> Dict:
    """
    Generate the complete shard manifest.
    
    Returns:
        Dictionary with metadata and shard definitions
    """
    shards = create_shards(tests, num_shards)
    stats = calculate_statistics(tests, shards)
    
    manifest = {
        "metadata": {
            "generated_at": datetime.now().isoformat(),
            "generated_by": "scripts/generate-shard-manifest.py",
            **stats
        }
    }
    
    # Add each shard
    for i, shard in enumerate(shards, 1):
        manifest[f"shard-{i}"] = {
            "tests": shard["tests"],
            "test_count": len(shard["tests"]),
            "total_duration": round(shard["total_duration"], 2)
        }
    
    return manifest


def print_summary(manifest: Dict):
    """Print human-readable summary to stderr."""
    meta = manifest["metadata"]
    
    print("\n=== Shard Manifest Summary ===", file=sys.stderr)
    print(f"Total tests: {meta['total_tests']}", file=sys.stderr)
    print(f"Total duration: {meta['total_duration']:.1f}s", file=sys.stderr)
    print(f"Shards: {meta['shard_count']}", file=sys.stderr)
    print(f"\nShard balance:", file=sys.stderr)
    print(f"  Mean: {meta['mean_shard_duration']:.1f}s", file=sys.stderr)
    print(f"  Range: {meta['min_shard_duration']:.1f}s - {meta['max_shard_duration']:.1f}s", file=sys.stderr)
    print(f"  Std dev: {meta['duration_std_dev']:.1f}s", file=sys.stderr)
    print(f"  Balance coefficient: {meta['balance_coefficient']:.1f}% (lower is better)", file=sys.stderr)
    
    print("\nPer-shard breakdown:", file=sys.stderr)
    for key in sorted(manifest.keys()):
        if key.startswith("shard-"):
            shard = manifest[key]
            print(f"  {key}: {shard['test_count']} tests, {shard['total_duration']:.1f}s", 
                  file=sys.stderr)
    
    if meta['balance_coefficient'] < 10:
        print("\n✓ Excellent balance (variance <10%)", file=sys.stderr)
    elif meta['balance_coefficient'] < 20:
        print("\n✓ Good balance (variance <20%)", file=sys.stderr)
    else:
        print(f"\n⚠ Consider rebalancing (variance {meta['balance_coefficient']:.1f}%)", 
              file=sys.stderr)
    
    print("", file=sys.stderr)


def main():
    parser = argparse.ArgumentParser(
        description="Generate balanced test shard manifest",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__
    )
    parser.add_argument(
        "timings_file",
        help="File containing test timing data (TIMING: path duration)"
    )
    parser.add_argument(
        "--shards",
        type=int,
        default=4,
        help="Number of shards to create (default: 4)"
    )
    parser.add_argument(
        "--quiet",
        action="store_true",
        help="Suppress summary output to stderr"
    )
    
    args = parser.parse_args()
    
    # Validate inputs
    if args.shards < 1:
        print("Error: Number of shards must be at least 1", file=sys.stderr)
        sys.exit(1)
    
    # Parse timings
    try:
        tests = parse_timings(args.timings_file)
    except FileNotFoundError:
        print(f"Error: Timings file not found: {args.timings_file}", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f"Error reading timings file: {e}", file=sys.stderr)
        sys.exit(1)
    
    if not tests:
        print("Error: No test timings found in input file", file=sys.stderr)
        print("Expected format: TIMING: /tests/category/test.sh 12.345", file=sys.stderr)
        sys.exit(1)
    
    # Generate manifest
    manifest = generate_manifest(tests, args.shards)
    
    # Print summary to stderr (unless quiet)
    if not args.quiet:
        print_summary(manifest)
    
    # Output JSON to stdout
    print(json.dumps(manifest, indent=2))


if __name__ == "__main__":
    main()
