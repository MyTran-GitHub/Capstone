#!/usr/bin/env python3
"""Finalize K recommendation by combining cohort policy and robustness outputs."""

from __future__ import annotations

import argparse
import json
import logging
from pathlib import Path
from typing import Dict, List, Optional

BASE_DIR = Path(__file__).resolve().parent.parent
K_SELECTION_DIR = BASE_DIR / "data" / "k_selection"

logging.basicConfig(level=logging.INFO, format='[%(levelname)s] %(asctime)s - %(name)s - %(message)s')
logger = logging.getLogger(__name__)


def load_json(path: Path) -> Optional[Dict]:
    if not path.exists():
        return None
    try:
        return json.loads(path.read_text())
    except Exception as exc:
        logger.error("Failed to parse JSON %s: %s", path, exc)
        return None


def to_int_keys(d: Dict) -> Dict[int, int]:
    out = {}
    for k, v in d.items():
        try:
            out[int(k)] = int(v)
        except Exception:
            continue
    return out


def mode_from_freq(freq: Dict[int, int]) -> Optional[int]:
    if not freq:
        return None
    max_count = max(freq.values())
    cands = sorted([k for k, c in freq.items() if c == max_count])
    return cands[0]


def decide_recommendation(
    policy: Optional[Dict],
    robustness: Optional[Dict],
    stability_std_threshold: float = 8.0,
    agreement_threshold: float = 0.60,
) -> Dict:
    # Defaults from available artifacts
    policy_default_k = None
    policy_intersection = []
    if policy:
        policy_default_k = int(policy.get("default_k")) if policy.get("default_k") is not None else None
        policy_intersection = [int(x) for x in policy.get("robustness", {}).get("intersection_plateau_k", [])]

    rob_overall = (robustness or {}).get("overall", {})
    rob_freq = to_int_keys(rob_overall.get("k_frequency", {}))
    rob_mode = int(rob_overall.get("mode_k")) if rob_overall.get("mode_k") is not None else mode_from_freq(rob_freq)
    rob_std = float(rob_overall.get("k_std", float("nan"))) if rob_overall else float("nan")

    n_total = sum(rob_freq.values()) if rob_freq else 0
    mode_share = (rob_freq.get(rob_mode, 0) / n_total) if (rob_mode is not None and n_total > 0) else 0.0

    # Candidate sets
    in_both = []
    if policy_intersection and rob_freq:
        in_both = sorted([k for k in policy_intersection if k in rob_freq])

    # Decision logic
    decision_type = "range"
    rationale = []

    if policy_default_k is not None and rob_mode is not None and policy_default_k == rob_mode:
        if mode_share >= agreement_threshold and (rob_std == rob_std and rob_std <= stability_std_threshold):
            decision_type = "single"
            rationale.append("Policy default K agrees with robustness mode K")
            rationale.append("Robustness mode share and standard deviation indicate stable recommendation")
            chosen_single_k = policy_default_k
        else:
            chosen_single_k = None
            rationale.append("Policy and robustness agree on mode, but robustness dispersion is still moderate")
    else:
        chosen_single_k = None
        rationale.append("Policy and robustness do not strongly agree on a single K")

    if decision_type == "single":
        recommendation = {
            "type": "single",
            "k": int(chosen_single_k),
            "k_range": [int(chosen_single_k), int(chosen_single_k)],
        }
    else:
        if in_both:
            k_min, k_max = min(in_both), max(in_both)
            rationale.append("Using overlap between policy plateau intersection and robustness-supported K values")
        elif policy_intersection:
            k_min, k_max = min(policy_intersection), max(policy_intersection)
            rationale.append("Using policy plateau intersection due to limited robustness overlap")
        elif rob_freq:
            rob_keys = sorted(rob_freq.keys())
            k_min, k_max = min(rob_keys), max(rob_keys)
            rationale.append("Using robustness-supported K range due to missing policy intersection")
        elif policy_default_k is not None:
            k_min = k_max = policy_default_k
            rationale.append("Fallback to policy default K (robustness unavailable)")
        elif rob_mode is not None:
            k_min = k_max = rob_mode
            rationale.append("Fallback to robustness mode K (policy unavailable)")
        else:
            recommendation = {
                "type": "unresolved",
                "k": None,
                "k_range": None,
                "preferred_k": None,
            }
            rationale.append("Insufficient evidence: both policy and robustness artifacts are missing or contain no usable K information")
            return {
                "recommendation": recommendation,
                "diagnostics": {
                    "policy_default_k": policy_default_k,
                    "policy_intersection_plateau_k": policy_intersection,
                    "robustness_mode_k": rob_mode,
                    "robustness_mode_share": mode_share,
                    "robustness_std": rob_std,
                    "robustness_k_frequency": rob_freq,
                },
                "rationale": rationale,
            }

        preferred_candidate = policy_default_k if policy_default_k is not None else (rob_mode if rob_mode is not None else k_min)
        preferred_k = int(min(max(int(preferred_candidate), int(k_min)), int(k_max)))
        recommendation = {
            "type": "range",
            "k": int(k_min),
            "k_range": [int(k_min), int(k_max)],
            "preferred_k": preferred_k,
        }

    return {
        "recommendation": recommendation,
        "diagnostics": {
            "policy_default_k": policy_default_k,
            "policy_intersection_plateau_k": policy_intersection,
            "robustness_mode_k": rob_mode,
            "robustness_mode_share": mode_share,
            "robustness_std": rob_std,
            "robustness_k_frequency": rob_freq,
        },
        "rationale": rationale,
    }


def write_outputs(out_dir: Path, result: Dict) -> None:
    out_dir.mkdir(parents=True, exist_ok=True)

    out_json = out_dir / "final_k_recommendation.json"
    out_json.write_text(json.dumps(result, indent=2))

    rec = result["recommendation"]
    diag = result["diagnostics"]
    lines: List[str] = [
        "# Final K Recommendation",
        "",
        f"Recommendation type: **{rec['type']}**",
    ]

    if rec["type"] == "single":
        lines.append(f"Recommended K: **{rec['k']}**")
    elif rec["type"] == "unresolved":
        lines.append("Recommended K: **unresolved**")
        lines.append("Recommendation could not be determined from available artifacts.")
    else:
        lines.append(f"Recommended K range: **[{rec['k_range'][0]}, {rec['k_range'][1]}]**")
        if "preferred_k" in rec:
            lines.append(f"Preferred K within range: **{rec['preferred_k']}**")

    lines.extend([
        "",
        "## Diagnostics",
        f"- Policy default K: {diag.get('policy_default_k')}",
        f"- Policy intersection plateau K: {diag.get('policy_intersection_plateau_k')}",
        f"- Robustness mode K: {diag.get('robustness_mode_k')}",
        f"- Robustness mode share: {diag.get('robustness_mode_share', 0.0):.3f}",
        f"- Robustness std: {diag.get('robustness_std')}",
        "",
        "## Rationale",
    ])
    for r in result.get("rationale", []):
        lines.append(f"- {r}")

    out_md = out_dir / "final_k_recommendation.md"
    out_md.write_text("\n".join(lines))

    logger.info("Saved final recommendation JSON: %s", out_json)
    logger.info("Saved final recommendation markdown: %s", out_md)


def main() -> int:
    parser = argparse.ArgumentParser(description="Finalize K recommendation from policy + robustness outputs")
    parser.add_argument(
        "--experiment-name",
        type=str,
        default="",
        help="Optional legacy experiment namespace used by K-selection artifacts",
    )
    parser.add_argument(
        "--year",
        type=int,
        default=None,
        help="Optional treated year for year-scoped policy/robustness artifacts",
    )
    parser.add_argument(
        "--policy-json",
        type=str,
        default=None,
        help="Path to default_k_policy.json",
    )
    parser.add_argument(
        "--robustness-json",
        type=str,
        default=None,
        help="Path to k_robustness_summary.json",
    )
    parser.add_argument(
        "--out-dir",
        type=str,
        default=None,
        help="Output directory for final recommendation files",
    )
    parser.add_argument(
        "--stability-std-threshold",
        type=float,
        default=8.0,
        help="Std threshold for single-K recommendation",
    )
    parser.add_argument(
        "--agreement-threshold",
        type=float,
        default=0.60,
        help="Minimum robustness mode share for single-K recommendation",
    )
    args = parser.parse_args()

    if args.year is not None:
        base_scope = (K_SELECTION_DIR / args.experiment_name / str(args.year)) if args.experiment_name else (K_SELECTION_DIR / str(args.year))
    else:
        base_scope = (K_SELECTION_DIR / args.experiment_name) if args.experiment_name else K_SELECTION_DIR

    policy_path = Path(args.policy_json) if args.policy_json else (base_scope / "policy" / "default_k_policy.json")
    robustness_path = Path(args.robustness_json) if args.robustness_json else (base_scope / "robustness" / "k_robustness_summary.json")
    out_dir = Path(args.out_dir) if args.out_dir else (base_scope / "policy")

    policy = load_json(policy_path)
    robustness = load_json(robustness_path)

    if policy is None and robustness is None:
        logger.error("Neither policy nor robustness JSON could be loaded.")
        return 2

    result = decide_recommendation(
        policy=policy,
        robustness=robustness,
        stability_std_threshold=args.stability_std_threshold,
        agreement_threshold=args.agreement_threshold,
    )
    write_outputs(out_dir, result)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
