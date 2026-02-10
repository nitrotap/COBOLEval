# Adapted from: https://github.com/openai/human-eval/blob/master/human_eval/evaluate_functional_correctness.py

from typing import List, Optional

import fire

from scripts.data import HUMAN_EVAL
from scripts.evaluation import evaluate_functional_correctness


def evaluate(
    pred_path: str,
    k: str = "1,10,100",
    problem_file: str = HUMAN_EVAL,
) -> dict:
    """
    Evaluates the functional correctness of generated samples.

    Args:
        pred_path: Path to predictions folder (e.g., 'preds/gpt-4')
        k: Comma-separated list of k values for pass@k calculation (default: "1,10,100")
        problem_file: Path to the evaluation dataset (default: CobolEval.jsonl)

    Returns:
        Dictionary with pass@k results
    """
    if isinstance(k, int):
        k_values = [k]
    else:
        k_values = list(map(int, str(k).split(",")))
    results = evaluate_functional_correctness(pred_path, k_values, problem_file)
    print(results)
    return results


def batch_evaluate(
    pred_paths: List[str],
    k: str = "1,10,100",
    problem_file: str = HUMAN_EVAL,
) -> dict:
    """
    Evaluate multiple prediction folders and compare results.

    Args:
        pred_paths: List of paths to prediction folders
        k: Comma-separated list of k values for pass@k calculation
        problem_file: Path to the evaluation dataset

    Returns:
        Dictionary mapping folder names to results
    """
    all_results = {}
    for folder in pred_paths:
        results = evaluate(folder, k, problem_file)
        all_results[folder] = results

    print("\n=== Summary ===")
    for folder, res in all_results.items():
        print(f"{folder}: {res}")

    return all_results


def main() -> None:
    """CLI entry point for COBOLEval evaluation."""
    fire.Fire({
        "evaluate": evaluate,
        "batch": batch_evaluate,
    })


if __name__ == "__main__":
    main()
