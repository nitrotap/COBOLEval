use std::collections::HashMap;

/// Estimate pass@k for a single problem.
/// n = total samples, c = correct samples, k = attempts.
///
/// Formula: pass@k = 1 - C(n-c, k) / C(n, k)
/// Computed as: 1 - prod_{i=1..k} (n-c-k+i)/(n-k+i)
pub fn estimate_pass_at_k(n: usize, c: usize, k: usize) -> f64 {
    if n.saturating_sub(c) < k {
        return 1.0;
    }
    1.0 - (1..=k).fold(1.0_f64, |acc, i| {
        acc * (n - c - k + i) as f64 / (n - k + i) as f64
    })
}

/// Compute mean pass@k across all tasks for each k value.
///
/// `results` is a Vec of (n_samples, n_correct) per task.
/// `ks` is the list of k values to compute.
pub fn compute_pass_at_k(
    results: &[(usize, usize)],
    ks: &[usize],
) -> HashMap<String, f64> {
    let mut pass_at_k = HashMap::new();

    for &k in ks {
        // Only compute if all tasks have at least k samples
        if results.iter().all(|(n, _)| *n >= k) {
            let mean: f64 = results
                .iter()
                .map(|(n, c)| estimate_pass_at_k(*n, *c, k))
                .sum::<f64>()
                / results.len() as f64;
            pass_at_k.insert(format!("pass@{}", k), mean);
        }
    }

    pass_at_k
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pass_at_k_all_correct() {
        assert_eq!(estimate_pass_at_k(10, 10, 1), 1.0);
    }

    #[test]
    fn test_pass_at_k_none_correct() {
        assert_eq!(estimate_pass_at_k(10, 0, 1), 0.0);
    }

    #[test]
    fn test_pass_at_k_half_correct() {
        let result = estimate_pass_at_k(10, 5, 1);
        assert!((result - 0.5).abs() < 0.01);
    }

    #[test]
    fn test_pass_at_k_overflow_protection() {
        // When n-c < k, should return 1.0
        assert_eq!(estimate_pass_at_k(5, 4, 3), 1.0);
    }

    #[test]
    fn test_compute_pass_at_k() {
        let results = vec![(10, 5), (10, 3), (10, 7)];
        let ks = vec![1];
        let pass = compute_pass_at_k(&results, &ks);
        assert!(pass.contains_key("pass@1"));
        let val = pass["pass@1"];
        assert!(val > 0.0 && val < 1.0);
    }
}
