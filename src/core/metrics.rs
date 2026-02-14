//! Performance metrics for incremental parsing
//!
//! This module provides atomic counters and timing infrastructure to track
//! incremental vs full parse performance. Metrics are zero-overhead when
//! the LUANEXT_LSP_PARSE_STATS environment variable is not set.

use std::sync::atomic::{AtomicU64, AtomicUsize, Ordering};
use std::time::Duration;

/// Performance metrics for incremental parsing
///
/// Uses atomic operations for thread-safe metric collection without locks.
/// All metrics are cumulative across the lifetime of the ParseMetrics instance.
#[derive(Debug)]
pub struct ParseMetrics {
    incremental_parse_count: AtomicUsize,
    full_parse_count: AtomicUsize,
    total_incremental_time_micros: AtomicU64,
    total_full_parse_time_micros: AtomicU64,
}

impl ParseMetrics {
    /// Create a new ParseMetrics instance with all counters at zero
    pub fn new() -> Self {
        Self {
            incremental_parse_count: AtomicUsize::new(0),
            full_parse_count: AtomicUsize::new(0),
            total_incremental_time_micros: AtomicU64::new(0),
            total_full_parse_time_micros: AtomicU64::new(0),
        }
    }

    /// Record an incremental parse operation
    ///
    /// Atomically increments the incremental parse counter and adds the
    /// duration to the cumulative time.
    pub fn record_incremental_parse(&self, duration: Duration) {
        self.incremental_parse_count.fetch_add(1, Ordering::Relaxed);
        self.total_incremental_time_micros
            .fetch_add(duration.as_micros() as u64, Ordering::Relaxed);
    }

    /// Record a full parse operation
    ///
    /// Atomically increments the full parse counter and adds the
    /// duration to the cumulative time.
    pub fn record_full_parse(&self, duration: Duration) {
        self.full_parse_count.fetch_add(1, Ordering::Relaxed);
        self.total_full_parse_time_micros
            .fetch_add(duration.as_micros() as u64, Ordering::Relaxed);
    }

    /// Get a snapshot of current statistics
    ///
    /// Calculates averages and ratios from the atomic counters. This is
    /// a non-atomic snapshot, so values may be slightly inconsistent if
    /// parses are happening concurrently.
    pub fn get_stats(&self) -> ParseStats {
        let inc_count = self.incremental_parse_count.load(Ordering::Relaxed);
        let full_count = self.full_parse_count.load(Ordering::Relaxed);
        let inc_time = self.total_incremental_time_micros.load(Ordering::Relaxed);
        let full_time = self.total_full_parse_time_micros.load(Ordering::Relaxed);

        ParseStats {
            incremental_count: inc_count,
            full_count: full_count,
            avg_incremental_time_ms: if inc_count > 0 {
                (inc_time as f64 / inc_count as f64) / 1000.0
            } else {
                0.0
            },
            avg_full_parse_time_ms: if full_count > 0 {
                (full_time as f64 / full_count as f64) / 1000.0
            } else {
                0.0
            },
            incremental_ratio: if inc_count + full_count > 0 {
                inc_count as f64 / (inc_count + full_count) as f64
            } else {
                0.0
            },
        }
    }

    /// Log metrics if LUANEXT_LSP_PARSE_STATS environment variable is set
    ///
    /// This is a no-op if the environment variable is not set, making metrics
    /// zero-overhead in production unless explicitly enabled.
    pub fn maybe_log_stats(&self) {
        if std::env::var("LUANEXT_LSP_PARSE_STATS").is_ok() {
            let stats = self.get_stats();
            tracing::info!(
                "Parse metrics: incremental={} ({:.2}ms avg), full={} ({:.2}ms avg), ratio={:.1}%",
                stats.incremental_count,
                stats.avg_incremental_time_ms,
                stats.full_count,
                stats.avg_full_parse_time_ms,
                stats.incremental_ratio * 100.0
            );
        }
    }
}

impl Default for ParseMetrics {
    fn default() -> Self {
        Self::new()
    }
}

/// Snapshot of parse performance statistics
#[derive(Debug, Clone, PartialEq)]
pub struct ParseStats {
    /// Number of incremental parses performed
    pub incremental_count: usize,
    /// Number of full parses performed
    pub full_count: usize,
    /// Average time per incremental parse in milliseconds
    pub avg_incremental_time_ms: f64,
    /// Average time per full parse in milliseconds
    pub avg_full_parse_time_ms: f64,
    /// Ratio of incremental to total parses (0.0 to 1.0)
    pub incremental_ratio: f64,
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::Duration;

    #[test]
    fn test_new_metrics_are_zero() {
        let metrics = ParseMetrics::new();
        let stats = metrics.get_stats();

        assert_eq!(stats.incremental_count, 0);
        assert_eq!(stats.full_count, 0);
        assert_eq!(stats.avg_incremental_time_ms, 0.0);
        assert_eq!(stats.avg_full_parse_time_ms, 0.0);
        assert_eq!(stats.incremental_ratio, 0.0);
    }

    #[test]
    fn test_record_incremental_parse() {
        let metrics = ParseMetrics::new();
        metrics.record_incremental_parse(Duration::from_micros(1000));
        metrics.record_incremental_parse(Duration::from_micros(2000));

        let stats = metrics.get_stats();
        assert_eq!(stats.incremental_count, 2);
        assert_eq!(stats.full_count, 0);
        assert_eq!(stats.avg_incremental_time_ms, 1.5); // (1000 + 2000) / 2 / 1000
        assert_eq!(stats.incremental_ratio, 1.0);
    }

    #[test]
    fn test_record_full_parse() {
        let metrics = ParseMetrics::new();
        metrics.record_full_parse(Duration::from_micros(5000));

        let stats = metrics.get_stats();
        assert_eq!(stats.incremental_count, 0);
        assert_eq!(stats.full_count, 1);
        assert_eq!(stats.avg_full_parse_time_ms, 5.0);
        assert_eq!(stats.incremental_ratio, 0.0);
    }

    #[test]
    fn test_mixed_parses() {
        let metrics = ParseMetrics::new();
        metrics.record_incremental_parse(Duration::from_micros(1000));
        metrics.record_incremental_parse(Duration::from_micros(1000));
        metrics.record_incremental_parse(Duration::from_micros(1000));
        metrics.record_full_parse(Duration::from_micros(3000));

        let stats = metrics.get_stats();
        assert_eq!(stats.incremental_count, 3);
        assert_eq!(stats.full_count, 1);
        assert_eq!(stats.avg_incremental_time_ms, 1.0);
        assert_eq!(stats.avg_full_parse_time_ms, 3.0);
        assert_eq!(stats.incremental_ratio, 0.75); // 3/4
    }

    #[test]
    fn test_incremental_ratio_calculation() {
        let metrics = ParseMetrics::new();

        // 80% incremental, 20% full
        for _ in 0..8 {
            metrics.record_incremental_parse(Duration::from_micros(100));
        }
        for _ in 0..2 {
            metrics.record_full_parse(Duration::from_micros(100));
        }

        let stats = metrics.get_stats();
        assert_eq!(stats.incremental_ratio, 0.8);
    }
}
