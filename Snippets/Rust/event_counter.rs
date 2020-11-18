use std::time::SystemTime;

struct HitCounter {
    events: [usize; 300],
    head: usize,
    tail: usize,
    last_timestamp: u64,
}

fn now() -> u64 {
    SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap()
        .as_secs()
}

impl HitCounter {
    fn new() -> Self {
        Self {
            events: [0; 300],
            head: 0,
            tail: 0,
            last_timestamp: now(),
        }
    }

    fn zero(&mut self, current_timestamp: u64) {
        for t in self.last_timestamp..current_timestamp {
            self.events[(t % 300) as usize] = 0;
        }
        self.last_timestamp = current_timestamp;
    }

    fn add(&mut self) {
        let current_timestamp = now();
        self.zero(current_timestamp);
        self.events[(current_timestamp % 300) as usize] += 1;
    }

    fn count(&mut self) -> usize {
        let current_timestamp = now();
        self.zero(current_timestamp);
        self.events.iter().cloned().sum()
    }
}

#[cfg(test)]
mod tests {
    use crate::HitCounter;

    #[test]
    fn empty() {
        let mut hc = HitCounter::new();
        assert_eq!(hc.count(), 0);
    }

    #[test]
    fn normal_operation() {
        let mut hc = HitCounter::new();
        hc.add();
        hc.add();
        assert_eq!(hc.count(), 2);
    }

    #[test]
    fn no_events_in_the_last_5min() {
        let mut hc = HitCounter::new();
        hc.add();
        hc.add();
        // delay
        assert_eq!(hc.count(), 0);
    }
}
