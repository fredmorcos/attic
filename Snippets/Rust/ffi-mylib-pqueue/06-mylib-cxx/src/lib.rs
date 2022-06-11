use std::collections::BinaryHeap as PQueue;

#[cxx::bridge]
mod ffi {
    #[derive(Clone, Copy, PartialEq, Eq)]
    pub enum PqPopStatus {
        Empty,
        Success,
    }

    #[derive(Clone, Copy, PartialEq, Eq)]
    pub struct PqPopResult {
        status: PqPopStatus,
        value: u8,
    }

    extern "Rust" {
        type Pq;

        // Freestanding function
        fn pq_create(elements: &[u8]) -> Box<Pq>;

        // Methods
        fn push(self: &mut Pq, element: u8);
        fn pop(self: &mut Pq) -> PqPopResult;
    }
}

pub struct Pq(PQueue<u8>);

pub fn pq_create(elements: &[u8]) -> Box<Pq> {
    let mut pq = Pq(PQueue::new());
    for &element in elements {
        pq.0.push(element);
    }
    Box::new(pq)
}

impl Pq {
    pub fn push(&mut self, element: u8) {
        self.0.push(element)
    }

    pub fn pop(&mut self) -> ffi::PqPopResult {
        match self.0.pop() {
            Some(value) => ffi::PqPopResult {
                status: ffi::PqPopStatus::Success,
                value,
            },
            None => ffi::PqPopResult {
                status: ffi::PqPopStatus::Empty,
                value: 0,
            },
        }
    }
}
