use std::collections::BinaryHeap as PQueue;

pub fn pq_create(elements: &[u8]) -> PQueue<u8> {
    let mut pq = PQueue::new();
    for &element in elements {
        pq.push(element);
    }
    pq
}

pub fn pq_push(pq: &mut PQueue<u8>, element: u8) {
    pq.push(element);
}

pub fn pq_pop(pq: &mut PQueue<u8>) -> Option<u8> {
    pq.pop()
}

pub fn pq_free(_pq: PQueue<u8>) {}
