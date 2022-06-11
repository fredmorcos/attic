use core::slice;
use std::collections::BinaryHeap as PQueue;

/// # Safety
///
/// Dereferences `elements`.
#[no_mangle]
pub unsafe extern "system" fn pq_create(elements: *const u8, len: usize) -> *mut PQueue<u8> {
    let mut pq = PQueue::new();

    // Dereferences `elements` without checking whether it's NULL.
    let elements = slice::from_raw_parts(elements, len);
    for &element in elements {
        pq.push(element);
    }

    Box::into_raw(Box::new(pq))
}

#[no_mangle]
pub extern "system" fn pq_push(pq: &mut PQueue<u8>, element: u8) {
    pq.push(element);
}

/// # Safety
///
/// Dereferences `result`.
#[no_mangle]
pub unsafe extern "system" fn pq_pop(pq: &mut PQueue<u8>, result: *mut u8) -> bool {
    if let Some(val) = pq.pop() {
        // Dereferences `result` without checking whether it's NULL.
        *result = val;
        true
    } else {
        false
    }
}

/// # Safety
///
/// Assumes that `pq` is following the Box memory layout, and cannot guarantee that it
/// hasn't already been freed.
#[no_mangle]
pub unsafe extern "system" fn pq_free(pq: *mut PQueue<u8>) {
    Box::from_raw(pq);
}
