use core::slice;
use std::collections::BinaryHeap as PQueue;

pub struct Pq(PQueue<u8>);

#[no_mangle]
pub extern "system" fn pq_create(elements: Option<&u8>, len: usize) -> *mut Pq {
    let mut pq = Pq(PQueue::new());

    // Does not "dereference" `elements` without checking for NULL anymore.
    let elements = if let Some(elements) = elements {
        unsafe { slice::from_raw_parts(elements, len) }
    } else {
        return std::ptr::null_mut();
    };

    for &element in elements {
        pq.0.push(element);
    }

    Box::into_raw(Box::new(pq))
}

#[no_mangle]
pub extern "system" fn pq_push(pq: Option<&mut Pq>, element: u8) -> bool {
    if let Some(pq) = pq {
        pq.0.push(element);
        return true;
    }

    false
}

#[no_mangle]
pub extern "system" fn pq_pop(pq: Option<&mut Pq>, result: Option<&mut u8>) -> bool {
    if let Some(pq) = pq {
        if let Some(result) = result {
            if let Some(val) = pq.0.pop() {
                *result = val;
                return true;
            }
        }
    }

    false
}

/// # Safety
///
/// Assumes that `pq` is following the Box memory layout, and cannot guarantee that it
/// hasn't already been freed.
#[no_mangle]
pub extern "system" fn pq_free(pq: Option<&mut Pq>) {
    if let Some(pq) = pq {
        unsafe {
            Box::from_raw(pq);
        }
    }
}
