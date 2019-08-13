#![warn(clippy::all)]

struct List<T> {
    element: T,
    next: Option<Box<Self>>,
}

impl<T> List<T> {
    fn push(self, element: T) -> Self {
        List {
            element,
            next: Some(Box::new(self)),
        }
    }

    fn pop(self) -> (T, Option<Self>) {
        let element = self.element;
        if let Some(next) = self.next {
            (element, Some(*next))
        } else {
            (element, None)
        }
    }

    fn peek(&self) -> &T {
        &self.element
    }
}

// struct Iter<'a, T> {
//     next: Option<&'a List<T>>,
// }

// impl<'a, T> Iterator for Iter<'a, T> {
//     type Item = &'a T;

//     fn next(&mut self) -> Option<Self::Item> {
//         let res = self.next;
//         self.next = if let Some(next) = self.next {
//             Some(next.next)
//         } else {
//             None
//         }
//         res
//     }
// }

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
