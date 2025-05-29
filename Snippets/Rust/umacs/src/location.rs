use derive_more::Display;
use derive_new::new;

/// Locations in source code.
#[derive(new, Display, Debug, PartialEq, Eq, Clone, Copy)]
#[display("{line}:{column}")]
pub struct Location {
    /// Line in buffer.
    line: usize,

    /// Column in line.
    column: usize,
}

impl Default for Location {
    fn default() -> Self {
        Self { line: 1, column: 1 }
    }
}

impl Location {
    /// Increment the location based on `byte`.
    ///
    /// If `byte` is a newline, `line` is incremented and `column` is reset to
    /// `1`. Otherwise, `column` is incremented.
    pub fn increment(&mut self, byte: Byte) {
        if byte == b'\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
    }

    /// Returns the line for a location.
    pub fn line(&self) -> usize {
        self.line
    }

    /// Returns the column on the location's line.
    pub fn column(&self) -> usize {
        self.column
    }
}

#[cfg(test)]
mod tests {
    use super::Location;

    #[test]
    fn display() {
        let location = Location::new(2, 3);
        assert_eq!(format!("{}", location), "2:3");
    }

    #[test]
    fn default() {
        assert_eq!(Location::default(), Location::new(1, 1));
    }

    #[test]
    fn increment() {
        let mut location = Location::new(2, 3);
        location.increment(b'a');
        location.increment(b'b');
        location.increment(b'c');
        assert_eq!(location, Location::new(2, 6));
        location.increment(b'\n');
        assert_eq!(location, Location::new(3, 1));
    }
}
