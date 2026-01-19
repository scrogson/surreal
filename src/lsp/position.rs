//! Line index for byte offset ↔ line/column conversion.

use tower_lsp::lsp_types::{Position, Range};

/// Index for efficient byte offset ↔ position conversion.
///
/// The LSP protocol uses line/character positions while the Dream compiler
/// uses byte offsets. This struct enables conversion between the two.
#[derive(Debug, Clone)]
pub struct LineIndex {
    /// Byte offset where each line starts
    line_starts: Vec<usize>,
    /// Total length of the source in bytes
    len: usize,
}

impl LineIndex {
    /// Create a new line index from source text.
    pub fn new(source: &str) -> Self {
        let mut line_starts = vec![0];

        for (offset, c) in source.char_indices() {
            if c == '\n' {
                line_starts.push(offset + 1);
            }
        }

        Self {
            line_starts,
            len: source.len(),
        }
    }

    /// Convert a byte offset to an LSP Position.
    ///
    /// Returns None if the offset is out of bounds.
    pub fn offset_to_position(&self, offset: usize) -> Option<Position> {
        if offset > self.len {
            return None;
        }

        // Binary search for the line containing this offset
        let line = match self.line_starts.binary_search(&offset) {
            Ok(line) => line,
            Err(line) => line.saturating_sub(1),
        };

        let line_start = self.line_starts[line];
        let character = offset - line_start;

        Some(Position {
            line: line as u32,
            character: character as u32,
        })
    }

    /// Convert an LSP Position to a byte offset.
    ///
    /// Returns None if the position is out of bounds.
    pub fn position_to_offset(&self, position: Position) -> Option<usize> {
        let line = position.line as usize;

        if line >= self.line_starts.len() {
            return None;
        }

        let line_start = self.line_starts[line];
        let offset = line_start + position.character as usize;

        if offset > self.len {
            return None;
        }

        Some(offset)
    }

    /// Convert a compiler span (byte range) to an LSP Range.
    pub fn span_to_range(&self, span: std::ops::Range<usize>) -> Option<Range> {
        let start = self.offset_to_position(span.start)?;
        let end = self.offset_to_position(span.end)?;
        Some(Range { start, end })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_offset_to_position() {
        let source = "fn main() {\n    println(\"hello\")\n}";
        let index = LineIndex::new(source);

        // Start of file
        assert_eq!(index.offset_to_position(0), Some(Position { line: 0, character: 0 }));

        // "fn"
        assert_eq!(index.offset_to_position(2), Some(Position { line: 0, character: 2 }));

        // Start of second line
        assert_eq!(index.offset_to_position(12), Some(Position { line: 1, character: 0 }));

        // "println" on second line
        assert_eq!(index.offset_to_position(16), Some(Position { line: 1, character: 4 }));
    }

    #[test]
    fn test_position_to_offset() {
        let source = "fn main() {\n    println(\"hello\")\n}";
        let index = LineIndex::new(source);

        // Start of file
        assert_eq!(index.position_to_offset(Position { line: 0, character: 0 }), Some(0));

        // Start of second line
        assert_eq!(index.position_to_offset(Position { line: 1, character: 0 }), Some(12));

        // End of file
        assert_eq!(index.position_to_offset(Position { line: 2, character: 1 }), Some(source.len()));
    }

    #[test]
    fn test_span_to_range() {
        let source = "fn main() {\n    println(\"hello\")\n}";
        let index = LineIndex::new(source);

        // "main" spans from offset 3 to 7
        let range = index.span_to_range(3..7).unwrap();
        assert_eq!(range.start, Position { line: 0, character: 3 });
        assert_eq!(range.end, Position { line: 0, character: 7 });
    }
}
