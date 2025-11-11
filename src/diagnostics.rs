#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub struct SourceSpan {
    start: usize,
    len: usize,
}

impl SourceSpan {
    pub fn new(start: usize, len: usize) -> Self {
        Self { start, len }
    }

    pub fn start(&self) -> usize {
        self.start
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn end(&self) -> usize {
        self.start.saturating_add(self.len)
    }

    pub fn merge(&self, other: &SourceSpan) -> SourceSpan {
        let start = self.start.min(other.start);
        let end = self.end().max(other.end());
        SourceSpan::new(start, end.saturating_sub(start))
    }

    fn clamped_start(&self, source: &str) -> usize {
        self.start.min(source.len())
    }

    fn line_bounds(&self, source: &str) -> (usize, usize) {
        let start = self.clamped_start(source);
        let line_start = source[..start].rfind('\n').map(|idx| idx + 1).unwrap_or(0);
        let line_end = source[start..]
            .find('\n')
            .map(|idx| start + idx)
            .unwrap_or(source.len());
        (line_start, line_end)
    }

    pub fn line_and_column(&self, source: &str) -> (usize, usize) {
        let start = self.clamped_start(source);
        let mut line = 1usize;
        let mut column = 1usize;
        for ch in source[..start].chars() {
            if ch == '\n' {
                line += 1;
                column = 1;
            } else {
                column += 1;
            }
        }
        (line, column)
    }

    fn caret_info(&self, source: &str) -> (usize, usize, String) {
        let (line_start, line_end) = self.line_bounds(source);
        let start = self.clamped_start(source);
        let line_text = source[line_start..line_end].to_string();
        let prefix_slice = &source[line_start..start.min(line_end)];
        let prefix_len = prefix_slice.chars().count();
        let highlight_slice = if start >= line_end {
            "".to_string()
        } else {
            source[start..self.end().min(line_end)].to_string()
        };
        let caret_width = highlight_slice.chars().count().max(1);
        (prefix_len, caret_width, line_text)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Diagnostic {
    pub message: String,
    pub span: Option<SourceSpan>,
}

impl Diagnostic {
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            span: None,
        }
    }

    pub fn with_span(mut self, span: SourceSpan) -> Self {
        self.span = Some(span);
        self
    }

    pub fn render_with_source(&self, source: &str) -> String {
        match &self.span {
            Some(span) => {
                let (line, column) = span.line_and_column(source);
                let (prefix_len, caret_width, line_text) = span.caret_info(source);
                let gutter_width = line.to_string().len().max(2);
                let spacer = format!("{:>width$} |", "", width = gutter_width);
                let line_display = format!(
                    "{line:>width$} | {line_text}",
                    line = line,
                    width = gutter_width,
                    line_text = line_text
                );
                let caret_line = format!(
                    "{spacer} {spaces}{markers}",
                    spacer = spacer,
                    spaces = " ".repeat(prefix_len),
                    markers = "^".repeat(caret_width)
                );
                format!(
                    "{message}\n --> line {line}, column {column}\n{spacer}\n{line_display}\n{caret_line}",
                    message = self.message,
                    line = line,
                    column = column,
                    spacer = spacer,
                    line_display = line_display,
                    caret_line = caret_line
                )
            }
            None => self.message.clone(),
        }
    }
}
