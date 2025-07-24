/// Retrieve the range of the word or symbol at the provided position in the text.
pub fn find_word_boundary(text: &str, position: usize) -> std::ops::Range<usize> {
    let separator = |c: char| c.is_whitespace() || matches!(c, '(' | ')' | '[' | ']' | '{' | '}');

    let mut end = 0;
    for (separator_index, separator_str) in text.match_indices(separator) {
        if separator_index >= position {
            return end..separator_index;
        } else {
            end = separator_index + separator_str.len();
        }
    }

    end..text.len()
}
