mod instrument;
mod optimize;
mod profile;

fn main() {}

pub fn assert_no_diff(expected: &str, actual: &str) {
    if expected == actual {
        return;
    }

    let mut difference = String::new();
    for diff in diff::lines(expected, actual) {
        let (c, line) = match diff {
            diff::Result::Left(left) => ('-', left),
            diff::Result::Both(left, _right) => (' ', left),
            diff::Result::Right(right) => ('+', right),
        };
        difference.push(c);
        difference.push_str(line);
        difference.push('\n');
    }
    assert!(false, "expected != actual:\n\n{difference}");
}
