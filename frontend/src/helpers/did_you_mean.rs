use itertools::Itertools;

pub fn comment(name: &str) -> Option<()> {
    (name == "//" || name == "#" || name == "/*" || name == "*/").then_some(())
}

pub fn name<'a>(name: &str, candidates: impl IntoIterator<Item = &'a str>) -> Option<&'a str> {
    const THRESHOLD: f64 = 0.4;

    let len = name.len() as f64;

    candidates
        .into_iter()
        .filter(|candidate| (candidate.len() as f64 - len).abs() / len < THRESHOLD)
        .sorted()
        .map(|candidate| {
            (
                candidate,
                distance::damerau_levenshtein(name, candidate) as f64 / len,
            )
        })
        .filter(|(_, distance)| *distance < THRESHOLD)
        .min_by(|(_, a), (_, b)| a.partial_cmp(b).unwrap())
        .map(|(candidate, _)| candidate)
}
