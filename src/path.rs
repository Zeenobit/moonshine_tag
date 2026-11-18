use bevy_reflect::Reflect;
use itertools::Itertools;

use crate::{Tag, ANY, NONE};

#[derive(Clone, Debug, Hash, PartialEq, Eq, Reflect)]
#[reflect(Hash, PartialEq)]
pub struct TagPath<const N: usize>([Tag; N]);

impl<const N: usize> Default for TagPath<N> {
    fn default() -> Self {
        Self([NONE; N])
    }
}

impl<const N: usize> TagPath<N> {
    pub fn from_str(path: &str) -> Self {
        let mut tokens = path.split(".");
        let mut tags = [NONE; N];
        for i in 0..N {
            if let Some(token) = tokens.next() {
                tags[i] = Tag::new(token);
            } else {
                break;
            }
        }
        assert!(tokens.next().is_none());
        Self(tags)
    }

    pub fn matches(&self, pattern: &TagPath<N>) -> bool {
        for (&v, &p) in self.0.iter().zip(&pattern.0) {
            match p {
                NONE => return true,
                ANY if v != NONE => continue,
                _ if v == p => continue,
                _ => return false,
            }
        }
        true
    }

    pub fn matches_str(&self, pattern: &str) -> bool {
        self.matches(&TagPath::from_str(pattern))
    }

    pub fn matches_exact(&self, pattern: &TagPath<N>) -> bool {
        self.0.iter().zip(&pattern.0).all(|(&v, &p)| match p {
            ANY => v != NONE,
            _ => v == p,
        })
    }

    pub fn matches_exact_str(&self, pattern: &str) -> bool {
        self.matches_exact(&TagPath::from_str(pattern))
    }

    pub fn pretty_name(&self) -> String {
        self.0.iter().map(|tag| tag.pretty_name()).join(".")
    }
}

#[cfg(test)]
mod tests {
    use crate::tags;

    use super::*;

    tags! {
        A, B, C
    }

    #[test]
    fn match_path() {
        let ____ = TagPath::<4>::from_str("*.*.*.*");
        let axxx = TagPath::<4>::from_str("A");
        let a___ = TagPath::<4>::from_str("A.*");
        let abxx = TagPath::<4>::from_str("A.B");
        let ab__ = TagPath::<4>::from_str("A.B.*");
        let abcx = TagPath::<4>::from_str("A.B.C");
        let abc_ = TagPath::<4>::from_str("A.B.C.*");
        let abcd = TagPath::<4>::from_str("A.B.C.D");
        let bxxx = TagPath::<4>::from_str("B.*");
        let b___ = TagPath::<4>::from_str("B");
        let baxx = TagPath::<4>::from_str("B.A.*");
        let ba__ = TagPath::<4>::from_str("B.A");
        let bacx = TagPath::<4>::from_str("B.A.C.*");
        let bac_ = TagPath::<4>::from_str("B.A.C");
        let bacd = TagPath::<4>::from_str("B.A.C.D");

        assert!(abcd.matches(&____));
        assert!(abcd.matches(&axxx));
        assert!(abcd.matches(&a___));
        assert!(abcd.matches(&abxx));
        assert!(abcd.matches(&ab__));
        assert!(abcd.matches(&abcx));
        assert!(abcd.matches(&abc_));
        assert!(abcd.matches(&abcd));

        assert!(!____.matches(&abcd));
        assert!(!axxx.matches(&abcd));
        assert!(!a___.matches(&abcd));
        assert!(!abxx.matches(&abcd));
        assert!(!ab__.matches(&abcd));
        assert!(!abcx.matches(&abcd));
        assert!(!abc_.matches(&abcd));

        assert!(!abcd.matches(&bxxx));
        assert!(!abcd.matches(&b___));
        assert!(!abcd.matches(&baxx));
        assert!(!abcd.matches(&ba__));
        assert!(!abcd.matches(&bacx));
        assert!(!abcd.matches(&bac_));
        assert!(!abcd.matches(&bacd));

        assert!(!bxxx.matches(&abcd));
        assert!(!b___.matches(&abcd));
        assert!(!baxx.matches(&abcd));
        assert!(!ba__.matches(&abcd));
        assert!(!bacx.matches(&abcd));
        assert!(!bac_.matches(&abcd));
        assert!(!bacd.matches(&abcd));

        assert!(____.matches_exact(&____));
        assert!(!____.matches_exact(&axxx));
        assert!(!____.matches_exact(&a___));
        assert!(!____.matches_exact(&abxx));
        assert!(!____.matches_exact(&ab__));
        assert!(!____.matches_exact(&abcx));
        assert!(!____.matches_exact(&abc_));
        assert!(!____.matches_exact(&abcd));

        assert!(!axxx.matches_exact(&____));
        assert!(axxx.matches_exact(&axxx));
        assert!(!axxx.matches_exact(&a___));
        assert!(!axxx.matches_exact(&abxx));
        assert!(!axxx.matches_exact(&ab__));
        assert!(!axxx.matches_exact(&abcx));
        assert!(!axxx.matches_exact(&abc_));
        assert!(!axxx.matches_exact(&abcd));

        assert!(!a___.matches_exact(&____));
        assert!(!a___.matches_exact(&axxx));
        assert!(a___.matches_exact(&a___));
        assert!(!a___.matches_exact(&abxx));
        assert!(!a___.matches_exact(&ab__));
        assert!(!a___.matches_exact(&abcx));
        assert!(!a___.matches_exact(&abc_));
        assert!(!a___.matches_exact(&abcd));

        assert!(!abxx.matches_exact(&____));
        assert!(!abxx.matches_exact(&axxx));
        assert!(abxx.matches_exact(&a___));
        assert!(abxx.matches_exact(&abxx));
        assert!(!abxx.matches_exact(&ab__));
        assert!(!abxx.matches_exact(&abcx));
        assert!(!abxx.matches_exact(&abc_));
        assert!(!abxx.matches_exact(&abcd));

        assert!(!ab__.matches_exact(&____));
        assert!(!ab__.matches_exact(&axxx));
        assert!(!ab__.matches_exact(&a___));
        assert!(!ab__.matches_exact(&abxx));
        assert!(ab__.matches_exact(&ab__));
        assert!(!ab__.matches_exact(&abcx));
        assert!(!ab__.matches_exact(&abc_));
        assert!(!ab__.matches_exact(&abcd));

        assert!(!abcx.matches_exact(&____));
        assert!(!abcx.matches_exact(&axxx));
        assert!(!abcx.matches_exact(&a___));
        assert!(!abcx.matches_exact(&abxx));
        assert!(abcx.matches_exact(&ab__));
        assert!(abcx.matches_exact(&abcx));
        assert!(!abcx.matches_exact(&abc_));
        assert!(!abcx.matches_exact(&abcd));

        assert!(abc_.matches_exact(&____));
        assert!(!abc_.matches_exact(&axxx));
        assert!(!abc_.matches_exact(&a___));
        assert!(!abc_.matches_exact(&abxx));
        assert!(!abc_.matches_exact(&ab__));
        assert!(!abc_.matches_exact(&abcx));
        assert!(abc_.matches_exact(&abc_));
        assert!(!abc_.matches_exact(&abcd));

        assert!(abcd.matches_exact(&____));
        assert!(!abcd.matches_exact(&axxx));
        assert!(!abcd.matches_exact(&a___));
        assert!(!abcd.matches_exact(&abxx));
        assert!(!abcd.matches_exact(&ab__));
        assert!(!abcd.matches_exact(&abcx));
        assert!(abcd.matches_exact(&abc_));
        assert!(abcd.matches_exact(&abcd));
    }
}
