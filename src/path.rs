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
    fn foo() {
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
    }
}
