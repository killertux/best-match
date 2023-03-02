use std::{
    collections::HashMap,
    fs::{read_dir, File},
    io::{BufRead, BufReader},
    path::{Path, PathBuf},
};

use crate::tokenizer::{Term, Tokenizer};
use anyhow::Result;
use rayon::prelude::*;
use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize)]
pub struct Index {
    terms_scores: HashMap<Term, Vec<(f32, PathBuf)>>,
}

impl Index {
    pub fn build_from_dir<T>(dir_path: impl AsRef<Path>, extension_filter: &[T]) -> Result<Self>
    where
        T: AsRef<str>,
    {
        let paths = Self::get_all_files_from_dir(dir_path)?;
        let extension_filter: Vec<&str> =
            extension_filter.into_iter().map(|x| x.as_ref()).collect();
        let document_frequencies = paths
            .into_par_iter()
            .filter(|path| {
                path.extension()
                    .map(|ext| extension_filter.contains(&ext.to_str().unwrap_or("")))
                    .unwrap_or(false)
            })
            .map(|path| DocumentFrequency::from_path(path))
            .collect::<Result<Vec<DocumentFrequency>>>()?;
        Ok(Self::from_document_frequencies(document_frequencies))
    }

    pub fn search(&self, terms: impl Iterator<Item = Term>) -> Vec<(PathBuf, f32)> {
        terms
            .flat_map(|term| self.terms_scores.get(&term))
            .flatten()
            .fold(HashMap::new(), |mut acc, (score, path)| {
                *acc.entry(path.clone()).or_insert(0.0) += score;
                acc
            })
            .into_iter()
            .collect()
    }

    fn get_all_files_from_dir(dir_path: impl AsRef<Path>) -> Result<Vec<PathBuf>> {
        let mut files = Vec::new();
        for path in read_dir(dir_path)? {
            let path = path?.path();
            if path.is_file() {
                files.push(path);
            } else if path.is_dir() {
                files.append(&mut Self::get_all_files_from_dir(path)?);
            }
        }
        Ok(files)
    }

    fn from_document_frequencies(document_frequencies: Vec<DocumentFrequency>) -> Self {
        println!(
            "Calculating scores for terms from {n_documents} documents",
            n_documents = document_frequencies.len()
        );
        let avg_document_lenght: f32 = document_frequencies
            .iter()
            .map(|df| df.term_frequency.n_terms)
            .sum::<u64>() as f32
            / document_frequencies.len() as f32;
        let idf = Idf::from_terms_in_document(&TermsInDocuments::from_document_frequencies(
            document_frequencies.iter(),
        ));
        let mut terms_scores = HashMap::new();
        for df in document_frequencies {
            for term in df.term_frequency.frequency.keys() {
                let k1 = 1.2;
                let b = 0.75;
                let idf = idf.idf_by_term.get(term).unwrap(); // Should not fail
                let tf = df.term_frequency.frequency.get(term).copied().unwrap_or(0) as f32;
                let score = idf * (tf * (k1 + 1.0))
                    / (tf
                        + k1 * (1.0 - b
                            + b * (df.term_frequency.n_terms as f32 / avg_document_lenght as f32)));
                terms_scores
                    .entry(term.clone())
                    .or_insert(Vec::new())
                    .push((score, df.path.clone()));
            }
        }
        Index { terms_scores }
    }
}

#[derive(PartialEq, Eq, Debug)]
struct TermFrequency {
    n_terms: u64,
    frequency: HashMap<Term, u64>,
}

impl TermFrequency {
    pub fn from_reader(reader: impl BufRead) -> Self {
        Tokenizer::new(reader).fold(
            TermFrequency {
                n_terms: 0,
                frequency: HashMap::new(),
            },
            |mut term_frequency, term| {
                term_frequency.n_terms += 1;
                *term_frequency.frequency.entry(term).or_insert(0) += 1;
                term_frequency
            },
        )
    }
}

#[derive(PartialEq, Eq, Debug)]
struct DocumentFrequency {
    path: PathBuf,
    term_frequency: TermFrequency,
}

impl DocumentFrequency {
    pub fn from_path(path: impl AsRef<Path>) -> Result<Self> {
        println!(
            "Getting document frequencies from {path}",
            path = path.as_ref().display()
        );
        Ok(Self {
            path: path.as_ref().to_path_buf(),
            term_frequency: TermFrequency::from_reader(BufReader::new(File::open(path.as_ref())?)),
        })
    }
}

#[derive(Debug, PartialEq, Eq)]
struct TermsInDocuments {
    n_documents: usize,
    terms_in_documents: HashMap<Term, u64>,
}

impl TermsInDocuments {
    pub fn from_document_frequencies<'a>(
        document_frequencies: impl Iterator<Item = &'a DocumentFrequency>,
    ) -> Self {
        document_frequencies.fold(
            TermsInDocuments {
                n_documents: 0,
                terms_in_documents: HashMap::new(),
            },
            |mut terms_in_document, document_frequency| {
                document_frequency
                    .term_frequency
                    .frequency
                    .iter()
                    .for_each(|(term, _)| {
                        *terms_in_document
                            .terms_in_documents
                            .entry(term.clone())
                            .or_insert(0) += 1
                    });
                terms_in_document.n_documents += 1;
                terms_in_document
            },
        )
    }
}

#[derive(Debug, PartialEq)]
struct Idf {
    idf_by_term: HashMap<Term, f32>,
}

impl Idf {
    pub fn from_terms_in_document(terms: &TermsInDocuments) -> Self {
        Idf {
            idf_by_term: terms
                .terms_in_documents
                .iter()
                .map(|(term, n_documents)| {
                    (
                        term.clone(),
                        ((terms.n_documents as f32 - *n_documents as f32 + 0.5)
                            / (*n_documents as f32 + 0.5)
                            + 1.0)
                            .ln(),
                    )
                })
                .collect(),
        }
    }
}

#[cfg(test)]
mod test {
    use float_cmp::approx_eq;
    use std::{
        io::{Cursor, Write},
        str::FromStr,
    };
    use tempfile::{NamedTempFile, TempDir};

    use super::*;
    #[test]
    fn build_term_frequency() {
        let text = Cursor::new("Text example, . example another repeat repeat");
        let term_frequency = TermFrequency::from_reader(text);
        assert_eq!(
            TermFrequency {
                n_terms: 6,
                frequency: HashMap::from([
                    (Term::new("TEXT".into()), 1),
                    (Term::new("EXAMPLE".into()), 2),
                    (Term::new("ANOTHER".into()), 1),
                    (Term::new("REPEAT".into()), 2),
                ])
            },
            term_frequency
        );
    }

    #[test]
    fn build_document_frequency() -> anyhow::Result<()> {
        let mut temp_file = NamedTempFile::new()?;
        temp_file
            .as_file_mut()
            .write_all(b"My list of terms. List my terms")?;
        let document_frequency = DocumentFrequency::from_path(&temp_file)?;
        assert_eq!(
            DocumentFrequency {
                path: temp_file.path().to_path_buf(),
                term_frequency: TermFrequency {
                    n_terms: 7,
                    frequency: HashMap::from([
                        (Term::new("MY".into()), 2),
                        (Term::new("LIST".into()), 2),
                        (Term::new("OF".into()), 1),
                        (Term::new("TERMS".into()), 2),
                    ])
                }
            },
            document_frequency
        );
        Ok(())
    }

    #[test]
    fn build_terms_in_documents() -> anyhow::Result<()> {
        let document_frequencies = vec![
            DocumentFrequency {
                path: PathBuf::from_str("document_1")?,
                term_frequency: TermFrequency::from_reader(Cursor::new("First Document")),
            },
            DocumentFrequency {
                path: PathBuf::from_str("document_2")?,
                term_frequency: TermFrequency::from_reader(Cursor::new("Second Document")),
            },
            DocumentFrequency {
                path: PathBuf::from_str("document_3")?,
                term_frequency: TermFrequency::from_reader(Cursor::new("Third Document")),
            },
        ];
        let terms_in_documents =
            TermsInDocuments::from_document_frequencies(document_frequencies.iter());
        assert_eq!(
            TermsInDocuments {
                n_documents: 3,
                terms_in_documents: HashMap::from([
                    (Term::new("FIRST".into()), 1),
                    (Term::new("SECOND".into()), 1),
                    (Term::new("THIRD".into()), 1),
                    (Term::new("DOCUMENT".into()), 3)
                ])
            },
            terms_in_documents
        );
        Ok(())
    }

    #[test]
    fn idf() -> anyhow::Result<()> {
        let document_frequencies = vec![
            DocumentFrequency {
                path: PathBuf::from_str("document_1")?,
                term_frequency: TermFrequency::from_reader(Cursor::new("First Document")),
            },
            DocumentFrequency {
                path: PathBuf::from_str("document_2")?,
                term_frequency: TermFrequency::from_reader(Cursor::new("Second Document")),
            },
            DocumentFrequency {
                path: PathBuf::from_str("document_3")?,
                term_frequency: TermFrequency::from_reader(Cursor::new("Third Document")),
            },
        ];
        let idf = Idf::from_terms_in_document(&TermsInDocuments::from_document_frequencies(
            document_frequencies.iter(),
        ));
        assert!(approx_eq!(
            f32,
            0.9808292,
            idf.idf_by_term
                .get(&Term::new("FIRST".into()))
                .copied()
                .unwrap(),
            epsilon = 0.0000001
        ));
        assert!(approx_eq!(
            f32,
            0.9808292,
            idf.idf_by_term
                .get(&Term::new("SECOND".into()))
                .copied()
                .unwrap(),
            epsilon = 0.0000001
        ));
        assert!(approx_eq!(
            f32,
            0.9808292,
            idf.idf_by_term
                .get(&Term::new("THIRD".into()))
                .copied()
                .unwrap(),
            epsilon = 0.0000001
        ));
        assert!(approx_eq!(
            f32,
            0.1335314,
            idf.idf_by_term
                .get(&Term::new("DOCUMENT".into()))
                .copied()
                .unwrap(),
            epsilon = 0.0000001
        ));
        Ok(())
    }

    #[test]
    fn index() -> anyhow::Result<()> {
        let head_dir = TempDir::new()?;
        let sub_dir = TempDir::new_in(&head_dir)?;
        let mut file_1 = File::create(&head_dir.path().join("file_1.txt"))?;
        let mut file_2 = File::create(&head_dir.path().join("file_2.txt"))?;
        let mut file_3 = File::create(&sub_dir.path().join("file_3.txt"))?;
        File::create(&sub_dir.path().join("file_4.zip"))?.write_all(b"File to ignore")?;
        file_1.write_all(b"File 1 terms")?;
        file_2.write_all(b"File 2 terms")?;
        file_3.write_all(b"File repeat repeat")?;

        let index = Index::build_from_dir(head_dir.path(), &["txt"])?;

        let file_term_score = index.terms_scores.get(&Term::new("FILE".into())).unwrap();
        assert_eq!(3, file_term_score.len());
        assert_has_files(
            &["file_1.txt", "file_2.txt", "file_3.txt"],
            file_term_score.iter(),
        );
        assert_all_approx(0.1335314, file_term_score.iter().map(|x| x.0));

        let terms_term_score = index.terms_scores.get(&Term::new("TERMS".into())).unwrap();
        assert_eq!(2, terms_term_score.len());
        assert_has_files(&["file_1.txt", "file_2.txt"], terms_term_score.iter());
        assert_all_approx(0.4700036, terms_term_score.iter().map(|x| x.0));

        let repeat_term_score = index.terms_scores.get(&Term::new("REPEAT".into())).unwrap();
        assert_eq!(1, repeat_term_score.len());
        assert_has_files(&["file_3.txt"], repeat_term_score.iter());
        assert_all_approx(1.3486402, repeat_term_score.iter().map(|x| x.0));

        let one_term_score = index.terms_scores.get(&Term::new("1".into())).unwrap();
        assert_eq!(1, one_term_score.len());
        assert_has_files(&["file_1.txt"], one_term_score.iter());
        assert_all_approx(0.9808292, one_term_score.iter().map(|x| x.0));

        let two_term_score = index.terms_scores.get(&Term::new("2".into())).unwrap();
        assert_eq!(1, two_term_score.len());
        assert_has_files(&["file_2.txt"], two_term_score.iter());
        assert_all_approx(0.9808292, two_term_score.iter().map(|x| x.0));

        Ok(())
    }

    fn assert_all_approx(expected: f32, values: impl Iterator<Item = f32>) {
        for value in values {
            assert!(
                approx_eq!(f32, expected, value, epsilon = 0.0000001),
                "Failed to assert that {expected} is approx to {value}"
            );
        }
    }
    fn assert_has_files<'a>(files: &[&str], values: impl Iterator<Item = &'a (f32, PathBuf)>) {
        let values: Vec<String> = values
            .map(|x| x.1.file_name().unwrap().to_string_lossy().to_string())
            .collect();
        for file in files {
            assert!(
                values.contains(&file.to_string()),
                "Failed to assert that {values:?} contains {file}"
            );
        }
    }
}
