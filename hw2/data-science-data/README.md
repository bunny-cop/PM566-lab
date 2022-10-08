# Data repository for data science

We will organize the data as first-come-first-served using folders with the following name structure
(expressed as a [Perl regular expression](https://en.wikipedia.org/wiki/Perl_Compatible_Regular_Expressions)):
`[0-9]{2}-[a-z]{1,}`, for example:

```
00-children-study
01-COVID-la-county
```

Each folder, besides the data, should contain a README file in markdown format. The header of the file should include the following:

```
name: [name of the dataset]
short-desc: [short description (one-sentence)]
date-collected:  YYYY-MM-DD
source-url: [URL to the source]
keywords: [kw1, kw2,etc.]
format: [csv, tab, ...]
```

The body of the MD file should be a more detailed description of the data. We need the structure of the MD
file so that we can automatically process the datasets and list them here, in the repository's main README
file.


