# SPARK course catalogue

This repository hosts the static site at `training.spark.edu.au`. Each immediate
subdirectory of `courses/` that contains an `index.html` is automatically shown
on the homepage.

## Deployment

The `Deploy course site` workflow runs on every push to `main`. It:

1. Discovers courses and generates `index.html` and `catalogue.json`.
2. Packages only the static homepage assets and rendered courses.
3. Deploys that artifact using GitHub Pages.

In the repository's GitHub **Settings → Pages**, the publishing source must be
set to **GitHub Actions**.

To preview generated files locally, run:

```sh
node scripts/build-catalogue.mjs
```

Do not edit `index.html` or `catalogue.json` directly; the deployment regenerates
both from the current contents of `courses/`.

## Optional course metadata

The generator can discover a course using only its directory and `index.html`.
For a better card, a course release can also include `course.json` at the root
of its rendered course:

```json
{
  "title": "Introduction to mathematical modelling",
  "description": "Build practical infectious disease modelling skills.",
  "image": "logo.png",
  "topics": ["Modelling", "Infectious diseases"]
}
```

`image` is relative to the course directory. Without metadata, the generator
uses the page title, meta description, a root-level `logo` image where present,
and safe fallback text.
