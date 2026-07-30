import { access, mkdir, readFile, readdir, writeFile } from "node:fs/promises";
import path from "node:path";
import { fileURLToPath } from "node:url";

const scriptDirectory = path.dirname(fileURLToPath(import.meta.url));
const repositoryRoot = path.resolve(scriptDirectory, "..");
const coursesDirectory = path.join(repositoryRoot, "courses");
const templatePath = path.join(repositoryRoot, "src", "index.template.html");
const cataloguePath = path.join(repositoryRoot, "catalogue.json");
const homepagePath = path.join(repositoryRoot, "index.html");

const titleCase = (value) =>
  value
    .split("-")
    .filter(Boolean)
    .map((word) => word.charAt(0).toUpperCase() + word.slice(1))
    .join(" ");

const escapeHtml = (value) =>
  String(value)
    .replaceAll("&", "&amp;")
    .replaceAll("<", "&lt;")
    .replaceAll(">", "&gt;")
    .replaceAll('"', "&quot;")
    .replaceAll("'", "&#039;");

const stripHtml = (value) =>
  value
    .replace(/<[^>]*>/g, " ")
    .replace(/\s+/g, " ")
    .trim();

const extract = (html, expression) => {
  const match = html.match(expression);
  return match ? stripHtml(match[1]) : "";
};

const exists = async (filePath) => {
  try {
    await access(filePath);
    return true;
  } catch {
    return false;
  }
};

const findImage = async (courseDirectory, metadata) => {
  if (metadata.image && (await exists(path.join(courseDirectory, metadata.image)))) {
    return metadata.image;
  }

  const entries = await readdir(courseDirectory, { withFileTypes: true });
  const preferredNames = ["logo.png", "logo.jpg", "logo.jpeg", "logo.webp"];
  return preferredNames.find((name) =>
    entries.some((entry) => entry.isFile() && entry.name.toLowerCase() === name)
  ) ?? "";
};

const readMetadata = async (courseDirectory) => {
  const metadataPath = path.join(courseDirectory, "course.json");
  if (!(await exists(metadataPath))) return {};

  try {
    return JSON.parse(await readFile(metadataPath, "utf8"));
  } catch (error) {
    throw new Error(`Could not parse ${metadataPath}: ${error.message}`);
  }
};

const readEntryDocument = async (courseDirectory, entryPage) => {
  const html = await readFile(entryPage, "utf8");
  const title = extract(html, /<title[^>]*>([\s\S]*?)<\/title>/i);
  if (!/^redirect\b/i.test(title)) return html;

  const refreshTag = html.match(
    /<meta(?=[^>]+http-equiv=["']refresh["'])[^>]*>/i
  )?.[0];
  const refreshContent =
    refreshTag?.match(/content="([^"]*)"/i)?.[1] ||
    refreshTag?.match(/content='([^']*)'/i)?.[1];
  const refreshTarget = refreshContent?.match(
    /url\s*=\s*['"]?([^'";\s>]+)/i
  )?.[1];
  if (!refreshTarget) return html;

  const targetPath = path.resolve(courseDirectory, refreshTarget.trim());
  const relativeTarget = path.relative(courseDirectory, targetPath);
  const isInsideCourse =
    relativeTarget && !relativeTarget.startsWith("..") && !path.isAbsolute(relativeTarget);

  return isInsideCourse && (await exists(targetPath))
    ? readFile(targetPath, "utf8")
    : html;
};

const courseDirectories = (await readdir(coursesDirectory, { withFileTypes: true }))
  .filter((entry) => entry.isDirectory() && !entry.name.startsWith("."))
  .map((entry) => entry.name)
  .sort((a, b) => a.localeCompare(b));

const courses = [];

for (const slug of courseDirectories) {
  const courseDirectory = path.join(coursesDirectory, slug);
  const entryPage = path.join(courseDirectory, "index.html");
  if (!(await exists(entryPage))) continue;

  const [html, metadata] = await Promise.all([
    readEntryDocument(courseDirectory, entryPage),
    readMetadata(courseDirectory),
  ]);

  const htmlTitle = extract(html, /<title[^>]*>([\s\S]*?)<\/title>/i);
  const metaDescription =
    extract(
      html,
      /<meta[^>]+name=["']description["'][^>]+content=["']([^"']*)["'][^>]*>/i
    ) ||
    extract(
      html,
      /<meta[^>]+content=["']([^"']*)["'][^>]+name=["']description["'][^>]*>/i
    );

  const title =
    metadata.title ||
    (!/^redirect\b/i.test(htmlTitle) ? htmlTitle : "") ||
    titleCase(slug);
  const description =
    metadata.description ||
    metaDescription ||
    `Explore the ${title} course materials, activities and resources.`;
  const topics = Array.isArray(metadata.topics)
    ? metadata.topics.filter((topic) => typeof topic === "string" && topic.trim())
    : [];
  const image = await findImage(courseDirectory, metadata);

  courses.push({
    slug,
    title,
    description,
    href: `courses/${slug}/`,
    image: image ? `courses/${slug}/${image}` : "",
    topics,
  });
}

courses.sort((a, b) => a.title.localeCompare(b.title));

const cards = courses
  .map((course, index) => {
    const visual = course.image
      ? `<img src="${escapeHtml(course.image)}" alt="" loading="${
          index < 3 ? "eager" : "lazy"
        }">`
      : `<span class="course-monogram" aria-hidden="true">${escapeHtml(
          course.title.charAt(0)
        )}</span>`;
    const topics = course.topics.length
      ? `<ul class="course-topics" aria-label="Topics">${course.topics
          .map((topic) => `<li>${escapeHtml(topic)}</li>`)
          .join("")}</ul>`
      : "";
    const searchText = [course.title, course.description, ...course.topics]
      .join(" ")
      .toLowerCase();

    return `        <article class="course-card" data-course data-search="${escapeHtml(
      searchText
    )}">
          <a class="course-link" href="${escapeHtml(course.href)}">
            <span class="course-visual">${visual}</span>
            <span class="course-content">
              <span class="course-label">Training course</span>
              <h2>${escapeHtml(course.title)}</h2>
              <p>${escapeHtml(course.description)}</p>
              ${topics}
              <span class="course-action">View course <span aria-hidden="true">→</span></span>
            </span>
          </a>
        </article>`;
  })
  .join("\n");

const template = await readFile(templatePath, "utf8");
const homepage = template
  .replace("<!-- COURSE_CARDS -->", cards)
  .replaceAll("{{COURSE_COUNT}}", String(courses.length));

await mkdir(coursesDirectory, { recursive: true });
await Promise.all([
  writeFile(cataloguePath, `${JSON.stringify(courses, null, 2)}\n`),
  writeFile(homepagePath, homepage),
]);

console.log(`Generated homepage with ${courses.length} courses.`);
