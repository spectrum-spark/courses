const searchInput = document.querySelector("#course-search");
const courseCards = [...document.querySelectorAll("[data-course]")];
const visibleCount = document.querySelector("#visible-count");
const courseNoun = document.querySelector("#course-noun");
const emptyState = document.querySelector("#empty-state");
const clearSearch = document.querySelector("#clear-search");

const normalise = (value) =>
  value
    .toLocaleLowerCase()
    .normalize("NFD")
    .replace(/\p{Diacritic}/gu, "")
    .trim();

const updateResults = () => {
  const query = normalise(searchInput.value);
  let count = 0;

  courseCards.forEach((card) => {
    const matches = !query || normalise(card.dataset.search).includes(query);
    card.hidden = !matches;
    if (matches) count += 1;
  });

  visibleCount.textContent = String(count);
  courseNoun.textContent = count === 1 ? "course" : "courses";
  emptyState.hidden = count !== 0;
};

searchInput?.addEventListener("input", updateResults);
clearSearch?.addEventListener("click", () => {
  searchInput.value = "";
  updateResults();
  searchInput.focus();
});
