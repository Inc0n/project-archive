
function day_theme() {
  document.body.style.setProperty("--body-color", "var(--gray-800)");
  document.body.style.setProperty("--body-bg", "var(--gray-100)");
  document.body.style.setProperty("--heading-color", "var(--gray-900)");
  document.body.style.setProperty("--link-color", "var(--blue-300)");
  document.body.style.setProperty("--link-hover-color", "var(--blue-400)");
  document.body.style.setProperty("--border-color", "rgba(10, 10, 10, 0.15)");
  document.body.style.setProperty("--code-bg", "var(--gray-300)");
}

function night_theme() {
  // const styleSetProperty = document.body.style.setProperty;
  document.body.style.setProperty("--body-color", "var(--gray-300)");
  document.body.style.setProperty("--body-bg", "var(--gray-800)");
  document.body.style.setProperty("--heading-color", "#e3e3e3");
  document.body.style.setProperty("--link-color", "var(--blue-300)");
  document.body.style.setProperty("--link-hover-color", "var(--blue-400)");
  document.body.style.setProperty("--border-color", "rgba(255, 255, 255, 0.15)");
  document.body.style.setProperty("--code-bg", "var(--gray-900)");
}

// function path_directory(path) {
//   let list = path.split("/");
//   return list.slice(0, list.length-1).join("/");
// }

function switch_theme(override) {
  // initialisation
  if (typeof switch_theme.night == 'undefined') {
    switch_theme.night = false;
  }
  //
  if (override == null) {
    switch_theme.night = !switch_theme.night;
  } else
    switch_theme.night = override;
  //
  const img = document.getElementById("switch-theme-icon-top");
  // let path = path_directory(img.getAttribute("src"));
  //
  if (switch_theme.night) {
    img.setAttribute("class", "transparent");
    night_theme();
  } else {
    img.setAttribute("class", "");
    day_theme();
  }
  update_theme_urlparam();
}

function onPageChange(event) {
  const a = event.currentTarget;
  window.location = a.href + "?night=" + switch_theme.night;
  return false; // disable original a href link change behaviour
}

function body_onLoad() {
  const queryString = window.location.search;
  const urlParams = new URLSearchParams(queryString);
  console.log("page loading");
  //
  switch_theme(urlParams.get('night') == 'true');

  const as = document.getElementsByTagName("a");
  console.log(as);
  Array.from(as).forEach(e => { e.onclick = onPageChange; });
}

//

function update_theme_urlparam() {
  const url = new URL(window.location);
  url.searchParams.set("night", switch_theme.night);
  window.history.replaceState(null, null, url.toString());
}
