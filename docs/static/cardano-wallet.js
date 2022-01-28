// Keep anchor #links on the same page, when they would otherwise redirect to
// the page's <base href>. Ref: https://stackoverflow.com/q/8108836
(function() {
  function fixLink(el) {
    if (el.tagName.toLowerCase() === "a") {
      var href = el.getAttribute("href");
      if (href && href.indexOf("#") === 0) {
        el.href = location.pathname + el.getAttribute("href");
      }
    }
  }
  // Adjust href for all existing links.
  document.addEventListener("DOMContentLoaded", function() {
    const es = document.getElementsByTagName("a");
    for (var i = 0; i < es.length; i++) {
      fixLink(es[i]);
    }
  });
  // Adjust href for dynamically added links - when they are clicked.
  document.addEventListener("click", function(ev) { fixLink(ev.target); });
})();

function mermaidClick(nodeId) {
  if (nodeId) {
    window.location.hash = "#" + nodeId;
  }
}
