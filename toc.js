// Populate the sidebar
//
// This is a script, and not included directly in the page, to control the total size of the book.
// The TOC contains an entry for each page, so if each page includes a copy of the TOC,
// the total size of the page becomes O(n**2).
class MDBookSidebarScrollbox extends HTMLElement {
    constructor() {
        super();
    }
    connectedCallback() {
        this.innerHTML = '<ol class="chapter"><li class="chapter-item expanded "><a href="index.html"><strong aria-hidden="true">1.</strong> Introduction</a></li><li class="chapter-item expanded "><a href="user.html"><strong aria-hidden="true">2.</strong> User Manual</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="user/when.html"><strong aria-hidden="true">2.1.</strong> When to use</a></li><li class="chapter-item expanded "><a href="user/common-use-cases.html"><strong aria-hidden="true">2.2.</strong> How to</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="user/common-use-cases/start-wallet-server.html"><strong aria-hidden="true">2.2.1.</strong> Start a server</a></li><li class="chapter-item expanded "><a href="user/common-use-cases/create-a-wallet.html"><strong aria-hidden="true">2.2.2.</strong> Create a wallet</a></li><li class="chapter-item expanded "><a href="user/common-use-cases/how-to-manage-wallets.html"><strong aria-hidden="true">2.2.3.</strong> Manage wallets</a></li><li class="chapter-item expanded "><a href="user/common-use-cases/how-to-create-addresses.html"><strong aria-hidden="true">2.2.4.</strong> Create addresses</a></li><li class="chapter-item expanded "><a href="user/common-use-cases/how-to-make-a-transaction.html"><strong aria-hidden="true">2.2.5.</strong> Create a transaction</a></li><li class="chapter-item expanded "><a href="user/common-use-cases/assets.html"><strong aria-hidden="true">2.2.6.</strong> Handle assets</a></li><li class="chapter-item expanded "><a href="user/common-use-cases/delegation.html"><strong aria-hidden="true">2.2.7.</strong> Handle delegation</a></li><li class="chapter-item expanded "><a href="user/common-use-cases/handle-metadata.html"><strong aria-hidden="true">2.2.8.</strong> Handle metadata</a></li><li class="chapter-item expanded "><a href="user/common-use-cases/shared-wallets.html"><strong aria-hidden="true">2.2.9.</strong> Create shared-wallets</a></li></ol></li><li class="chapter-item expanded "><a href="user/installation.html"><strong aria-hidden="true">2.3.</strong> Installation</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="user/installation/use-docker.html"><strong aria-hidden="true">2.3.1.</strong> Use docker</a></li><li class="chapter-item expanded "><a href="user/installation/use-nixos.html"><strong aria-hidden="true">2.3.2.</strong> Use NixOS</a></li></ol></li><li class="chapter-item expanded "><a href="user/cli.html"><strong aria-hidden="true">2.4.</strong> CLI</a></li><li class="chapter-item expanded "><a href="user/http-api.html"><strong aria-hidden="true">2.5.</strong> HTTP-API</a></li><li class="chapter-item expanded "><a href="user/hardware-recommendations.html"><strong aria-hidden="true">2.6.</strong> Hardware Recommendations</a></li><li class="chapter-item expanded "><a href="user/security.html"><strong aria-hidden="true">2.7.</strong> Security</a></li><li class="chapter-item expanded "><a href="user/integrations.html"><strong aria-hidden="true">2.8.</strong> Known integrations</a></li><li class="chapter-item expanded "><a href="user/ekg-and-prometheus.html"><strong aria-hidden="true">2.9.</strong> EKG and prometheus</a></li><li class="chapter-item expanded "><a href="user/common-use-cases/plutus-application-backend.html"><strong aria-hidden="true">2.10.</strong> Plutus application backend</a></li><li class="chapter-item expanded "><a href="user/faq.html"><strong aria-hidden="true">2.11.</strong> FAQ</a></li></ol></li><li class="chapter-item expanded "><a href="design.html"><strong aria-hidden="true">3.</strong> Design</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="design/architecture.html"><strong aria-hidden="true">3.1.</strong> Architecture</a></li><li class="chapter-item expanded "><a href="design/adrestia-architecture.html"><strong aria-hidden="true">3.2.</strong> Adrestia Architecture</a></li><li class="chapter-item expanded "><a href="design/links.html"><strong aria-hidden="true">3.3.</strong> Links</a></li><li class="chapter-item expanded "><a href="design/concepts.html"><strong aria-hidden="true">3.4.</strong> Concepts</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="design/concepts/eras.html"><strong aria-hidden="true">3.4.1.</strong> Eras</a></li><li class="chapter-item expanded "><a href="design/concepts/recovery-phrases.html"><strong aria-hidden="true">3.4.2.</strong> Recovery Phrases</a></li><li class="chapter-item expanded "><a href="design/concepts/master-key-generation.html"><strong aria-hidden="true">3.4.3.</strong> Master Key Generation</a></li><li class="chapter-item expanded "><a href="design/concepts/Notes-about-BIP-44.html"><strong aria-hidden="true">3.4.4.</strong> Notes about BIP 44</a></li><li class="chapter-item expanded "><a href="design/concepts/address-derivation.html"><strong aria-hidden="true">3.4.5.</strong> Address Derivation</a></li><li class="chapter-item expanded "><a href="design/concepts/byron-address-format.html"><strong aria-hidden="true">3.4.6.</strong> Byron Address Format</a></li><li class="chapter-item expanded "><a href="design/concepts/coin-selection.html"><strong aria-hidden="true">3.4.7.</strong> Coin Selection</a></li><li class="chapter-item expanded "><a href="design/concepts/hierarchical-deterministic-wallets.html"><strong aria-hidden="true">3.4.8.</strong> Hierarchical Deterministic Wallets</a></li><li class="chapter-item expanded "><a href="design/concepts/transaction-lifecycle.html"><strong aria-hidden="true">3.4.9.</strong> Transaction Lifecycle</a></li><li class="chapter-item expanded "><a href="design/concepts/utxo.html"><strong aria-hidden="true">3.4.10.</strong> UTxO</a></li><li class="chapter-item expanded "><a href="design/concepts/multisig.html"><strong aria-hidden="true">3.4.11.</strong> Multi-signatures</a></li></ol></li><li class="chapter-item expanded "><a href="design/specs.html"><strong aria-hidden="true">3.5.</strong> Specifications</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="design/specs/wallet-id.html"><strong aria-hidden="true">3.5.1.</strong> Wallet ID</a></li></ol></li><li class="chapter-item expanded "><a href="design/prototypes.html"><strong aria-hidden="true">3.6.</strong> Prototypes</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="design/prototypes/light-mode.html"><strong aria-hidden="true">3.6.1.</strong> Light Mode</a></li></ol></li></ol></li><li class="chapter-item expanded "><a href="contributor.html"><strong aria-hidden="true">4.</strong> Contributor Manual</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="contributor/what.html"><strong aria-hidden="true">4.1.</strong> What – Code and Languages</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="contributor/what/building.html"><strong aria-hidden="true">4.1.1.</strong> Building</a></li><li class="chapter-item expanded "><a href="contributor/what/coding-standards.html"><strong aria-hidden="true">4.1.2.</strong> Coding Standards</a></li><li class="chapter-item expanded "><a href="contributor/what/logging-guidelines.html"><strong aria-hidden="true">4.1.3.</strong> Logging Guidelines</a></li><li class="chapter-item expanded "><a href="contributor/what/swagger-development.html"><strong aria-hidden="true">4.1.4.</strong> Swagger Development</a></li><li class="chapter-item expanded "><a href="contributor/what/specifying-exceptions-with-servant-and-swagger.html"><strong aria-hidden="true">4.1.5.</strong> Specifying exceptions with Servant and Swagger</a></li><li class="chapter-item expanded "><a href="contributor/what/nix.html"><strong aria-hidden="true">4.1.6.</strong> Nix build language</a></li><li class="chapter-item expanded "><a href="contributor/what/nix-flake.html"><strong aria-hidden="true">4.1.7.</strong> Nix flake</a></li></ol></li><li class="chapter-item expanded "><a href="contributor/how.html"><strong aria-hidden="true">4.2.</strong> How – Processes</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="contributor/how/testing.html"><strong aria-hidden="true">4.2.1.</strong> Testing</a></li><li class="chapter-item expanded "><a href="contributor/how/continuous-integration.html"><strong aria-hidden="true">4.2.2.</strong> Continuous Integration</a></li><li class="chapter-item expanded "><a href="contributor/how/release-process.html"><strong aria-hidden="true">4.2.3.</strong> Release Process</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="contributor/how/release-checklist.html"><strong aria-hidden="true">4.2.3.1.</strong> Release Checklist template</a></li></ol></li><li class="chapter-item expanded "><a href="contributor/how/code-review-guidelines.html"><strong aria-hidden="true">4.2.4.</strong> Code Review Guidelines</a></li></ol></li><li class="chapter-item expanded "><a href="contributor/notes.html"><strong aria-hidden="true">4.3.</strong> Notes</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="contributor/notes/updating-dependencies.html"><strong aria-hidden="true">4.3.1.</strong> Updating Dependencies</a></li><li class="chapter-item expanded "><a href="contributor/notes/notes-from-upgrading-ghc-version.html"><strong aria-hidden="true">4.3.2.</strong> Notes from upgrading GHC version</a></li></ol></li><li class="chapter-item expanded "><a href="contributor/decisions.html"><strong aria-hidden="true">4.4.</strong> Decisions Record</a></li><li><ol class="section"><li class="chapter-item expanded "><a href="contributor/decisions/2025-02-03-dependency-versions.html"><strong aria-hidden="true">4.4.1.</strong> 2025-02-03 - Manage versions in Cabal</a></li><li class="chapter-item expanded "><a href="contributor/decisions/2024-12-03-document-with-code.html"><strong aria-hidden="true">4.4.2.</strong> 2024-12-03 - Store documents alongside code</a></li><li class="chapter-item expanded "><a href="contributor/decisions/2024-03-13-release-process.html"><strong aria-hidden="true">4.4.3.</strong> 2024-03-23 - Release Process</a></li><li class="chapter-item expanded "><a href="contributor/decisions/2023-07-28-workflow-review.html"><strong aria-hidden="true">4.4.4.</strong> 2023-07-28 - Team Workflow</a></li><li class="chapter-item expanded "><a href="contributor/decisions/2023-01-27-continuous-integration.html"><strong aria-hidden="true">4.4.5.</strong> 2023-01-27 - Continuous Integration</a></li><li class="chapter-item expanded "><a href="contributor/decisions/2022-10-04-document-storage.html"><strong aria-hidden="true">4.4.6.</strong> 2022-10-04 - Document Storage</a></li></ol></li></ol></li></ol>';
        // Set the current, active page, and reveal it if it's hidden
        let current_page = document.location.href.toString();
        if (current_page.endsWith("/")) {
            current_page += "index.html";
        }
        var links = Array.prototype.slice.call(this.querySelectorAll("a"));
        var l = links.length;
        for (var i = 0; i < l; ++i) {
            var link = links[i];
            var href = link.getAttribute("href");
            if (href && !href.startsWith("#") && !/^(?:[a-z+]+:)?\/\//.test(href)) {
                link.href = path_to_root + href;
            }
            // The "index" page is supposed to alias the first chapter in the book.
            if (link.href === current_page || (i === 0 && path_to_root === "" && current_page.endsWith("/index.html"))) {
                link.classList.add("active");
                var parent = link.parentElement;
                if (parent && parent.classList.contains("chapter-item")) {
                    parent.classList.add("expanded");
                }
                while (parent) {
                    if (parent.tagName === "LI" && parent.previousElementSibling) {
                        if (parent.previousElementSibling.classList.contains("chapter-item")) {
                            parent.previousElementSibling.classList.add("expanded");
                        }
                    }
                    parent = parent.parentElement;
                }
            }
        }
        // Track and set sidebar scroll position
        this.addEventListener('click', function(e) {
            if (e.target.tagName === 'A') {
                sessionStorage.setItem('sidebar-scroll', this.scrollTop);
            }
        }, { passive: true });
        var sidebarScrollTop = sessionStorage.getItem('sidebar-scroll');
        sessionStorage.removeItem('sidebar-scroll');
        if (sidebarScrollTop) {
            // preserve sidebar scroll position when navigating via links within sidebar
            this.scrollTop = sidebarScrollTop;
        } else {
            // scroll sidebar to current active section when navigating via "next/previous chapter" buttons
            var activeSection = document.querySelector('#sidebar .active');
            if (activeSection) {
                activeSection.scrollIntoView({ block: 'center' });
            }
        }
        // Toggle buttons
        var sidebarAnchorToggles = document.querySelectorAll('#sidebar a.toggle');
        function toggleSection(ev) {
            ev.currentTarget.parentElement.classList.toggle('expanded');
        }
        Array.from(sidebarAnchorToggles).forEach(function (el) {
            el.addEventListener('click', toggleSection);
        });
    }
}
window.customElements.define("mdbook-sidebar-scrollbox", MDBookSidebarScrollbox);
