# Pin mdbook-mermaid to 0.16.x from an older nixpkgs.
#
# haskell.nix's nixpkgs ships mdbook-mermaid 0.17 which requires
# mdbook 0.5, but still ships mdbook 0.4.52. The 0.17 preprocessor
# can't parse the 0.4.52 JSON context and crashes with
# "Unable to parse the input".
#
# Until haskell.nix bumps mdbook to 0.5, pin mdbook-mermaid from
# a nixpkgs revision where it was still 0.16.x.
let
  oldNixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/647e5c14cbd5067f44ac86b74f014962df460840.tar.gz";
    sha256 = "0m30xfi18dgxi5lx9zgqpq1kxwdwrdg9wdbzzgs8cicmsvq6ami5";
  };
in
final: prev: {
  mdbook-mermaid = (import oldNixpkgs { inherit (final) system; }).mdbook-mermaid;
}
