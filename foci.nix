{ pkgs, ... }:

let
  emacsGtk = pkgs.emacs30-gtk3;
  emacsPgtk = pkgs.emacs30-pgtk;
  emacsPackages = pkgs.emacsPackagesFor emacsPgtk;
  emacsWithPackages = emacsPackages.emacsWithPackages;

  # --- Alloy 6 support (not packaged in nixpkgs) --------------------------
  # Tree-sitter grammar for the Alloy 6 specification language.
  tree-sitter-alloy = pkgs.tree-sitter.buildGrammar {
    language = "alloy";
    version = "0-unstable-2026-03-14";
    src = pkgs.fetchFromGitHub {
      owner = "vincentlu";
      repo = "tree-sitter-alloy";
      rev = "7bcc865b0d12ff4f20876efd072fd1bc11faff48";
      hash = "sha256-pkSsv0zrVmwtWCfE3Luji6Gt0gtv70dZy35Muga9A0A=";
    };
    meta.homepage = "https://github.com/vincentlu/tree-sitter-alloy";
  };

  # Tree-sitter-backed major mode for Alloy 6.
  alloy-ts-mode = emacsPackages.trivialBuild {
    pname = "alloy-ts-mode";
    version = "0-unstable-2026-03-10";
    src = pkgs.fetchFromGitHub {
      owner = "vincentlu";
      repo = "alloy-ts-mode";
      rev = "2e8764db16ed81669f0be17bd1056d1b70acaf96";
      hash = "sha256-wT1N6Xgq0LWtIoGcojVOfcYxRR+6GQULg4vs1/4XLZg=";
    };
    meta.homepage = "https://github.com/vincentlu/alloy-ts-mode";
  };

  # lsp-mode client for the Alloy language server (bundled with the Alloy
  # distribution; invoked as `alloy6 lsp` in this flake).
  alloy-lsp = emacsPackages.trivialBuild {
    pname = "alloy-lsp";
    version = "0-unstable-2026-03-10";
    src = pkgs.fetchFromGitHub {
      owner = "vincentlu";
      repo = "alloy-lsp";
      rev = "01f8b437e5c0f79cf6fdc1c4b95098db900f30e4";
      hash = "sha256-Bj+FrQjZvkSN4JQq2aFy1Tq3ITHZX0RecEf1UID0Y/c=";
    };
    packageRequires = [ emacsPackages.lsp-mode ];
    meta.homepage = "https://github.com/vincentlu/alloy-lsp";
  };
in
emacsWithPackages (epkgs: (with epkgs.melpaPackages; [
  # Use docker-tramp for emacs 28.x, for newer emacs, builtin tramp container should suffice
  # docker-tramp
  codespaces
  winum
  dockerfile-mode
  font-lock-studio
  all-the-icons
  move-text
  all-the-icons-dired
  realgud
  powerline
  flycheck
  htmlize
  highlight-symbol
  projectile
  window-purpose
  polymode
  visual-fill-column
  fill-column-indicator
  demap
  paradox
  vline
  package-build
  edit-indirect
  editorconfig
  ansi
  memoize
  f
  s
  yasnippet
  origami
  which-key
  # org
  toc-org
  ox-gfm
  org-bullets
  org-roam
  # terms
  vterm
  xterm-color
  # company
  company
  company-box
  company-posframe
  company-quickhelp
  company-shell
  company-math
  # magit
  magit
  forge
  magit-delta
  # helm
  helm
  helm-gtags
  helm-xref
  # flyspell
  flyspell-correct
  flyspell-correct-helm
  # langs
  nix-mode
  php-mode
  kotlin-mode
  nginx-mode
  json-mode
  groovy-mode
  cmake-mode
  company-auctex                # Tex
  company-c-headers             # C
  disaster                      # C
  jq-mode                       # jq
  slime                         # Lisp
  slime-company                 # Lisp
  aggressive-indent             # Lisp alike (elisp)
  paredit                       # Lisp alike (elisp)
  elpy                          # Python
  conda                         # Python
  # jupyter
  ninja-mode
  opencl-c-mode
  racket-mode
  lua-mode
  meson-mode
  julia-mode
  pyim
  rust-mode                     # Rust
  cargo                         # Rust
  scala-mode
  toml-mode
  vala-mode
  cython-mode
  ess                           # R/S
  geiser                        # Scheme
  go-mode
  haskell-mode
  markdown-mode
  markdown-toc
  powershell
  sage-shell-mode
  # lsp mode
  lsp-mode
  lsp-ui
  # lsp ext
  lsp-treemacs
  lsp-docker
  lsp-origami
  lsp-grammarly
  dap-mode
  # lsp langs
  lsp-metals
  lsp-haskell
  lsp-java
  # treemacs
  treemacs
  # mu
  mu4e-alert
  # llm
  gptel
  aidermacs
  copilot
  copilot-chat
  agent-shell
]) ++ (with epkgs.elpaPackages; [
  auctex         # ; LaTeX mode
  sml-mode
  project
  rainbow-mode
  debbugs
  csv-mode
  minimap
  xref
  xr
  eldoc
]) ++ (with epkgs.nongnuPackages; [
  eat
]) ++ (with epkgs; [
  bind-key
  cask
  # Tree sitter: all nixpkgs grammars plus our out-of-tree Alloy grammar
  (treesit-grammars.with-grammars
    (g: (builtins.attrValues g) ++ [ tree-sitter-alloy ]))
  # Alloy 6 support (out-of-tree; see `let` bindings above)
  alloy-ts-mode
  alloy-lsp
]) ++ (with pkgs; [
  mu
  alloy6                        # Alloy CLI + bundled language server
]))
