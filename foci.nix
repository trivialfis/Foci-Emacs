{ pkgs, ... }:

let
  emacsPgtk = pkgs.emacs-gtk;
  # emacsPgtk = pkgs.emacsPgtk;
  emacsWithPackages = (pkgs.emacsPackagesFor emacsPgtk).emacsWithPackages;
in
emacsWithPackages (epkgs: (with epkgs.melpaPackages; [
  # Use docker-tramp for emacs 28.x, for newer emacs, builtin tramp container should suffice
  # docker-tramp
  winum
  dockerfile-mode
  font-lock-studio
  all-the-icons
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
  use-package
  bind-key
  edit-indirect
  ansi
  memoize
  f
  s
  yasnippet
  origami
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
  slime                         # Lisp
  slime-company                 # Lisp
  aggressive-indent             # Lisp alike (elisp)
  paredit                       # Lisp alike (elisp)
  elpy                          # Python
  conda                         # Python
  ninja-mode
  opencl-mode
  racket-mode
  lua-mode
  meson-mode
  pyim
  rust-mode                     # Rust
  cargo                         # Rust
  scala-mode
  toml-mode
  vala-mode
  yaml-mode
  cython-mode
  ess                           # R/S
  geiser                        # Scheme
  go-mode
  haskell-mode
  markdown-mode
  markdown-toc
  yaml-mode
  powershell
  sage-shell-mode
  # lsp mode
  lsp-mode
  lsp-ui
  # lsp ext
  lsp-treemacs
  lsp-docker
  lsp-origami
  dap-mode
  # lsp langs
  lsp-metals
  lsp-haskell
  lsp-java
  # treemacs
  treemacs
  # mu
  mu4e-alert
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
  cask
]) ++ (with pkgs; [
  mu
]))
