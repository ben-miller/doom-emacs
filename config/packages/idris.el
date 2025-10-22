;;; idris.el --- Idris2 configuration -*- lexical-binding: t; -*-

(use-package! idris2-mode
  :mode "\\.idr\\'"
  :config
  (setq idris2-interpreter-path "idris2"))
