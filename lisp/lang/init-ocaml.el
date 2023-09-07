;;; init-ocaml.el --- Support the OCaml language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; OCaml
(use-package utop)
(use-package ocamlformat)
(use-package dune-format)
(use-package dune)

(use-package tuareg
:defer t
:commands (ocamlformat-before-save)
:config
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
(define-key tuareg-mode-map (kbd "C-I") 'ocamlformat-before-save))

(provide 'init-ocaml)
;;; init-ocaml.el ends here
