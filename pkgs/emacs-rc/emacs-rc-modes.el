;;; emacs-rc-modes.el --- Require in miscellaneous modes
;;;
;;; Commentary:
;;;
;;; This module requires in all the miscellaneous modes that don't really belong
;;; anywhere else and don't have any configuration to warrant their own module.
;;;
;;; Code:

(eval-when-compile (require 'use-package))

(use-package csv-mode)
(use-package dhall-mode)
(use-package diff-mode)
(use-package dockerfile-mode)
(use-package go-mode)
(use-package graphql-mode)
(use-package groovy-mode)
(use-package haskell-mode)
(use-package json-mode)
(use-package kotlin-mode)
(use-package nix-mode)
(use-package rust-mode)
(use-package scad-mode)
(use-package svelte-mode)
(use-package yaml-mode)

(provide 'emacs-rc-modes)
;;; emacs-rc-modes.el ends here
