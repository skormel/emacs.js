;;; setup-telegram.el -*- lexical-binding: t; -*-
;; Time-stamp: <2021-02-27 12:42:50 adelgado>

;; Copyright (C) 2016-2020 Antonio Delgado
;; Author: Antonio Delgado <adelgado@chaputronia.com>

;; manage telegram from emacs
;; https://github.com/zevlg/telega.el
(use-package telega
  :defer t
  :bind ("C-c p t" . telega)
  )

(provide 'setup-telegram)

