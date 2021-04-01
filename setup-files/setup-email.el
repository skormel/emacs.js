;;; setup-docker.el -*- lexical-binding: t; -*-
;; Time-stamp: <2021-04-01 19:19:00 adelgado>

;; Copyright (C) 2016-2020 Antonio Delgado
;; Author: Antonio Delgado <adelgado@kalifato.com>

;; dockerfile-mode: An emacs mode for handling Dockerfiles

;; docker: manager docker from emacs
;; https://github.com/emacs-straight/excorporate
(use-package excorporate
  :defer t
  :config
  (setq excorporate-configuration '("adelgado496@alumno.uned.es" . "https://outlook.office365.com/EWS/Exchange.asmx"))
  )

;; Extra documents
;; https://www.emacswiki.org/emacs/MsOutlook
