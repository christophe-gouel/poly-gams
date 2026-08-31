;;; poly-gams.el --- Polymode for GAMS -*- lexical-binding: t -*-
;;
;; Author: Shiro Takeda
;; Maintainer: Shiro Takeda
;; Copyright (C) Shiro Takeda
;; Version: 0.9
;; First created: 2022-06-25
;; Package-Requires: ((emacs "25") (polymode "0.2.2") (gams-mode "6.12"))
;; URL: https://github.com/ShiroTakeda/poly-gams
;; Keywords: languages, multi-modes, GAMS
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file is *NOT* part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; This package provides polymode support for GAMS, allowing for
;; embedded Python and YAML code blocks within GAMS files.
;;
;; A single innermode covers every embedded code section, and the engine named
;; on the head line decides which major mode its body gets.  Python sections
;; use the Python mode, Connect sections the YAML mode, and engines with no
;; dedicated mode, such as GAMS and ReSHOP, keep the host GAMS mode.
;;
;; The engine is mapped to a mode *name*, which polymode resolves through
;; `polymode-mode-name-aliases', `major-mode-remap-alist' and
;; `auto-mode-alist'.  Whichever mode your Emacs already uses for .py and .yaml
;; files is therefore used here too, including the tree-sitter modes.  No YAML
;; mode is required as a dependency: install `yaml-mode', or a YAML tree-sitter
;; grammar, and Connect sections will follow.


;;; Code:

(require 'gams-mode)
(require 'polymode)

(define-hostmode poly-gams-hostmode
  :mode 'gams-mode)

(defvar poly-gams-head-regexp
  "^[ \t]*\\($on\\|continue\\)*embeddedcode[^ \t\n:]*[ \t:]+"
  "Regular expression for the start part of embedded codes.")

(defvar poly-gams-tail-regexp
  "^\\($[ \t]*off\\|[ \t]*end\\|[ \t]*pause\\)+embeddedcode.*"
  "Regular expression for the end part of embedded codes.")

(defconst poly-gams--engine-mode-names
  '(("python"  . "python")
    ("connect" . "yaml")
    ("gams"    . nil)
    ("reshop"  . nil))
  "Map GAMS embedded code engine names to Emacs major mode names.
Keys are lower case.  A nil value marks an engine that has no
dedicated inner mode, so its sections stay in the host mode: the
body of a GAMS section is GAMS code, and that of a ReSHOP section
is a small declarative EMP syntax.

Listing those two rather than leaving them out matters on a
`continueEmbeddedCode' line, where the word before the colon is an
optional handle rather than an engine.  A word absent from this
alist is therefore taken to be a handle.

Values are mode *names* rather than symbols on purpose: polymode
turns a name into a symbol with `pm-get-mode-symbol-from-name',
which honours `polymode-mode-name-aliases',
`major-mode-remap-alist' and `auto-mode-alist'.  A user whose YAML
files open in `yaml-ts-mode' therefore gets that mode inside
Connect sections without configuring anything here.")

(defconst poly-gams--head-engine-regexp
  "embeddedcode[sv]?\\(?:\\.[^ \t\n:]+\\)?[ \t]+\\([[:alpha:]]+\\)[ \t]*:"
  "Regexp matching an embedded code head that names its engine.
Group 1 is the engine name.  Covers the S and V variants and an
optional `.tag' suffix.")

(defconst poly-gams--opener-engine-regexp
  (concat "^[ \t]*\\(?:\\$on\\)?" poly-gams--head-engine-regexp)
  "Regexp matching an embedded code opener that names its engine.
Deliberately does not match a continuation, so that it can be used
to recover the engine of a `continueEmbeddedCode' line, which
names an optional handle rather than an engine.")

(defconst poly-gams--continuation-regexp "continueembeddedcode"
  "Regexp matching the keyword that resumes a paused section.")

(defun poly-gams--mode-matcher ()
  "Return the mode name for the embedded code head at point, or nil.
Called by polymode with point at the beginning of a head span.
Returning nil selects the innermode's `:fallback-mode', which is
the host mode, so sections of an engine without a dedicated inner
mode are left as GAMS code.

On a continuation the word before the colon is an optional handle,
not an engine, so a word that is not a known engine sends the
search back to the section that was paused.  Which section a given
handle refers to is a run time value and cannot be known here, so
the nearest opener above is assumed."
  (let* ((case-fold-search t)
         (eol (line-end-position))
         (continuation
          (save-excursion
            (re-search-forward poly-gams--continuation-regexp eol t)))
         (word
          (save-excursion
            (when (re-search-forward poly-gams--head-engine-regexp eol t)
              (downcase (match-string-no-properties 1)))))
         (entry (and word (assoc word poly-gams--engine-mode-names))))
    (cond
     ;; The head names a known engine, as in `embeddedCode Python:' or
     ;; `continueEmbeddedCode GAMS:'.
     (entry (cdr entry))
     ;; A continuation, either bare or naming a handle.
     (continuation
      (save-excursion
        (goto-char (line-beginning-position))
        (when (re-search-backward poly-gams--opener-engine-regexp nil t)
          (cdr (assoc (downcase (match-string-no-properties 1))
                      poly-gams--engine-mode-names)))))
     ;; An opener whose engine we do not handle.
     (t nil))))

;; A single innermode owns every embedded code section, whatever its engine.
;; The head and tail matchers are the plain regexps, so polymode does the
;; searching and there is no way for one section to hide the sections after it;
;; the engine is resolved per section afterwards by the mode matcher.
(define-auto-innermode poly-gams-embedded-innermode
  :head-matcher poly-gams-head-regexp
  :tail-matcher poly-gams-tail-regexp
  :mode-matcher #'poly-gams--mode-matcher
  :head-mode 'host
  :tail-mode 'host
  :fallback-mode 'host)

;;;###autoload (autoload 'poly-gams-mode "poly-gams" nil t)
(define-polymode poly-gams-mode
  :hostmode 'poly-gams-hostmode
  :innermodes '(poly-gams-embedded-innermode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.gms\\'" . poly-gams-mode))

(provide 'poly-gams)

;;; poly-gams.el ends here
