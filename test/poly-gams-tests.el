;;; poly-gams-tests.el --- Tests for poly-gams -*- lexical-binding: t -*-

;; Copyright (C) Shiro Takeda

;; This file is *NOT* part of GNU Emacs.

;;; Commentary:

;; ERT test suite for `poly-gams'.  The tests assert on the *effective* major
;; mode that polymode installs for the span at a given position, via
;; `pm-span-mode', which is what a user actually experiences.
;;
;; Run from the project root with `make test', or equivalently:
;;
;;   emacs -Q --batch --eval '(package-initialize)' \
;;     -L . -L test \
;;     -l test/poly-gams-tests.el \
;;     -f ert-run-tests-batch-and-exit
;;
;; Note that `-L' only puts a directory on `load-path'; it does not autoload
;; anything.  `yaml-mode' is therefore required explicitly below, and the
;; Connect tests skip themselves when it is absent, since it is an optional
;; dependency.
;;
;; The fixtures use the syntax documented at
;; https://www.gams.com/latest/docs/UG_EmbeddedCode.html
;;
;; Every marker inside a fixture is unique, so a test can address a position by
;; searching for its marker without ambiguity.

;;; Code:

(require 'ert)
(require 'seq)
(require 'yaml-mode nil t)              ; optional dependency
(require 'poly-gams)

;;; Helpers

(defmacro poly-gams-tests-with-fixture (content &rest body)
  "Turn on `poly-gams-mode' in a temporary buffer holding CONTENT, run BODY.
Killing the base buffer also kills the indirect buffers polymode
creates, so no further cleanup is needed."
  (declare (indent 1) (debug (form body)))
  `(let ((buffer (generate-new-buffer " *poly-gams-tests*")))
     (unwind-protect
         (with-current-buffer buffer
           (insert ,content)
           (poly-gams-mode)
           (goto-char (point-min))
           ,@body)
       (kill-buffer buffer))))

(defconst poly-gams-tests-yaml-modes '(yaml-mode yaml-ts-mode)
  "Modes that count as \"YAML\" when checking a Connect section.
Which one is installed is up to the user: poly-gams asks polymode
to resolve the name \"yaml\", which consults
`major-mode-remap-alist' and `auto-mode-alist'.")

(defun poly-gams-tests-skip-unless-yaml ()
  "Skip the current test unless a YAML mode is available."
  (unless (seq-some #'fboundp poly-gams-tests-yaml-modes)
    (ert-skip "no YAML mode available; it is an optional dependency")))

(defun poly-gams-tests--goto-marker (marker)
  "Move point to the beginning of MARKER, or signal an error.
Signalling keeps a typo in a fixture from masquerading as a test
failure."
  (goto-char (point-min))
  (unless (search-forward marker nil t)
    (error "Fixture does not contain the marker %S" marker))
  (goto-char (match-beginning 0)))

(defun poly-gams-tests--walk-to (position)
  "Compute the span of every line from `point-min' up to POSITION.
Polymode caches span decisions, so the order in which spans are
requested is part of the behaviour under test.  Display and
fontification visit a buffer forwards from the top, and a lookup
order that differs from that can hide real bugs; this helper
reproduces the realistic order."
  (goto-char (point-min))
  (while (< (point) position)
    (pm-span-mode)
    (forward-line 1)))

(defun poly-gams-tests-mode-at (marker)
  "Return the effective major mode of the span containing MARKER.
Spans are first computed forwards from the top of the buffer, as
fontification would do."
  (poly-gams-tests--goto-marker marker)
  (let ((target (point)))
    (poly-gams-tests--walk-to target)
    (goto-char target)
    (pm-span-mode)))

(defun poly-gams-tests-span-type-at (marker)
  "Return the span type (nil, `head', `body' or `tail') at MARKER."
  (poly-gams-tests--goto-marker marker)
  (let ((target (point)))
    (poly-gams-tests--walk-to target)
    (goto-char target)
    (car (pm-innermost-span))))

;;; Fixtures

(defconst poly-gams-tests-compile-time "\
set i / a /;
$onEmbeddedCode Python:
marker_on_plain = 1
$offEmbeddedCode
$onEmbeddedCodeS Python:
marker_on_s = 1
$offEmbeddedCode
$onEmbeddedCodeV Python:
marker_on_v = 1
$offEmbeddedCode
$onEmbeddedCode.mytag Python:
marker_on_tag = 1
$offEmbeddedCode.mytag
display i;
"
  "Compile-time embedded code: plain, S and V variants, and a tag.")

(defconst poly-gams-tests-execution-time "\
set i / a /;
embeddedCode Python:
marker_exec_plain = 1
endEmbeddedCode
embeddedCodeS Python:
marker_exec_s = 1
endEmbeddedCode
embeddedCodeV Python:
marker_exec_v = 1
endEmbeddedCode
embeddedCode.mytag Python:
marker_exec_tag = 1
endEmbeddedCode.mytag
display i;
"
  "Execution-time embedded code: plain, S and V variants, and a tag.")

(defconst poly-gams-tests-pause-continue "\
set i / a /;
embeddedCode Python:
marker_before_pause = 1
pauseEmbeddedCode
marker_between_pause_and_continue = 0;
continueEmbeddedCode:
marker_after_continue = 1
endEmbeddedCode
display i;
"
  "A paused and continued Python section, with host code in between.")

(defconst poly-gams-tests-continue-handle "\
set i / a /;
EmbeddedCode Python:
marker_paused = 1
pauseEmbeddedCode
scalar handle;
handle = embeddedHandle;
continueEmbeddedCode handle:
marker_after_handle = 1
endEmbeddedCode
display i;
"
  "A Python section resumed through an explicit handle.
The word before the colon on a continuation is a handle, not an
engine name.  Taken from EMBPY04 in the GAMS test library.")

(defconst poly-gams-tests-continue-gams-engine "\
set i / a /;
embeddedCode GAMS: args
marker_gams_paused = 0;
pauseEmbeddedCode
display i;
continueEmbeddedCode GAMS:
marker_gams_continued = 0;
endEmbeddedCode
"
  "A GAMS-engine section resumed with the engine named again.
Here the word before the colon really is an engine, and it is one
without a dedicated inner mode.")

(defconst poly-gams-tests-connect "\
set i / a /;
embeddedCode Connect:
- GAMSReader:
    symbols: [{name: marker_connect}]
endEmbeddedCode
display i;
"
  "An embedded Connect (YAML) section.")

(defconst poly-gams-tests-case-folding "\
set i / a /;
EMBEDDEDCODE PYTHON:
marker_upcased = 1
ENDEMBEDDEDCODE
display i;
"
  "Embedded code keywords in upper case; GAMS is case insensitive.")

(defconst poly-gams-tests-host-only "\
set i / a, b /;
parameter marker_host_parameter(i);
marker_host_parameter(i) = 1;
"
  "A file with no embedded code at all.")

(defconst poly-gams-tests-gams-engine-first "\
set i / a /;
embeddedCode GAMS: args
marker_gams_engine_body = 0;
endEmbeddedCode
embeddedCode Python:
marker_python_after_gams = 1
endEmbeddedCode
embeddedCode Connect:
- GAMSReader:
    symbols: [{name: marker_connect_after_gams}]
endEmbeddedCode
"
  "A GAMS-engine section preceding Python and Connect sections.
The GAMS engine has no dedicated innermode, so its body stays in
the host mode; the sections after it must still be recognised.")

(defconst poly-gams-tests-commented-out "\
set i / a /;
$onText
embeddedCode Python:
marker_inside_ontext = 1
endEmbeddedCode
$offText
display i;
"
  "Embedded code syntax appearing inside a $onText comment block.")

;;; Compile-time syntax

(ert-deftest poly-gams-test-compile-time-plain ()
  "`$onEmbeddedCode Python:' bodies are Python."
  (poly-gams-tests-with-fixture poly-gams-tests-compile-time
    (should (eq (poly-gams-tests-mode-at "marker_on_plain") 'python-mode))))

(ert-deftest poly-gams-test-compile-time-s-variant ()
  "`$onEmbeddedCodeS' bodies are Python."
  (poly-gams-tests-with-fixture poly-gams-tests-compile-time
    (should (eq (poly-gams-tests-mode-at "marker_on_s") 'python-mode))))

(ert-deftest poly-gams-test-compile-time-v-variant ()
  "`$onEmbeddedCodeV' bodies are Python."
  (poly-gams-tests-with-fixture poly-gams-tests-compile-time
    (should (eq (poly-gams-tests-mode-at "marker_on_v") 'python-mode))))

(ert-deftest poly-gams-test-compile-time-tag ()
  "A `.tag' suffix does not prevent recognition."
  (poly-gams-tests-with-fixture poly-gams-tests-compile-time
    (should (eq (poly-gams-tests-mode-at "marker_on_tag") 'python-mode))))

;;; Execution-time syntax

(ert-deftest poly-gams-test-execution-time-plain ()
  "`embeddedCode Python:' bodies are Python."
  (poly-gams-tests-with-fixture poly-gams-tests-execution-time
    (should (eq (poly-gams-tests-mode-at "marker_exec_plain") 'python-mode))))

(ert-deftest poly-gams-test-execution-time-s-variant ()
  "`embeddedCodeS' bodies are Python."
  (poly-gams-tests-with-fixture poly-gams-tests-execution-time
    (should (eq (poly-gams-tests-mode-at "marker_exec_s") 'python-mode))))

(ert-deftest poly-gams-test-execution-time-v-variant ()
  "`embeddedCodeV' bodies are Python."
  (poly-gams-tests-with-fixture poly-gams-tests-execution-time
    (should (eq (poly-gams-tests-mode-at "marker_exec_v") 'python-mode))))

(ert-deftest poly-gams-test-execution-time-tag ()
  "A `.tag' suffix does not prevent recognition at execution time."
  (poly-gams-tests-with-fixture poly-gams-tests-execution-time
    (should (eq (poly-gams-tests-mode-at "marker_exec_tag") 'python-mode))))

;;; Pause and continue

(ert-deftest poly-gams-test-body-before-pause ()
  "Code before `pauseEmbeddedCode' is Python."
  (poly-gams-tests-with-fixture poly-gams-tests-pause-continue
    (should (eq (poly-gams-tests-mode-at "marker_before_pause") 'python-mode))))

(ert-deftest poly-gams-test-host-between-pause-and-continue ()
  "GAMS code between `pauseEmbeddedCode' and `continueEmbeddedCode' is host."
  (poly-gams-tests-with-fixture poly-gams-tests-pause-continue
    (should (eq (poly-gams-tests-mode-at "marker_between_pause_and_continue")
                'gams-mode))))

(ert-deftest poly-gams-test-body-after-continue ()
  "Code after a bare `continueEmbeddedCode:' inherits the Python engine."
  (poly-gams-tests-with-fixture poly-gams-tests-pause-continue
    (should (eq (poly-gams-tests-mode-at "marker_after_continue") 'python-mode))))

(ert-deftest poly-gams-test-continue-with-handle ()
  "A handle on a continuation is not mistaken for an engine name."
  (poly-gams-tests-with-fixture poly-gams-tests-continue-handle
    (should (eq (poly-gams-tests-mode-at "marker_after_handle") 'python-mode))))

(ert-deftest poly-gams-test-continue-with-gams-engine ()
  "`continueEmbeddedCode GAMS:' resumes a section that stays host."
  (poly-gams-tests-with-fixture poly-gams-tests-continue-gams-engine
    (should (eq (poly-gams-tests-mode-at "marker_gams_paused") 'gams-mode))
    (should (eq (poly-gams-tests-mode-at "marker_gams_continued") 'gams-mode))))

;;; Connect

(ert-deftest poly-gams-test-connect-is-yaml ()
  "`embeddedCode Connect:' bodies use the YAML mode."
  (poly-gams-tests-skip-unless-yaml)
  (poly-gams-tests-with-fixture poly-gams-tests-connect
    (should (memq (poly-gams-tests-mode-at "marker_connect")
                  poly-gams-tests-yaml-modes))))

;;; Case folding

(ert-deftest poly-gams-test-case-insensitive ()
  "Embedded code keywords are recognised in upper case."
  (poly-gams-tests-with-fixture poly-gams-tests-case-folding
    (should (eq (poly-gams-tests-mode-at "marker_upcased") 'python-mode))))

;;; Host mode

(ert-deftest poly-gams-test-host-only-buffer ()
  "A buffer without embedded code is entirely in the host mode."
  (poly-gams-tests-with-fixture poly-gams-tests-host-only
    (should (eq (poly-gams-tests-mode-at "marker_host_parameter") 'gams-mode))))

(ert-deftest poly-gams-test-head-and-tail-are-host ()
  "The delimiter lines themselves stay in the host mode."
  (poly-gams-tests-with-fixture poly-gams-tests-execution-time
    (should (eq (poly-gams-tests-mode-at "embeddedCode Python:") 'gams-mode))
    (should (eq (poly-gams-tests-span-type-at "embeddedCode Python:") 'head))))

;;; GAMS engine sections must not hide later sections

;; A section whose engine has no dedicated inner mode used to hide every
;; section after it: the per-engine head matcher returned nil, which polymode
;; reads as "no inner span anywhere ahead", and it then cached a host span
;; reaching to the end of the buffer.  The bug only showed up when spans were
;; computed forwards from the top, which is what display does.

(ert-deftest poly-gams-test-gams-engine-body-is-host ()
  "An `embeddedCode GAMS:' body has no innermode and stays host."
  (poly-gams-tests-with-fixture poly-gams-tests-gams-engine-first
    (should (eq (poly-gams-tests-mode-at "marker_gams_engine_body") 'gams-mode))))

(ert-deftest poly-gams-test-python-after-gams-engine ()
  "A Python section following a GAMS-engine section is still Python."
  (poly-gams-tests-with-fixture poly-gams-tests-gams-engine-first
    (should (eq (poly-gams-tests-mode-at "marker_python_after_gams")
                'python-mode))))

(ert-deftest poly-gams-test-connect-after-gams-engine ()
  "A Connect section following a GAMS-engine section is still YAML."
  (poly-gams-tests-skip-unless-yaml)
  (poly-gams-tests-with-fixture poly-gams-tests-gams-engine-first
    (should (memq (poly-gams-tests-mode-at "marker_connect_after_gams")
                  poly-gams-tests-yaml-modes))))

;;; Comment awareness

;; Also known to fail, and tracked separately from the shadowing bug above: a
;; regexp head matcher cannot tell code from comment.  Fixing it needs a
;; function matcher that consults `syntax-ppss' or the on/off-text state, and is
;; deliberately left for a later change.

(ert-deftest poly-gams-test-ontext-block-is-host ()
  "Embedded code syntax inside `$onText' is a comment, not a real section."
  :expected-result :failed
  (poly-gams-tests-with-fixture poly-gams-tests-commented-out
    (should (eq (poly-gams-tests-mode-at "marker_inside_ontext") 'gams-mode))))

(provide 'poly-gams-tests)

;;; poly-gams-tests.el ends here
