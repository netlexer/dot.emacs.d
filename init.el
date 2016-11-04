;;; init.el --- GNU Emacs init file.
;;
;; Filename: init.el
;; Description: GNU Emacs initialisation.
;; Author: Neil Woods
;; Created: Fri Oct 14 19:58:40 2016 (+0100)
;; Version: 20161024
;; Package-Requires: ()
;; Last-Updated: Fri Nov  4 02:46:03 2016 (+0000)
;;           By: Neil Woods
;;     Update #: 231
;; URL: https://github.com/netlexer/dot.emacs.d/blob/master/init.el
;; Keywords: initialization, startup.
;; Compatibility: GNU Emacs >= 24.4
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This is my personal startup file for GNU Emacs.  I've only recently
;; updated to version 25.1.1, though it should run fine on versions
;; of GNU Emacs >= 24.4.
;; Settings saved by the customize system are saved to a separate file
;; (custom-file), which mainly contains settings saved by the Gnu
;; and Melpa package archives. Packages of note which I use include
;; `helm' and `helm-dash', `magit', `company', `paredit', `sx'
;; `yasnippet' and more. 
;; (c) Neil Woods, 1992-2016.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Startup stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(dolist (i '("~/.emacs.d/lisp" "~/.emacs.d/config" "~/elisp/"))
  (add-to-list 'load-path (file-name-as-directory i) t))

(require 'package)
;; default ("gnu") and "melpa" latest elisp packages
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu"   . "https://elpa.gnu.org/packages/"))
      package-check-signature  nil)
(add-hook 'package-menu-mode-hook 'hl-line-mode)
(package-initialize)
;; use paradox package interface.
(setq paradox-execute-asynchronously t)

;;; load customizations.
(setq custom-file (expand-file-name (concat user-emacs-directory "custom.el")))
(load custom-file)

;; useful to list the dependencies of an emacs lisp file.
(require 'elisp-depend)

;; appearance: load a cool theme & mode-line
;;(load-theme 'naquadah t)
(load-theme 'alect-black t)
;; I really like the spacemacs themeing; this is just the mode-line from it.
;; TODO: Spend more time configuring this.
(require 'spaceline-config)
(spaceline-spacemacs-theme)
;(spaceline-emacs-theme)
(spaceline-helm-mode)
(spaceline-info-mode)
(eyebrowse-mode t)
(setq spaceline-workspace-numbers-unicode t
      spaceline-window-numbers-unicode t
      spaceline-minor-modes-separator "|")

;; display symbols like lambda as λ, for example.
(global-prettify-symbols-mode +1)
(global-paren-face-mode)

;; flycheck doesn't use load-path, so set this...
(setq-default flycheck-emacs-lisp-load-path 'inherit)

;; Kill emacs when daemon active (`important')
(defun nw-stop-emacs ()
  (interactive)
  (if (daemonp)
      (save-buffers-kill-emacs)
      (save-buffers-kill-terminal)))

(global-set-key [remap save-buffers-kill-terminal] 'nw-stop-emacs)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Use-package. (TODO: Convert rest of init to use this)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-when-compile (require 'use-package))
(setq use-package-verbose t)
(defun advice--use-package-ensure-elpa (package &optional no-refresh)
  "Prefer the elpa version of built-in packages if available.
This allow installation of org from melpa when :ensure is specified."
  (let ((pkg (assq package package-alist)))
    (if pkg
        t
        (when (and (not no-refresh)
                   (assoc package
                          (bound-and-true-p package-pinned-packages)))
          (package-read-all-archive-contents))
        (setq pkg (assq package package-archive-contents))
        (if (or pkg no-refresh)
            (package-install (cadr pkg))
            (package-refresh-contents)
            (use-package-ensure-elpa package t)))))
(advice-add 'use-package-ensure-elpa :override #'advice--use-package-ensure-elpa)


(use-package image-file
  :config (auto-image-file-mode 1))

;;; Winner
(use-package winner
  :config
  (setq winner-boring-buffers '("*Completions*"
                                "*Compile-Log*"
                                "*inferior-lisp*"
                                "*Fuzzy Completions*"
                                "*Apropos*"
                                "*Help*"
                                "*cvs*"
                                "*Buffer List*"
                                "*Ibuffer*"
                                ))
(winner-mode 1))

;;; W3m
(use-package w3m
    :ensure t
    :init (require 'config-w3m)
    :bind
    (("<f7> h" . w3m)
     :map w3m-mode-map
("F" . w3m-view-url-with-browse-url)))

;;; mu4e
(require 'config-mu4e)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'helm-config)
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
;; (global-set-key (kbd "C-x C-f") #'helm-find-files)  
(helm-mode 1)
;; prefer ido to helm for find-file
(add-to-list 'helm-completing-read-handlers-alist '(find-file . ido))

(define-key global-map [remap list-buffers] 'helm-buffers-list)
(define-key global-map [remap occur] 'helm-occur)
(define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
(unless (boundp 'completion-in-region-function)
  (define-key lisp-interaction-mode-map
    [remap completion-at-point] 'helm-lisp-completion-at-point)
  (define-key emacs-lisp-mode-map
    [remap completion-at-point] 'helm-lisp-completion-at-point))


(setq helm-dash-docsets-path (expand-file-name "~/.local/share/dasht/docsets/")
      helm-dash-browser-func 'w3m-browse-url
      helm-dash-common-docsets '("Bash" "Emacs_Lisp"))
;; support for dash documentation (docset) lookup using helm
(global-set-key "\C-cd" 'helm-dash-at-point)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Set a few variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq Info-directory-list '("/usr/local/share/info/" "/usr/share/info/")
      apropos-do-all t
      column-number-mode t
      display-time-mode t
      ediff-window-setup-function 'ediff-setup-windows-plain
      font-lock-maximum-decoration t
      frame-title-format '("emacs@" system-name " [%b]" )
      global-auto-revert-mode t
      global-font-lock-mode t
      gc-cons-threshold 20000000
      icon-title-format '("[%b]")
      inhibit-startup-screen t
      initial-major-mode 'lisp-interaction-mode
      line-number-mode t
      max-mini-window-height 1
      mouse-autoselect-window -0.5
      mouse-yank-at-point t
      next-line-add-newlines nil
      require-final-newline t
      revert-without-query (cons "TAGS" revert-without-query)
      save-place-file (concat user-emacs-directory "places")
      scroll-preserve-screen-position 'always
      show-paren-mode t
      show-paren-ring-bell-on-mismatch t
      show-paren-style 'expression
      show-trailing-whitespace t
      system-time-locale "Europe/London"
      tab-width 4
      tab-stop-list (number-sequence 4 120 4)
      indent-tabs-mode nil
      transient-mark-mode t
      use-dialog-box nil
      use-file-dialog nil       ;; lines are set to t.
      select-enable-clipboard t  ;; copy emacs clipboard to system
      interprogram-paste-function 'x-cut-buffer-or-selection-value
      vc-follow-symlinks t      ;; avoid 'Symbolic link to Git-controlled
      )




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Convenience functions etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ido-mode is incompatable with helm.
(unless (fboundp 'helm-mode)
  (ido-mode t)
  (setq ido-enable-flex-matching t))

;; http://github.com/nonsequitur/smex/blob/master/README.markdown
;; (smex-initialize)
;; (global-set-key (kbd "M-x") 'smex)
;; (global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
;; (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

;; this works well with smart-mode-line (sml)
(require 'uniquify)
(setq uniquify-buffer-name-style 'nil)  ; or 'forward etc.

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

(save-place-mode 1)

;; disable backups for files in /tmp or in my Mail or News directories.
(defun nw-backup-enable-predicate (filename)
  (and (not (string= "/tmp/" (substring filename 0 5)))
       (not (string-match "~/Mail/" filename))
       (not (string-match "~/News/" filename))))
(setq backup-enable-predicate 'nw-backup-enable-predicate)

(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      make-backup-files t
      backup-by-copying nil
      backup-by-copying-when-linked t
      delete-old-versions t
      kept-new-versions 10
      kept-old-versions 2
      version-control t)

;; Insert date/time string, bound to Meta-f9
(defun nw-insert-time ()
  "Insert current date and time at point."
  (interactive)
  ;; parameters are: FORMAT (like date(1)), TIME (ie now), and an optional
  ;; third argument, UNIVERSAL. Example: Mon, 12 Nov 2001 17:25 +0000
  ;; Example similar to "date -R" cmd. & Debian changelog time format.
  (insert (format-time-string "%a, %-d %b %Y %T %z " (current-time) t)))
(define-key global-map [(meta f9)] 'nw-insert-time)

;; Now define a function to insert the time + personal identity (eg email).
;; neil <neil@nova.lan> -- 11/12/01 17:51:26 : modified format,
;; that is, it doesn't use nw-insert-time anymore.
(setq nw-identifier "Neil Woods <neil@nova>")
(defun nw-insert-ident ()
  (interactive)
  (save-excursion)
  (beginning-of-line)
  (if (stringp comment-start)
      (if (= 1 (string-width comment-start))
	  (insert (concat comment-start comment-start " "))
	(insert comment-start)))
  (insert (concat nw-identifier " -- "
		  (format-time-string "%x %X " (current-time) t)
		  ": ")))
;; Bind it to M-F10
(define-key global-map [(meta f10)] 'nw-insert-ident)

;; Count words in buffer
(defun count-words-buffer ()
  "Count the number of words in current the buffer;
print a message in the minibuffer with the result."
  (interactive)
  (save-excursion
    (let ((count 0))
      (goto-char (point-min))
      (while (< (point) (point-max))
	(forward-word 1)
	(setq count (1+ count)))
      (message "buffer contains %d words." count))))

;; press HOME key to either move to bol or beginning of indentation.
(defun back-to-indentation-or-beginning ()
  (interactive)
  (if (= (point) (save-excursion (back-to-indentation) (point)))
      (beginning-of-line)
    (back-to-indentation)))
(global-set-key [home] 'back-to-indentation-or-beginning)

(defun insert-sequence-key (key)
  "Insert a keystroke suitable for use in fcns like 'global-set-key'."
  (interactive "kInsert key chord: ")
  (insert (format "(kbd \"%s\")" (key-description key))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Global Key assignments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set kp-enter  to `newline-and-indent' - globally; use the vector
;; definition -- this is also bound to C-j.
(define-key global-map [kp-enter] 'newline-and-indent)
;; Bind the Function keys for useful shortcuts
;; Make F1 invoke help : f1 = help; shift-f1 = "man"; ctrl-f1 = info.
;; TODO: Make possibly better/useful bindings here...
(global-set-key [f1]    'help-command)
(global-set-key [S-f1]  'man)
(global-set-key [ (control f1) ] 'info)
(global-set-key [f2]    'start-kbd-macro)	; Also C-X(
(global-set-key [f3]    'end-kbd-macro)		; Also C-X)
(global-set-key [f4]    'call-last-kbd-macro)	; Also C-Xe
(global-set-key [S-f4]  'name-last-kbd-macro)	; So we can save it?
(global-set-key [f5]    'dictionary-lookup-definition)
;;(global-set-key [f6]    'first-error)
;;(global-set-key [f7]    'previous-error)
;;(global-set-key [f8]    'next-error)
(global-set-key [f10]   'ispell-buffer)         ; normally F10 = menu
(global-set-key [S-f10] 'delete-other-windows)
(global-set-key [f11]   'undo)
(global-set-key [f12]   'other-window)
(global-set-key [S-f12] 'delete-window)

(if (string= (getenv "TERM") "screen")
    (progn
       (global-set-key "\e[V"  'scroll-down)
       (global-set-key "\e[U"  'scroll-up)))

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-z") 'zap-up-to-char)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)


;; Re-define the normal `save-buffers-kill-emacs' function when in X to
;; delete the current frame. Will NOT delete the LAST frame.
(global-unset-key "\C-x\C-c")
(global-set-key "\C-x\C-c" 'delete-frame)
(global-set-key [(alt ?i)] 'iconify-frame)   ; Alt-i

(define-key global-map [M-S-down-mouse-3] 'imenu)
(global-set-key [(hyper ?b)] 'ibuffer)

;; smilies. Use F9 then a, b, c etc (more to add later...)
;; a = U+1F603 SMILING FACE WITH OPEN MOUTH
;; b = U+1F609 WINKING FACE
;; c = U+1F606 SMILING FACE WITH OPEN MOUTH AND TIGHTLY-CLOSED EYES
;;
;; (see http://ergoemacs.org/emacs/emacs_n_unicode.html)
;; (maybe better to use autocomplete, see below.)
;;
(global-set-key (kbd "<f9> a") (lambda () (interactive) (insert "😉")))
(global-set-key (kbd "<f9> b") (lambda () (interactive) (insert "😉")))
(global-set-key (kbd "<f9> c") (lambda () (interactive) (insert "😆")))

(global-set-key (kbd "C-x g") 'magit-status)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dired / Ibuffer / Web
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'dired-x)
(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")))

(add-hook 'dired-mode-hook
          (lambda ()
            (setq dired-omit-files "^\\.[a-z|A-Z]+\\|^\\.?#\\|^\\.$")
	    ;; could add dired-omit-extentions here, also.
	    (setq dired-omit-mode nil)   ;; disabled by default
	    (hl-line-mode)
	    (define-key dired-mode-map [delete] 'dired-flag-file-deletion)
	    (define-key dired-mode-map [return] 'dired-find-file-other-window)
	    (define-key dired-mode-map [C-down-mouse-1]
	      'dired-mouse-find-file-other-window)))

(setq ibuffer-formats '((mark modified read-only " " (name 32 32) " "
			      (size 6 -1 :right) " " (mode 16 16 :center)
			      " " (process 8 -1) " " filename)
                        (mark " " (name 36 -1) " " filename))
      ibuffer-elide-long-columns t
      ibuffer-eliding-string "&")

(add-hook 'ibuffer-mode-hook 'hl-line-mode)

;; WWW -- Read URL's with specified browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "/usr/bin/light")

(global-set-key "\C-xm" 'browse-url-at-point)

;; super-click-button-1 (Windows key + click btn 1 )
(global-set-key [s-mouse-1] 'browse-url-at-mouse)

;; turn on ffap (emacs-goodies) (best loaded after browse-url or w3)(drazi)
;;(ffap-bindings)
;;(setq ffap-url-regexp nil)           ; disable URL features in ffap


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Spell check & dictionary lookup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Standard location of personal dictionary
(setq ispell-personal-dictionary "~/.flydict")

(if (file-exists-p "/usr/bin/hunspell")
    (progn
      ;; Add english-hunspell as a dictionary
      (setq-default ispell-program-name "hunspell"
                    ispell-dictionary "en_US"))
  (progn (setq-default ispell-program-name "aspell")
         (setq ispell-extra-args '("--sug-mode=normal" "--ignore=3"))))

(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+"))

;; flyspell
(require 'flyspell)
(define-key flyspell-mode-map (kbd "M-n") 'flyspell-goto-next-error)
(define-key flyspell-mode-map (kbd "M-.") 'ispell-word)

(load "dictionary-init")
(global-set-key "\C-cs" 'dictionary-search)
(global-set-key "\C-cm" 'dictionary-match-words)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Completion modes: dabbrev, hippie-expand, complete, etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Hippie expand enables completion of filenames/dirs in buffers
(require 'hippie-exp)
(setq hippie-expand-verbose t)
(global-set-key [(hyper return)] 'hippie-expand)
(global-set-key [(control tab)] 'hippie-expand)

;; This binds word completions to Shift-Tab.
(global-set-key [S-iso-lefttab] 'dabbrev-completion)

;; Enables completion of recently used words (bound to M-RET & C-RET)
(require 'completion)
(dynamic-completion-mode)
(initialize-completions)

;; Icomplete-mode hooks - contrain minibuffer height...
(add-hook 'icomplete-minibuffer-setup-hook
	  (function
	   (lambda ()
	     (make-local-variable 'resize-minibuffer-window-max-height)
	     (setq resize-minibuffer-window-max-height 3))))

(add-hook 'message-setup-hook 'mail-abbrevs-setup)
(minibuffer-electric-default-mode 1)

;; COMPlete ANYthing basic setup.
(add-hook 'after-init-hook 'global-company-mode)
(company-quickhelp-mode)
(global-set-key (kbd "M-/") 'company-complete)
;; (popwin-mode nil)
(which-key-mode)
(which-key-setup-side-window-right-bottom)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parens, paredit & related, eldoc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define function to match a parenthesis otherwise insert a % (like vi !;-P )
(global-set-key "%" 'match-paren)
(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
	((looking-at "\\s\)") (forward-char 1) (backward-list 1))
	(t (self-insert-command (or arg 1)))))

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
;; paredit is great, but this is a useful aide-memoir to learn it
(eval-after-load 'paredit
  '(progn 
     (diminish 'paredit-mode "Ⓟ")
     (require 'paredit-menu)))

(require 'eldoc) ; if not already loaded
(eldoc-add-command
 'paredit-backward-delete
 'paredit-close-round)

(defvar electrify-return-match
  "[\]}\)\"]"
  "If this regexp matches the text after the cursor, do an \"electric\"
  return.")

(defun electrify-return-if-match (arg)
  "If the text after the cursor matches `electrify-return-match' then
  open and indent an empty line between the cursor and the text.  Move the
  cursor to the new line."
  (interactive "P")
  (let ((case-fold-search nil))
    (if (looking-at electrify-return-match)
	(save-excursion (newline-and-indent)))
    (newline arg)
    (indent-according-to-mode)))

;; Using local-set-key in a mode-hook is a better idea.
(global-set-key (kbd "RET") 'electrify-return-if-match)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MODES & HOOKS & other useful stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; v. useful: M-.         -> jump to definition of identifier at point
;;            M-,         -> jump back.
;;            C-c C-d d   -> elisp-slime-nav-describe-elisp-thing-at-point
;;            C-c C-d C-d -> (ditto)
(require 'elisp-slime-nav)
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'turn-on-elisp-slime-nav-mode))

;; Create a menu from matched section headers in this file. Bound to a local 
;; key (C-*) at end of file.
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (when (string= (buffer-name) "init.el")
              (setq imenu-generic-expression
                    '((nil "^;\\{70,\\}\n;;; \\(.+\\)" 1))))))


;; Keep a list of recently opened files (kept across sessions, too!)
(require 'recentf)
(recentf-mode 1)
(savehist-mode 1)

(eval-after-load "info" '(require 'info+))

;; EXtra modes for editing various generic UNIX specific files:
(require 'generic-x)

(autoload 'xrdb-mode "xrdb-mode" "Mode for editing X resource files" t)
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)

(setq auto-mode-alist
      (append '(("\\.Xdefaults$"    . xrdb-mode)
		("\\.Xenvironment$" . xrdb-mode)
		("\\.Xresources$"   . xrdb-mode)
		("*.\\.ad$"	    . xrdb-mode)
                ("\.lua$"           . lua-mode)
                ("/rfc[0-9]+\\.txt\\'" . rfcview-mode)
                )
	      auto-mode-alist))

(define-generic-mode 'xmodmap-mode
  '(?!)
  '("add" "clear" "keycode" "keysym" "pointer" "remove")
  nil
  '("[xX]modmap\\(rc\\)?\\'")
  nil
  "Simple mode for xmodmap files.")

(add-hook 'find-file-hooks 'auto-insert)

;; Automatic opening of zipped files.
(auto-compression-mode 1)

;; Add Time-stamp <> or Time-stamp " " anywhere in the top 8 lines of a
;; file to insert save date and time and user:
(add-hook 'write-file-hooks 'time-stamp)

;; Calendar, diary, and todo-modes...
(autoload 'todo-mode "todo-mode"
  "Major mode for editing TODO lists." t)
(autoload 'todo-show "todo-mode"
  "Show TODO items." t)
(autoload 'todo-insert-item "todo-mode"
  "Add TODO item." t)

(global-set-key "\C-ct" 'todo-show) ;; switch to TODO buffer
(global-set-key "\C-ci" 'todo-insert-item) ;; insert new item

;; Mode for viewing FAQ's...
(autoload 'faq-mode "faq-mode"
  "Major mode for reading faq files." t)
(autoload 'rfcview-mode "rfcview" nil t)

;; TeX support
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

;; Support .md files, as used on github and elsewhere
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(autoload 'gfm-mode "markdown-mode"
   "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))


;; A few variables which affect the *shell* (emacs terminal) in a window
;; (also includes the general 'comint' = COMMand INTerpreter functions).
(setq shell-prompt-pattern "^[^#$%>\n]*[#$%>] *")

;; Shell-mode hooks:

(add-hook 'comint-output-filter-functions
	  'comint-strip-ctrl-m)

(add-hook 'comint-output-filter-functions
	  'comint-truncate-buffer)

(add-hook 'shell-mode-hook
	  'ansi-color-for-comint-mode-on)

(setq comint-scroll-to-bottom-on-input 't
      comint-scroll-show-maximum-output 't
      comint-scroll-to-bottom-on-output 'all
      comint-input-ignoredups 't)

(define-key comint-mode-map [up] 'comint-previous-input)
(define-key comint-mode-map [down] 'comint-next-input)

;; Support Arch Linux PKGBUILD
(autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
(setq auto-mode-alist (append '(("/PKGBUILD$" . pkgbuild-mode)) auto-mode-alist))

;; header creation & update (autoloaded via package-initialise)
(autoload 'auto-update-file-header "header2")
(add-hook 'write-file-hooks 'auto-update-file-header)
(autoload 'auto-make-header "header2")
(add-hook 'emacs-lisp-mode-hook 'auto-make-header)
(add-hook 'c-mode-common-hook   'auto-make-header)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Programming: Haskell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Make Emacs look in Cabal directory for binaries
(let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
  (setenv "PATH" (concat my-cabal-path ":" (getenv "PATH")))
  (add-to-list 'exec-path my-cabal-path))

(setq haskell-process-auto-import-loaded-modules t
      haskell-process-log t
      haskell-process-suggest-remove-import-lines t
      haskell-process-type (quote cabal-repl)
      haskell-tags-on-save t)

; Choose indentation mode
;; Use haskell-mode indentation
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;; Use hi2
;(require 'hi2)
;(add-hook 'haskell-mode-hook 'turn-on-hi2)
;; Use structured-haskell-mode
;(add-hook 'haskell-mode-hook 'structured-haskell-mode)

; Add F8 key combination for going to imports block
(eval-after-load 'haskell-mode
  '(define-key haskell-mode-map [f8] 'haskell-navigate-imports))

; Add key combinations for interactive haskell-mode
(eval-after-load 'haskell-mode
  '(progn
     (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
     (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
     (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
     (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
     (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
     (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)))
(eval-after-load 'haskell-cabal
  '(progn
     (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
     (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
     (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
     (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))

(eval-after-load 'haskell-mode
  '(define-key haskell-mode-map (kbd "C-c C-o") 'haskell-compile))
(eval-after-load 'haskell-cabal
  '(define-key haskell-cabal-mode-map (kbd "C-c C-o") 'haskell-compile))

;; GHC-MOD
;; -------

(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Programming: C/C++ modes.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use Gnu Coding Standards.
;; RET now works as C-j to re-indent & indent in C/C++ and related modes.
(require 'cc-mode)
(add-hook 'c-mode-common-hook
          (lambda ()
	    (c-set-style "bsd")
            (define-key c-mode-base-map
	      "\C-m" 'c-context-line-break)))

;; could add a (local-set-key [return] 'reindent-then-newline-and-indent) too
(setq c-default-style "bsd")
(setq cperl-hairy t)

(require 'ggtags)
(add-hook 'c-mode-common-hook
          (lambda ()
	    (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
	      (ggtags-mode 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rs-info: Enhancements to info (esp. with Gnus) by Reiner Steib
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'rs-info-insert-current-node "rs-info"
  "Insert reference to current Info node using STYPE in buffer." t nil)
(autoload 'rs-info-boxquote "rs-info"
  "Yank text (from an info node), box it and use current info node as title."
  t nil)
(autoload 'rs-info-reload "rs-info" "Reload current info node." t nil)
(autoload 'rs-info-insert-node-for-variable "rs-info"
  "Insert a custom style info node for the top level form at point." t nil)
(defalias 'boxquote-info 'rs-info-boxquote)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GENERAL: Menu Interface/Mouse related
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Disable menubar and toolbar on the console, enable menu under X.
(menu-bar-mode 1)
(setq window-system-default-frame-alist
      '((x (menu-bar-lines . 1) (tool-bar-lines . 0))
        (nil (menu-bar-lines . 0) (tool-bar-lines . 0))))

(scroll-bar-mode nil)

;; for use in xterm
(require 'xt-mouse)
(unless window-system (xterm-mouse-mode 1))

;; Replace yes/no+enter prompts with y/n prompts
(fset 'yes-or-no-p 'y-or-n-p)

;; Emacs 22+ - revert space completing filenames in minibuffer (see FAQ)
(define-key minibuffer-local-filename-completion-map (kbd "SPC")
  'minibuffer-complete-word)

(put 'narrow-to-page 'disabled nil)

(require 'diminish)

(diminish 'auto-revert-mode)
(diminish 'which-key-mode "Ⓦ")
(diminish 'paredit-mode "Ⓟ")
(diminish 'company-mode)
(diminish 'elisp-slime-nav-mode "Ⓢ")
(diminish 'helm-mode "Ⓗ")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mule and UNICODE Support :: not needed, generally. (See earlier revs).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; but this is useful to define...
;; Next ln equiv: ISO 2022 based 8-bit encoding for Latin-1 (MIME:ISO-8859-1)
(set-language-environment "English")
(setq system-time-locale "Europe/London")
(prefer-coding-system 'utf-8)

(setq initial-scratch-message (concat initial-scratch-message
";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n\
;; This Emacs is Powered by \`HELM'\n\
;; Some originals emacs commands have been replaced by own \`helm' commands:\n\n\
;; - \`find-file'(C-x C-f)            =>\`helm-find-files'\n\
;; - \`occur'(M-s o)                  =>\`helm-occur'\n\
;; - \`list-buffers'(C-x C-b)         =>\`helm-buffers-list'\n\
;; - \`completion-at-point'(M-tab)    =>\`helm-lisp-completion-at-point'[1]\n\
;; - \`dabbrev-expand'(M-/)           =>\`helm-dabbrev'\n\n\
;; - \`execute-extended-command'(M-x) =>\`helm-M-x'\n\n
;; Some others native emacs commands are \"helmized\" by \`helm-mode'.\n\
;; [1] Coming with emacs-24.4 \`completion-at-point' is \"helmized\" by \`helm-mode'\n\
;; which provide helm completion in many other places like \`shell-mode'.\n\
;; You will find embeded help for most helm commands with \`C-h m'.\n\
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n\n"))

;; Local Variables:
;; eval: (local-set-key (kbd "C-*") 'imenu)
;; End:
;;
;; init.el ends here

