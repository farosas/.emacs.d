(let ((default-directory "~/.emacs.d/site-lisp"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(let ((default-directory "~/.emacs.d/elpa"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(setq backup-directory-alist '((".*" . "~/.emacs.d/.temp")))

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))

(with-eval-after-load "persp-mode-autoloads"
  (setq wg-morph-on nil)
  (add-hook 'after-init-hook #'(lambda ()
                                 (progn
				   (require 'tramp)
                                   (persp-mode 1)
                                   (setq persp-add-buffer-on-after-change-major-mode t)))))

(setq pkgs-to-install '(paredit persp-mode multiple-cursors expand-region imenu-list eglot))

(package-initialize)
(setq package-enable-at-startup nil)

(unless package-archive-contents
  (package-refresh-contents))

(mapc (lambda (pkg)
	(unless (package-installed-p pkg)
	    (progn
	      (message "Installing %s" pkg)
	      (package-install pkg))))
      pkgs-to-install)


(defun c-mode-disables ()
  (auto-revert-mode -1)
  (flymake-mode -1)
  (eldoc-mode -1)
  (cscope-minor-mode)
  (abbrev-mode -1))

(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'c-mode-hook 'c-mode-disables)
(add-hook 'find-file-hook 'c-mode-disables)

(load "org-mode-confs.el")

(column-number-mode)
(if (display-graphic-p)
    (progn
      (scroll-bar-mode -1)
      (tool-bar-mode -1)
      (fringe-mode 0)
      )
    )
(menu-bar-mode -1)
(show-paren-mode)

(global-auto-revert-mode -1)
(global-eldoc-mode -1)
(global-subword-mode 1)

(setq auto-save-default nil)
(setq-default truncate-lines t)
(set-display-table-slot standard-display-table 'truncation ? )
(setq c-default-style "linux")
(setq comment-style 'multi-line)
(setq comment-style 'extra-line)
(setq tramp-default-method "ssh")
(setq backup-enable-predicate
      (lambda (name)
	(not (file-remote-p name))))
(setq tramp-ssh-controlmaster-options
      (concat
       "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p "
       "-o ControlMaster=auto -o ControlPersist=no"))

(setq tramp-verbose 1)
(setq persp-is-ibc-as-f-supported t)
(add-to-list 'default-frame-alist '(font . "monospace-12"))
(add-to-list 'default-frame-alist '(fullscreen . fullboth))
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(setq next-line-add-newlines nil)
(setq inhibit-startup-screen t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq display-time-default-load-average nil)
(setq display-time-24hr-format t)
(setq display-time-format nil)
(display-time-mode)
(setq scroll-step 1)
(setq hscroll-margin 1)
(setq hscroll-step 1)
(setq groovy-indent-offset 2)
(setq etags-table-search-up-depth 10)
(setq Buffer-menu-name-width 40)

(setq clean-buffer-list-kill-regexps '(
				       ("\\*Customize .*\\*" . 0)
				       ("\\*Annotate .*\\*" . 0)
				       ("\\*Completions.*\\*\\'" . 0)
				       ("\\*vc-change.*" . 0)
				       ("\\*unsent mail.*" . 0)
				       ))

;; macros
(fset 'breakpoint
      (lambda (&optional arg) "Keyboard macro."
	(interactive "p")
	(kmacro-exec-ring-item
	 (quote ([15 105 109 112 111 114 116 32 112 100 98 59 32 112 100 98 46 115 101 116 95 116 114 97 99 101 40 41 1] 0 "%d"))
	 arg)))

(fset 'pr
      (kmacro-lambda-form [?p ?r ?i ?n ?t ?k ?\( ?K ?E ?R ?N ?_ ?E ?R ?R ?  ?\" ?% ?s ?: ?  ?\\ ?n ?\" ?, ?  ?_ ?_ ?f ?u ?n ?c ?_ ?_ ?\) ?\; ?\M-b ?\M-b ?\C-b] 0 "%d"))

;; functions
(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
		      default-directory
		    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
	(buffer (current-buffer))
	(name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
	(ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
	(delete-file filename)
	(kill-buffer buffer)
	(message "File '%s' successfully removed" filename)))))

(defun dos2unix ()
  "Convert EOLs from dos to unix"
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (prin1 (eval (read (current-kill 0)))
	 (current-buffer)))

(defun insert-date ()
  "Insert date at point."
  (insert (format-time-string "%d/%m/%y-%R")))

(defun hex()
  (interactive)
  (print (format "0x%x" (thing-at-point 'number))))

(defun mrc-dired-do-command (command)
  "Run COMMAND on marked files. Any files not already open will be opened.
After this command has been run, any buffers it's modified will remain
open and unsaved."
  (interactive "CRun on marked files M-x ")
  (save-window-excursion
    (mapc (lambda (filename)
	    (find-file filename)
	    (call-interactively command))
	  (dired-get-marked-files))))

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
	(filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
	(error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
	(if (get-buffer new-name)
	    (error "A buffer named '%s' already exists!" new-name)
	  (rename-file filename new-name 1)
	  (rename-buffer new-name)
	  (set-visited-file-name new-name)
	  (set-buffer-modified-p nil)
	  (message "File '%s' successfully renamed to '%s'"
		   name (file-name-nondirectory new-name)))))))

(defun smart-tab ()
  ;; TODO: make this a list of functions
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
      (if mark-active
	  (indent-region (region-beginning)
			 (region-end))
	(if (and (bound-and-true-p go-mode)
		 (bound-and-true-p auto-complete-mode))
	    (ac-complete-go)
	  (if (looking-at "\\_>")
	      (dabbrev-expand nil)
	    (indent-for-tab-command))))))

(defun uncamelcase ()
  (interactive)
  (progn (replace-regexp "\\([A-Z]\\)" "_\\1" nil (region-beginning)(region-end))
	 (downcase-region (region-beginning)(region-end))))

(defun unix2dos ()
  "Convert EOLs from unix to dos"
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\n" nil t) (replace-match "\r\n")))

(defun repo-root (dir)
  (if (string= "/" (if (tramp-tramp-file-p default-directory)
		       (tramp-file-name-localname (tramp-dissect-file-name dir))
		     dir))
      default-directory
    (if (file-exists-p (expand-file-name ".git/" dir))
        dir
      (repo-root (expand-file-name "../" dir)))))

(defun isearch-with-region ()
  "Use region as the isearch text."
  (when mark-active
    (let ((region (funcall region-extract-function nil)))
      (goto-char (region-end))
      (deactivate-mark)
      (isearch-update)
      (isearch-yank-string region))))
(add-hook 'isearch-mode-hook #'isearch-with-region)

(require 'grep)
(add-to-list 'grep-find-ignored-directories ".ccls-cache")
(add-to-list 'grep-find-ignored-directories  "build")
(setq grep-save-buffers nil)
(defun fgrep ()
  (interactive)
  (grep-compute-defaults)
  (rgrep (grep-read-regexp) "*.[chS]*" (repo-root default-directory)))

(defalias 'grep 'fgrep)

;; key bindings
(global-set-key (kbd "C-x p p") 'persp-switch)
(global-set-key (kbd "C-x p a") 'persp-add-buffer)
(global-set-key (kbd "C-M-n") 'persp-next)
(global-set-key (kbd "C-M-p") 'persp-prev)
(global-set-key (kbd "<C-up>") 'shrink-window)
(global-set-key (kbd "<C-down>") 'enlarge-window)
(global-set-key (kbd "<C-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)
(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)
(global-set-key (kbd "C-.") 'mc/mark-next-like-this)
(global-set-key (kbd "C-,") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c mi") 'mc/insert-numbers)
(global-set-key (kbd "C-c rr") 'mc/reverse-regions)
(global-set-key (kbd "C-=") 'er/expand-region)
(if (display-graphic-p)
    (progn
      (global-set-key (vector (list 'control mouse-wheel-down-event)) 'zoom-frm-in)
      (global-set-key (vector (list 'control mouse-wheel-up-event))   'zoom-frm-out)
      (global-set-key (kbd "C-c C-z") 'zoom-frm-unzoom)))
(global-set-key (kbd "C-c C-e") 'eval-and-replace)
(global-set-key [(control h)] 'delete-backward-char)
(global-set-key [(super h)] 'help-command)
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "M-j")
		(lambda ()
		  (interactive)
		  (join-line -1)
		  (beginning-of-line)))
(global-set-key [(tab)] 'smart-tab)
(global-set-key (kbd "C-M-.") 'xref-find-definitions-other-window)
(global-set-key (kbd "C--") 'isearch-forward-symbol-at-point)
(global-set-key (kbd "<f5>") '(lambda () (interactive) (revert-buffer nil t)))
(global-set-key (kbd "C-M-m") 'show-marks)
(global-set-key (kbd "C-x z") 'zoom-window-zoom)
(global-set-key (kbd "C-x x") 'point-to-register)
(global-set-key (kbd "C-x j") 'jump-to-register)
(global-set-key (kbd "M-`") 'other-frame)


(defun tags-xref-asm (identifier)
  (interactive (list (xref--read-identifier "Find asm definition of: ")))
  (progn
    (xref-etags-mode)
    (xref--find-definitions identifier nil)))

(defun tags-xref-asm-return-hook ()
  (let ((mode (buffer-local-value 'major-mode (current-buffer))))
    (if (and (not xref-etags-mode--saved) (string= mode "c-mode"))
	(xref-etags-mode))))

(require 'xref)
;;(add-to-list 'xref-after-return-hook 'tags-xref-asm-return-hook)

(global-set-key (kbd "M-s M-.") 'tags-xref-asm)

;; for eglot
(require 'project)
(require 'eglot)
(with-eval-after-load 'eglot
  (add-to-list 'eglot-stay-out-of 'imenu)
  (add-to-list 'eglot-stay-out-of 'eldoc)
  (add-to-list 'eglot-stay-out-of 'flymake))

(defun project-root (project)
  (car (project-roots project)))

(defun project-name ()
  (project-root (project-current)))

;; ;; eglot iterates over buffer-list and some of the function it calls
;; ;; hang on tramp buffers
;; ;; (defun filter-tramp-buffers (l)
;; ;;   (mapcar
;; ;;    (lambda (x)
;; ;;      (with-current-buffer x (when (not (tramp-tramp-file-p default-directory)) x))
;; ;;      )
;; ;;    l))
;; ;;

;; per-project xref marker stack
(setq project-xref-marker-alist '())
(setq xref--marker-ring nil)

(defun set-project-xref-marker-ring (&optional a b c)
  (add-to-list 'project-xref-marker-alist
	       `(,(project-name) . ,(make-ring xref-marker-ring-length))
	       nil
	       #'(lambda (elem1 elem2) (equal (car elem1) (car elem2))))
  (setq xref--marker-ring
	(cdr (assoc (project-name) project-xref-marker-alist))))

;;(advice-add 'switch-to-buffer :after #'set-project-xref-marker-ring)
(add-hook 'find-file-hook 'set-project-xref-marker-ring)
(add-hook 'window-configuration-change-hook 'set-project-xref-marker-ring)
;;(remove-hook 'window-configuration-change-hook 'set-project-xref-marker-ring)
;;

;; {write,find}-alternate-file-other-path
(require 's)
(setq alternate-file-local-path "~/kvm/")
(setq alternate-file-remote-path "/ssh::")

(defmacro alternate-file-interactive (str)
  `(interactive
    (let* ((local-path alternate-file-local-path)
	   (remote-path alternate-file-remote-path)
	   (other-path (if (tramp-tramp-file-p default-directory)
			   local-path remote-path)))
      (list (read-from-minibuffer (concat ,str " alternate path: ") other-path nil nil other-path nil)))))

(defun find-alternate-file-other-path (otherpath)
  (alternate-file-interactive "Find")
  (alternate-file-other-path #'find-file otherpath))

(defun write-alternate-file-other-path (otherpath)
  (alternate-file-interactive "Write")
  (alternate-file-other-path #'write-file otherpath))

(defvar point-pos nil)
(defun update-point ()
  (interactive)
  (goto-char point-pos))
(global-set-key (kbd "C-M-g") 'update-point)

(defun alternate-file-other-path (fn otherpath)
  (setq point-pos (point))
  (let* ((root-path (if (tramp-tramp-file-p default-directory)
			(repo-root default-directory)
		      (projectile-project-root default-directory)))
	 (root-dir (directory-file-name root-path))
	 (root-dir-name (file-name-nondirectory root-dir)))
    (funcall fn (s-replace root-dir (concat otherpath root-dir-name) buffer-file-name))))

(global-set-key (kbd "C-x v") 'find-alternate-file-other-path)
(global-set-key (kbd "C-x w") 'write-alternate-file-other-path)
;;

;; for ido
;; This function is slow. Can't be bothered.
(defun ido-ignore-item-p (name re-list &optional ignore-ext) 0)

;; ------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(tango-dark))
 '(describe-char-unidata-list
   '(name old-name general-category canonical-combining-class bidi-class decomposition decimal-digit-value digit-value numeric-value mirrored iso-10646-comment uppercase lowercase titlecase))
 '(dtrt-indent-mode t nil (dtrt-indent))
 '(etags-table-search-up-depth 15)
 '(ido-ignore-buffers '("\\` " "^\\*"))
 '(ido-mode 'buffer nil (ido))
 '(imenu-list-size 0.17)
 '(package-selected-packages
   '(ccls dash-functional lsp-mode zoom-window markdown-mode+ markdown-preview-mode go-mode show-marks gnu-elpa-keyring-update ansi shut-up epl git commander f systemtap-mode imenu-list htmlize dash s cask xcscope etags-table ggtags auto-complete dtrt-indent yaml-mode zoom-frm expand-region multiple-cursors persp-mode paredit))
 '(persp-auto-resume-time 0.1)
 '(persp-auto-save-opt 2)
 '(persp-filter-save-buffers-functions
   '((lambda
       (b)
       (string-prefix-p " "
			(buffer-name b)))
     (lambda
       (b)
       (string-prefix-p "*"
			(buffer-name b)))
     (lambda
       (b)
       (string-prefix-p "/ssh"
			(buffer-name b)))))
 '(persp-set-ido-hooks t)
 '(read-file-name-completion-ignore-case t)
 '(safe-local-variable-values
   '(`(xref--marker-ring \,
			 (project-xref-marker-ring))
     (eval when
	   (fboundp 'c-toggle-comment-style)
	   (c-toggle-comment-style 1))
     (eval c-set-offset 'innamespace 0)
     (c-indent-level . 4)))
 '(tab-stop-list '(4 8 12 16 20 24 28 32 36))
 '(tab-width 8)
 '(vc-git-print-log-follow t)
 '(web-mode-extra-snippets '((nil ("slide" "<section>
" . "
</section>"))))
 '(zoom-window-use-persp t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eglot-mode-line ((t nil)))
 '(highlight ((t (:background "gray20" :foreground "white"))))
 '(org-hide ((t (:foreground "gray20"))))
 '(sh-heredoc ((t (:foreground "gray" :weight bold)))))
