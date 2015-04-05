;;; Paths
;;--------------------------------------------------------------
(let ((default-directory "~/.emacs.d/site-lisp"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "https://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))

(setq backup-directory-alist '((".*" . "~/.emacs.d/.temp")))

;;; Load
;;---------------------------------------------------------------------------------------
(load "org-mode-confs.el")

;;; Autoload
;;---------------------------------------------------------------------------------------
(autoload 'enable-paredit-mode "paredit")

;;; Modes
;;--------------------------------------------------------------
(with-eval-after-load "persp-mode-autoloads"
  (setq wg-morph-on nil)
  (add-hook 'after-init-hook #'(lambda ()
				 (persp-mode 1))))
(column-number-mode)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(show-paren-mode)
(fringe-mode 0)

;;; Setqs
;;--------------------------------------------------------------
(setq c-default-style "linux")
(setq comment-style 'multi-line)
(setq comment-style 'extra-line)
(setq tramp-default-method "ssh")
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
(display-battery-mode)

;;; Functions
;;--------------------------------------------------------------
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

(defun install-required-package (pkg)
  (if (null (package-installed-p pkg))
      (package-install pkg)))

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

(defun uncamelcase ()
  (interactive)
  (progn (replace-regexp "\\([A-Z]\\)" "_\\1" nil (region-beginning)(region-end))
	 (downcase-region (region-beginning)(region-end))))

(defun unix2dos ()
  "Convert EOLs from unix to dos"
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\n" nil t) (replace-match "\r\n")))

;;; Keybindings
;;--------------------------------------------------------------
(global-set-key (kbd "C-x p p") 'persp-switch)
(global-set-key (kbd "C-x p a") 'persp-add-buffer)
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
(global-set-key (vector (list 'control mouse-wheel-down-event)) 'zoom-frm-in)
(global-set-key (vector (list 'control mouse-wheel-up-event))   'zoom-frm-out)
(global-set-key (kbd "C-c C-z") 'zoom-frm-unzoom)
(global-set-key (kbd "C-c C-e") 'eval-and-replace)
(global-set-key [(control h)] 'delete-backward-char)
(global-set-key [(super h)] 'help-command)
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "M-j")
		(lambda ()
		  (interactive)
		  (join-line -1)))

;;; Hooks
;;---------------------------------------------------------------------------------------
(add-hook 'after-init-hook #'(lambda ()
			       (package-initialize)
			       (install-required-package 'persp-mode)
			       (install-required-package 'multiple-cursors)
			       (install-required-package 'expand-region)))

;;; Advices
;;---------------------------------------------------------------------------------------


;;; Custom
;;--------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tango-dark)))
 '(emacs-lisp-mode-hook (quote (enable-paredit-mode)))
 '(ido-ignore-buffers (quote ("\\` " "^\\*")))
 '(ido-mode (quote buffer) nil (ido))
 '(lisp-mode-hook (quote (enable-paredit-mode)))
 '(persp-auto-resume-time 0.1)
 '(persp-auto-save-opt 2)
 '(persp-filter-save-buffers-functions
   (quote
    ((lambda (b) (string-prefix-p " " (buffer-name b)))
     (lambda (b) (string-prefix-p "*" (buffer-name b)))
     (lambda (b) (string-prefix-p "/ssh" (buffer-name b))))))
 '(read-file-name-completion-ignore-case t)
 '(tab-width 8)
 '(web-mode-extra-snippets (quote ((nil ("slide" "<section>
" . "
</section>"))))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-hide ((t (:foreground "gray20"))))
 '(sh-heredoc ((t (:foreground "gray" :weight bold)))))

;;; EOF
;;=======================================================================================
