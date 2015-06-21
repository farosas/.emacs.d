;; Task managements settings
(defvar tasks-file
  (quote "~/todo/tasks.org"))

;; Show blocked items in a dimmed font
(setq org-agenda-dim-blocked-tasks t)

(setq org-clock-x11idle-program-name "xprintidle")
(setq org-clock-idle-time nil)

(setq org-log-into-drawer t)
(setq org-clock-into-drawer t)

(defun open-tasks-file ()
  (interactive)
  (find-file tasks-file))

(defun open-org-log ()
  (interactive)
  (org-agenda-list)
  (org-agenda-log-mode)
  (org-agenda-week-view))

(defun toggle-tasks-file ()
  (interactive)
  (setq buf (buffer-name))
  (if (equal buf "tasks.org")
	  (if (windowp (window-parent))
		  (delete-window)
		(switch-to-buffer (other-buffer (current-buffer) t)))
	(split-window)
	(open-tasks-file)))

(defun toggle-agenda-file (fun)
  (setq buf (buffer-name))
  (if (equal buf "*Org Agenda*")
	  (progn
		(if (windowp (window-parent))
			(delete-window))
		(kill-buffer buf))
	(split-window)
	(funcall fun)))

(global-set-key (kbd "<f5>") 'toggle-tasks-file)

(add-hook 'org-mode-hook
		  (lambda ()
			(add-to-list 'org-agenda-files tasks-file)
			(global-set-key (kbd "<f6>") (lambda () (interactive) (toggle-agenda-file 'org-agenda-list)))
			(global-set-key (kbd "<f8>") (lambda () (interactive) (toggle-agenda-file 'open-org-log)))
			(global-set-key (kbd "<f9>") 'org-clock-modify-effort-estimate)
			(local-set-key (kbd "<f11>") 'org-clock-in)
			(global-set-key (kbd "<f12>") 'org-clock-out)
			(setq org-hide-leading-stars t)))

(add-hook 'org-agenda-mode-hook
		  (lambda ()
			(local-set-key (kbd "<f11>") 'org-agenda-clock-in)
			(local-set-key (kbd "<f12>") 'org-agenda-clock-out)))
