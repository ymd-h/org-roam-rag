;;; org-roam-rag.el --- RAG over Org Roam -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Hiroyuki Yamada

;; Author: Hiroyuki Yamada
;; Created: 2025-03-20
;; Package-Version: 1.0.0
;; Package-Requires: (llm markdown-mode org-roam ox-gfm)


;; This file is not part of GNU Emacs.


;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;; This package provides RAG functionalities over org-roam files using LLM.
;; This package calls LLM through ahyatt/llm <https://github.com/ahyatt/llm>,
;; so that you need to set one of the LLM providers to `orr-llm-provider'.


;;; Code:
(require 'llm)
(require 'markdown-mode)
(require 'org-roam)
(require 'ox-gfm)


(defgroup org-roam-rag nil
  "RAG over Org Roam."
  :group 'external)


(defvar orr-llm-provider nil
  "LLM provider for org-roam-rag.")


(defcustom orr-llm-system-prompt
  "You are skillfull, kind, and friendly assistant.
Users will ask you questions with some context documents.
You must answer their questions based on these context documents."
  "System Prompt to guide LLM."
  :type '(string)
  :group 'org-roam-rag)


(defcustom orr-llm-user-prompt
  "Please answer user question based on context documents.

User Question
-------------
%1$s

Context Documents
-----------------
%2$s
"
  "Prompt Template for RAG.

User question will be inserted at %1$s,
retrieved context documents will be inserted at %2$s by `format' function."
  :type '(string)
  :group 'org-roam-rag)

(defcustom orr-show-prompt
  t "Whether to show prompt buffer.
If non-nil, show prompt."
  :type '(boolean)
  :group 'org-roam-rag)

(defcustom orr-prompt-buffer-name
  "*Org-Roam-RAG-Prompt*" "Buffer name of LLM prompt."
  :type '(string)
  :group 'org-roam-rag)


(defcustom orr-response-buffer-name
  "*Org-Roam-RAG*" "Buffer name of LLM response."
  :type '(string)
  :group 'org-roam-rag)


(defcustom orr-duckdb-file
  (locate-user-emacs-file "org-roam-rag.duckdb")
  "The path to file where the Org Roam RAG database is stored."
  :type '(string)
  :group 'org-roam-rag)


(defcustom orr-duckdb-executable
  "duckdb" "DuckDB executable."
  :type '(string)
  :group 'org-roam-rag)


(defcustom orr-top-contexts
  5 "Number of top contexts for retrieval."
  :type '(natnum)
  :group 'org-roam-rag)


(defcustom orr-forward-links
  3 "Number of forward links included in contexts."
  :type '(natnum)
  :group 'org-roam-rag)


(defcustom orr-backward-links
  2 "Number of backward links included in contexts."
  :type '(natnum)
  :group 'org-roam-rag)

(defcustom orr-batch-size
  32 "Batch size of creating  embedding."
  :type '(natnum)
  :group org-roam-rag)

(defcustom orr-debug
  nil "Debug flag."
  :type '(boolean)
  :group 'org-roam-rag)

(defun orr--make-llm-prompt (prompt)
  "Make LLM prompt from PROMPT."
  (if (fboundp 'llm-make-chat-prompt)
      (llm-make-chat-prompt prompt :context orr-llm-system-prompt)
    (make-llm-chat-prompt
     :context orr-system-prompt
     :interactions
     (list (make-llm-chat-prompt-interaction :role 'user :content prompt)))))

(defun orr--error-callback (error-symbol error-message)
  "Error callback function.
ERROR-SYMBOL and ERROR-MESSAGE will be passed to `error'."
  (error "Error %1$s: %2$2" error-symbol error-message))

(defun orr--response-buffer ()
  "Create and display org-roam-rag response buffer."
  (let ((buffer (generate-new-buffer orr-response-buffer-name)))
    (save-excursion
      (with-current-buffer buffer
        (display-buffer buffer)
        (gfm-mode)
		(insert "Waiting for LLM response...")))
    buffer))

(defun orr--chat-streaming (prompt)
  "Chat with LLM streaming using PROMPT."
  (let* ((buffer (orr--response-buffer))
         (callback (lambda (response) (with-current-buffer buffer
										(erase-buffer) (insert response))))
		 (finalize (lambda (response) (with-current-buffer buffer
										(erase-buffer) (insert response)
										(gfm-view-mode)))))
    (llm-chat-streaming orr-llm-provider
                        (orr--make-llm-prompt prompt)
                        callback finalize
						#'orr--error-callback)))

(defun orr--query-db (query)
  "Query db with QUERY."
  (when orr-debug (message "%s" query))
  (let* ((q (if (length< query 100)
				(cons "-s" query)
			  (cons "-f" (make-temp-file "orr-query-" nil ".sql"
										 (concat query "\n")))))
		 (out (process-lines orr-duckdb-executable
							 "-noheader" "-column" (car q) (cdr q)
							 orr-duckdb-file)))
	(when orr-debug (message "%s" out))
	(mapcar (lambda (line) (read (concat "(" line ")")))
			(seq-filter (lambda (line) (not (equal line ""))) out))))

(defun orr--create-embedding-table-query (embeddings)
  "Construct query from EMBEDDINGS."
  (let* ((id-col (mapconcat #'car embeddings "','"))
         (embedding-col (mapconcat #'cdr embeddings ",")))
    (format
     "CREATE OR REPLACE TABLE embedding AS
(SELECT unnest(['%1$s']) AS \"id\", unnest([%2$s]) AS \"embedding\");"
     id-col embedding-col)))

(defun orr--embedding (text)
  "Create embedding vector string for TEXT."
  (if (equal text "")
	  "null"
	(let* ((e (mapconcat
               (lambda (d) (format "%s" d))
               (llm-embedding orr-llm-provider text) ",")))
      (concat "[" e "]"))))

(defun orr--embedding-async (text callback)
  "Create embedding vector string for TEXT asynchronically.
CALLBACK is called with embedding string."
  (if (equal text "")
	  (funcall callback "null")
	(llm-embedding-async
	 orr-llm-provider text
	 (lambda (e) (funcall callback
						  (concat "[" (mapconcat (lambda (d) (format "%s" d)) e ",") "]")))
	 #'orr--error-callback)))

(defun orr--node-to-string (node)
  "Convert NODE to markdown string."
  (let* ((broken org-export-with-broken-links)
		 (toc org-export-with-toc)
		 (file (org-roam-node-file node))
		 (node-point (org-roam-node-point node))
		 (text (string-trim
				(org-roam-with-file file nil
				  (setq org-export-with-broken-links t
						org-export-with-toc nil)
				  (goto-char node-point)
				  (unless (= 1 (point)) (org-narrow-to-subtree))
				  (org-export-as 'gfm)))))
	(setq org-export-with-broken-links broken
		  org-export-with-toc toc)
	text))

;;;###autoload
(defun orr-rebuild-all-embeddings ()
  "Rebuild all embeddings in Org Roam RAG database.
This function must be called when initialization or changing embedding model."
  (interactive)
  (let* ((nodes (org-roam-node-list))
         (embeddings nil))
    (dolist-with-progress-reporter (node nodes) "Rebuild embeddings... "
      (let* ((id (org-roam-node-id node))
             (embedding (orr--embedding (orr--node-to-string node))))
        (setq embeddings (cons (cons id embedding) embeddings))))
    (orr--query-db (orr--create-embedding-table-query embeddings))))

(defun orr-initialize (&optional force)
  "Initialize Org Roam RAG.
When FORCE is not nil or `orr-duckdb-file' doesn't exists,
Initialize (or rebuild) database by calling `orr-rebuild-all-embeddings'."
  (when (or force
			(and (not (file-exists-p orr-duckdb-file))
				 (y-or-n-p "DB doesn't exist.  Initialize DB? ")))
	(orr-rebuild-all-embeddings)))

(defun orr--create-update-query (id embedding)
  "Create update query from ID and EMBEDDING."
  (format
   "DELETE FROM embedding WHERE id = '%1$s'; INSERT INTO embedding VALUES ('%1$s', %2$s);"
   id embedding))

(defun orr--update-node (node)
  "Update embedding for NODE."
  (when orr-debug
	(message "Update: %s" (org-roam-node-title node)))
  (orr--embedding-async
   (orr--node-to-string node)
   (lambda (embedding) (orr--query-db (orr--create-update-query
									   (org-roam-node-id node) embedding)))))

;;;###autoload
(defun orr-update-node-at-point ()
  "Update embedding for node at point."
  (interactive)
  (let* ((node (org-roam-node-at-point t)))
	(when node
	  (unless (org-roam-node-file node)
		;; Ensure file
		(setf (org-roam-node-file node)
			  (file-truename (buffer-file-name))))
	  (orr--update-node node))))

(defun orr--autosync-on-save ()
  "Update embedding for all nodes in the current buffer."
  (save-excursion
	(goto-char (point-min))
	(when (org-roam-db-node-p)
	  (orr-update-node-at-point))
	(org-roam-db-map-nodes
	 (list #'orr-update-node-at-point))))

(defun orr--autosync-setup ()
  "Setup the autosync if it is Org-Roam file."
  (when (org-roam-file-p)
	(add-hook 'after-save-hook #'orr--autosync-on-save nil t)))

(defun orr--enable-autosync-mode ()
  "Enable `orr-autosync-mode'."
  (add-hook 'find-file-hook #'orr--autosync-setup)
  (dolist (buf (org-roam-buffer-list))
	(with-current-buffer buf (orr--autosync-setup))))

(defun orr--disable-autosync-mode ()
  "Disable `orr-autosync-mode'."
  (remove-hook 'find-file-hook #'orr--autosync-on-save)
  (dolist (buf (org-roam-buffer-list))
	(with-current-buffer buf
	  (remove-hook 'after-save-hook #'orr--autosync-on-save t))))

;;;###autoload
(define-minor-mode orr-autosync-mode
  "Toggle orr-autosync mode.
orr-autosync mode is global minor mode to keep embedding databse updated."
  :group 'org-roam-rag
  :global t
  :init-value nil
  (if orr-autosync-mode
	  (orr--enable-autosync-mode)
	(orr--disable-autosync-mode)))

(defun orr--create-retrieve-query (embedding)
  "Create retrieve query from EMBEDDING."
  (format
   "INSTALL sqlite; LOAD sqlite; WITH
s AS (SELECT '\"' || id || '\"' AS id,
             list_cosine_similarity(embedding, %1$s) AS similarity FROM embedding),

r AS (SELECT decode(source) AS source,
             decode(dest) AS dest
      FROM sqlite_scan('%2$s', 'links') sq WHERE decode(sq.type) = '\"id\"'),

top AS (SELECT id FROM s ORDER BY similarity DESC LIMIT %3$d),

forward AS (SELECT d.id AS id FROM (SELECT dest AS id FROM r WHERE EXISTS (FROM top WHERE top.id = r.source)) d LEFT JOIN s ON (d.id = s.id) ORDER BY s.similarity DESC LIMIT %4$d),

backward AS (SELECT d.id AS id FROM (SELECT source AS id FROM r WHERE EXISTS (FROM top WHERE top.id = r.dest)) d LEFT JOIN s ON (d.id = s.id) ORDER BY s.similarity DESC LIMIT %5$d)

SELECT id FROM top UNION
SELECT id FROM forward UNION
SELECT id FROM backward;"
   embedding (file-truename org-roam-db-location)
   orr-top-contexts orr-forward-links orr-backward-links))

(defun orr--retrieve (question)
  "Retrieve contexts for QUESTION."
  (let* ((embedding (orr--embedding question))
         (query (orr--create-retrieve-query embedding))
         (ids (orr--query-db query)))
    (mapconcat
	 #'identity
	 (seq-filter
	  (lambda (text) (not (equal text "")))
	  (mapcar
       (lambda (id) (orr--node-to-string (org-roam-node-from-id (nth 0 id))))
	   ids))
	 "\n\n----\n\n")))

(defun orr--ask (question)
  "Ask QUESTION to LLM."
  (let* ((contexts (orr--retrieve question))
         (prompt (format orr-llm-user-prompt question contexts)))
	(when orr-show-prompt
	  (pop-to-buffer (generate-new-buffer orr-prompt-buffer-name))
	  (insert prompt)
	  (gfm-view-mode))
    (orr--chat-streaming prompt)))

;;;###autoload
(defun orr-ask (question)
  "Ask QUESTION to LLM based on Org Roam."
  (interactive "sQuestion: ")
  (orr--ask question))

;;;###autoload
(defun orr-ask-region (start end)
  "Ask question to LLM based on Org Roam.
Argument START is regions start.
Argument END is region end."
  (interactive "r")
  (if (use-retion-p)
      (orr--ask (buffer-substring start end))
    (message "Region is not set")))

;;;###autoload
(defun orr-ask-buffer ()
  "Ask question to LLM based on Org Roam."
  (interactive)
  (orr--ask (buffer-string)))

(provide 'org-roam-rag)
;;; org-roam-rag.el ends here
