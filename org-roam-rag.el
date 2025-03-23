;;; org-roam-rag.el --- RAG over Org Roam -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Hiroyuki Yamada

;; Author: Hiroyuki Yamada
;; Created: 2025-03-20
;; Package-Version: 1.0.0
;; Package-Requires: (llm markdown-mode org-roam)


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
(require 'ox-md)


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


(defcustom orr-response-buffer-name
  "*org-roam-rag*" "Buffer name of LLM response."
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
  :type '(integer)
  :group 'org-roam-rag)


(defun orr--make-llm-prompt (prompt)
  "Make LLM prompt from PROMPT."
  (if (fboundp 'llm-make-chat-prompt)
      (llm-make-chat-prompt prompt :context orr-llm-system-prompt)
    (make-llm-chat-prompt
     :context orr-system-prompt
     :interactions
     (list (make-llm-chat-prompt-interaction :role 'user :content prompt)))))


(defun orr--response-buffer ()
  "Create and display org-roam-rag response buffer."
  (let ((buffer (generate-new-buffer orr-response-buffer-name)))
    (save-excursion
      (with-current-buffer buffer
        (display-buffer buffer)
        (markdown-mode)))
    buffer))

(defun orr--show-response-streaming (buffer response)
  "Show LLM (partial) RESPONSE at specified BUFFER."
  (save-excursion
    (with-current-buffer buffer
      (erase-buffer)
      (insert response))))

(defun orr--chat-streaming (prompt)
  "Chat with LLM streaming using PROMPT."
  (let* ((buffer (orr--response-buffer))
         (callback (lambda (response)
                     (orr--show-response-streaming buffer response)))
		 (error-callback
		  (lambda (error-symbol msg) (error "Error %1$s: %2$s" error-symbol msg))))
    (llm-chat-streaming orr-llm-provider
                        (orr--make-llm-prompt prompt)
                        callback callback error-callback)))

(defun orr--query-db (query)
  "Query db with QUERY."
  (let* ((q (if (length< query 100)
				(cons "-s" query)
			  (cons "-f" (let* ((tmp (make-temp-file "orr-query" nil ".sql"
													 (concat query "\n"))))
						   (message "Write Temporary File: %s" tmp)
						   tmp))))
		 (out (process-lines orr-duckdb-executable
							 "-noheader" "-column" (car q) (cdr q)
							 orr-duckdb-file)))
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
  (let* ((e (mapconcat
             (lambda (d) (format "%s" d))
             (llm-embedding orr-llm-provider text) ",")))
    (concat "[" e "]")))

(defun orr--node-to-string (node)
  "Convert NODE to markdown string."
  (let* ((orig org-export-with-broken-links)
		 (m (org-roam-node-marker node))
		 (buffer (marker-buffer m))
		 (text (with-current-buffer buffer
				 (setq org-export-with-broken-links t)
				 (goto-char m)
				 (unless (= 1 (point)) (org-narrow-to-subtree))
				 (org-export-as 'md))))
	(setq org-export-with-broken-links orig)
	(unless (buffer-modified-p buffer) (kill-buffer buffer))
	text))

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

(defun orr--create-retrieve-query (embedding)
  "Create retrieve query from EMBEDDING."
  (format
   "WITH similarity AS
(SELECT array_cosine_distance(\"embedding\", %1$s) AS \"similarity\" FROM embedding)
SELECT \"id\" FROM similarity ORDER BY \"similarity\" LIMITS %2$d;"
   embedding orr-top-contexts))

(defun orr--retrieve (question)
  "Retrieve contexts for QUESTION."
  (let* ((embedding (orr--embedding question))
         (query (orr--create-retrieve-query embedding))
         (ids (orr--query-db query)))
    (mapconcat
     (lambda (id) (orr--node-to-string (org-roam-node-from-id id))
       ids "\n\n----\n\n"))))

(defun orr--ask (question)
  "Ask QUESTION to LLM."
  (let* ((contexts (orr--retrieve question))
         (prompt (format orr-llm-user-prompt question contexts)))
    orr--chat-streaming prompt))

(defun orr-ask (question)
  "Ask QUESTION to LLM based on Org Roam."
  (interactive "sQuestion: ")
  (orr--ask question))

(defun orr-ask-region (start end)
  "Ask question to LLM based on Org Roam.
Argument START is regions start.
Argument END is region end."
  (interactive "r")
  (if (use-retion-p)
      (orr--ask (buffer-substring start end))
    (message "Region is not set")))

(defun orr-ask-buffer ()
  "Ask question to LLM based on Org Roam."
  (interactive)
  (orr--ask (buffer-string)))

(provide 'org-roam-rag)
;;; org-roam-rag.el ends here
