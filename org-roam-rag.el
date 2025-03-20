;;; org-roam-rag.el --- RAG over Org Roam -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Hiroyuki Yamada

;; Author: Hiroyuki Yamada
;; Created: 2025-03-20
;; Package-Version: 1.0.0
;; Package-Requires: (llm org-roam)


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
;; so that you need to set one of the llm providers to `org-roam-rag-llm-provider'.


;;; Code:
(require 'llm)
(require 'org-roam)


(defgroup org-roam-rag nil
  "RAG over Org Roam"
  :group 'external)

(defvar org-roam-rag-llm-provider nil
  "LLM provider for org-roam-rag.")


(defcustom org-roam-rag-llm-system-prompt
  "You are skillfull, kind, and friendly assistant.
Users will ask you questions with some context documents.
You must answer their questions based on these context documents.
These context documents are written in emacs org-mode."
  "System Prompt to guid LLM"
  :type '(string)
  :group 'org-roam-rag)


(defcustom org-roam-rag-llm-user-prompt
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
context documents retrieved at %2$s by `format' function"
  :type '(string)
  :group 'org-roam-rag)


(defun org-roam-rag--make-llm-prompt (prompt)
  "Make LLM prompt from PROMPT."
  (if (fboundp 'llm-make-chat-prompt)
      (llm-make-chat-prompt prompt :context org-roam-rag-llm-system-prompt)
    (make-llm-chat-prompt
     :context org-roam-rag-system-prompt
     :interactions
     (list (make-llm-chat-prompt-interaction :role 'user :content prompt)))))

(defcustom org-roam-rag-response-buffer-name
  "*org-roam-rag*" "Buffer name of LLM response."
  :type '(string)
  :group 'org-roam-rag)


(defun org-roam-rag--response-buffer ()
  "Create and display org-roam-rag response buffer."
  (let ((buffer (generate-new-buffer org-roam-rag-response-buffer-name)))
    (save-excursion
      (with-current-buffer buffer
        (display-buffer buffer)
        (markdown-mode)))
    buffer))

(defun org-roam-rag--show-response-streaming (buffer response)
  "Show LLM (partial) RESPONSE at specified BUFFER."
  (save-excursion
    (with-current-buffer buffer
      (erase-buffer)
      (insert response))))

(defun org-roam-rag--chat-steaming (prompt)
  "Chat with LLM streaming using PROMPT."
  (let* ((buffer (org-roam-rag--response-buffer))
         (callback #'(lambda (response)
                       (org-roam-rag--show-response-streaming buffer response))))
    (llm-chat-streaming org-roam-rag-llm-provider
                        (org-roam-rag--make-llm-prompt prompt)
                        callback callback #'ignore)))

(defcustom org-roam-rag-db-location
  (locate-user-emacs-file "org-roam-rag.duckdb")
  "The path to file where the Org-Roam-RAG database is stored.")

(defun org-roam-rag--retrieve (question)
  "Retrieve documents for QUESTION")

(defun org-roam-rag--ask (question)
  "Ask QUESTION to LLM."
  (let* ((contexts (org-roam-rag--retrieve question))
         (prompt (format org-roam-rag-llm-user-prompt question contexts)))
    org-roam-rag--chat-streaming prompt))


(provide 'org-roam-rag)
;;; org-roam-rag.el ends here
