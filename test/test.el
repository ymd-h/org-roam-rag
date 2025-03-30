(when (featurep 'org-roam-rag)
  (unload-feature 'org-roam-rag t))

(use-package org-roam-rag
  :load-path
  (lambda ()
	(directory-file-name
	 (file-name-directory
	  (directory-file-name
	   (file-name-directory
		(file-truename (buffer-file-name)))))))
  :config
  (setq orr-llm-provider
		(make-llm-ollama
		 :chat-model "gemma3:4b"
		 :embedding-model "nomic-embed-text"))
  (orr-initialize))

(orr-rebuild-all-embeddings)
