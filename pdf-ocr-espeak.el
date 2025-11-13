;;; pdf-ocr-espeak.el --- Read PDF text aloud using OCR for column-aware selection -*- lexical-binding: t; -*-

;; Author: Raoul Comninos
;; Description: Use OCR on PDF regions to read text aloud, respecting column boundaries

;;; Commentary:
;; This provides a solution for reading multi-column PDFs aloud by using
;; OCR on the visually selected region, which naturally respects column
;; boundaries unlike pdf-tools' text extraction.

;;; Code:

(require 'pdf-view)

(defvar pdf-ocr-espeak-screenshot-file "/tmp/pdf-region-screenshot.png"
  "Temporary file for PDF region screenshots.")

(defvar pdf-ocr-espeak-text-file "/tmp/pdf-region-text"
  "Temporary file for OCR'd text output.")

(defun pdf-ocr-espeak-read-region ()
  "Take a screenshot of the current PDF window, OCR it, and read the text aloud.
This respects visual column boundaries unlike standard text extraction."
  (interactive)
  (unless (eq major-mode 'pdf-view-mode)
    (user-error "This command only works in pdf-view-mode"))

  (message "Select the region to read aloud...")

  ;; Use maim to capture a screen region
  (let* ((screenshot-file pdf-ocr-espeak-screenshot-file)
         (text-file-base pdf-ocr-espeak-text-file)
         (text-file (concat text-file-base ".txt")))

    ;; Take screenshot with region selection
    (let ((exit-code (call-process "maim" nil nil nil "-s" screenshot-file)))
      (if (not (= exit-code 0))
          (message "Screenshot cancelled or failed")

        ;; Perform OCR
        (message "Performing OCR...")
        (call-process "tesseract" nil nil nil screenshot-file text-file-base)

        ;; Check if OCR succeeded
        (if (not (file-exists-p text-file))
            (message "OCR failed")

          ;; Read the OCR'd text
          (let ((ocr-text (with-temp-buffer
                           (insert-file-contents text-file)
                           (buffer-string))))

            (if (string-empty-p (string-trim ocr-text))
                (message "No text recognized")

              ;; Clean up the text and read it aloud
              (let ((cleaned-text (read-aloud-cleanup-hyphens ocr-text)))
                (setq cleaned-text (read-aloud-normalize-text cleaned-text))
                (message "Reading OCR'd text...")

                ;; Kill any existing reading process
                (when (and read-aloud-process (process-live-p read-aloud-process))
                  (kill-process read-aloud-process))

                ;; Start eSpeak
                (setq read-aloud-process
                      (start-process "espeak-ocr" nil "/usr/bin/espeak"
                                    "-s" "180"
                                    "-v" "en"
                                    cleaned-text))

                (set-process-sentinel
                 read-aloud-process
                 (lambda (_proc _event)
                   (setq my/reading-aloud nil)))

                (setq my/reading-aloud t))))

          ;; Clean up temporary files
          (when (file-exists-p screenshot-file)
            (delete-file screenshot-file))
          (when (file-exists-p text-file)
            (delete-file text-file)))))))

(defun pdf-ocr-espeak-read-region-fancy ()
  "Take a screenshot of the current PDF window, OCR it, and read with Edge TTS.
This respects visual column boundaries unlike standard text extraction."
  (interactive)
  (unless (eq major-mode 'pdf-view-mode)
    (user-error "This command only works in pdf-view-mode"))

  (message "Select the region to read aloud...")

  ;; Use maim to capture a screen region
  (let* ((screenshot-file pdf-ocr-espeak-screenshot-file)
         (text-file-base pdf-ocr-espeak-text-file)
         (text-file (concat text-file-base ".txt")))

    ;; Take screenshot with region selection
    (let ((exit-code (call-process "maim" nil nil nil "-s" screenshot-file)))
      (if (not (= exit-code 0))
          (message "Screenshot cancelled or failed")

        ;; Perform OCR
        (message "Performing OCR...")
        (call-process "tesseract" nil nil nil screenshot-file text-file-base)

        ;; Check if OCR succeeded
        (if (not (file-exists-p text-file))
            (message "OCR failed")

          ;; Read the OCR'd text
          (let ((ocr-text (with-temp-buffer
                           (insert-file-contents text-file)
                           (buffer-string))))

            (if (string-empty-p (string-trim ocr-text))
                (message "No text recognized")

              ;; Clean up and read with Edge TTS
              (message "Reading OCR'd text with Edge TTS...")

              ;; Kill any existing reading process
              (when (and read-aloud-process (process-live-p read-aloud-process))
                (kill-process read-aloud-process))

              ;; Use the fancy Edge TTS reader
              (condition-case err
                  (read-aloud-with-edge-tts ocr-text)
                (error (message "Edge TTS failed: %s" (error-message-string err))))

              (setq my/reading-aloud t)))

          ;; Clean up temporary files
          (when (file-exists-p screenshot-file)
            (delete-file screenshot-file))
          (when (file-exists-p text-file)
            (delete-file text-file)))))))

;; Keybindings for pdf-view-mode
(with-eval-after-load 'pdf-view
  (define-key pdf-view-mode-map (kbd "O") #'pdf-ocr-espeak-read-region)
  (define-key pdf-view-mode-map (kbd "C-c o") #'pdf-ocr-espeak-read-region-fancy))

(provide 'pdf-ocr-espeak)
;;; pdf-ocr-espeak.el ends here
