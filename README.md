# my-espeak

Text-to-speech integration for Emacs using eSpeak and Edge TTS.

## Features

- **eSpeak Integration** - Fast, lightweight text-to-speech (F9)
- **Edge TTS Integration** - Higher quality neural voices (F10)
- **PDF Support** - Read PDF text aloud from cursor or selection
- **Smart Text Processing** - Handles hyphenated words, ligatures, and footnotes
- **Toggle Controls** - Easy start/stop with keyboard shortcuts

## Installation

### Manual Installation

1. Clone this repository:
   ```bash
   git clone https://github.com/yourusername/my-espeak.git
   ```

2. Add to your Emacs configuration:
   ```elisp
   (add-to-list 'load-path "/path/to/my-espeak")
   (require 'my-espeak)
   ```

### Dependencies

**Required:**
- `espeak` - For basic text-to-speech

**Optional:**
- `edge-tts` - For higher quality voices (install via `pip install edge-tts`)
- Audio player: `paplay`, `ffplay`, `mplayer`, `vlc`, or `aplay`

## Usage

### Basic Commands

- `F9` - Toggle eSpeak reading (fast, lightweight)
- `F10` - Toggle Edge TTS reading (higher quality)

Both commands read from cursor position or selected text in any buffer, including PDFs.

## Optional: PDF OCR-Based Reading

For multi-column PDFs where standard text extraction doesn't respect column boundaries, use the optional `pdf-ocr-espeak.el` module.

### OCR Module Requirements

The OCR functionality depends on [screenshot-ocr](https://github.com/emacselements/screenshot-ocr):

**System dependencies:**
- `maim` - Screenshot tool
- `tesseract` - OCR engine

**Installation:**

1. Install system dependencies:
   ```bash
   # Arch Linux
   sudo pacman -S maim tesseract

   # Ubuntu/Debian
   sudo apt install maim tesseract-ocr
   ```

2. Clone screenshot-ocr:
   ```bash
   git clone https://github.com/emacselements/screenshot-ocr.git
   ```

3. Load the OCR module in your config:
   ```elisp
   (require 'pdf-ocr-espeak)
   ```

### OCR Commands (in pdf-view-mode)

- `O` - Select region with mouse, OCR it, read with eSpeak
- `C-c O` - Select region with mouse, OCR it, read with Edge TTS

These commands let you visually select a region (including single columns in multi-column layouts), then OCR and read that specific area.

## Configuration

### Customize eSpeak Settings

```elisp
;; Example: Change voice speed
(defun read-aloud-from-cursor-espeak ()
  "Read aloud with custom speed."
  (interactive)
  (let ((text (read-aloud-get-text)))
    (when (and text (not (string-empty-p (string-trim text))))
      (start-process "espeak-process" nil "/usr/bin/espeak"
                     "-s" "200"  ; Adjust speed (default: 180)
                     "-v" "en"   ; Change voice/language
                     text))))
```

## How It Works

1. **Text Extraction** - Gets text from cursor/selection or entire PDF page
2. **Text Cleaning** - Joins hyphenated words, removes ligatures and footnotes
3. **Speech Synthesis** - Sends to eSpeak or Edge TTS
4. **Playback** - Reads aloud with progress tracking

## Author

Author: Raoul Comninos

## License

MIT

If you find this project helpful, consider supporting it!

[Donate via PayPal](https://www.paypal.com/paypalme/revrari)
